raw01_tbl = readxl::read_xlsx("energy.xlsx")
library(ggplot2)
library(h20)
library(tidyquant)
library(timetk)
raw01_tbl %>% glimpse()
# Step 1
raw01_tbl %>%
  tibble::as_tibble() %>%
  group_by(Date)
raw01_tbl_aug <- raw01_tbl %>%
  tk_augment_timeseries_signature()
raw01_tbl_aug %>% glimpse()

#Step 2
raw01_tbl_aug_clean <- raw01_tbl_aug %>%
  select_if(~ !is.Date(.)) %>%
  select_if(~ !any(is.na(.))) %>%
  mutate_if(is.ordered, ~ as.character(.) %>% as.factor)
raw01_tbl_aug_clean %>% glimpse()

# Spliting
train_tbl <- raw01_tbl_aug_clean %>% filter(month < 2 )
test_tbl <- raw01_tbl_aug_clean %>% filter(month == 2)

library(h2o)
h2o.init()
h2o.no_progress()

# Converting to h2oFrame objects
train_h20 <- as.h2o(train_tbl)
test_h2o <- as.h2o(test_tbl)
y <- "kwh"
x <- setdiff(names(train_h20), y)

# model
automl_models_h2o <- h2o.automl(x = x,
                                y = y,
                                training_frame = train_h20,
                                leaderboard_frame = test_h2o,
                                max_runtime_secs = 60,
                                stopping_metric = "deviance")
# Extract leader model
automl_leader <- automl_models_h2o
@leader

# Predict
pred_h2o <- h2o.predict(automl_leader, newdata = test_h2o)

#Evaluation
h2o.performance(automl_leader, newdata =  test_h2o)

# Investigate test error
error_tbl <- raw01_tbl %>%
  filter(lubridate::year(date) == 2018) %>%
  add_column(pred = pred_h2o %>%
               as.tibble() %>%
               pull(predict)) %>%
  rename(actual = kwh) %>%
  mutate(error = actual-pred,
         error_pct = error/actual)
error_tbl