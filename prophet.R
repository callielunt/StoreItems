library(modeltime) #Extensions of tidymodels to time series1
library(timetk) #Some nice time series functions2
library(vroom)
library(tidyverse)
library(tidymodels)

train <- vroom("train.csv")
test <- vroom("test.csv")

## Read in the Data and filter to store/item1
storeItemTrain731 <- train %>%
  filter(store==7, item==31)
storeItemTest731 <- test %>%
  filter(store==7, item==31)

## Create the CV split for time series7
cv_split <- time_series_split(storeItemTrain731, assess="3 months", cumulative = TRUE) #this is a one fold split

cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame7
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

## Create a recipe for the linear model part10

## Define the prophet Model13
prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split))

# Merge into a single workflow and fit to the training data1
# DON'T Do this step
prophet_wf <- workflow() %>%
  add_model(prophet_model) %>%
  fit(data=training(cv_split))


## Calibrate (tune) the models (find p,d,q,P,D,Q)7
cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))

## Visualize results11
p1731 <- cv_results %>%
  modeltime_forecast(
    new_data    = testing(cv_split),
    actual_data = training(cv_split)
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Now that you have calibrated (tuned) refit to whole dataset19
fullfit <- cv_results %>%
  modeltime_refit(data=storeItemTrain731)

p2731 <-fullfit %>%
  modeltime_forecast(
    new_data    = storeItemTest731,
    actual_data = storeItemTrain731
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)



## Read in the Data and filter to store/item1
storeItemTrain83 <- train %>%
  filter(store==8, item==3)
storeItemTest83 <- test %>%
  filter(store==8, item==3)

## Create the CV split for time series7
cv_split <- time_series_split(storeItemTrain83, assess="3 months", cumulative = TRUE) #this is a one fold split

cv_split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame7
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)



## Define the prophet Model13
prophet_model <- prophet_model <- prophet_reg() %>%
  set_engine(engine = "prophet") %>%
  fit(sales ~ date, data = training(cv_split))

# Merge into a single workflow and fit to the training data1
# Don't do this step
# prophet_wf <- workflow() %>%
#   add_recipe(prophet_recipe) %>%
#   add_model(prophet_model) %>%
#   fit(data=training(cv_split))

# prophet_wf <- workflow() %>%
#   add_recipe(prophet_recipe) %>%
#   add_model(prophet_model) %>%
#   fit(data=training(cv_split))
cv_results <- modeltime_calibrate(prophet_model,
                                  new_data = testing(cv_split))
## Calibrate (tune) the models (find p,d,q,P,D,Q)7
#cv_results <- modeltime_calibrate(prophet_wf,
#                                  new_data = testing(cv_split))

## Visualize results11
p183 <- cv_results %>%
  modeltime_forecast(
    new_data    = testing(cv_split),
    actual_data = training(cv_split)
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

## Now that you have calibrated (tuned) refit to whole dataset19
fullfit <- cv_results %>%
  modeltime_refit(data=storeItemTrain83)

p283 <- fullfit %>%
  modeltime_forecast(
    new_data    = storeItemTest83,
    actual_data = storeItemTrain83
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)

plotly::subplot(p1731,p2731,p183,p283, nrows=2)




