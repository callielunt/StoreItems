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
arima_recipe <- recipe(sales ~ ., data = storeItemTrain731) %>% 
  step_rm(store, item) #%>% 
  # step_date(date, features = "doy") %>% 
  # step_range(date_doy, min=0, max = pi) %>%
  # step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  # step_date(date, features = "decimal") # For the linear model part11

## Define the ARIMA Model13
arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2) %>%
  set_engine("auto_arima")

# Merge into a single workflow and fit to the training data1
arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))

## Calibrate (tune) the models (find p,d,q,P,D,Q)7
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))

## Visualize results11
p1731 <- cv_results %>%
  modeltime_forecast(
    new_data    = testing(split),
    actual_data = train
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

## Create a recipe for the linear model part10
arima_recipe <- recipe(sales ~ ., data = storeItemTrain83) %>% 
  step_rm(store, item) #%>% 
# step_date(date, features = "doy") %>% 
# step_range(date_doy, min=0, max = pi) %>%
# step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
# step_date(date, features = "decimal") # For the linear model part11

## Define the ARIMA Model13
arima_model <- arima_reg(seasonal_period=365,
                         non_seasonal_ar=5, # default max p to tune
                         non_seasonal_ma=5, # default max q to tune
                         seasonal_ar=2, # default max P to tune
                         seasonal_ma=2, #default max Q to tune
                         non_seasonal_differences=2, # default max d to tune
                         seasonal_differences=2) %>%
  set_engine("auto_arima")

# Merge into a single workflow and fit to the training data1
arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))

arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(cv_split))
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(cv_split))
## Calibrate (tune) the models (find p,d,q,P,D,Q)7
#cv_results <- modeltime_calibrate(arima_wf,
#                                  new_data = testing(cv_split))

## Visualize results11
p183 <- cv_results %>%
  modeltime_forecast(
    new_data    = testing(split),
    actual_data = train
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










## Dr Heaton's Code

train <- train %>%
  filter(store==7, item==31)
test <- test %>%
  filter(store==7, item==31)

split <- time_series_split(train, assess="3 months", cumulative = TRUE) #this is a one fold split

split %>%
  tk_time_series_cv_plan() %>% #Put into a data frame7
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)



arima_recipe <- recipe(sales~., data=train) %>%
  step_rm(item, store) %>%
  step_date(date, features=c("doy", "decimal")) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_rm(date_doy)
bake(prep(arima_recipe), new_data=train)

arima_model <- arima_reg() %>%
  set_engine("auto_arima")
arima_wf <- workflow() %>%
  add_recipe(arima_recipe) %>%
  add_model(arima_model) %>%
  fit(data=training(split))
cv_results <- modeltime_calibrate(arima_wf,
                                  new_data = testing(split))
## Visualize results
cv_results %>%
  modeltime_forecast(
    new_data    = testing(split),
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)
## Evaluate the accuracy
cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )
## Refit to whole data
fullfit <- cv_results %>%
  modeltime_refit(data = train)
fullfit %>%
  modeltime_forecast(
    new_data    = test,
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive=TRUE)










