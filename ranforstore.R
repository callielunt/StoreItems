library(tidyverse)
library(tidymodels)
library(vroom)
library(embed)
library(themis)
library(discrim)
library(bonsai)
library(lightgbm)

train <- vroom("train.csv")
test <- vroom("test.csv")

## Filter down to just 1 store item for exploration and model building5
storeItem38 <- train %>%
  filter(store==3, item==8)

#Recipe

my_recipe <- recipe(sales ~ ., data = storeItem38) %>% 
  step_rm(store, item)
  step_date(date, features = "doy") %>% 
  step_range(date_doy, min=0, max = pi) %>% 
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>% 
  step_date(date, features = "decimal")



## Define a model
my_mod_rf <- rand_forest(mtry = tune(),
                         min_n = tune(),
                         trees = 700) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

#Workflow
randfor_wf <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(my_mod_rf)

## Set up a grid of tuning values
grid_of_tuning_params_rf <- grid_regular(mtry(range = c(1, 10)),
                                         min_n(),
                                         levels = 5)

## Set up K-fold CV
folds_rf <- vfold_cv(storeItem38, v = 5, repeats = 1)

## Find best tuning parameters
CV_results_rf <- randfor_wf %>% 
  tune_grid(resamples = folds_rf,
            grid = grid_of_tuning_params_rf,
            metrics = metric_set(smape))

bestTune_rf <- CV_results_rf %>% 
  select_best(metric = "smape")

best_results_rf <- CV_results_rf %>% 
  show_best(metric = "smape", n = 1)

best_results_rf

