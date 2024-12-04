#load libraries
library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(vroom)
library(embed)
library(bonsai)
library(lightgbm)

#read in the data
train_path <- "/kaggle/input/demand-forecasting-kernels-only/train.csv"
test_path <- "/kaggle/input/demand-forecasting-kernels-only/test.csv"

train <- vroom(train_path)
test <- vroom(test_path)

#define stores and items for double loop
nStores <- max(train$store)
nItems <- max(train$item)

#create recipe and workflow
store_recipe <- recipe(sales~., data=train) %>%
  step_date(date, features = c("dow", "month", "decimal", "doy", "year")) %>% # Extract useful date features
  step_holiday(date, role = "predictor") %>% # Add holiday indicator
  step_range(date_doy, min = 0, max = pi) %>%                                 # Scale doy
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) %>%             # Add seasonal features
  step_lencode_mixed(all_nominal_predictors(), outcome = vars(sales)) %>%     # Encode relationships to sales
  step_normalize(all_numeric_predictors()) %>%                                # Normalize predictors
  step_rm(date, item, store)                                                  # Remove unnecessary columns

lgbm_model <- boost_tree(tree_depth=2, 
                         trees=1000,
                         learn_rate=0.01) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")
boost_wf <- workflow() %>%
  add_recipe(store_recipe) %>%
  add_model(lgbm_model)

#double loop over 500 store and item combos
for(s in 1:nStores){
  for(i in 1:nItems){
    
    #subset the data
    storeItemTrain <- train %>%
      filter(store==s, item==i)
    storeItemTest <- test %>%
      filter(store==s, item==i)
    
    #fit the data and predict
    fitted_wf <- boost_wf %>%
      fit(data=storeItemTrain)
    preds <- predict(fitted_wf, new_data=storeItemTest) %>%
      bind_cols(storeItemTest) %>%
      rename(sales=.pred) %>%
      select(id, sales)
    
    
    #save the results
    if(s==1 && i==1){
      all_preds <- preds
    } else {
      all_preds <- bind_rows(all_preds,
                             preds)
    }
    
  }
}

vroom_write(all_preds, file="submission.csv", delim=",")
