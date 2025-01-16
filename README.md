# StoreItems
This repository contains the training and test data sets from the Kaggle "Store Item Demand Forecasting Challenge" Competition. Code for different predictive models can also be found. The purpose of participating retrospectively in this competition was to learn predicictive models for time series data. In this specific competition, we predicted 3 months of sales for 50 different items at 10 different stores.

Because of the large size of this study. Initial models were built using different store/item combinations rather than all combinations.

A few notable models used were Random Forests, Boosted Trees, ARIMA, and Facebook's Prophet Model. Models were scored using the SMAPE metric and the model that performed the best was Boosted Trees with feature engineering and tree depth of 2, 1000 trees, and a  learning rate of 0.01. The model was submitted as a Kaggle Kernal. The code can be found in the Kaggle notebook at the following link: https://www.kaggle.com/code/callielunt/lightgbm-storeitem
