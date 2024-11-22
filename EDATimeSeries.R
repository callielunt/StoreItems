library(vroom)
library(tidyverse)
library(forecast)
library(patchwork)

train <- vroom("train.csv")
test <- vroom("test.csv")

## Filter down to just 1 store item for exploration and model building5
storeItem38 <- train %>%
  filter(store==3, item==8)

storeItem731<- train %>%
  filter(store==7, item==31)

## Time Series Plot
ts38 <- storeItem38 %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE)

ts731 <- storeItem731 %>%
  ggplot(mapping=aes(x=date, y=sales)) +
  geom_line() +
  geom_smooth(se=FALSE)

## ACF plot up to 1 month lag

acf1month38 <- storeItem38 %>%
  pull(sales) %>% 
  forecast::ggAcf(., lag.max=31)

acf1month731 <- storeItem731 %>%
  pull(sales) %>% 
  forecast::ggAcf(., lag.max=31)

## ACF plot up to 2 years lag

acf2years38 <- storeItem38 %>%
  pull(sales) %>% 
  forecast::ggAcf(., lag.max=2*365)

acf2years731 <- storeItem731 %>%
  pull(sales) %>% 
  forecast::ggAcf(., lag.max=2*365)

## patchwork time
(ts38 + acf1month38 + acf2years38)/(ts731 + acf1month731 + acf2years731)


