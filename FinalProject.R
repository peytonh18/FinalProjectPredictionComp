library(tidyverse)
library(fpp3)
library(patchwork)
library(forecast)
library(tidyr)
library(lubridate)
library(zoo)

#Load data frame
CREDIT <-read.csv("credit.csv")
CREDIT <- CREDIT %>% arrange(desc(row_number()))

#Create index variable
Months<- seq(ymd('1981-05-01'),ymd('2022-04-01'),by='months')
Date <- yearmonth(Months)

#Create data frame with added index column
CREDIT <- data.frame(Date, CREDIT)

#Convert data frame into tsibble
CREDIT <- as_tsibble(CREDIT,
           index=Date)

#Create training & holdout rows
TRAIN <- CREDIT %>%
  filter_index("1981 May" ~ "2020 Mar")
HOLDOUT <- CREDIT %>%
  filter_index("2020 Apr" ~ "2022 Apr")



CREDIT %>% 
  autoplot()


fit<-TRAIN %>% 
  stretch_tsibble(.init=48, .step=24) %>% 
  model(
    ets=ETS(ï..credit_in_millions),
    
    arima=ARIMA(ï..credit_in_millions),
    
    lm=TSLM(ï..credit_in_millions))
fit %>% forecast(h=6) %>% accuracy(TRAIN) %>% arrange(RMSE)

