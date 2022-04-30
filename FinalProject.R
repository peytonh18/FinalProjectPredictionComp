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
    
    lm=TSLM(ï..credit_in_millions),
    
    rw = RW(ï..credit_in_millions ~drift()),
    
    Naive = NAIVE(ï..credit_in_millions),
    
    SN = SNAIVE(ï..credit_in_millions),
    
    ETS2 = ETS(ï..credit_in_millions~ error("A") + trend("N") + season("N")),
    
    ETS3 = ETS(ï..credit_in_millions~ error("A") + trend("A") + season("N")),
    
    ETS4 = ETS(ï..credit_in_millions~ error("A") + trend("A") + season("A")),
    
    ETS5 = ETS(ï..credit_in_millions ~ error("A") + trend("Ad", phi = 0.9) + season("N")),
    
    ETS6 = ETS(ï..credit_in_millions~ error("M") + trend("A") + season("M")),
    
    ETS7 = ETS(ï..credit_in_millions~ error("M") + trend("Ad") + season("M")),
    
    arima210 = ARIMA(ï..credit_in_millions ~ pdq(2,1,0)),
    
    arima013 = ARIMA(ï..credit_in_millions ~ pdq(0,1,3)),
    
    arima012011 = ARIMA(ï..credit_in_millions ~ pdq(0,1,2) + PDQ(0,1,1)),
    
    arima210011 = ARIMA(ï..credit_in_millions ~ pdq(2,1,0) + PDQ(0,1,1)),
    
    stochastic = ARIMA(ï..credit_in_millions ~ pdq(d = 1)),
    
    CROSTON = CROSTON(ï..credit_in_millions),
    
    lm2 = TSLM(ï..credit_in_millions ~ trend()))
fit %>% forecast(h=6) %>% accuracy(TRAIN) %>% arrange(RMSE)

