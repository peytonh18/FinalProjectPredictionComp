library(tidyverse)
library(fpp3)
library(patchwork)
library(forecast)
library(tidyr)
library(lubridate)
library(zoo)
library(lubridate)

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



CREDIT %>% 
  autoplot()



#ETS: 0.143
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(ETS(credit_in_millions)) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)

#ARIMA: 0.125
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(ARIMA(credit_in_millions,stepwise=FALSE)) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)

#TLSM: 0.228
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(TSLM(credit_in_millions)) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#RW: 0.143
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(RW(credit_in_millions ~drift())) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#Naive: 0.141
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(NAIVE(credit_in_millions)) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#Seasonal Naive: 0.223
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(SNAIVE(credit_in_millions)) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#ETS2: 0.125
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(ETS(credit_in_millions~ error("A") + trend("N") + season("N"))) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#ETS3: 0.128
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(ETS(credit_in_millions~ error("A") + trend("A") + season("N"))) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#ETS4: 0.149
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(ETS(credit_in_millions~ error("A") + trend("A") + season("A"))) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#ETS5: 0.125
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(ETS(credit_in_millions ~ error("A") + trend("Ad", phi = 0.9) + season("N"))) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#ETS6: 0.167
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(ETS(credit_in_millions~ error("M") + trend("A") + season("M"))) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#ETS7: 0.158
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(ETS(credit_in_millions~ error("M") + trend("Ad") + season("M"))) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#arima210: 0.125
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(ARIMA(credit_in_millions ~ pdq(2,1,0))) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#arima013: 0.124
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(ARIMA(credit_in_millions ~ pdq(0,1,3))) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


##############################################################################
#arima013: 0.0923          BEST PERFORMING MODEL
##############################################################################
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=1)
CREDIT_Stretch %>% 
  model(ARIMA(credit_in_millions ~ pdq(0,1,3))) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#arima012011: 0.150
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(ARIMA(credit_in_millions ~ pdq(0,1,2) + PDQ(0,1,1))) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#arima210011: 0.147
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(ARIMA(credit_in_millions ~ pdq(2,1,0) + PDQ(0,1,1))) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#stochastic: 0.125
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(ARIMA(credit_in_millions ~ pdq(d = 1))) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#CROSTON: 0.134
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(CROSTON(credit_in_millions)) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#lm2: 0.158
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(TSLM(credit_in_millions ~ trend())) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


#lm3: 0.172
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=24)
CREDIT_Stretch %>% 
  model(TSLM(credit_in_millions ~ trend() + season())) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)


fit_credit <- CREDIT_Stretch %>%
  model(ETS(credit_in_millions))
fc_credit <- forecast(fit_credit,h=1)
fc_credit %>%
  autoplot() +
  labs(
    title = "Star Wars Forecast",
    y = "credit in millions"
  )





CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=1)
CREDIT_Stretch %>% 
  model(ARIMA(credit_in_millions ~ pdq(0,1,3))) %>% 
  forecast(h=1) %>% 
  accuracy(CREDIT)

CreditForcast <- CREDIT_Stretch %>% 
  model(ARIMA(credit_in_millions ~ pdq(0,1,3))) %>% 
  forecast(h=12)

tail(CreditForcast,12)


############################################################################################################
CREDIT_Stretch <- CREDIT %>% 
  stretch_tsibble(.init = 48,.step=15)
CreditForcast <- forecast(fit_credit,h=12)
CreditForcast <- fc_credit$ï..credit_in_millions

write.csv(CreditForcast,file='CreditForcast',row.names=FALSE)
############################################################################################################

