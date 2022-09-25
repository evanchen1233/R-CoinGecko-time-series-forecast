#Collect the current and historical crypto currency market data 
#using the public 'CoinGecko' API (<https://www.coingecko.com/en/api>).

library(geckor)
library(dplyr)
library(tseries)
library(forecast)
library(Metrics)
library(ggplot2)

## import coin data from CionGecko API
coin <- "dogecoin"
r <- coin_history(coin_id = coin, vs_currency = "usd", days = "max")

## some data pre-processing and save data as ts object
var <- c("timestamp", "price")
df <- r[var]
df <- df[c(1:nrow(df) - 1),]

date <- df$timestamp
date_start <- date[1]

dayOfYear <- as.numeric(format(as.Date(date_start),"%j"))
year <- as.numeric(format(as.Date(date_start),"%Y"))

df <- ts(df$price, start = c(year, dayOfYear), frequency = 365)

## create training and test set
train_end <-  length(df) - 7
test_start <-  length(df) - 6

df_train <-  ts(df[c(1:train_end)])
df_test <-  ts(df[c(test_start:length(df))])

## check stationary
adf.test(df)

## ARIMA model
fit_arima <- auto.arima(df_train)
fcast_arima <- forecast(fit_arima, h = 7, level = 95)

mape_arima <- mape(df_test,as.numeric(fcast_arima$mean))*100
mape_arima

## Exponential smoothing
fit_ets <- ets(df_train)
fcast_ets <- forecast(fit_ets, h = 7, level = 95)

mape_ets <- mape(df_test,as.numeric(fcast_ets$mean))*100
mape_ets

result <- cbind(df_test,as.numeric(fcast_arima$mean),as.numeric(fcast_ets$mean))
result <- rbind(result,c(0,mape_arima,mape_ets))
colnames(result) <- c("Actual","ARIMA","ETS")
round(result,4)

autoplot(fcast_arima) + (labs(y = "Price", x = "Days"))
autoplot(fcast_ets) + (labs(y = "Price", x = "Days"))


