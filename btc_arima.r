#Collect the current and historical crypto currency market data 
#using the public 'CoinGecko' API (<https://www.coingecko.com/en/api>).

library(geckor)
library(dplyr)
library(tseries)
library(forecast)
library(Metrics)
library(ggplot2)

r <- coin_history(coin_id = "bitcoin", vs_currency = "usd", days = "max")

var <- c("timestamp", "price")
df <- r[var]
df <- df[c(1:nrow(df) - 1),]
date <- df$timestamp
start_date <- date[1]
dayOfYear <- as.numeric(format(as.Date(start_date),"%j"))
year <- as.numeric(format(as.Date(start_date),"%Y"))
df <- ts(df$price, start = c(year, dayOfYear), frequency = 365)
tail(df,10)

n = length(df)
n1 = n - 6
n2 = n - 7

df_train = df[c(1:n2)]
df_test = df[c(n1:n)]

adf.test(df)

model <- auto.arima(df_train)
fcast <- forecast(model, h = 7, level = 95)

mape <- mape(df_test,as.numeric(fcast$mean))
mape*100

result <- cbind(as.numeric(fcast$mean),df_test)
colnames(result) <- c("auto.arima","Actual")
result

autoplot(fcast) + (labs(y = "Price", x = "Days"))


