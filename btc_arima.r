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
df <- ts(df["price"], start = c(2013,4,28), frequency = 365)

n = nrow(df)
n1 = nrow(df) - 6
n2 = nrow(df) - 7


df_train = ts(df[c(4:n2),])
df_test = ts(df[c(n1:n),])
df_full = ts(df[c(4:n),])

adf.test(df_full)

model <- auto.arima(df_train)
fcast <- forecast(model, h = 7, level = 95)

mape <- mape(df_test,as.numeric(fcast$mean))
mape*100

result <- cbind(df_test,as.numeric(fcast$mean))
colnames(result) <- c("Actual","auto.arima")
result

autoplot(fcast) + (labs(y = "Price", x = "Days"))
