# Aral Dörtoğul, IE 360: Statistical Forecasting and Time Series - Homework 4
library(xts)
library(lubridate)
library(forecast)
library(tseries)
library(urca)
data <- read.csv(file = "hw4data.csv")
data$Date <- paste0(data$Date," ", data$Hour)
#data$Date <- as.POSIXlt(x = data$Date, format = "%d.%m.%Y %H:%M")
data$Consumption..MWh. <- as.numeric(gsub(pattern = ",", replacement = "", x = data$Consumption..MWh.))

daily_consumption <- 0
date <- as.Date(as.Date("2017-01-01"):as.Date("2021-01-08"))

for (i in 0:(length(data$Consumption..MWh.)/24 - 1)) {
  daily_consumption[i+1] <- mean(data$Consumption..MWh.[(24*i - 23):(24*i)])
}

daily_data <- data.frame(date, daily_consumption[1:1469])
colnames(daily_data) <- c("Date", "Consumption")
data_ts <- xts(x = daily_data$Consumption, order.by = daily_data$Date, frequency = 7)
d_ts <- ts(daily_data$Consumption, start = as.Date("2017-01-01"), end = as.Date("2021-01-08"), frequency = 7)


plot(data_ts,main = "Daily Average Electricity Consumption Rate in Turkey vs. Time",
     ylab = "Electricity Consumption (MWh)",
     xlab = "Time",lwd = 0.75, col ="darkblue")


tsdisplay(data_ts,
          main = "Electricity Consumption Rate in Turkey",
          xlab = "Electricity Cponsumption Rate (MWh)")


ts_decomposed <- decompose(x = d_ts,type = "additive")
plot(ts_decomposed)
plot(ts_decomposed$seasonal[1:7], type = "o",main = "Seasonality for One Week", ylab = "Electricity Consumption Rate (MWh)", xlab = "Week Days")


random <- ts_decomposed$random
arima(random, order=c(0,0,1))
arima(random, order=c(1,0,0), seasonal = c(0,1,0))
model <- auto.arima(random)
model


prediction <- predict(model, n.ahead = 14)
isNA <- is.na(ts_decomposed$trend)
lasttrends <- ts_decomposed$trend[!isNA]
lasttrends <- lasttrends[(length(lasttrends)-13):length(lasttrends)]
lastseasonality <- ts_decomposed$seasonal[(length(lasttrends)-13):length(lasttrends)]
final <- prediction$pred + lasttrends + lastseasonality
nexttwoweeks <- as.Date(as.Date("2021-01-09"):as.Date("2021-01-22"))

final <- xts(x = data.frame(prediction$pred + lasttrends + lastseasonality, daily_consumption[1470:1483]), order.by = nexttwoweeks, frequency = 7)
colnames(final) <- c("Prediction", "Real Values")

print(final)
plot(final, main = "Electricity Consumption Rate in Turkey vs. Time", legend.loc = "topleft", ylab = "Electricity Consumption Rate (MWh)")


error = final$`Real Values` - final$Prediction
dperror <- abs(error) / final$`Real Values` * 100
e <- xts(x = data.frame(dperror), order.by = nexttwoweeks, frequency = 7)
colnames(e) <- "Daily % Error"
print(e)


error = final$`Real Values` - final$Prediction
n=length(final$`Real Values`)

mean=mean(final$`Real Values`)
sd=sd(final$`Real Values`)
bias = sum(error)/sum(final$`Real Values`)
mape = sum(abs(error/final$`Real Values`))/length(final$`Real Values`)
mad = sum(abs(error))/length(final$`Real Values`)
wmape = mad/mean
print(data.frame(BIAS = bias,MAPE =  mape, MAD = mad,WMAPE = wmape))