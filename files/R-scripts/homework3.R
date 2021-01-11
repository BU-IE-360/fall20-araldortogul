#=====Data Manipulation Chunk=====#

library(ggplot2)    # for plotting
library(forecast)   # forecasting library
library(xts)        # extended time series library

data <- read.csv(file = "hw3data.csv")
data$Date <- as.yearmon(data$Date)
time_series <- xts(x = data[-1], order.by = data$Date, frequency = 12)

plot(time_series$Firms / 1000,
     main = "Total Number of Firms Established in Turkey vs. Time",
     ylab = "Total Number of Firms Established (x1000)",
     xlab = "Time",
     yaxis.right = FALSE,
     minor.ticks = "quarters",
     grid.ticks.on = "quarters",
     grid.ticks.lty = 3)

#=====Linear Regression Model Chunk=====#

data$Firms <- log(data$Firms)
data$month <- rep(1:12, 8)

data$coup <- rep(0,96)
data$coup[43:45] <- rep(1,3)

data$covid <- rep(0,96)
data$covid[88:89] <- c(1,1)

data$trend <- 1:96

time_series <- xts(x = data[-1], order.by = data$Date, frequency = 12)
model <- lm(formula = Firms ~ trend + CommercialLoan + as.factor(month) + coup + covid, data = time_series)
summary(model)
checkresiduals(model)

#=====IPI vs Residuals Chunk=====#

plot(x = data$IPI[1:95], y = model$residuals[1:95], col = "darkred",
     main = "Residuals vs. Industrial Production Index",
     xlab = "Industrial Production Index (IPI)",
     ylab = "Residuals")
abline(h = 0, lty = 2)
abline(h = 0.1, lty = 3, col = "blue")
abline(h = -0.1, lty = 3, col = "blue")

#=====Exchange Rates vs Residuals Chunk=====#

plot(x = data$USD[1:95], y = model$residuals[1:95], col = "darkgreen",
     main = "Residuals vs. USD Exchange Rates",
     xlab = "USD Exchange Rates (TRY)",
     ylab = "Residuals")
abline(h = 0, lty = 2)
abline(h = 0.1, lty = 3, col = "blue")
abline(h = -0.1, lty = 3, col = "blue")

#=====Updated Linear Regression Model Chunk=====#

model <- lm(formula = Firms ~ trend + CommercialLoan + IPI + as.factor(month) + coup + covid, data = time_series)
summary(model)
checkresiduals(model)

#=====Final Linear Regression Model Chunk=====#

data$residual_lag1 <- c(0, model$residuals)

time_series <- xts(x = data[-1], order.by = data$Date, frequency = 12)

model <- lm(formula = Firms ~ trend + CommercialLoan + IPI + residual_lag1 + as.factor(month) + coup + covid, data = time_series)
summary(model)
checkresiduals(model)

#=====Final Plot Chunk=====#

final <- xts(x = data.frame(exp(data$Firms[1:96]),exp(predict(model, time_series))), order.by = data$Date[1:96], frequency = 12)
colnames(final) <- c("Real", "Predicted")
plot(final,
     legend.loc = "topleft",
     main = "Total Number of Firms Established in Turkey vs. Time",
     minor.ticks = "quarters",
     grid.ticks.on = "quarters",
     yaxis.right = FALSE, col = c("black","goldenrod"),
     grid.ticks.lty = 3)

#=====Predicted Value Chunk=====#

final$Predicted[96]
