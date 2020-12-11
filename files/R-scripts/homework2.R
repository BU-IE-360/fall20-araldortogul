# Homework 1 - Homework 1: Data Visualization and Google Trends on Three Different Measures from Turkey
# Author: Aral Dortogul
# Date: 11/12/2020

#===== Data Manipulation Chunk =====#
library(ggplot2)    # for plotting histograms and boxplots
library(ggfortify)  # for plotting ts objects with ggplot
library(forecast)   # forecasting library
library(xts)        # extended time series library
library(ggcorrplot) # for plotting the correlation matrix

data <- read.csv("data.csv")
# Converts the type of "Date" column from char to yearmon (date-time format)
data$Date <- as.yearmon(x = data$Date)

# Creates a time series object from the obtained data -Date column omitted-
data_ts <- xts(x = data[-1],order.by = data$Date,frequency = 12)

# Column names are changed for better readability.
colnames(data_ts) <- c("Exchange", "Interest", "Unemployment")

#===== Head of Data Chunk =====#
head(data_ts)

#===== Line Graph Chunk =====#
colors = c("darkred","darkolivegreen","navyblue")
colnames <- colnames(data_ts)
ratenames <- c("USD Exchange Rate (TRY)", "Interest Rate (%)", "Unemployment Rate (%)")

colnames(data_ts) <- ratenames
plot(x = data_ts,ylab = "Rate (TRY/%)", main = "Unemployment, Interest & USD Exchange Rates vs. Time",
     col = colors, lwd = c(2,2,3),legend.loc = "topleft", minor.ticks = "months",
     grid.ticks.on = "quarter", grid.ticks.lty = "dotted")
colnames(data_ts) <- colnames

#===== Unemployment Rate Histograms Chunk =====#
data$Year <- factor(format(x = data$Date, format = "%Y"), ordered = TRUE)
ggplot(data = data,aes(x = Unemp)) +
  geom_histogram(bins = 14, alpha = 0.6,aes(color = Year, fill = Year)) +
  facet_wrap(facets = .~Year,scales = "free_x",ncol = 4) +
  scale_color_brewer(palette = "Paired",aesthetics = c("color", "fill")) +
  labs(title = "Histograms of Monthly Unemployment Rates",
       x = "Unemployment Rate (%)",
       y = "Frequency") +
  theme_minimal()

#===== Exchange Rate Histograms Chunk =====#
ggplot(data = data,aes(x = Deposit)) +
  geom_histogram(bins = 14, alpha = 0.6,aes(color = Year, fill = Year)) +
  facet_wrap(facets = .~Year,scales = "free_x",ncol = 4) +
  scale_color_brewer(palette = "Paired",aesthetics = c("color", "fill")) +
  labs(title = "Histograms of Monthly Interest Rates for Deposit Accounts (TRY)",
       x = "Interest Rate (%)",
       y = "Frequency") +
  theme_minimal()

#===== Interest Rate Histograms Chunk =====#
ggplot(data = data,aes(x = Dollar)) +
  geom_histogram(bins = 14, alpha = 0.6,aes(color = Year, fill = Year)) +
  facet_wrap(facets = .~Year,scales = "free_x",ncol = 4) +
  scale_color_brewer(palette = "Paired",aesthetics = c("color", "fill")) +
  labs(title = "Histograms of Monthly USD Exchange Rates",
       x = "USD Exchange Rate (TRY)",
       y = "Frequency") +
  theme_minimal()

#===== Boxplots Chunk =====#
data = data[,1:4]
data_stacked <- cbind("Date" = as.yearmon(rep(data$Date,3)), stack(data))

years <- factor(format(data$Date, format = "%Y"),ordered = TRUE)
data_stacked$Year <- rep(years,3)

ggplot(data = data_stacked,mapping = aes(x = Year,y = values)) +
  geom_boxplot(mapping = aes(fill = ind)) +
  scale_fill_manual(values = colors, labels = ratenames) +
  labs(title = "Boxplots of Unemployment, Interest & USD Exchange Rates for Each Year",
       x = "Years", "Rate (TRY/%)",
       y = "Rate (TRY/%)",
       fill = "Rate Type") +
  scale_y_continuous(minor_breaks = seq(0,25,by = 1))

#===== Correlation Test 1 Chunk =====#
cor.test(x = data_ts$Unemployment, y = data_ts$Exchange, method = "pearson", alternative = "greater")

#===== Correlation Test 2 Chunk =====#
cor.test(x = data_ts$Unemployment, y = data_ts$Interest, method = "pearson", alternative = "greater")

#===== Correlation Test 3 Chunk =====#
cor.test(x = data_ts$Exchange, y = data_ts$Interest, method = "pearson", alternative = "greater")

#===== Correlation Matrix Chunk =====#
ggcorrplot(corr = cor(data_ts),
           hc.order = TRUE,
           outline.col = "white",
           type = "upper",lab = TRUE,
           title = "Correlation Matrix",
           colors = c("darkred","white","darkgreen"),
           legend.title = "Correlation",
           ggtheme = theme_void)

#===== Scatter Plots Chunk =====#
par(mfrow=c(1,3))
plot(x = data$Dollar,y = data$Unemp,
     main = "Unemployment Rate vs. USD Exchange Rate",
     xlab = "USD Exchange Rate (TRY)", ylab = "Unemployment Rate (%)",
     sub = "Correlation: 81.81%", col="darkblue")
plot(x = data$Deposit,y = data$Unemp,
     main = "Unemployment Rate vs. Interest Rate",
     xlab = "Interest Rate (%)", ylab = "Unemployment Rate (%)",
     sub = "Correlation: 51.04%", col="darkgreen")
plot(x = data$Dollar,y = data$Deposit,
     main = "Interest Rate vs. USD Exchange Rate",
     xlab = "USD Exchange Rate (TRY)", ylab = "Interest Rate (%)",
     sub = "Correlation: 57.79%", col = "darkred")

#===== Linear Regression Model Chunk =====#
data_ts$Month <- xts(x = c(rep(1:12, 7),1:8), order.by = data$Date, frequency = 12)

fit <- lm(formula = Unemployment ~ Exchange + Interest + as.factor(Month), data = data_ts)

data_ts$Fitted <- xts(x = fitted(fit), order.by = data$Date, frequency = 12)
summary(fit)

#===== Check Residuals Chunk =====#
checkresiduals(fit)

#===== Unemployment and Fitted vs. Time Graph Chunk =====#
plot(x = data_ts[,c("Unemployment","Fitted")], lty = 1:2,
     main = "Unemployment and Its Fitted Model vs. Time",
     legend.loc = "topleft", ylim = c(8,15), minor.ticks = "months", 
     grid.ticks.on = "quarters", grid.ticks.lty = "dotted", 
     col = c("black", "goldenrod"))