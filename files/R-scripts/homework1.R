# Homework 1 - Homework 1: Data Visualization and Google Trends on Three Different Measures from Turkey
# Author: Aral Dortogul
# Date: 13/11/2020

library(ggplot2) #for plotting graphs
library(lubridate) #for manipulating dates

#=====Dollar Line Graph Chunk=====#

yearOrder <-  c('2013', '2014', '2015', '2016', '2017', '2018', '2019', '2020')
dolar <- read.csv(file = "dailydollar.csv")

dolar[,"Date"] <- as.Date(dolar[,"Date"],format = "%d-%m-%Y")
dolar$Year <- factor(format(dolar$Date, "%Y"),
                     levels = yearOrder)

ggp1_1 <- ggplot(data = dolar,aes(x = Date,y = Rate))

#Plots the USD/TRY line graph
ggp1_1 + geom_line(color ="black") +
  geom_smooth(fill = NA, color="orange",linetype = "twodash", size = 0.5) +
  labs(title = "USD/TRY Exchange Rates vs. Time",
       x = "Time",
       y = "US Dollar Exchange Rate" ) +
  scale_x_date(date_breaks = "6 month",
               date_labels = "%Y %b",
               date_minor_breaks = "1 month") +
  theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +
  scale_y_continuous(breaks = seq(from = 0,to = 8,by = 1),
                     minor_breaks = seq(from = 0,to = 9,by = .2))

#=====Dollar Histograms Chunk=====#

ggp1_2 <- ggplot(data = dolar,aes(x = Rate))

#Plots the USD/TRY histograms
ggp1_2 + geom_histogram(bins = 24, alpha = 0.6,aes(color = Year, fill = Year)) +
  facet_wrap(Year~.,scales = "free_x",ncol = 4) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(title = "Histograms of Daily US Dollar Exchange Rates for Years 2013-2020",
       x = "US Dollar Exchange Rate (TRY)",
       y = "Frequency") +
  theme(legend.position = "none")

#=====Dollar Boxplots Chunk=====#

ggp1_3 <- ggplot(data = dolar,aes(x = Year,y = Rate))

#Plots the USD/TRY boxplots
ggp1_3 + geom_boxplot(aes(y = Rate, fill=Year)) +
  scale_fill_brewer(palette="Paired") +
  labs(title = "Boxplots of Daily US Dollar Exchange Rates for Years 2013-2020",
       x = "Years",
       y = "US Dollar Exchange Rate (TRY)") +
  scale_y_continuous(breaks = seq(from = 0,to = 8,by = 1),
                     minor_breaks = seq(from = 0,to = 9,by = .2))

#=====Dollar Google Trends Chunk=====#

dolargt <- read.csv(file = "dollargt.csv")

dolargt$Month <- as.Date(parse_date_time(dolargt$Month,"Ym"),format = "%Y-%m-%d")
dolargt$Year <- factor(format(dolargt$Month, "%Y"), levels = yearOrder)

ggp1_4 <- ggplot(data = dolargt,aes(x = Year,y = ABD.Dolari))

#Plots the Google Trends "iş ilanı" search volume boxplots
ggp1_4 + geom_boxplot(aes(fill=Year)) +
  labs(title = "Boxplots of Google Trends Search Volume ",
       subtitle = "for the Keyword: \"ABD Doları\" in Turkey for Years 2013-2020",
       x = "Years",
       y = "Volume (%)") +
  scale_y_continuous(breaks = seq(from = 0,to = 100,by = 5),
                     minor_breaks = seq(from = 0,to = 100,by = 2.5)) +
  lims(y = c(0,45)) +
  scale_fill_brewer(palette="Paired")

#=====Interest Rates Line Graph Chunk=====#

intdata <- read.csv(file = "interest.csv")

intdata$Date <- as.Date(intdata$Date,format = "%d-%m-%Y")
intdata$Year <- factor(format(intdata$Date, "%Y"),
                       levels = yearOrder)
interest <- data.frame(
  "Loan" = c(intdata$Ihtiyac,intdata$Tasit,intdata$Konut,intdata$Ticari),
  "Date" = rep(x = intdata$Date,times = 4),
  "Year" = rep(x = intdata$Year,times = 4),
  "Type" = c(rep("Consumer Loan",nrow(intdata)),
             rep("Vehicle Loan",nrow(intdata)),
             rep("Housing Loan",nrow(intdata)),
             rep("Commerical Loan",nrow(intdata))))

colors <- c("darkred","darkolivegreen","orange","mediumpurple")
ggp2_1 <- ggplot(data = interest, aes(x = Date))

#Plots the interest rates line graph
ggp2_1 +
  geom_line(aes(y = Loan,color = Type), alpha = .8) +
  scale_x_date(date_breaks = "6 month",
               date_labels = "%Y %b",
               date_minor_breaks = "1 month") +
  theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +
  scale_y_continuous(breaks = seq(from = 0,to = 40,by = 4),
                     minor_breaks = seq(from = 0,to = 40,by = 1)) +
  labs(title = "Interest Rates of Different Types of Bank Loans vs. Time",
       color = "Loan Types",
       x = "Time",
       y = "Interest Rate (%)") +
  scale_color_manual(values = colors)

#=====Interest Rates Histograms Chunk=====#

ggp2_2 <- ggplot(data = interest)

#Plots the interest rates histograms
ggp2_2 +
  geom_histogram(aes(x = Loan, fill = Type),color = "black",bins = 24,alpha = .8,size = .25) +
  facet_wrap(Year~.,scales = "free_x",nrow = 2) +
  labs(title = "Stacked Histograms of the Interest Rates of Different Bank Loans for years 2013-2020",
       fill = "Loan Types",
       x = "Interest Rate (%)",
       y = "Frequency") +
  scale_fill_manual(values = colors) +
  scale_y_continuous(breaks = seq(from = 0,to = 35,by = 5),
                     minor_breaks = seq(from = 0,to = 35,by = 1))

#=====Interest Rates Boxplots Chunk=====#

ggp2_3 <- ggplot(data = interest,aes(x = Year,y = Loan))

#Plots the interest rates boxplots
ggp2_3 + geom_boxplot(aes(fill=Type)) +
  labs(title = "Boxplots of Interest Rates for Years 2013-2020",
       x = "Years",
       y = "US Dollar Exchange Rate (TRY)",
       fill = "Loan Types") + 
  scale_fill_manual(values = colors) + 
  scale_y_continuous(breaks = seq(from = 0,to = 40,by = 2),
                     minor_breaks = seq(from = 0,to = 40,by = .5))

#=====Interest Rates Google Trends Chunk=====#

intgt <- read.csv(file = "interestgt.csv")

intgt$Month <- as.Date(parse_date_time(intgt$Month,"Ym"),format = "%Y-%m-%d")
intgt$Year <- factor(format(intgt$Month, "%Y"), levels = yearOrder)

interestgt <- data.frame(
  "Loan" = c(intgt$Consumer,intgt$Vehicle,intgt$Housing,intgt$Interest),
  "Date" = rep(x = intgt$Month,times = 4),
  "Year" = rep(x = intgt$Year,times = 4),
  "Type" = c(rep("İhtiyaç Kredisi",nrow(intgt)),
             rep("Taşıt Kredisi",nrow(intgt)),
             rep("Konut Kredisi",nrow(intgt)),
             rep("Faiz",nrow(intgt))))

ggp2_4 <- ggplot(data = interestgt,aes(x = Year,y = Loan))

#Plots the Google Trends "faiz", "ihtiyaç kredisi", taşıt kredisi", "konut kredisi" search volume boxplots
ggp2_4 + geom_boxplot(aes(fill=Type)) +
  labs(title = "Boxplots of Google Trends Search Volume for the Keywords: ",
       subtitle = "\"Faiz\", \"İhtiyaç Kredisi\", \"Taşıt Kredisi\", \"Konut Kredisi\" in Turkey for Years 2013-2020",
       x = "Years",
       y = "Volume (%)",
       fill = "Keywords") +
  scale_y_continuous(breaks = seq(from = 0,to = 100,by = 10),
                     minor_breaks = seq(from = 0,to = 100,by = 2)) +
  scale_fill_manual(values=c("skyblue","darkolivegreen","orange","mediumpurple"))

#=====Unemployment Rate Line Graph Chunk=====#

unemployment <- read.csv(file = "unemployment.csv")

unemployment[,"Date"] <- as.Date(parse_date_time(unemployment$Date,"Ym"),format = "%Y-%m-%d")
unemployment$Year <- factor(format(unemployment$Date, "%Y"),
                            levels = yearOrder)

ggp3_1 <- ggplot(data = unemployment,aes(x = Date,y = Rate))

#Plots the unemployment rate line graph
ggp3_1 + geom_line(color ="black") +
  geom_smooth(fill = NA, color="orange",linetype = "dashed", size = 0.5) +
  labs(title = "Unemployment Rate vs. Time",
       x = "Time",
       y = "Unemployment Rate (%)" ) +
  scale_x_date(date_breaks = "6 month",
               date_labels = "%Y %b",
               date_minor_breaks = "1 month") +
  theme(axis.text.x=element_text(angle=60, hjust=1.4, vjust = 1.4)) +
  scale_y_continuous(breaks = seq(from = 8,to = 15,by = 1),
                     minor_breaks = seq(from = 8,to = 15,by = .2))

#=====Unemployment Rate Histograms Chunk=====#

ggp3_2 <- ggplot(data = unemployment,aes(x = Rate))

#Plots the unemployment rate histograms
ggp3_2 + geom_histogram(bins = 8, alpha = 0.7,aes(color = Year, fill = Year)) +
  facet_wrap(Year~.,scales = "free_x",ncol = 4) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Histograms of Unemployment Rate for Years 2013-2020",
       x = "Unemployment Rate (%)",
       y = "Frequency") +
  theme(legend.position = "none")

#=====Unemployment Rate Boxplots Chunk=====#

ggp3_3 <- ggplot(data = unemployment,aes(x = Year,y = Rate))

#Plots the unemployment rate boxplots
ggp3_3 + geom_boxplot(aes(y = Rate, fill=Year)) +
  scale_fill_brewer(palette="Dark2") +
  labs(title = "Boxplots of Unemployment Rates for Years 2013-2020",
       x = "Years",
       y = "Unemployment Rate (%)") +
  scale_y_continuous(breaks = seq(from = 8,to = 15,by = 1),
                     minor_breaks = seq(from = 8,to = 15,by = .2))

#=====Unemployment Rate Google Trends Chunk=====#

unempgt <- read.csv(file = "unemploymentgt.csv")

unempgt$Month <- as.Date(parse_date_time(unempgt$Month,"Ym"),format = "%Y-%m-%d")
unempgt$Year <- factor(format(unempgt$Month, "%Y"), levels = yearOrder)

ggp3_4 <- ggplot(data = unempgt,aes(x = Year,y = Isilani))

#Plots the Google Trends "iş ilanı" search volume boxplots
ggp3_4 + geom_boxplot(aes(fill=Year)) +
  labs(title = "Boxplots of Google Trends Search Volume ",
       subtitle = "for the Keyword: \"iş ilanı\" in Turkey for Years 2013-2020",
       x = "Years",
       y = "Volume (%)") +
  scale_y_continuous(breaks = seq(from = 0,to = 100,by = 5),
                     minor_breaks = seq(from = 0,to = 100,by = 2.5)) +
  scale_fill_brewer(palette="Dark2")
