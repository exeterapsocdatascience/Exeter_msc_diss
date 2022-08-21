#ARIMA Model for 311 - Trash Complaints
setwd("~/Desktop/Dissertation/Trash")
library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(astsa)
library(sarima)
library(tseries)
library(lmtest)
library(Hmisc)
library(foreign)
library(car)
library(RColorBrewer)

##########################Load and Clean Data########################
data<-read.csv('~/Desktop/Dissertation/Trash/trashclean.csv')
data$open_dt_ymd<-as.Date(data$open_dt_ymd)

#subset the years we are analyzing
data<-subset(data, data$open_dt_ymd >= "2016-01-01" & data$open_dt_ymd< "2020-03-01")
data$Week <-as.Date(cut(data$open_dt_ymd, breaks = "week", start.on.monday = FALSE)) 

#create sums of all 311 requests at each date, by week
trashsum <-data %>% count(Week)
trashsum$month<-format(as.Date(trashsum$Week, format="%Y/%m/%d"),"%m")
trashsum<-subset(trashsum, trashsum$Week < "2020-03-01")
trashsum$logn<-log(trashsum$n)

######################Creating the SARIMA Model###############################
ts<-ts(trashsum$n, frequency = 52)
class(ts)
library(forecast)
library(tseries)

plot(ts)
acf(ts) #shows lots of autocorrelation

tsmodel= auto.arima(ts, ic="aic", trace = TRUE, seasonal = TRUE) 
tsmodel

#check for stationary - some over but not too much
par(mfrow=c(2,1)) 
acf1(ts(tsmodel$residuals), main = "Autocorrelation in Model Residuals")
pacf(ts(tsmodel$residuals))
par(mfrow=c(1,1))

#forecasting
enforecast<-forecast(tsmodel, level = c(95), h = 104)
plot(enforecast, main = "ARIMA Model Forecast of 2021", xlab = "Year", ylab = "Forecasted Number of 311 Complaints - Trash")
enforecast #copy/paste predicted values into excel sheet with actual values from trash_viz file
# to be used in visualizing model forecast in line 57

#validate forecasting: p value should be > 0.05 to prove no autocorrelation
Box.test(enforecast$residuals, lag = 5, type = "Ljung-Box")

############################Visualizing Model Forecast########################
trashmodel<-read.csv("~/Desktop/Dissertation/Trash/trash_model_coef.csv")
trashmodel$Week<-as.Date(trashmodel$Week)
ggplot(trashmodel, aes(x = Week, y = n)) + 
  geom_ribbon(aes(ymin = pred_low, ymax = pred_hi), 
              fill = "grey", alpha = 0.3) +
  geom_line(color = "#000080", alpha = 0.6) + 
  scale_x_date(date_labels = "%Y") + 
  geom_line(aes(x = Week, y = pred_n), color = "#8b0000", alpha = 0.6) + 
  labs(title = "Predicted and Actual 311 Complaints -Trash",
       y = "Volume", x = "Year")

#create moving week variable and move out week variable
trashmodel$moving<-ifelse(trashmodel$Week == "2020-08-30", "#D95F02",  "#000000")
trashmodel$moving[trashmodel$Week == "2021-08-29"] <- "#D95F02"
trashmodel$moving[trashmodel$Week == "2020-03-08"] <- "#7570B3"
trashmodel$moving[trashmodel$Week == "2020-03-15"] <- "#7570B3"

#plotting the difference between model and actual
difference<-subset(trashmodel, trashmodel$Week > "2020-02-23")
ggplot(difference, aes(x = Week, y = dif)) + 
  geom_bar(stat = "identity", fill = as.factor(difference$moving)) +
  scale_x_date(date_labels = "%Y") + scale_fill_brewer(palette = "Set2") +
  labs(title = "Difference between Model and Actual Volume of 311 Complaints - Trash", y = "Volume", x = "Year")

