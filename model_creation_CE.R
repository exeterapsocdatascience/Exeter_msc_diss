#Creating an ARIMA Model
#Using Code Enforcement Data
setwd("~/Desktop/Dissertation/Code Enforcements")
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
data<-read.csv("~/Desktop/Dissertation/Code Enforcements/code_enforcements.csv")
data$status_dttm<-as.Date(data$status_dttm)

#subset the years we are analyzing
data<-subset(data, data$status_dttm >= "2016-01-01" & data$status_dttm< "2020-03-01")

#create vector of trash related code enforcements & subset these CEs
codevec<-c('Improper storage trash: res', 'Improper storage trash: com',
           'Trash illegally dump container','Illegal dumping < 1 cubic yd',
           'Illegal dumping 1-5 cubic yd.:','Conditions Deemed to Endanger or Impair Health or Safety - Failure to provide adequate exits, or the obstruction of any exit, passageway or common area caused by any object, including garbage or trash, which prevents egress in case of an emergency 105 CMR 410.450, 410.451 and 410.452.','Storage of Garbage & Rubbish - The occupants of each dwelling, dwelling unit, and rooming unit shall be responsible for the proper placement of his garbage and rubbish in the receptacles required in 105 CMR 410.600C')
data<-subset(data, data$description %in% codevec)
data$Week <-as.Date(cut(data$status_dttm, breaks = "week", start.on.monday = FALSE)) 

#create sums of all CEs at each date, by week
cesum <-data %>% count(Week)
cesum$month<-format(as.Date(cesum$Week, format="%Y/%m/%d"),"%m")
cesum<-subset(cesum, cesum$Week < "2020-03-01")

##################Making Data Stationary - just for visuals###################################
#detrend the data did not do much but differencing helped it pass the acf1 test

##Creating the lagged variable:
cesum$lag_n<-Lag(cesum$n, +1) #first lag variable
cesum$lag_dif<-cesum$n-cesum$lag_n #first difference
cesum$lag_2n<-Lag(cesum$lag_dif, +1) #second lag variable
cesum$year_lag<-Lag(cesum$lag_2n, + 51) #seasonal difference
cesum$lag_dif2<-cesum$lag_dif-cesum$lag_2n #second difference
cesum$year_dif<-cesum$lag_dif2 - cesum$year_lag #year difference

#Plotting making the data stationary
par(mfrow=c(3,1)) #Three plots in window
tsplot(cesum$n, main="Raw data") 
tsplot(cesum$lag_dif, main = "First difference")
tsplot(cesum$lag_dif2, main = "Second difference")

##Comparing the autocorrelation function for each series
acf1(cesum$n, 200, main="Raw data")
acf1(cesum$lag_dif,100, main="First difference", na.action = na.pass)
acf1(cesum$lag_dif2, 200, main = "Second difference", na.action = na.pass)
par(mfrow=(c(1,1))) #one plot in window

##Testing for stationarity
cesum<-subset(cesum, cesum$Week > "2016-01-03")
adf.test(cesum$lag_dif2) #want to reject null, so p < 0.05 
#it's stationary!

##########################Building ARIMA model using auto arima###################
enfts<-ts(cesum$n, frequency = 52)
class(enfts)
library(forecast)
library(tseries)

plot(enfts)
acf(enfts) #shows lots of autocorrelation

enfmodel= auto.arima(enfts, ic="aic", trace = TRUE) 
enfmodel #best model: 3,0,0 (1,0,0)[52]

#check for stationary - all good
par(mfrow=c(2,1))
acf1(ts(enfmodel$residuals), main = "Autocorrelation in Model Residuals")
pacf(ts(enfmodel$residuals))
par(mfrow=c(1,1))

#forecasting
enforecast<-forecast(enfmodel, level = c(95), h = 104)
plot(enforecast, main = "ARIMA Model Forecast of Code Enforcements", xlab = "Year", ylab = "Number of Code Enforcements")
enforecast #copy/paste these values to excel sheet with actual volume. 
#save as csv to use in modeling (line 90)
#validate forecasting: p value should be > 0.05 to prove no autocorrelation
Box.test(enforecast$residuals, lag = 5, type = "Ljung-Box")

############################Visualizing Model Forecast########################
cemodel<-read.csv("~/Desktop/Dissertation/Code Enforcements/ce_newdata.csv")
cemodel$Week<-as.Date(cemodel$Week)
ggplot(cemodel, aes(x = Week, y = n)) +
  geom_ribbon(aes(ymin = pred_low, ymax = pred_hi), fill = "grey", alpha = 0.3) + 
  geom_line(color = "#000080", alpha = 0.6) +scale_x_date(date_labels = "%Y") + 
  geom_line(aes(x = Week, y = pred_n), color = "#8b0000", alpha = 0.6) + labs(title = "Predicted and Actual Code Enforcement Volume", y = "Volume", x = "Year")

#plotting the difference between model and actual
difference<-subset(cemodel, cemodel$Week > "2020-02-23")
#create moving week variable and move out week variable
cemodel$moving<-ifelse(cemodel$Week == "2020-08-30", "#D95F02",  "#000000")
cemodel$moving[cemodel$Week == "2021-08-29"] <- "#D95F02"
cemodel$moving[cemodel$Week == "2020-03-08"] <- "#7570B3"
cemodel$moving[cemodel$Week == "2020-03-15"] <- "#7570B3"

ggplot(difference, aes(x = Week, y = dif)) + 
  geom_bar(stat = "identity", fill = as.factor(difference$moving)) +
  scale_x_date(date_labels = "%Y") + 
  labs(title = "Difference between Model and Actual Volume of Code Enforcements", y = "Volume", x = "Year")

#####################Model total differences##################
model<-read.csv("~/Desktop/Dissertation/model_dif_total.csv")
model$Week<-as.Date(model$Week)

#create moving week variable and move out week variable
model$moving<-ifelse(model$Week == "2020-08-30", "#D95F02",  "#000000")
model$moving[model$Week == "2021-08-29"] <- "#D95F02"
model$moving[model$Week == "2020-03-08"] <- "#7570B3"
model$moving[model$Week == "2020-03-15"] <- "#7570B3"

ggplot(model, aes(x = Week, y = dif_total)) + 
  geom_bar(stat = "identity", fill = as.factor(model$moving)) +
  scale_x_date(date_labels = "%Y") + scale_fill_brewer(palette = "Set2") +
  labs(title = "Difference between Total Model and Actual Volume", y = "Volume", x = "Year")
