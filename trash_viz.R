##Dissertation: visualizing trash data

setwd("~/Desktop/Dissertation/Trash")
library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)

data<-read.csv('~/Desktop/Dissertation/Trash/trashclean.csv')
data<-subset(data, data$open_dt_ymd > "2015-12-31")
data$open_dt_ymd<-as.Date(data$open_dt_ymd)
data$open_dt<-as.Date(data$open_dt)
#data$date_test<-as.Date(data$date_test)

data$month<-as.Date(cut(data$open_dt_ymd, breaks = "month"))
data$Week <-as.Date(cut(data$open_dt_ymd, breaks = "week", start.on.monday = FALSE)) 

data$month<-as.Date(cut(data$date_test, breaks = "month"))
data$Weektest <-as.Date(cut(data$date_test, breaks = "week", start.on.monday = FALSE)) 

#create sums of each type at each date
data_sum<-data %>% count(Week)
data_sum$Week<-as.Date(data_sum$Week)
data_sum$year<-format(as.Date(data_sum$Week, format="%Y/%m/%d"),"%Y")
data_sum$month<-format(as.Date(data_sum$Week, format="%Y/%m/%d"),"%m")
data_sum<-subset(data_sum, data_sum$year > "2015") 

#ggplot graph of total number of requests by day over time
p1<-ggplot(data = data_sum, aes(x = Week, y = n)) + geom_line() +scale_x_date(date_labels = "%Y") + labs(x="Year", y = "Number of Requests", title = "311 Complaints for garbage, 2016 - 2021") +
  geom_vline(xintercept = as.numeric(as.Date("2016-08-28")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2017-08-27")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2018-08-26")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2019-09-01")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-08-30")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), linetype = 3)
p1

########ggplot of number of requests by day with color by year############
year(data_sum$Week)<-0 
p2<-ggplot(data = data_sum, aes((Week), n, group=factor(year), colour=factor(year))) +
  geom_line()  +
  labs(x="Month", colour="Year", 
  title = "311 Requests - Trash", y = "Number of Requests") + scale_x_date(date_labels = "%m") + theme_minimal() + scale_color_brewer(palette = "Set2") 
p2

##########################recreate Haddadin's work#######################
data_2016<-subset(data, data$open_dt_ymd> "2015-12-31" & data$open_dt_ymd < "2017-01-01")
sum_16<-data_2016 %>% count(Week)

p3<-ggplot(data = sum_16, aes(x = Week, y = n)) + geom_line() +scale_x_date(date_labels = "%m") + labs(x="Months", y = "Number of Requests", title = "311 Complaints for garbage (2016)") 
p3
