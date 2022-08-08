##Dissertation: visualizing code enforcements

setwd("~/Desktop/Dissertation/Code Enforcements")
library(ggplot2)
library(lubridate)
library(dplyr)
library(plotly)
library(astsa)
library(xtls)
library(sarima)
library(tseries)
library(lmtest)
library(Hmisc)
library(foreign)
library(car)

###################Load and Clean Data#########################################
code_enforce<-read.csv("~/Desktop/Dissertation/Code Enforcements/code_enforcements.csv")
code_enforce$status_dttm<-as.Date(code_enforce$status_dttm)

#subset the years we are analyzing
code_enforce_sub<-subset(code_enforce, code_enforce$status_dttm >= "2016-01-01" & code_enforce$status_dttm< "2020-03-01")
enforce_total<-subset(code_enforce, code_enforce$status_dttm >= "2016-01-01")

#create vector of trash related code enforcements
codevec<-c('Improper storage trash: res', 'Improper storage trash: com',
           'Trash illegally dump container','Illegal dumping < 1 cubic yd',
           'Illegal dumping 1-5 cubic yd.:','Conditions Deemed to Endanger or Impair Health or Safety - Failure to provide adequate exits, or the obstruction of any exit, passageway or common area caused by any object, including garbage or trash, which prevents egress in case of an emergency 105 CMR 410.450, 410.451 and 410.452.','Storage of Garbage & Rubbish - The occupants of each dwelling, dwelling unit, and rooming unit shall be responsible for the proper placement of his garbage and rubbish in the receptacles required in 105 CMR 410.600C')

#subset trash enforcements
enforcements<-subset(code_enforce_sub, code_enforce_sub$description %in% codevec)
enforcements_total<-subset(enforce_total, code_enforce_sub$description %in% codevec)
enforcements_total$month<-as.Date(cut(enforcements_total$status_dttm, breaks = "month"))
enforcements_total$Week <-as.Date(cut(enforcements_total$status_dttm, breaks = "week", start.on.monday = FALSE)) 
enforcements_total$year<-format(as.Date(enforcements_total$status_dttm, format = "%Y/%m/%d"), "%Y")


#create sums of each type at each date
enforcsum<-enforcements_total %>% count(Week)
enforcsum$year<-format(as.Date(enforcsum$Week, format="%Y/%m/%d"),"%Y")
enforcsum$month<-format(as.Date(enforcsum$Week, format="%Y/%m/%d"),"%m")
enforcsum<-subset(enforcsum, enforcsum$year > "2015" & 
                    enforcsum$year <
                   "2022")

###################Recreation of Haddadin's work################################
#ggplot graph of total number of requests by day over years
p1<-ggplot(data = enforcsum, aes(x = Week, y = n)) + geom_line() +scale_x_date(date_labels = "%Y") + labs(x="Year", y = "Number of Requests", title = "Code Enforcements for Trash, 2016 - 2021") +   geom_vline(xintercept = as.numeric(as.Date("2016-08-28")), linetype = 3) + geom_vline(xintercept = as.numeric(as.Date("2016-08-28")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2017-08-27")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2018-08-26")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2019-09-01")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-08-30")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), linetype = 3)
p1

enforce_2016<-subset(enforce_2016, enforce_2016$description %in% codevec)
enforce_2016$month<-as.Date(cut(enforce_2016$status_dttm, breaks = "month"))
enforce_2016$Week <-as.Date(cut(enforce_2016$status_dttm, breaks = "week", start.on.monday = FALSE)) 

enforce_2016sum<-enforce_2016 %>% count(Week)
enforce_2016sum$year<-format(as.Date(enforce_2016sum$Week, format="%Y/%m/%d"),"%Y")

p2<-ggplot(data = enforce_2016sum, aes(x = Week,y = n)) + geom_line() +  
  scale_x_date(date_labels = "%m") + labs(x="Month", y = "Number of Requests",
  title = "Code Enforcements for trash (2016)") + 
  geom_vline(xintercept = as.numeric(as.Date("2016-08-28")), linetype = 3)
p2

#######Plotting total number of enforcements by day over years#######
data_test<-enforcements %>% count(Week, description)

p3<-ggplot(data = data_test, aes(x = Week, y = n)) + geom_line(aes(color = description)) +scale_x_date(date_labels = "%Y") + labs(x="Year", y = "Number of Requests", title = "Code Enforcements for Trash by Type, 2016 - 2021", color = "Type") +   geom_vline(xintercept = as.numeric(as.Date("2016-08-28")), linetype = 3) + geom_vline(xintercept = as.numeric(as.Date("2016-08-28")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2017-08-27")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2018-08-26")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2019-09-01")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2020-08-30")), linetype = 3) +
  geom_vline(xintercept = as.numeric(as.Date("2021-08-29")), linetype = 3)
p3

#######Plotting enforcement trends over the year #######
year(enforcsum$Week)<-0 #run before plotting p2

p4<-ggplot(data = enforcsum, aes((Week), n, group=factor(year), colour=factor(year))) + geom_line()  + labs(x="Month", colour="Year", 
       title = "Code Enforcements Annual Trends", y = "Number of Violations") + scale_x_date(date_labels = "%m") + theme_minimal() 

p4
