#Dissertation: Creating cleaned 311 trash requests


setwd("~/Desktop/Dissertation/Cleaning Data")
library(ggplot2)
library(lubridate)

data_2021<- read.csv("~/Desktop/Dissertation/Cleaning Data/2021_311Requests.csv")
data_2020<- read.csv("~/Desktop/Dissertation/Cleaning Data/2020_311Requests.csv")
data_2019<- read.csv("~/Desktop/Dissertation/Cleaning Data/2019_311Requests.csv")
data_2018<- read.csv("~/Desktop/Dissertation/Cleaning Data/2018_311Requests.csv")
data_2017<- read.csv("~/Desktop/Dissertation/Cleaning Data/2017_311Requests.csv")
data_2016<- read.csv("~/Desktop/Dissertation/Cleaning Data/2016_311Requests.csv")

#how many observations in total?
p = 216626 +251495+263105+259496+251374+273951

######TRASH: create vector for complaints about trash
trashvec<-c('Overflowing or Un-kept Dumpster', 'Improper Storage of Trash (Barrels)', 'Missed Trash/Recycling/Yard Waste/Bulk Item', 'Trash on Vacant Lot', 'Illegal Dumping')

#subset data into just 311 requests for trash 
trash_2021<-subset(data_2021, data_2021$type %in% trashvec)
trash_2020<-subset(data_2020, data_2020$type %in% trashvec)
trash_2019<-subset(data_2019, data_2019$type %in% trashvec)
trash_2018<-subset(data_2018, data_2018$type %in% trashvec)
trash_2017<-subset(data_2017, data_2017$type %in% trashvec)
trash_2016<-subset(data_2016, data_2016$type %in% trashvec)

#merge just trash related requests
test1<- rbind(trash_2016, trash_2017)
test2<-rbind(test1, trash_2018)
test3<-rbind(test2, trash_2019)
test4<-rbind(test3, trash_2020)
datafull<-rbind(test4, trash_2021) 

datafull$year<-format(as.Date(datafull$open_dt, format="%d/%m/%Y"),"%Y")
table(datafull$year) #check that it worked

#export cleaned dataset to seperate excel spreadsheet
write.csv(datafull,'~/Desktop/Dissertation/trashclean.csv', row.names = TRUE)

######PESTS: create vector for complains about pests
pestvec<-c("Rat Bite","Pigeon Infestation","Rodent Activity", "Bed Bugs", "Pest Infestation - Residential" , "Mice Infestation - Residential") 
#not capturing pest infestation though
pest_2021<-subset(data_2021, data_2021$type %in% pestvec)
pest_2020<-subset(data_2020, data_2020$type %in% pestvec)
pest_2019<-subset(data_2019, data_2019$type %in% pestvec)
pest_2018<-subset(data_2018, data_2018$type %in% pestvec)
pest_2017<-subset(data_2017, data_2017$type %in% pestvec)
pest_2016<-subset(data_2016, data_2016$type %in% pestvec)

#merge just trash related requests
test6<- rbind(pest_2016, pest_2017)
test7<-rbind(test6, pest_2018)
test8<-rbind(test7, pest_2019)
test9<-rbind(test8, pest_2020)
pestdata<-rbind(test9, pest_2021)

#export cleaned pest data as csv
write.csv(pestdata,'~/Desktop/Dissertation/pestclean.csv', row.names = TRUE)


