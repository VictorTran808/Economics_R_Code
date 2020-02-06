#Author: Victor Tran
#Class: ECON 256
#Professor: Justin Tyndall
#Excercise 4

#initialize working directory and tidyverse package
setwd("/Users/victor/Desktop/myproject")
library(tidyverse)

#Importing in 2010 county data
mydata<- read_csv("C:/Users/victor/Desktop/Economics_R_Code/Dataset/nhgis0002_ds172_2010_county.csv")
View(mydata)

help("select")

#Cleaning up data selecting specific variables under a specific filter
cleandata <- select(mydata, "STATE", "H7X005", "H7W002", "H7W005", "H7V001")
view(cleandata)
CaliforniaData <-filter(cleandata,  STATE == "California")
view(CaliforniaData)
#Creating variable objects from new California datatable
numberAsian <- CaliforniaData$H7X005/100
population <- CaliforniaData$H7V001/100
numberUrban <- CaliforniaData$H7W002/100 
numberRural <- CaliforniaData$H7W005/100

ggplot(CaliforniaData) + geom_point(aes(x = numberAsian, y= numberUrban, color = population)) +
  xlab("number of asian") + 
  ylab("asian in urban area per hundred") +
  ggtitle("Do asian like to live in urban area in California?")

ggplot(CaliforniaData) + 
  geom_point(aes(x = numberRural, y= numberUrban, size = numberAsian, color = numberAsian)) +
  xlab("asian in rural area per hundred") + 
  ylab("asian in urban area per hundred") +
  ggtitle("Asian in Urban vs Rural area in California?")



  