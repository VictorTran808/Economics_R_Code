#Author: Victor Tran
#Class: ECON 256
#Professor: Justin Tyndall
#Excercise 3

#initialize working directory and tidyverse package
setwd("/Users/victor/Desktop/myproject")
library(tidyverse)


#import state.cvs file into rstudio by setting it to a object name 'mydata'
mydata<- read_csv("acs_hi_2018.csv")
View(mydata)

#Summary of data
summary(mydata)

#filter data to have wommen data only
womandata <-filter(mydata, FEMALE == 1)
median(womandata$INCOME)

#Women makes $24000 average

income = mydata$INCOME

#graph histogram of hawaii resident income
help("geom_histogram")
ggplot(mydata, aes(x = income)) + geom_histogram(bins = 30, color= "black", fill= "white") + theme_dark()

#Turn college binary data into categorical data
mydata$COLLEGE<-as.factor(mydata$COLLEGE)
education <- mydata$COLLEGE

#graph bar graph of income as a relation to education (college or no college)
ggplot(mydata, aes(x = education, y = income))+
  geom_bar(stat="summary", fun.y="mean", color = "black", fill= "white") + theme_dark() +
 ggtitle("Income for college vs non-college ")+
  xlab("income")+
  ylab("college or not")

#graph age vs income with geom_smooth to see the relationship
 
age <- mydata$AGE 
ggplot(mydata, aes(x = income, y = age)) + geom_smooth()

#Relationship between income and ethic group

asian <- mydata$ASIAN

ggplot(mydata, aes(x = asian, y = income))+
  geom_bar(stat="summary", fun.y="mean", color = "black", fill= "white") + theme_dark() +
ggtitle("Income for asian vs non-asian")+
  xlab("asian or not")+
  ylab("income")

# 
#            .-"""-.
#          /       \
#          \       /
#  .-"""-. -`.-.-.<  _
# /        _,-\ ()()_/:)
# \       / ,  `     `|
#   '-..-| \-.,___,  /
#        \ `-.__/  /
#         `-.__.-'