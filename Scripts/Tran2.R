#Author: Victor Tran
#Class: ECON 256
#Professor: Justin Tyndall
#Excercise 2

#initialize working directory and tidyverse package
setwd("/Users/victor/Documents/ECON256_R_Lab")
library(tidyverse)

#import state.cvs file into rstudio by setting it to a object name 'mydata'
mydata <- read_csv("C:/Users/victor/Desktop/myproject/states.csv")
View(mydata)

ggplot(mydata,aes(x=median_home_value,y=median_rent))+ geom_point()

# Yes there is a a relative positive correlation between the two variables

mydata$hawaii <- as.factor(ifelse(mydata$state == "Hawaii",1,0))

view(mydata)


ggplot(data = mydata) + geom_point(mapping = aes(x = median_home_value, y = median_rent, color = hawaii))

#YEs Hawaii is expensive


pop <- mydata$population / 1000000
house <- mydata$total_housing_units /1000000
state <- mydata$state
ggplot(mydata) + geom_point(mapping = aes(x = pop, y = house, color = state)) +
  xlab("Population per million ") + 
  ylab("number of housing unit per million") +
  ggtitle("State housing unit vs population")
#State housing unit and population have a good linear relationships

#Summary of the states dataset
summary(mydata)


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