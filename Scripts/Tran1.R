#Author: Victor Tran
#Class: ECON 256
#Professor: Justin Tyndall
#Excercise 1

#initialize working directory and tidyverse package
setwd("/Users/victor/Documents/ECON256_R_Lab")
library(tidyverse)

#import state.cvs file into rstudio by setting it to a object name 'mydata'
mydata <- read_csv("C:/Users/victor/Desktop/myproject/states.csv")
View(mydata)

#Summary of the states dataset
summary(mydata)

#The Summary function return back a summary table of our dataset of the state.cvs
#Summary include the different metadata (attribute) for column such as state, population, total housing unit,
#medium rent and median home value of each state
#There is also further statistic value of each attribute such as: minimum, median, mean, 1st quarter and 3rd quarter
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