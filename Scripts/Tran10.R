#Name: Victor Tran
#Lab 10
#COurse: ECON 256

# Task 1 Set working environment & library
setwd("C:/Users/victor/Desktop/Economics_R_Code")
library(tidyverse)

# Task 2 Download & Load the data
chilli <- read_csv("Dataset/cookoff.csv")
view(chilli)

# Task 3 Linear regression model
chilli_pot <- lm(spice ~ redpeppers + blackpeppers, data = chilli)
summary(chilli_pot)
#' for each 1 additional redpepper added to the chilli will increase the spice level by 1.05081 spice value
#' for each 1 additional blackpepper added to the chilli will increase the spice level by 8.03516 spice value
#' The R^2 and adjusted R^2 is fairly high. This mean that this linear model can greatly explain our observed 
#' value of black and red pepper in term of spicyness.
#' In addition, this data is statistically significant as signal by the low P-value.
#' 

