#Name: Victor Tran
#Lab 9
#COurse: ECON 256

# Task 1 Set working environment & library
setwd("C:/Users/victor/Desktop/Economics_R_Code")
library(tidyverse)

# Task 2 Read data in R environment
oahulistings <- read_csv("Dataset/oahulistings.csv")

# Task 3 Find the statistical answers to complete the report

#' How many units listed on Airbnb in 2016?
#' How many units listed on Airbnb by 2018?
#' What is the zipcode that has the most Airbnb unit in 2018?
#' How many Airbnb unit was in total was in that zipcode in 2018?
#' In 2018, how many Airbnb were apartment? Condo? and houses?
#' What is the average nightly price $ of an Airbnb listed in 2018? 
#' What was the median price?
#' What was the range of the price? 
#' Do a linear regression on effect of additional person has on price
#' How many unique listing under the host name 'Evolve Vacation Rental'?
#' How many different zipcode are under that host name?

## Getting the statistic from the dataset

#Get a overview of dataset structure, variables names,class types, and preview the first few data point.
str(oahulistings)

#'Identified the Variables I am looking to analyze:
#'year | zipcode | price | property_type | host_name | accommodates
#' Note: property_type and host_name are character class and need to change to factor class analyze with summary function
#' 

# Create a cleaner data set with selected variables needed for this assignment
tidy_oahu_data <- select(oahulistings, 
                         "year", 
                         "zipcode",
                         "price", 
                         "property_type", 
                         "host_name",
                         "accommodates")

view(tidy_oahu_data)

# Change property_type and host_name into factor (categorical data) class
# otherwise, summary() will not get the necessary statistic on those variables
tidy_oahu_data$property_type <- as.factor(tidy_oahu_data$property_type)
tidy_oahu_data$host_name <- as.factor(tidy_oahu_data$host_name)

# Partition the clean dataset in two different dataset (2016 & 2018)
Oahu_Airbnb_2016 <- filter(tidy_oahu_data, year == 2016)
view(Oahu_Airbnb_2016)

Oahu_Airbnb_2018 <- filter(tidy_oahu_data, year == 2018)
view(Oahu_Airbnb_2018)

#Start exploring the data for statistic value
nrow(Oahu_Airbnb_2016)
# There are 2049 Airbnb unit in 2016
nrow(Oahu_Airbnb_2018)
# There are 4535 Airbnb unit in 2018

summary(Oahu_Airbnb_2018)
# By checking the summary, we the following info:
# (Max) The zipcode that has the most Airbnb unit in 2018 was 96815 
# (Check number next to each category) For the year 2018, there were 1379 condominiums, 1256 houses, and 1087 apartments
# (Mean of price) The average nightly price is $238.60
# (Median of price) The median nightly price is $175
# (MAx & MIN of price) The range of nightly price is minimum to maximum value, $101 - $995

#Get the frequency distribution table of zipcode
table(Oahu_Airbnb_2018$zipcode)
# There are 2101 unit under zipcode:96815 

#Creating a simple linear regression of accommodates on price 
regression_model <- lm(price ~ accommodates, data = Oahu_Airbnb_2018)
summary(regression_model)
# For 1 additional person an unit can accommodates means a $40.98 increase in price

#Create new data set of unit own by "Evolve Vacation Rental"
Evolve_Vacation_Rental <- filter(Oahu_Airbnb_2018, host_name == "Evolve Vacation Rental")
summary(Evolve_Vacation_Rental)
nrow(Evolve_Vacation_Rental)
# In 2018, Evolve Vacation Rental manage 51 unique units

#Get the frequency distribution table of zipcode for units own by "Evolve Vacation Rental"
table(Evolve_Vacation_Rental$zipcode)
# The 51 unique units are under 13 different zipcode




