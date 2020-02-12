#Author: Victor Tran
#Class: ECON 256
#Professor: Justin Tyndall
#Excercise 5

#initialize working directory and tidyverse package

library(tidyverse)
setwd("C:/Users/victor/Desktop/Economics_R_Code")

#Read in dataset
medal <- read_csv("Dataset/medalcount.csv")
pop <- read.csv("Dataset/unpopulations.csv")
gdp <- read.csv("dataset/imf_gdp.csv")

#Task 1 
medalsgdp <- left_join(medal,gdp,by="country")
view(medalsgdp)
medalsgdppop <- left_join(medalsgdp,pop,by="country")
view(medalsgdppop)

#Task 2
new_medal <- mutate(medal, total_medals = gold + silver + bronze)
view(new_medal)
clean_medal <- select(new_medal, "country" ,"total_medals" )
view(clean_medal)
new_pop <- mutate(pop, pop_per_mills = population/1000000)
view(new_pop)

#Task 3
#Creating new dataset with all variables
CountryData<- left_join(medalsgdppop, clean_medal, by= 'country')
view(CountryData)
#Task 3

#Creating a new categorical variable to determine if a country is the United state
CountryData$country <- as.factor(ifelse(CountryData$country == "United States of America",1,0))
view(CountryData)
CountryData2 <- mutate(CountryData, US_or_Not = as.factor(ifelse(CountryData$country == "United States of America",1,0)) )
view(CountryData2)

#Initialize Variables
is_country_US  <- CountryData2$US_or_Not
country_pop <- CountryData2$population
country_gdp <- CountryData2$gdp_billions
Country_medal <- CountryData2$total_medals


#Graphing relationship between GDP and medal won
ggplot(CountryData2) + 
  geom_point(aes(x = country_gdp,
                 y= Country_medal, 
                 size = country_pop, 
                 color = is_country_US)) +
  xlab("GDP (billions)") + 
  ylab("Total Medals") +
  ggtitle("GDP per Billions Vs. Medals Won")


