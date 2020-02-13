#Task 1
# Setting up working directory
setwd("C:/Users/victor/Desktop/Economics_R_Code")
library(tidyverse)
library(sf)

#Task 2
#Downloaded Counties and equavilent data
#Shape file are located in working directory file 'Dataset' of my directory

#Task 3 
#Reading the nytiowa data set and tidy it up (reuse code form lab 6)
nytiowa <- read_csv("Dataset/nytiowa.csv")
View(nytiowa)

long_nytiowa <- pivot_longer(nytiowa,
                             c("Buttigieg", "Sanders", "Warren", "Biden"),
                             names_to = "Canidates",
                             values_to = "Delegate")
view(long_nytiowa)

long_nytiowa <- group_by(long_nytiowa, County)
long_nytiowa <- mutate(long_nytiowa, winner =max(Delegate))
Winner_nytiowa <- filter(long_nytiowa, Delegate == winner)
view(Winner_nytiowa)

#Task 4
#Loading our Shape file
counties <- read_sf(dsn = "Dataset", layer = "tl_2019_us_county")
iowa<-filter(counties,STATEFP=="19")

#Task 5 
#Join our spatial data set with our tidy data table
iowa <- rename(iowa, County = NAME)
spatial_iowa <- left_join(iowa, Winner_nytiowa, by="County")

#Task 6
#plotting the choropleth graph of Iowa Caucus Winner by county
plot(spatial_iowa ["Canidates"],main="Iowa Caucus Result",key.pos = 1)
