#Task 1 
#Setting up working directory
setwd("C:/Users/victor/Desktop/Economics_R_Code")
library(tidyverse)
library(sf)

#Task 2
#Reading data set 
Oahu <- read_sf(dsn = "Dataset", layer = "oahu")
listings_2016 <- read_csv("Dataset/listings_2016.csv")
listings_2018 <- read_csv("Dataset/listings_2018.csv")

#Tidying data


#Task 3
lat = listings_2016$latitude
lon = listings_2016$longitude

lat2 = listings_2018$latitude
lon2 = listings_2018$longitude
# plot Oahu shape file only
#ggplot() + geom_sf(data=Oahu, fill= "green4", color= "black")

# plot Airbnb 2016 locations only
#ggplot() + geom_point(listings_2016, mapping =  aes(x = lat, y = lon, color = "red3")) 

#Create map of Airbnb on Oahu in 2016  
ggplot(data=Oahu) + geom_sf(fill = "green4", color = "black") +
  geom_point(data= listings_2016, aes(x = lon,
                                      y = lat, 
                                      color = property_type)) +
  theme_void()+
  theme(panel.background = element_rect(fill = 'azure2')) +
  labs(title = "Airbnb locations in Oahu 2016",
      caption = "Data sources are: US Census Bureau and Inside Airbnb.") 
  


#Create map of Airbnb on Oahu in 2018  
ggplot(data=Oahu) + geom_sf(fill = "green4", color = "black") +
  geom_point(data= listings_2018, aes(x = lon2,
                                      y = lat2, 
                                      color = property_type)) +
  theme_void()+
  theme(panel.background = element_rect(fill = 'azure2')) +
  labs(title = "Airbnb locations in Oahu 2016",
       caption = "Data sources are: US Census Bureau and Inside Airbnb.") 

