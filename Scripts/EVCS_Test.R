#Name: Victor Tran
#Final Project
#COurse: ECON 256

# Task 1 Set working environment & library
setwd("C:/Users/victor/Desktop/Economics_R_Code")

library(pdftools)
library(tidyverse)
library(sf)

# Read in the data
EV_Charge_Station <- read.csv("C:/Users/victor/Desktop/Economics_R_Code/Dataset/Hawaii_EV_Charging_Stations_Map.CSV")
view(EV_Charge_Station)
Oahu <- read_sf(dsn = "Dataset", layer = "oahu")
solar_permits <- read_csv("C:/Users/victor/Desktop/Economics_R_Code/Dataset/solar_permits.csv")
View(solar_permits)
street <- read_sf(dsn = "Dataset", layer = "Street_HARN")

#check what class type are the coordinate on the solar permit data to know how they are plotted
class(solar_permits$lat)
PV_lon <- solar_permits$lng
PV_lat <- solar_permits$lat

#Clean up the EV charging station data to truncate the coordinate points
EVCS <- EV_Charge_Station%>% separate(Location.Address, 
                                      c(NA, NA, NA, "coordinate"), 
                                      sep ="[(]", 
                                      extra = "merge", 
                                      fill = "left")%>%
  separate(coordinate, c("Latitude","Longitude"), sep = ",")%>%
  separate(Longitude, c("Longitude",NA), sep = "[)]", extra = "drop", fill ="right")
#Check if location.address is truncated into coordinates 
head(EVCS,10)
#Shrink dataset to oahu coordinate only
EVCS <- filter(EVCS, EVCS$Island == "Oahu")
view(EVCS)
#create longitude and latitude variables for plot points
EV_lon <- EVCS$Longitude
EV_lat <- EVCS$Latitude
#Check class type of our variable to ensure they can be plot. They are character class type
class(EV_lat)
class(EV_lon)
#Convert our longitude and latitude varibles to numerical data type so they can be plot
EV_lat <- as.numeric(EVCS$Latitude)
EV_lon <- as.numeric(EVCS$Longitude)

view(EVCS)


ggplot(data=Oahu) + geom_sf(fill = "olivedrab4", color = "black") +
  geom_point(data= EVCS, aes(x = EV_lon,
                             y = EV_lat),
                             color = "royalblue3") +
  geom_point(data= solar_permits, aes(x = PV_lon,
                             y = PV_lat),
             color = "indianred2", alpha = 0.1) + 
  geom_sf(data=street, color= "ivory1")+
  theme_void()+
  theme(panel.background = element_rect(fill = 'azure2')) +
  labs(title = "Exploratory Map of PV & EV station location") + 
  xlab("longitude") +
  ylab("Latitude")
  

