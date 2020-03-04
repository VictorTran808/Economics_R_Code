setwd("C:/Users/victor/Desktop/Economics_R_Code")

library(pdftools)
library(tidyverse)
library(sf)
EV_Charge_Station <- read.csv("C:/Users/victor/Desktop/Economics_R_Code/Dataset/Hawaii_EV_Charging_Stations_Map.CSV")
view(EV_Charge_Station)

solar_permits <- read_csv("C:/Users/victor/Desktop/Economics_R_Code/Dataset/solar_permits.csv")
View(solar_permits)

Oahu <- read_sf(dsn = "Dataset", layer = "oahu")

lon <- solar_permits$lng
lat <- solar_permits$lat

ggplot(data=Oahu) + geom_sf(fill = "green4", color = "black")  +
  geom_point(data= solar_permits, aes(x = lon,
                                      y = lat), color = "red1")

ggplot(data=Oahu) + geom_sf(fill = "green4", color = "black")  +
  geom_point(data= New_EV_Charge_Station, aes(x = EV_lon,
                                      y = EV_lat), color = "red1")

# station_Location <- EV_Charge_Station$Location.Address
# view(station_Location)
?separate

#class(station_Location)
# This is a categorical class

separate(EV_Charge_Station, Location.Address,  c(NA, "Coordinate"), sep ="[(]")
view(EV_Charge_Station)

EV_Charge_Station$Location.Address[39]


# EV_Charge_Station %>% separate(Location.Address,  c("a", "b", "c", "d"), sep ="[(]", extra = "merge", fill = "left")
New_EV_Charge_Station <-EV_Charge_Station%>% 
  separate(Location.Address,  c(NA, NA, NA, "coordinate"), sep ="[(]", extra = "merge", fill = "left")%>%
  separate(coordinate, c("Latitude","Longitude"), sep = ",") %>%
  separate(Latitude, c("Latitude",NA), sep = "[)]", extra = "drop", fill = "right")

view(New_EV_Charge_Station)  

EV_lon <- New_EV_Charge_Station$Longitude
EV_lat <- New_EV_Charge_Station$Latitude

#   separate(Coordinate, c(NA, "Coordinate"), sep ="[\\(\\)]", extra = "drop", fill = "right")  
# 
# EV_Charge_Station %>% separate(Location.Address,  c(NA, "Coordinate"), sep ="[\\(\\)]") %>% 
#   separate(Coordinate, c("Longitude", "Latitude"), sep = ",") 




#========================Guide===============================================

df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))
df %>% separate(x, c("A", "B"))

# If you just want the second variable:
df %>% separate(x, c(NA, "B"))

# If every row doesn't split into the same number of pieces, use
# the extra and fill arguments to control what happens
df <- data.frame(x = c("a", "a b", "a b c", NA))
df %>% separate(x, c("a", "b"))
# The same behaviour drops the c but no warnings
df %>% separate(x, c("a", "b"), extra = "drop", fill = "right")

class(df)
#df is a data frame, so separate function only work on dataframe class

# Use regular expressions to separate on multiple characters:
df <- data.frame(x = c(NA, "a?b", "a.d", "b:c"))
df %>% separate(x, c("A","B"), sep = "([\\.\\?\\:])")
