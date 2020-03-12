setwd("C:/Users/victor/Desktop/Economics_R_Code")
library(tidyverse)
library(sf)
library(leaflet)
Income <- read_csv("Dataset/Income.csv")
solar_permits <- read_csv("C:/Users/victor/Desktop/Economics_R_Code/Dataset/solar_permits.csv")
tractsf <- read_sf(dsn = "Dataset", layer = "tl_2019_15_tract")
Oahutract <- filter(tractsf, COUNTYFP == "003")
tail(Oahutract, 10)
tail(Income)
view(Income)


Rename_Income <- Income %>% rename(GEOID = "ID Geography")
view(Rename_Income)
Rename_Income <- mutate(Rename_Income, GEOID = str_remove(Rename_Income$GEOID, "14000US"))
#Renamed ID Geography to GEOID and remove 14000US from GEOID data

censusInc <-  left_join(Oahutract, Rename_Income, by='GEOID')

censusInc <- filter(censusInc, Year == 2017)

Household_Income <- censusInc$`Household Income by Race`

ggplot(censusInc) +geom_sf(aes(fill = Household_Income))

class(solar_permits$lat)
PV_lon <- solar_permits$lng
PV_lat <- solar_permits$lat
view(solar_permits)

ggplot(censusInc) +geom_sf(aes(fill = Household_Income))+
  geom_point(data= solar_permits, aes(x = PV_lon,
                                      y = PV_lat),
             color = "indianred2", alpha = 0.1)

leaflet(solar_permits)%>%
  addProviderTiles("OpenStreetMap.Mapnik")





