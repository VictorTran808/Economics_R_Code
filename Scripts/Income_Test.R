setwd("C:/Users/calamity/Documents/R_Research/Economics_R_Code")
library(tidyverse)
library(sf)
library(leaflet)
library(rgdal)
Income <- read_csv("Dataset/Income.csv")
view(Income)
str(Income)

#Oahu <- read_sf(dsn = "Dataset", layer = "oahu")
#tract <- readOGR(dsn="Dataset", layer = "tl_2019_15_tract")
## OGR data source with driver: ESRI Shapefile 
## Source: "x:/junk/claire/leaflet_plot", layer: "cb_2014_36_tract_500k"
## with 4906 features
## It has 9 fields

# convert the GEOID to a character
tract@data$GEOID<-as.character(tract@data$GEOID)

Oahu <- read_sf(dsn = "Dataset", layer = "oahu")

tractsf <- read_sf(dsn = "Dataset", layer = "tl_2019_15_tract")
Oahutract <- filter(tractsf, COUNTYFP == "003")

ggplot(data=Oahutract) + geom_sf(fill = "green4", color = "black")
ggplot(data=tractsf) + geom_sf(fill = "green4", color = "black")

tail(Oahutract, 10)
table(Oahutract$MTFCC)
table(Oahutract$NAME)

Rename_Income <- Income %>% rename(GEOID = "ID Geography")
view(Rename_Income)
Rename_Income <- mutate(Rename_Income, GEOID = str_remove(Rename_Income$GEOID, "14000US"))
#Renamed ID Geography to GEOID and remove 14000US from GEOID data

censusInc <-  left_join(Oahutract, Rename_Income, by='GEOID')
censusInc2 <-  left_join(Rename_Income, Oahutract, by='GEOID')

table(censusInc2$GEOID)
is.na(censusInc2)
#ggplot(data=Oahu) + geom_sf(fill = "green4", color = "black")
censusInc2 <- filter(censusInc2, Year == 2017)
censusInc <- filter(censusInc, Year == 2017)
table(censusInc2$AWATER)
tail(censusInc2, 10)

#Yes, censusInc Work! the northwestern HI Island are removed
ggplot(censusInc) +geom_sf()
ggplot(censusInc) +geom_sf(col= censusInc$`Household Income by Race`)
ggplot(censusInc) +geom_sf(col= censusInc$`Household Income by Race`, fill=censusInc$NAMELSAD)
#Yes Finally got a good map!, but there are some blank unfill spot on Oahu
ggplot(censusInc) +geom_sf(aes(fill = censusInc$`Household Income by Race`))





view(Rename_Income)

head(Rename_Income$GEOID,3)
testremove <- c("14000US15003009503", "14000US15003940002", "14000US15003000106")
str_remove(testremove, "14000US")


