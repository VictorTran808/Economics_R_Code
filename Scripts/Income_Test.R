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
view(Oahutract)
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


#better method to remove NW Hawaiian Island from Honolulu County Data
# after joining the data to create the censusInc dataset, there are some row w/ NA value
# put rows with NA into a seperate dataframe so that we can explore the unkown
unknown <- filter(censusInc, is.na(Year) == TRUE)
view(unknown)
# notice that there are now 13 object in the dataframe, and one of the row has a longiutude of -175
# This is the row we have to remove to get rid of the NW Hawaiian island from our dataset
# the name value in this row is '9812'
unknown <- filter(unknown, NAME != '9812')
ggplot(unknown) +geom_sf()
#Removing this object will now leave use with just a map of the Oahu census tract

#Now cleaning censusInc
#First remove the census with NW Hawaiian Islands
censusInc <- filter(censusInc, NAME != '9812')
#Then filter dataframe with year 2017 or is NA
censusInc <- filter(censusInc, Year == 2017 | is.na(Year) == TRUE)
censusInc <- filter(censusInc, NAME != '9900.01')
view(censusInc)
#Finally a perfect map


view(Rename_Income)

head(Rename_Income$GEOID,3)
testremove <- c("14000US15003009503", "14000US15003940002", "14000US15003000106")
str_remove(testremove, "14000US")


