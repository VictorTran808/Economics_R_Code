# set up working directory and packages
#setwd("C:/Users/victor/Desktop/Economics_R_Code")
library(tidyverse)
library(sf)
library(leaflet)
library(sp)

#Importing Raw Data
Income <- read_csv("Dataset/Income.csv")
solar_permits <- read_csv("C:/Users/calamity/Documents/R_Research/Economics_R_Code/Dataset/solar_permits.csv")
tractsf <- read_sf(dsn = "Dataset", layer = "tl_2019_15_tract")

#Cleaning up the raw data

#First, narrow down Hawaii spatial data to Honolulu County only, creating 'Oahutract'
Oahutract <- filter(tractsf, COUNTYFP == "003")
tail(Oahutract, 10)
#Check a sample of the Income dataset
head(Income)

# There are some common data value between Our Income and Oahutract such as Geography ID.
# However, the column name are a bit different, so we have to rename ID Geography to GEOID.
# Next, we have remove "14000US" characters from the GEOID column in Income data, so we can do a join.
Rename_Income <- Income %>% rename(GEOID = "ID Geography")
Rename_Income <- mutate(Rename_Income, GEOID = str_remove(Rename_Income$GEOID, "14000US"))
view(Rename_Income)
#Renamed ID Geography to GEOID and remove 14000US from GEOID data

# Now we can do a left join, combining oahutract and Rename_Income into a new dataframe called censusInc
censusInc <-  left_join(Oahutract, Rename_Income, by='GEOID')

# There are some issues with censusInc. The first is we only need year 2017 data.
# The second is we do not need the Northwestern Hawaiian Island in our map.

#' To determine how to remove the Northwestern Hawaiian Island from our map, we have to determnine the data object
#' in our dataset that is creating the Northwesten Hawaiian Island on the map. We can identify this data object by
#' looking at our rows with NA value. To further examine our NA data geography, we seperate them to a new dataframe

unknown <- filter(censusInc, is.na(Year) == TRUE)
view(unknown)
# notice that there are now 13 object in the dataframe, and one of the row has a longiutude of -175
# This is the row we have to remove to get rid of the NW Hawaiian island from our dataset
# the name value in this row is '9812'
unknown <- filter(unknown, NAME != '9812')
ggplot(unknown) +geom_sf()

#Now we do the same for our censusInc data.
#First remove the census with NW Hawaiian Islands
censusInc <- filter(censusInc, NAME != '9812')
#Then filter dataframe with year 2017 or is NA, so our finally data will include the whole area of Oahu
censusInc <- filter(censusInc, Year == 2017 | is.na(Year) == TRUE)
#We also have to remove the geographic water area surrounding Oahu.
#We identify this geographica area, by looking at the AWATER and ALAND attribute.
#the Census tract 9900.01 has a very large water area and no land area.
censusInc <- filter(censusInc, NAME != '9900.01')
#Now we should have a perfect map of Oahu
ggplot(censusInc) +geom_sf()

#Next we try to incorporate Income data visually onto the map

#Seperate Income into a seperate vector 
Household_Income <- censusInc$`Household Income by Race`
#Map out Household Income level on the map
ggplot(censusInc) +geom_sf(aes(fill = Household_Income))
#Note: there are still some small issues such as an empty hole in near the middle of Oahu.

#Now to include solar panel data
class(solar_permits$lat)
PV_lon <- solar_permits$lng
PV_lat <- solar_permits$lat
view(solar_permits)

ggplot(censusInc) +geom_sf(aes(fill = Household_Income))+
  geom_point(data= solar_permits, aes(x = PV_lon,
                                      y = PV_lat),
             color = "indianred2", alpha = 0.1)

leaflet(solar_permits)%>% 
  addCircles(lng = ~PV_lon, lat = ~PV_lat)%>%
  addProviderTiles("OpenStreetMap.Mapnik")

#' This is similar to the the first leaflet function, but this might be the better version
#' because it allows a map layer may use a different data object to override the data provided in the leaflet function
leaflet()%>% 
  addCircles(data = solar_permits, lng = ~PV_lon, lat = ~PV_lat)%>%
  addProviderTiles("OpenStreetMap.Mapnik")

#addPolygons function require lng & lat parm to be numeric values
census_lng <- as.numeric(censusInc$INTPTLON)
census_lat <- as.numeric(censusInc$INTPTLAT)

leaflet()%>%
  addPolygons(censusInc, lng = census_lng, lat = census_lat)%>%
  addProviderTiles("OpenStreetMap.Mapnik")

leaflet(censusInc)%>%
  addPolygons(censusInc, lng = census_lng, lat = census_lat)%>%
  addTiles()


leaflet()%>%
  addPolygons(censusInc, lng = census_lng, lat = census_lat, fill = Household_Income)

#Hmm this gave me the best result so far...
leaflet()%>%
  addPolygons(data = censusInc)
#Bet there is still a warning:
# Warning message:
# sf layer has inconsistent datum (+proj=longlat +datum=NAD83 +no_defs).
# Need '+proj=longlat +datum=WGS84'

#So this warning is probably due to some issue with projection/ CRN probably
# https://community.rstudio.com/t/how-to-map-tidycensus-list-output/13547
# https://juliasilge.com/blog/using-tidycensus/

#Trying some new stuff to get my census data map using leaflet
#The results looks good and it seems like it something I could work with and improve on
pal <- colorQuantile(palette = "viridis", domain = Household_Income, n = 10)

censusInc %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~Household_Income,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(Household_Income)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ Household_Income,
            title = "Household income percentiles",
            opacity = 1)
# The st_transform is what fix the error. More information from the 2nd weblink on this matter.

# The addTiles() function gives a good background to the current map
censusInc %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~Household_Income,
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(Household_Income)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ Household_Income,
            title = "Household income percentiles",
            opacity = 1) %>%
  addTiles()

# Current issues with this map is it uses colorquantile for the pal variables, hence household income is shown as percentage
# In addition the popup param does not work, a value does not pop up when I click on an area

# Now redefining pal variable again, but as a colorNumeric function. 
pal <- colorNumeric(palette = "plasma", 
                    domain = Household_Income)

#NEw remapping looks goods. Small issues such as the legend value are reverse
#Lowest income at the top of the bar and highest at the bottom of the bar. 
censusInc %>%
  st_transform(crs = "+init=epsg:4326") %>%
  leaflet(width = "100%") %>%
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
              stroke = FALSE,
              smoothFactor = 0,
              fillOpacity = 0.7,
              color = ~ pal(Household_Income)) %>%
  addLegend("bottomright", 
            pal = pal, 
            values = ~ Household_Income,
            title = "Household income",
            labFormat = labelFormat(prefix = "$"),
            opacity = 1) %>%
  addTiles()
