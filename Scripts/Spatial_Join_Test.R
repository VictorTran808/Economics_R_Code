
setwd("C:/Users/calamity/Documents/R_Research/Economics_R_Code")
library(tidyverse)
library(sf)

acs_2018_income <- read_csv("Dataset/acs_2018_income.csv")
hi_sf <- read_sf(dsn = "Dataset", layer = "tl_2019_15_tract")
hi_sf <- filter(hi_sf, COUNTYFP == "003")
hi_sf <- filter(hi_sf, NAME != '9812')
PV <- read.csv("Dataset/solar_permits.csv")
# This is a plot of hi_sf geometry
ggplot(hi_sf) + geom_sf()

#let try to plot census tract with pv
class(hi_sf$geometry)

hi_pv_tract <- hi_sf$geometry[PV,]

colnames(PV)
class(PV)

sf::st_as_sf(PV, coords = c("lng", "lat"), crs = 4326)
#But the geometry is not yet inputed in PV data

#This work! https://stackoverflow.com/questions/51494230/convert-from-data-frame-to-sf?noredirect=1&lq=1
#Ok now it work, if the function is assign to PV, lat & long are now geometry
PV= sf::st_as_sf(PV, coords = c("lng", "lat"), crs = st_crs(hi_sf))
#The crs is finally similar to hi_sf after restarting!
head(PV, 5)
colnames(PV)
class(PV)
class(hi_sf)
class(PV$lat)
#This did not work
st_as_sf(PV, coords = c("lng", "lat"), 
                 crs = 4326, relation_to_geometry = "field")
colnames(hi_sf)
#LEt try to do a spatial join
hi_sf[PV$geometry]

#This is the code below that did the join 
hi_pv_sf <- st_join(PV, hi_sf["GEOID"])
colnames(hi_pv_sf)

#Export the new hi_pv_sf file
st_write(hi_pv_sf, dsn = "hi_pv_sf.shp", layer = "hi_pv_sf.shp", driver = "ESRI Shapefile")

# FInally fix the issues!
# https://r-spatial.github.io/sf/reference/st_transform.html
# the crs of PV and hi_sf are different

st_crs(PV)
st_crs(hi_sf)

st_crs(PV %>% st_transform(crs = "+proj=longlat +datum=NAD83 +no_defs"))

hi_sf %>% st_transform(crs = "+init=epsg:4326")

# https://github.com/r-spatial/sf/issues/1215
#___________________________________________________________________________________
st_join(PV,hi_sf)
head(PV)
colnames(PV)
st_join(PV,hi_sf,join = st_within)
