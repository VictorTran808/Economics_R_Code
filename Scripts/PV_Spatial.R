setwd("C:/Users/calamity/Documents/R_Research/Economics_R_Code")
library(tidyverse)
library(sf)

pv_sf <- read_sf(dsn = "New-Dataset", layer = "hi_pv_sf")
view(pv_sf)
#The new hi_pv_sf dataset was exported sucessfull and is able to be imported

class(pv_sf)
ggplot(pv_sf) + geom_sf()

acs_2018_income <- read_csv("Dataset/acs_2018_income.csv")

#Now we join hi_income with our shapefile of hawaii
hi_sf <- read_sf(dsn = "Dataset", layer = "tl_2019_15_tract")
hi_sf <- filter(hi_sf, COUNTYFP == "003")
class(hi_sf$GEOID)#character
class(acs_2018_income$GEOID)#numeric
acs_2018_income$GEOID <- as.character(acs_2018_income$GEOID)

censusInc <-  left_join(hi_sf, acs_2018_income, by='GEOID')
head(censusInc)
view(censusInc)
colnames(censusInc)
censusInc <- select(censusInc, -c("state","NAME.y","state","county","NAME_1","B19013_001MA","B19013_001EA","FUNCSTAT","MTFCC"))
#Renaming the variables for longitude & latitude
censusInc <- rename(censusInc, lng = "INTPTLON")
censusInc <- rename(censusInc, lat = "INTPTLAT")
censusInc <- rename(censusInc, Median_Income = "B19013_001E")
censusInc <- rename(censusInc, Margin_Error = "B19013_001M")

censusInc <- filter(censusInc, NAME.x != '9812')

censusInc <- censusInc %>% mutate(Median_Income = na_if(censusInc$Median_Income,-666666666))


ggplot(censusInc) + 
  geom_sf(aes(fill=Median_Income), color="white") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  scale_fill_distiller(palette="Oranges", direction=1, name="Median income") +
  labs(title="2018 Median income in Oahu", caption="Data from: US Census/ACS5 2018")

#Let try to add the income data into our new pv sf data
st_crs(censusInc)
st_crs(pv_sf)
hi_pv_inc_sf <- st_join(pv_sf, censusInc["Median_Income"])
colnames(hi_pv_inc_sf)
colnames(pv_sf)
table(pv_sf$solar)
table(pv_sf$applcnt)
table(pv_sf$cmmrclr)
hi_pv_inc_sf <- select(hi_pv_inc_sf, -c("extrnld","applcnt","bldngprmtn","dscrptn","bldngprmtt", "solar", ))
head(hi_pv_inc_sf)

st_write(hi_pv_inc_sf, dsn = "main_data.shp", layer = "main_data.shp", driver = "ESRI Shapefile")
