#' In this code I was finally able to do a choropleth map of 2018 acs income data by means of properly retriving data from acs
#' I was able to uses the censusap to retrieve a list of census api, import in the api of my choosing
#' Find out what attribute I need by looking over the code
#' Do the data cleaning processes and joining the new acs data with my tract data of hawaii
#' Finally able to reproduce a map with 2018 income data directly retrieve from acs instead of 2017 data that was pre-clean


#setup working directory and packages

setwd("C:/Users/calamity/Documents/R_Research/Economics_R_Code")

library(tidyverse)
library(sf)
library(censusapi)
library(usethis)

#getting started with using census api

# Add key to .Renviron
Sys.setenv(CENSUS_KEY= "2ae81a8d1fc6373243e19200753483865fe62b50")
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

apis <- listCensusApis()
View(apis)
#_________________________________________________________________________________
install.packages("devtools")
devtools::install_github("hrecht/censusapi")

acs_vars <- listCensusMetadata(name="acs/acs1", type="variables", vintage=2018, group = "B19013")
head(acs_vars)
colnames(acs_vars)
View(acs_vars)

# sahie_vars <- listCensusMetadata(name = "timeseries/healthins/sahie", 
#                                  type = "variables")
# head(sahie_vars)

#B19013_001E This is the code for acs group for median household income
#
# B19013_001E	Estimate!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)	int	B19013	0	TRUE	MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2018 INFLATION-ADJUSTED DOLLARS)
# B19013_001M	Margin of Error!!Median household income in the past 12 months (in 2018 inflation-adjusted dollars)	int	B19013	0	TRUE	MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN

#_________________________________________________________________________________
#This was the first fail attempt.
#hi_income only has income by county and not by tract
hi_income <- getCensus(name = "acs/acs1", vintage = 2018, 
                       vars = c("NAME", "B19013_001E", "B19013_001M"), 
                       region = "county:*", regionin = "state:15")

view(hi_income)

#Now we join hi_income with our shapefile of hawaii
tractsf <- read_sf(dsn = "Dataset", layer = "tl_2019_15_tract")

hi_tract_income <- left_join(tractsf, hi_income, by=c("COUNTYFP"="county"))
head(hi_tract_income)
Oahutract <- filter(hi_tract_income, COUNTYFP == "003")
view(Oahutract)

ggplot(Oahutract) + 
  geom_sf(aes(fill=B19013_001E), color="white") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  scale_fill_distiller(palette="Oranges", direction=1, name="Median income") +
  labs(title="2018 Median income in Oahu", caption="Source: US Census/ACS5 2018")
#__________________________________________________________________________________
# acs_income_group was a sucess
acs_income_group <- getCensus(name = "acs/acs5", 
                              vintage = 2018, 
                              vars = c("NAME", "group(B19013)"), 
                              region = "tract:*", 
                              regionin = "state:15")
head(acs_income_group,5)
view(acs_income_group)

acs_income_group <- filter(acs_income_group, acs_income_group$county == "003")
acs_income_group <- acs_income_group %>% rename(GEOID = "GEO_ID")
acs_income_group <- mutate(acs_income_group, GEOID = str_remove(acs_income_group$GEOID, "1400000US"))
#++++++++++++++EXPORTING THIS DATA SET ++++++++++++++++++++++++++++++
write_csv(acs_income_group, "Dataset/acs_2018_income.csv")
#====================================================================

#_______________________________________________________________________________________
#Now we join hi_income with our shapefile of hawaii
hi_sf <- read_sf(dsn = "Dataset", layer = "tl_2019_15_tract")
hi_sf <- filter(hi_sf, COUNTYFP == "003")

censusInc <-  left_join(Oahutract, acs_income_group, by='GEOID')
head(censusInc)
view(censusInc)
colnames(censusInc)
censusInc <- select(censusInc, -c("state.x","NAME.y","B19013_001E.x","B19013_001M.x","state.y","county","NAME_1","B19013_001MA","B19013_001EA","FUNCSTAT","MTFCC"))
#Renaming the variables for longitude & latitude
censusInc <- rename(censusInc, lng = "INTPTLON")
censusInc <- rename(censusInc, lat = "INTPTLAT")
censusInc <- rename(censusInc, Median_Income = "B19013_001E.y")
censusInc <- rename(censusInc, Margin_Error = "B19013_001M.y")

censusInc <- filter(censusInc, NAME.x != '9812')

#____________________________________________________________________________________

ggplot(censusInc) + 
  geom_sf(aes(fill=Median_Income), color="white") +
  theme_void() +
  theme(panel.grid.major = element_line(colour = 'transparent')) +
  scale_fill_distiller(palette="Oranges", direction=1, name="Median income") +
  labs(title="2018 Median income in Oahu", caption="Data from: US Census/ACS5 2018")
#Did not work quite yet due to value -6666666 still in the column for Median_Income
# class(censusInc$Median_Income)
# na_if(censusInc$Median_Income,-666666666)
#This code turn the -66666666 value in Median Income into NA values
censusInc <- censusInc %>% mutate(Median_Income = na_if(censusInc$Median_Income,-666666666))

view(censusInc)
censusInc$Median_Income[is.na(censusInc$Median_Income)] = 0
#______________________________________________________________________________________
#get a closer look at which census are na value
unknown <- filter(censusInc, is.na(censusInc$Median_Income) == TRUE)
view(unknown)
