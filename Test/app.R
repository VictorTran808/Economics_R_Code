#This is the app script to test run my geo data app for oahu income census data.


setwd ("C:/Users/victor/Desktop/Economics_R_Code")
#------------------------------------------------------------
#import packages
library(tidyverse)
library(sf)
library(leaflet)
library(shiny)
library(shinydashboard)
library(highcharter)
#-------------------------------------------------------------


#-------------------------------------------------------------
#Importing Raw Data

Income <- read_csv("Test/Income.csv")
solar_permits <- read_csv("Test/solar_permits.csv")
#solar_permits <- read_csv("C:/Users/calamity/Documents/R_Research/Economics_R_Code/Dataset/solar_permits.csv")
tractsf <- read_sf(dsn = "Test", layer = "tl_2019_15_tract")

#Cleaning up the raw data

#First, narrow down Hawaii spatial data to Honolulu County only, creating 'Oahutract'
Oahutract <- filter(tractsf, COUNTYFP == "003")


# There are some common data value between Our Income and Oahutract such as Geography ID.
# However, the column name are a bit different, so we have to rename ID Geography to GEOID.
# Next, we have remove "14000US" characters from the GEOID column in Income data, so we can do a join.
Rename_Income <- Income %>% rename(GEOID = "ID Geography")
Rename_Income <- mutate(Rename_Income, GEOID = str_remove(Rename_Income$GEOID, "14000US"))

#Renamed ID Geography to GEOID and remove 14000US from GEOID data

# Now we can do a left join, combining oahutract and Rename_Income into a new dataframe called censusInc
censusInc <-  left_join(Oahutract, Rename_Income, by='GEOID')

# There are some issues with censusInc. The first is we only need year 2017 data.
# The second is we do not need the Northwestern Hawaiian Island in our map.



#Now we do the same for our censusInc data.
#First remove the census with NW Hawaiian Islands
censusInc <- filter(censusInc, NAME != '9812')
#Then filter dataframe with year 2017 or is NA, so our finally data will include the whole area of Oahu
censusInc <- filter(censusInc, Year == 2017 | is.na(Year) == TRUE)
#We also have to remove the geographic water area surrounding Oahu.
#We identify this geographica area, by looking at the AWATER and ALAND attribute.
#the Census tract 9900.01 has a very large water area and no land area.
censusInc <- filter(censusInc, NAME != '9900.01')


#Next we try to incorporate Income data visually onto the map

#Seperate Income into a seperate vector 
Household_Income <- censusInc$`Household Income by Race`
#Note: there are still some small issues such as an empty hole in near the middle of Oahu.
PV_lon <- solar_permits$lng
PV_lat <- solar_permits$lat

#---------------------------------------------------------------------------------


ui <- dashboardPage(
  
  dashboardHeader(title = span("Oahu 2017 Median Household Income")),
  
  dashboardSidebar(
     
     selectInput("year", "Year:",
                 c("2018","2017","2016","2015","2014","2013","2012","2011","2010"))
  ),
  
  dashboardBody(
    
    #display the objects you have created in the Server section
    
    leafletOutput("map",height=1000),
  
    #highchartOutput("plot",height=300),
    textOutput("description")
  )
)



server <- function(input, output) {
  
  #Generate a separate data set for each year of your data
  # for(y in unique(Texassf$year)){
  #   subsample<-filter(Texassf,year==y)
  #   assign(paste("Texassf", y, sep = ""),subsample)
  # }
  
  
  output$map<-renderLeaflet({
    # grabdata<-reactive ({get(paste("Texassf",input$year,sep=""))})
    # mapped<-grabdata()
    # 
    # #put a map in here, the data set you want to map will be called mapped
    # #ggplot()+ geom_sf(data = mapped, color = "black")
    # 
    # leaflet(mapped)%>%
    #   addProviderTiles("OpenStreetMap.Mapnik")%>%
    #   addCircles(radius = 1, color= "red", label = paste("Drunk Driver?", Texassf$drunk_drive, sep=""))
    # 
    # labs <- as.list(paste("Date: ", Texassf$month, "_", Texassf$day,"_", Texassf$year, "<br>",
    #                       "Num of death: ", Texassf$fatalities, "<br>",
    #                       "Drunk driver?", Texassf$drunk_driver, sep = "" ))
    # 
    # leaflet(mapped)%>%
    #   addProviderTiles("Hydda.Full",)%>%
    #   addCircles(radius = 1, color="red", label = lapply(labs, HTML))
    # Now redefining pal variable again, but as a colorNumeric function. 
    pal <- colorNumeric(palette = "viridis", 
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
      addCircles(data = solar_permits, lng = ~PV_lon, lat = ~PV_lat)%>%
      addLegend("bottomright", 
                pal = pal, 
                values = ~ Household_Income,
                title = "Household income",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1) %>%
      addTiles()
  })
  
 
  
  #output$plot<-renderHighchart({
    #Put a graph in here
    
    # sumdata <- aggregate(Texas$fatalities, list(Texas$year), sum)
    # sumdata <- rename(sumdata, year=Group.1, fatalities=x)
    # hchart(sumdata,"line",hcaes(x=year, y=fatalities), name= "Fatalities")%>%
    #   hc_xAxis(title = list(text = "Year"))%>%
    #   hc_yAxis(title = list(text= "Fatalities")) %>%
    #   hc_title(text= "Dumb Texan")%>%
    #   hc_add_theme(hc_theme_monokai())    
    
    # hchart(data = Texas,mapping = aes(x=year , y=fatalities)) +
    #   geom_line(stat = "summary", fun.y = "sum", color = "red") +
    #   xlab("Year") +
    #   ylab("number of fatalities") +
    #   ggtitle("Texas Fatalities") +
    #   theme_minimal() +
    #   theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"))
  #})
  
  output$description<-renderText({
    
    "This test app to run my interactive map of oahu income census data"
    
  })
  
  
}

shinyApp(ui = ui, server = server)
