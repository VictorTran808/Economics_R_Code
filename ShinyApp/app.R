##Shiny App Example

#setwd ("C:/Users/victor/Desktop/Economics_R_Code")
setwd("C:/Users/calamity/Documents/R_Research/Economics_R_Code") 

library(tidyverse)
library(sf)
library(leaflet)
library(shiny)
library(shinydashboard)
library(highcharter)
#fars<- read_csv("Dataset/fars.csv")
fars <- read_csv("ShinyApp/Dataset/fars.csv")
#pick your state
Texas<-filter(fars,state==48)

## Make a Simple Feature
Texassf<-st_as_sf(x = Texas, 
                  coords = c("longitude", "latitude")) 


ui <- dashboardPage(
  
  dashboardHeader(title = span("Texas Traffic Fatalities")),
  
  dashboardSidebar(
    
    selectInput("year", "Year:",
                c("2018","2017","2016","2015","2014","2013","2012","2011","2010"))
  ),
  
  dashboardBody(
    
    #display the objects you have created in the Server section
    
    leafletOutput("map",height=500),
    highchartOutput("plot",height=300),
    textOutput("description")
  )
)



server <- function(input, output) {
  
  #Generate a separate data set for each year of your data
  for(y in unique(Texassf$year)){
    subsample<-filter(Texassf,year==y)
    assign(paste("Texassf", y, sep = ""),subsample)
  }
  
  
  output$map<-renderLeaflet({
    grabdata<-reactive ({get(paste("Texassf",input$year,sep=""))})
    mapped<-grabdata()
    
    
   #Texassf$drunk_drive
    
    leaflet(mapped)%>%
      addProviderTiles("OpenStreetMap.Mapnik")%>%
      addCircles(radius = 1, color= "red", label = paste("Drunk Driver?", Texassf$drunk_driver, sep=" "))
    

    # labs <- as.list(paste("Date: ", Texassf$month, "_", Texassf$day,"_", Texassf$year, "<br>",
    #                       "Num of death: ", Texassf$fatalities, "<br>",
    #                       "Drunk driver?", Texassf$drunk_driver, sep = "" ))
    # 
    # leaflet(mapped)%>%
    #    addProviderTiles("Hydda.Full",)%>%
    #   addCircles(radius = 1, color="red", label = lapply(labs, HTML))
    # 
  })
  
  
  output$plot<-renderHighchart({
    #Put a graph in here
    
    sumdata <- aggregate(Texas$fatalities, list(Texas$year), sum)
    sumdata <- rename(sumdata, year=Group.1, fatalities=x)
    hchart(sumdata,"line",hcaes(x=year, y=fatalities), name= "Fatalities")%>%
      hc_xAxis(title = list(text = "Year"))%>%
      hc_yAxis(title = list(text= "Fatalities")) %>%
      hc_title(text= "Texas Fatalities")%>%
      hc_add_theme(hc_theme_monokai())    
    
    # hchart(data = Texas,mapping = aes(x=year , y=fatalities)) +
    #   geom_line(stat = "summary", fun.y = "sum", color = "red") +
    #   xlab("Year") +
    #   ylab("number of fatalities") +
    #   ggtitle("Texas Fatalities") +
    #   theme_minimal() +
    #   theme(axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"))
  })
  
  output$description<-renderText({
    
    "This shiny app dashboard is about the state of Texas traffic fatalities over the year 2008 - 2018. Over this 10 year timeframe, 
    Texas has a total of 28051 traffic fatalities and on average 2.6 traffic fatalities daily and on the worst day has experienced a
    record of 52 traffic fatalities in a single day. The year with the highest traffic fatalities is 2016, which has 9020 traffic 
    fatalities recorded, and the year with the least traffic fatalities in 2010, which has 7028 traffic fatalities. 
    In terms of traffic fatalities relating to drunk drivers in the 10 timeframe, there have been 8,084 traffic fatalities involving
    drunk driving and 19,967 traffic fatalities that are not. In terms of pedestrian fatalities relating to the traffic incidents, 
    on average there are 0.1661 pedestrian fatalities, with 4 pedestrian fatalities at worst. In terms of numbers of suvs vehicles, 
    on average there are 0.4524 suvs involved in each traffic fatalities and in the worst traffic fatalities, there were 22 suvs involved.
    In comparison to the entire US traffic fatalities, Texas average fatalities over the 10 year period is above the national average of 2 
    traffic fatalities daily. Texas makes up 9.9% of total fatalities in the US as well as 9.9% in traffic fatalities involving drunk drivers 
    and in terms of average pedestrian fatalities in those traffic incidents, Texas is below the national average of 0.1644 pedestrian fatalities. 
    Finally, Texas fatalities from a geographic standpoint seems to be evenly dispersed over the states of Texas, however, there does seem to be a 
    higher concentration/ density of record traffic fatalities toward the west than the east. This is likely due to major city such as Austin, Dallas 
    and Houstin being located in that area of Texas."
    
  })
  
  
}

shinyApp(ui = ui, server = server)


