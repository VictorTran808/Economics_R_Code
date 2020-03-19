##Shiny App Example

#setwd ("C:/Users/victor/Desktop/Economics_R_Code")

library(tidyverse)
library(sf)
library(leaflet)
library(shiny)
library(shinydashboard)
library(highcharter)
fars<- read_csv("Dataset/fars.csv")

#pick your state
Texas<-filter(fars,state==20)

## Make a Simple Feature
Texassf<-st_as_sf(x = Texas, 
                  coords = c("longitude", "latitude")) 


ui <- dashboardPage(
  
  dashboardHeader(title = span("Texan Fatalities")),
  
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
    
    #put a map in here, the data set you want to map will be called mapped
    #ggplot()+ geom_sf(data = mapped, color = "black")
    
    leaflet(mapped)%>%
      addProviderTiles("OpenStreetMap.Mapnik")%>%
      addCircles(radius = 1, color= "red", label = paste("Drunk Driver?", Texassf$drunk_drive, sep=""))
    
    labs <- as.list(paste("Date: ", Texassf$month, "_", Texassf$day,"_", Texassf$year, "<br>",
                          "Num of death: ", Texassf$fatalities, "<br>",
                          "Drunk driver?", Texassf$drunk_driver, sep = "" ))
    
    leaflet(mapped)%>%
      addProviderTiles("Hydda.Full",)%>%
      addCircles(radius = 1, color="red", label = lapply(labs, HTML))
    
  })
  
  
  output$plot<-renderHighchart({
    #Put a graph in here
    
    sumdata <- aggregate(Texas$fatalities, list(Texas$year), sum)
    sumdata <- rename(sumdata, year=Group.1, fatalities=x)
    hchart(sumdata,"line",hcaes(x=year, y=fatalities), name= "Fatalities")%>%
      hc_xAxis(title = list(text = "Year"))%>%
      hc_yAxis(title = list(text= "Fatalities")) %>%
      hc_title(text= "Dumb Texan")%>%
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
    
    "This is my ECON 256 shiny excercise"
    
  })
  
  
}

shinyApp(ui = ui, server = server)


