#Author: Victor Tran
#Class: ECON 256
#Professor: Justin Tyndall
#Excercise 4

#initialize working directory and tidyverse package
setwd("/Users/victor/Desktop/myproject")
library(tidyverse)
library(RColorBrewer)
#Importing in 2010 county data
mydata<- read_csv("C:/Users/victor/Desktop/Economics_R_Code/Dataset/nhgis0002_ds172_2010_county.csv")
View(mydata)

help("select")

#Cleaning up data selecting specific variables under a specific filter
cleandata <- select(mydata, "STATE", "H7X005", "H7W002", "H7W005", "H7V001")
view(cleandata)

is.na(cleandata)

CaliforniaData <-filter(cleandata,  STATE == "California")
view(CaliforniaData)
#Creating variable objects from new California datatable
#' H7X005 is the number of asian in the california county
#' H7v001 is the population in the california county
#' H7w002 is the number living in urban area
#' H7w005 is the number living in rural area
numberAsian <- CaliforniaData$H7X005
population <- CaliforniaData$H7V001/100
numberUrban <- CaliforniaData$H7W002/100 
numberRural <- CaliforniaData$H7W005/100

Asian_Percentage <- numberAsian/population
Asian_Percentage
#Note: remember that these variable were divied by 100 !!!
mean(population) # 6423.096
mean(numberAsian) #838.1047 
summary(CaliforniaData)

Cali_graph <- ggplot(CaliforniaData) + 
  geom_point(aes(x = numberRural, y= numberUrban, size = Asian_Percentage, color = numberAsian)) +
 # geom_smooth(method = "auto") +
  xlab("asian in rural area per hundred") + 
  ylab("asian in urban area per hundred") +
  ggtitle("Asian in Urban vs Rural area per California County")

#Cali_graph + scale_fill_manual(values = c("skyblue", "royalblue", "green", "red"))
# display.brewer.all()
# Cali_graph + scale_color_gradient(low = "red", high = "blue")

#Cali_graph + scale_color_brewer(palette = "Set1") 
# Error: Continuous value supplied to discrete scale

Cali_graph + scale_color_gradient2(midpoint =  8782, low = "blue", high = "red", mid = "purple")
?scale_alpha
?aes

Cali_graph + scale_color_gradientn(colours = rainbow(5)) + scale_size(range = c(1:5)) 




  