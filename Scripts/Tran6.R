#Task 1
# initialize working directory and library packages
setwd("C:/Users/victor/Desktop/Economics_R_Code")
library(tidyverse)

#Task 2
# importing in dataset
iowalatlon <- read_csv("Dataset/iowalatlon.csv")
View(iowalatlon)

nytiowa <- read_csv("Dataset/nytiowa.csv")
View(nytiowa)


#Task 3
?pivot_longer
#convert nytiowa data table into a 'long' data table
long_nytiowa <- pivot_longer(nytiowa,
                             c("Buttigieg", "Sanders", "Warren", "Biden"),
                             names_to = "Canidates",
                             values_to = "Delegate")
view(long_nytiowa)

#Task 4
#Tidy up data to show only the winner of that county
long_nytiowa <- group_by(long_nytiowa, County)

long_nytiowa <- mutate(long_nytiowa, winner =max(Delegate))
view(long_nytiowa)

Winner_nytiowa <- filter(long_nytiowa, Delegate == winner)
view(Winner_nytiowa)

#Task 5
#Join our two data set together
Tidy_iowa <- left_join(Winner_nytiowa,iowalatlon,by="County")
view(Tidy_iowa)

#Task 6
#Plot the data spatially

lon <- Tidy_iowa$lon
lat <- Tidy_iowa$lat
Delegate <- Tidy_iowa$Delegate
Canidates <- Tidy_iowa$Canidates


iowa_graph <- ggplot(Tidy_iowa) + 
  geom_point(aes(x = lon , y=lat , size = Delegate , color = Canidates)) +
  xlab("Longitude") + 
  ylab("Latitude") +
  ggtitle("Iowa Caucus Results")

iowa_graph
