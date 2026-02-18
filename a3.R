#Ryan Sordillo
#Assignment 3
#2/17/2026
library(ggplot2)
install.packages("dplyr")
library(dplyr)
library(lubridate)

datCO2 <- read.csv("/cloud/project/activity03/annual-co-emissions-by-region.csv")

colnames(datCO2)
#Rename
colnames(datCO2)[4] <- "CO2"
# convert the entity names to factor and store a variable with levels for
# easy reference
datCO2$Entity <- as.factor(datCO2$Entity)
# make a vector of all levels
name.Ent <- levels(datCO2$Entity)

#See all names of entities
name.Ent


#Visualizing Data
plot(datCO2$Year, datCO2$CO2)

# new data frame for US
US <- datCO2[datCO2$Entity == "United States",]
# new data frame for Mexico
ME <- datCO2[datCO2$Entity == "Mexico",]

# make a plot of US CO2
plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (tons CO2)", #y axis label
     xlab = "Year") #x axis label

# make a plot of US CO2
plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n") # turn off y axis
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )

# make a plot of US CO2 ----

plot(US$Year, # x data
     US$CO2, # y data
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Annual fossil fuel emissions (billons of tons CO2)", #y axis label
     xlab = "Year", #x axis label
     yaxt = "n") # turn off y axis
# add y axis
# arguments are axis number (1 bottom, 2 left, 3 top, 4 right)
# las = 2 changes the labels to be read in horizontal direction
axis(2, seq(0,6000000000, by=2000000000), #location of ticks
     seq(0,6, by = 2), # label for ticks
     las=2 )
# add mexico to plot ----
# add points
points(ME$Year, # x data
       ME$CO2, # y data
       type = "b", #b = points and lines
       pch = 19, # symbol shape,
       col= "darkgoldenrod3")
legend("topleft",
       c("United States", "Mexico"),
       col=c("black", "darkgoldenrod3"),
       pch=19, bty= "n")

library(ggplot2)

#Plot using ggplot
ggplot(data = US, # data for plot
       aes(x = Year, y=CO2 ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+ # make axis labels
  theme_classic()

# subset data for just
NorthA <- datCO2[datCO2$Entity == "United States" |
                   datCO2$Entity == "Canada" |
                   datCO2$Entity == "Mexico", ]

ggplot(data = NorthA, # data for plot
       aes(x = Year, y=CO2, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", y="US fossil fuel emissions (tons CO2)")+ # make axis labels
  theme_classic()+
    scale_color_manual(values = c("#7FB3D555","#34495E55", "#E7B80055"))

# subset CO2 to meet conditons
compCO2 <- datCO2[datCO2$Year >= 1950 & datCO2$Entity == "France" |
                    datCO2$Year >= 1950 & datCO2$Entity == "India" |
                    datCO2$Year >= 1950 &  datCO2$Entity == "Russia" , ]

ggplot(data = compCO2 , aes(x=Entity, y=CO2))+ # look at CO2 by country
  geom_violin(fill=rgb(0.933,0.953,0.98))+ # add a violin plot with blue color
  geom_boxplot(width=0.03,size=0.15, fill="grey90")+ # add grey 
  #boxplots and make them smaller to fit in the violin (width)
  #than normal with thinner lines (size_ than normal
  theme_classic()+ # get rid of ugly gridlines
  labs(x = "Country", y="Annual emissions (tons CO2)")

ggplot(data=compCO2, aes(x=Year, y=CO2, fill=Entity))+ # data
  geom_area() # geometry

ggplot(data=compCO2,
       aes(x=Year, ymin=0, ymax=CO2, fill=Entity))+ #fill works for polygons/shaded areas 
  geom_ribbon(alpha=0.5 )+ #fill in with 50% transparency
  labs(x="Year", y="Annual emissions (tons CO2)")

b <- ggplot(data=compCO2,aes(x=Year, ymin=0, ymax=CO2, fill=Entity))+
  geom_ribbon(alpha=0.5 )+
  labs(x="Year", y="Carbon emissions (tons CO2)") +
  theme_classic()

b + annotate("segment", # line label
             x=1991, # start x coordinate
             y=2450000000, # start y coordinate
             xend=1991, # end x coordinate
             yend=2600000000) + # end y coordinate
  annotate("text", # add text label
           x=1991, # center of label x coordinate
           y= 2700000000, # center of label y coordinate
           label="end of USSR") # label to add

#End of Tutorial

#In Class Prompts

data <- read.csv("/cloud/project/activity03/climate-change.csv")

#Question 1. 
#Make a plot of air temperature anomalies in the Northern and Southern Hemisphere in base R and in
#ggplot2.

#Base R plot:
#New Data frames for Northern and Southern Hemisphere
southern_hem <- data[data$Entity == "Southern Hemisphere",]
northern_hem <- data[data$Entity == "Northern Hemisphere",]
#Adjust day format to plot
northern_hem$Day <- ymd(northern_hem$Day)
southern_hem$Day <- ymd(southern_hem$Day)
#plot:
plot(northern_hem$Day, northern_hem$temperature_anomaly,
     type = "b", #b = points and lines
     pch = 19, # symbol shape
     ylab = "Temperature Anomaly", #y axis label
     xlab = "Day", #x axis label
     main = "Northern and Southern Temperature Changes"
    )
points(southern_hem$Day, # x data
       southern_hem$temperature_anomaly, # y data
       type = "b", #b = points and lines
       pch = 19, # symbol shape,
       col= "darkgoldenrod3")
legend("topleft",
       c("Northern Hemisphere", "Southern Hemisphere"),
       col=c("black", "darkgoldenrod3"),
       pch=19, bty= "n")

#ggplot:
#
data_no_world <- data[data$Entity != "World", ]
data_no_world$date <- ymd(data_no_world$Day)
ggplot(data = data_no_world, # data for plot
       aes(x = Day, y=temperature_anomaly, color=Entity) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Day", y="Temperature Anaomalies", title ="Northern and Southern Temperature Changes")+ # make axis labels
  theme_classic()+
  scale_color_manual(values = c("#7FB3D555", "#E7B80055"))


#Question 2: Plot the total all time emissions for the United States, Mexico, and Canada.

#Make cumulative CO2 emissions by country
NorthA <- NorthA %>%
  group_by(Entity) %>%
  mutate(cum_CO2 = cumsum(CO2))

ggplot(NorthA, aes(x = Year, y = cum_CO2, color = Entity)) +
  geom_line(size = 1) +
  theme_classic() +
  labs(x = "Year",
       y = expression("Cumulative emissions (tons " * CO[2] * ")"))

#HW 3:

#Question 1:
#Make a graph that communicates about emissions from any countries of your choice. Explain how
#you considered principles of visualization in making your graph.

#Looking at China CO2 Emissions:
asia_major <- datCO2[datCO2$Entity == "China" |
                       datCO2$Entity == "Russia" |
                       datCO2$Entity == "India",]
ggplot(data = asia_major, # data for plot
       aes(x = Year, y=CO2, color=Entity ) )+ # aes, x and y
  geom_point()+ # make points at data point
  geom_line()+ # use lines to connect data points
  labs(x="Year", 
       y = expression(" Fossil Fuel Emissions (tons " * CO[2] * ")"), 
       title="Carbon Emissions for Major Asia Countries")+ # make axis labels
  theme_classic()+
  scale_color_manual(values = c("#7FB3D555","#34495E55", "#E7B80055"))

#2.
#You are tasked with communicating the change in world air temperatures and CO emissions to a 
#broad audience in visually appealing graphs. Make two graphs to present in your word document side
#by side. Plot world CO emissions on one graph and world air temperature anomalies on the other
#graph.


world_yearly <- datCO2 %>%
  group_by(Year) %>%
  summarise(total_co2 = sum(CO2))

world_yearly$co2_mil <- world_yearly$total_co2 / 100000000

ggplot(world_yearly, aes(x = Year, y = co2_mil)) +
  geom_line(size = 1) +
  theme_classic() +
  labs(x = "Year",
       y = expression("World emissions (Hundred million tons " * CO[2] * ")"),
       title = "World CO2 Emisisons")


world_temp <- data[data$Entity == "World",]
world_temp$Day <- ymd(world_temp$Day)
ggplot(data = world_temp, # data for plot
       aes(x = Day, y=temperature_anomaly ) )+ # aes, x and y
  geom_point()+ # make points at data point
  labs(x="Year", y="World Temperature Anomaly(C)",
       title="World Temperature Anomalies")+ # make axis labels
  theme_classic()


#Question 3.
#Look up any type of environmental data of your interest in our world in data (link in tutorial).
#Download the csv and upload it to RStudio Cloud. Remake the graph. You may make the graph exactly
#as it is or alter it to present the data in a different format. Explain how you considered principles of
#visualization in making your graph. Explain the main conclusion of the graph

energy <- read.csv("/cloud/project/activity03/electricity-prod-source-stacked.csv")

energy$total <- rowSums(energy[,-c(1,2,3)])
energy$nuclear_per <- energy$Nuclear / energy$total

ggplot(energy, aes(x = Year, y = nuclear_per)) +
  geom_area(fill = "blue", alpha = 0.7) +
  labs(title="Nuclear as Percentage of All Energy",
       y="Percentage")+
  theme_classic()






