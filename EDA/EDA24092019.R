#install.packages("ggrepel")
library(ggrepel)
library(ggplot2)
library(tidyverse)
library(lubridate)
#install.packages("gganimate")
library(gganimate)
#install.packages("gifski")
library(gifski)
library(RColorBrewer)

library(usmap)
library(openintro)
library(viridis)


data <- read.csv(file.choose())  #first file with one breach per record and other non breached


#Considering only breach data in dataBreachedInfo
dataBreacheInfo <- data[!is.na(data$BreachYear),]

##Plots
library(Amelia)
#missmap to check missing data
missmap(data, main = "Missing values vs observed")
# typeURL, recordsTotalm NumBreach have missing values as there are more than 5500 records which were not breached


options("scipen"=100, "digits"=4 )
quantile(dataBreacheInfo$TotalRecords)
max(dataBreacheInfo$TotalRecords)





#converting str to date format
data$DateMadePublicDate <- mdy(as.character(data$DateMadePublic))


# Top Breached Records

tail(sort(dataBreacheInfo$TotalRecords),5)


CompTopRecords <- dataBreacheInfo %>% 
  group_by(dataBreacheInfo$Name) %>%
  summarise(RecordsBreached = sum(TotalRecords)) %>%
  arrange(desc(RecordsBreached))

head(CompTopRecords, 5) # top 5 orgs records breached

FinalData <- read.csv(file.choose())  # the new complete data
SectorTopRecords <- FinalData %>% 
  select(Name, Sector)%>%
  group_by(FinalData$Sector) %>%
  summarise(RecordsBreached = sum(FinalData$TotalRecords)) %>%
  arrange(desc(RecordsBreached))

#dataBreacheInfo <- data[!is.na(data$BreachYear),]

View(dataBreacheInfo)

#class(FinalData$TotalRecords)
#head(SectorTopRecords)

##

# The palette with grey colour:
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#ggplot(dataBreacheInfo, aes(y=dataBreacheInfo$TotalRecords)) + geom_boxplot()


summary(dataBreacheInfo)

#Industry

dTableIndustry <- as.data.frame(table(dataBreacheInfo$industry))

d <- dTableIndustry %>%
   arrange(desc(Freq)) %>%
   slice(1:7)
head(d)

ind <- ggplot(d, aes(x= reorder(Var1, Freq), y= Freq, fill = Var1)) +
  geom_bar(stat = "identity" ) + 
  xlab("Industry") +
  ylab("#Breaches") +
  coord_flip() +
  geom_text(aes(label=Freq), position = position_dodge(1), hjust= -0.2) +
  guides(fill=guide_legend(title="Industry")) +
  theme_minimal()+
  ggtitle("Number of Breaches over Top Industries")


ind +  scale_fill_brewer(type = "div", palette = "RdYlBu") +
  scale_color_brewer(type = "div", palette = "RdYlBu") 

#

#Sector

dTableSector <- as.data.frame(table(dataBreacheInfo$Sector))

d1 <- dTableSector %>%
  arrange(desc(Freq)) %>%
  slice(1:7)

d1 <- as.data.frame(d1)
# 
# for( i in 1:nrow(d1))
# {
#   if(d$Freq[i] < 30)
#     d$Ind[i] <- "Other"
#   else
#     d$Ind[i] <- as.character(d$Var1[i])
# }
# d$Ind <- as.factor(d$Ind)

ggplot(d1, aes(x= reorder(Var1, Freq), y= Freq, fill = Var1)) +
  geom_bar(stat = "identity" ) + 
  xlab("Sector") +
  ylab("#Breaches") +
  coord_flip() +
  geom_text(aes(label=Freq), position = position_dodge(1), hjust= -0.2) +
  theme_minimal()+
  ggtitle("Number of Breaches over Top Sectors")+
  scale_fill_brewer(type = "div", palette = "RdYlBu") +
  scale_color_brewer(type = "div", palette = "RdYlBu") 



  #guides(fill=guide_legend(title="Sector"))


# Year


dTableYear <- as.data.frame(table(dataBreacheInfo$BreachYear))
dTableYear <- dTableYear[!dTableYear$Var1 == "2005",]
dTableYear <- dTableYear[!dTableYear$Var1 == "2018",]
dTableYear <- dTableYear[!dTableYear$Var1 == "2019",]

dTableYear
#class(dTableYear$Var1)
# d1 <- dTableYear %>%
#   arrange(desc(Freq)) %>%
#   slice(1:7)
#dTableYear

#levels(dTableYear$Var1)

p <- ggplot(dTableYear, aes(x= 2004+as.numeric(Var1), y= Freq)) +
  geom_bar(stat = "identity") +
  xlab("Year") +
  ylab("Number of Breaches") +
  geom_text(aes(label=Freq), position = position_dodge(1), vjust= -0.3)+
  geom_smooth(method = "lm",se = FALSE, col="red")+
  ggtitle("Number of Breaches Over Years")+
  theme_minimal()+
  scale_color_brewer(palette = "RdYlBu")
  


p
  
p + transition_reveal(as.numeric(Var1))


### SectorWise Breaches

sec = "Health Care"
ggplot(dataBreacheInfo[dataBreacheInfo$Sector==sec,], aes(x=Sector, fill = BreachType)) +
  geom_bar(position = "dodge", col= "black")+
  coord_flip()+
  ggtitle("Breach Types for the Sector")+
  scale_fill_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") +
  theme_minimal()




### 

dataBreacheInfo1 <- dataBreacheInfo[!dataBreacheInfo$Sector=="n/a",] 
sp <- ggplot(dataBreacheInfo, aes(x=TotalRecords, y= Sector))+
  geom_point()

sp




###########

#BreachType

DTableBrchType <- as.data.frame(table(dataBreacheInfo$BreachType))

d2 <- DTableBrchType %>%
  arrange(desc(Freq)) %>%
  slice(1:7)

d2 <- as.data.frame(d2)
# 
# for( i in 1:nrow(d1))
# {
#   if(d$Freq[i] < 30)
#     d$Ind[i] <- "Other"
#   else
#     d$Ind[i] <- as.character(d$Var1[i])
# }
# d$Ind <- as.factor(d$Ind)

ggplot(d2, aes(x= reorder(Var1, Freq), y= Freq, fill = Var1)) +
  geom_bar(stat = "identity" ) + 
  xlab("Breach Type") +
  ylab("#Breaches") +
  coord_flip() +
  geom_text(aes(label=Freq), position = position_dodge(1), hjust= -0.2) +
  theme_minimal()+
  ggtitle("Total Data Breaches by Breach Type")+
  scale_fill_brewer(type = "div", palette = "RdYlBu") +
  scale_color_brewer(type = "div", palette = "RdYlBu") 



#guides(fill=guide_legend(title="Sector"))




##############

ggplot(dataBreacheInfo, aes(x= dataBreacheInfo$BreachYear, fill = dataBreacheInfo$BreachType )) +
  geom_histogram(stat = "identity" ) + 
  xlab("Breach Year") +
  ylab("#Breaches") +
  coord_flip() +
  #geom_text(aes(label=Freq), position = position_dodge(1), hjust= -0.2) +
  theme_minimal()+
  ggtitle("Total Data Breaches by Breach Year")+
  scale_fill_brewer(type = "div", palette = "RdYlBu") +
  scale_color_brewer(type = "div", palette = "RdYlBu") 
# 












######Map Updated - Code originally suggested by Richard



#df_Merged_final <- read_csv("CSV_EDA/20190919MergedData__mdStock.csv")
df_Merged_final <- read_csv(file.choose())
#df_temp <- read_csv("data/MergedData_InitialOutput.csv")
df_temp <- read.csv(file.choose())
####Themes
#Special theme for the map - no extra features shown on plot other than legend
theme_Custom_USMap <- theme_void()
#General graphs - can be modified by adding additional components to the theme either here or using the "+" method (e.g. new_Theme <- theme_Custom_USMap + theme_Custom_GeneralGraphs)
theme_Custom_GeneralGraphs <- theme(panel.background = element_blank(),
                                    axis.line = element_line(colour = "black"),
                                    axis.text = element_text(colour = "black"),
                                    plot.title = element_text(size = rel(2), hjust = 0.5))  #hjust is relative to the graph only

#Map plot generation
#Separate dataframe for the map plot only
df_Merged_final$StateAbbriv <- df_Merged_final$State
df_Merged_final$StateAbbriv[which(!is.na(df_Merged_final$StateAbbriv))] <- state2abbr(df_Merged_final$StateAbbriv[which(!is.na(df_Merged_final$StateAbbriv))])
#Morphing data
df_Map_Plot <- df_Merged_final[, c("StateAbbriv", "BreachType")]
df_Map_Plot <- df_Map_Plot %>% 
  filter(!is.na(StateAbbriv)) %>%
  group_by(StateAbbriv) %>%
  summarise(BreachesByState = n())
df_Map_Plot$fips <- fips(df_Map_Plot$StateAbbriv)

#The actual map plotting function
Map_Ouput <- plot_usmap(regions = "states", data = df_Map_Plot, values = "BreachesByState", theme = theme_Custom_USMap, color = "red") + 
  scale_fill_viridis_c() + 
  labs(fill = "Breaches")
Map_Ouput

Map_Ouput <- plot_usmap(regions = "states", data = df_Map_Plot, values = "BreachesByState", theme = theme_Custom_USMap, color = "red") + 
  scale_fill_continuous(low = "pink", high = "red", name = "#Breaches", label = scales::comma) + 
  labs(fill = "Breaches")

Map_Ouput

#Plotter saving
ggsave("USMapPlot.jpg", plot = Map_Ouput, device = "jpeg") #use a combination of variables width, height, and unit to define size of image as width and height in specified unit (in, cm, or mm)




###### Finance Sector
#df_Merged_final <- read_csv("CSV_EDA/20190919MergedData__mdStock.csv")
#df_Merged_final <- read_csv(file.choose())
df_Merged_finalFin <- df_Merged_final[df_Merged_final$Sector=="Finance",] 
#df_temp <- read_csv("data/MergedData_InitialOutput.csv")
#df_temp <- read.csv(file.choose())
#head(df_temp)
####Themes
#Special theme for the map - no extra features shown on plot other than legend
theme_Custom_USMap <- theme_void()
#General graphs - can be modified by adding additional components to the theme either here or using the "+" method (e.g. new_Theme <- theme_Custom_USMap + theme_Custom_GeneralGraphs)
theme_Custom_GeneralGraphs <- theme(panel.background = element_blank(),
                                    axis.line = element_line(colour = "black"),
                                    axis.text = element_text(colour = "black"),
                                    plot.title = element_text(size = rel(2), hjust = 0.5))  #hjust is relative to the graph only

#Map plot generation
#Separate dataframe for the map plot only
df_Merged_finalFin$StateAbbriv <- df_Merged_finalFin$State
df_Merged_finalFin$StateAbbriv[which(!is.na(df_Merged_finalFin$StateAbbriv))] <- state2abbr(df_Merged_finalFin$StateAbbriv[which(!is.na(df_Merged_finalFin$StateAbbriv))])
#Morphing data
df_Map_Plot <- df_Merged_finalFin[, c("StateAbbriv", "BreachType")]
df_Map_Plot <- df_Map_Plot %>% 
  filter(!is.na(StateAbbriv)) %>%
  group_by(StateAbbriv) %>%
  summarise(BreachesByState = n())
df_Map_Plot$fips <- fips(df_Map_Plot$StateAbbriv)

#The actual map plotting function
Map_Ouput <- plot_usmap(regions = "states", data = df_Map_Plot, values = "BreachesByState", theme = theme_Custom_USMap, color = "red") + 
  scale_fill_viridis_c() + 
  labs(fill = "Breaches")
Map_Ouput

Map_Ouput <- plot_usmap(regions = "states", data = df_Map_Plot, values = "BreachesByState", theme = theme_Custom_USMap, color = "red") + 
  scale_fill_continuous(low = "pink", high = "red", name = "#Breaches", label = scales::comma) + 
  labs(fill = "Breaches")

Map_Ouput



#####

###### Healthcare Sector
#df_Merged_final <- read_csv("CSV_EDA/20190919MergedData__mdStock.csv")
#df_Merged_final <- read_csv(file.choose())
df_Merged_finalFin <- df_Merged_final[df_Merged_final$Sector=="Health Care",] 
#df_temp <- read_csv("data/MergedData_InitialOutput.csv")
#df_temp <- read.csv(file.choose())
#head(df_temp)
####Themes
#Special theme for the map - no extra features shown on plot other than legend
theme_Custom_USMap <- theme_void()
#General graphs - can be modified by adding additional components to the theme either here or using the "+" method (e.g. new_Theme <- theme_Custom_USMap + theme_Custom_GeneralGraphs)
theme_Custom_GeneralGraphs <- theme(panel.background = element_blank(),
                                    axis.line = element_line(colour = "black"),
                                    axis.text = element_text(colour = "black"),
                                    plot.title = element_text(size = rel(2), hjust = 0.5))  #hjust is relative to the graph only

#Map plot generation
#Separate dataframe for the map plot only
df_Merged_finalFin$StateAbbriv <- df_Merged_finalFin$State
df_Merged_finalFin$StateAbbriv[which(!is.na(df_Merged_finalFin$StateAbbriv))] <- state2abbr(df_Merged_finalFin$StateAbbriv[which(!is.na(df_Merged_finalFin$StateAbbriv))])
#Morphing data
df_Map_Plot <- df_Merged_finalFin[, c("StateAbbriv", "BreachType")]
df_Map_Plot <- df_Map_Plot %>% 
  filter(!is.na(StateAbbriv)) %>%
  group_by(StateAbbriv) %>%
  summarise(BreachesByState = n())
df_Map_Plot$fips <- fips(df_Map_Plot$StateAbbriv)

#The actual map plotting function

Map_Ouput <- plot_usmap(regions = "states", data = df_Map_Plot, values = "BreachesByState", theme = theme_Custom_USMap, color = "red") + 
  scale_fill_continuous(low = "pink", high = "red", name = "#Breaches", label = scales::comma) + 
  labs(fill = "Breaches")

Map_Ouput






###### Technology Sector
#df_Merged_final <- read_csv("CSV_EDA/20190919MergedData__mdStock.csv")
#df_Merged_final <- read_csv(file.choose())
df_Merged_finalFin <- df_Merged_final[df_Merged_final$Sector=="Technology",] 
#df_temp <- read_csv("data/MergedData_InitialOutput.csv")
#df_temp <- read.csv(file.choose())
#head(df_temp)
####Themes
#Special theme for the map - no extra features shown on plot other than legend
theme_Custom_USMap <- theme_void()
#General graphs - can be modified by adding additional components to the theme either here or using the "+" method (e.g. new_Theme <- theme_Custom_USMap + theme_Custom_GeneralGraphs)
theme_Custom_GeneralGraphs <- theme(panel.background = element_blank(),
                                    axis.line = element_line(colour = "black"),
                                    axis.text = element_text(colour = "black"),
                                    plot.title = element_text(size = rel(2), hjust = 0.5))  #hjust is relative to the graph only

#Map plot generation
#Separate dataframe for the map plot only
df_Merged_finalFin$StateAbbriv <- df_Merged_finalFin$State
df_Merged_finalFin$StateAbbriv[which(!is.na(df_Merged_finalFin$StateAbbriv))] <- state2abbr(df_Merged_finalFin$StateAbbriv[which(!is.na(df_Merged_finalFin$StateAbbriv))])
#Morphing data
df_Map_Plot <- df_Merged_finalFin[, c("StateAbbriv", "BreachType")]
df_Map_Plot <- df_Map_Plot %>% 
  filter(!is.na(StateAbbriv)) %>%
  group_by(StateAbbriv) %>%
  summarise(BreachesByState = n())
df_Map_Plot$fips <- fips(df_Map_Plot$StateAbbriv)

#The actual map plotting function

Map_Ouput <- plot_usmap(regions = "states", data = df_Map_Plot, values = "BreachesByState", theme = theme_Custom_USMap, color = "red") + 
  scale_fill_continuous(low = "pink", high = "red", name = "#Breaches", label = scales::comma) + 
  labs(fill = "Breaches")

Map_Ouput




###### Consumer Services Sector
#df_Merged_final <- read_csv("CSV_EDA/20190919MergedData__mdStock.csv")
#df_Merged_final <- read_csv(file.choose())
df_Merged_finalFin <- df_Merged_final[df_Merged_final$Sector=="Consumer Services",] 
#df_temp <- read_csv("data/MergedData_InitialOutput.csv")
#df_temp <- read.csv(file.choose())
#head(df_temp)
####Themes
#Special theme for the map - no extra features shown on plot other than legend
theme_Custom_USMap <- theme_void()
#General graphs - can be modified by adding additional components to the theme either here or using the "+" method (e.g. new_Theme <- theme_Custom_USMap + theme_Custom_GeneralGraphs)
theme_Custom_GeneralGraphs <- theme(panel.background = element_blank(),
                                    axis.line = element_line(colour = "black"),
                                    axis.text = element_text(colour = "black"),
                                    plot.title = element_text(size = rel(2), hjust = 0.5))  #hjust is relative to the graph only

#Map plot generation
#Separate dataframe for the map plot only
df_Merged_finalFin$StateAbbriv <- df_Merged_finalFin$State
df_Merged_finalFin$StateAbbriv[which(!is.na(df_Merged_finalFin$StateAbbriv))] <- state2abbr(df_Merged_finalFin$StateAbbriv[which(!is.na(df_Merged_finalFin$StateAbbriv))])
#Morphing data
df_Map_Plot <- df_Merged_finalFin[, c("StateAbbriv", "BreachType")]
df_Map_Plot <- df_Map_Plot %>% 
  filter(!is.na(StateAbbriv)) %>%
  group_by(StateAbbriv) %>%
  summarise(BreachesByState = n())
df_Map_Plot$fips <- fips(df_Map_Plot$StateAbbriv)

#The actual map plotting function

Map_Ouput <- plot_usmap(regions = "states", data = df_Map_Plot, values = "BreachesByState", theme = theme_Custom_USMap, color = "red") + 
  scale_fill_continuous(low = "pink", high = "red", name = "#Breaches", label = scales::comma) + 
  labs(fill = "Breaches")

Map_Ouput







