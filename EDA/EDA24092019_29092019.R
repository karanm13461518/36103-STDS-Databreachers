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
library(Amelia) #missmap plots
library(usmap)
library(openintro)
library(viridis)
library(Hmisc)
library(e1071)  
data <- read.csv(file.choose())  #first file with one breach per record and other non breached
#20190914MergedData_mdVol

#Creating breached Variable

data$breached <- 0
data[!is.na(data$BreachYear),]$breached <- 1

#Breched Vs Non Breched Plot ##

dTableBreached <- as.data.frame(table(data$breached))

dTableBreached1 <- dTableBreached %>%
  arrange(desc(Freq))


dTableBreached1 <- as.data.frame(dTableBreached1)


ggplot(dTableBreached1, aes(x= reorder(Var1, Freq), y= Freq, fill = Var1)) +
  geom_bar(stat = "identity" ) + 
  xlab("Breached") +
  ylab("#Organizations") +
  coord_flip() +
  geom_text(aes(label=Freq), position = position_dodge(1), hjust= -0.2) +
  theme_minimal()+
  ggtitle("Breached Vs. Non-Breached Organizations")+
  scale_fill_brewer(type = "div", palette = "RdYlBu") +
  scale_color_brewer(type = "div", palette = "RdYlBu") +
  theme(legend.position = "none") +
  scale_x_discrete(labels=c("0" = "Not Breached", "1" = "Breached"))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1.5)))


################################################


#Considering only breach data in dataBreachedInfo
dataBreacheInfo <- data[!is.na(data$BreachYear),]


## Describe the data 
summary(dataBreacheInfo)
describe(dataBreacheInfo)


colnames(dataBreacheInfo)

##Plots
missmap(dataBreacheInfo, main = "Missing values vs observed")
# typeURL, recordsTotalm NumBreach have missing values as there are more than 5500 records which were not breached


options("scipen"=100, "digits"=4 )
quantile(dataBreacheInfo$TotalRecords)
max(dataBreacheInfo$TotalRecords)

skewness(dataBreacheInfo$TotalRecords)

skewness(dataBreacheInfo$LastSale)


# Top Breached Records

Table(tail(sort(dataBreacheInfo$TotalRecords),5))



CompTopRecords <- dataBreacheInfo %>% 
  group_by(dataBreacheInfo$Name) %>%
  summarise(RecordsBreached = sum(TotalRecords)) %>%
  arrange(desc(RecordsBreached))

head(CompTopRecords, 5) # top 5 orgs records breached

FinalData <- read.csv(file.choose())  # the new complete data 19092019
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

#Industry table

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
  ggtitle("Number of Breaches over Top Industries")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1.5)))+
  theme(legend.position = "none") 
  


ind +  scale_fill_brewer(type = "div", palette = "RdYlBu") +
  scale_color_brewer(type = "div", palette = "RdYlBu") 

#

#Sector table

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

summary(dataBreacheInfo)
ggplot(d1, aes(x= reorder(Var1, Freq), y= Freq, fill = Var1)) +
  geom_bar(stat = "identity" ) + 
  xlab("Sector") +
  ylab("#Breaches") +
  coord_flip() +
  geom_text(aes(label=Freq), position = position_dodge(1), hjust= -0.2) +
  theme_minimal()+
  ggtitle("Number of Breaches over Top Sectors")+
  scale_fill_brewer(type = "div", palette = "RdYlBu") +
  scale_color_brewer(type = "div", palette = "RdYlBu") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1.5)))+
  theme(legend.position = "none") 


levels(dataBreacheInfo$Sector)

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
  scale_color_brewer(palette = "RdYlBu")+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1.5)))


p
  
p + transition_reveal(as.numeric(Var1))


### SectorWise Breaches

sec = "Health Care"
HC <- ggplot(dataBreacheInfo[dataBreacheInfo$Sector==sec,], aes(x=BreachType, fill = BreachType)) +
  geom_bar(position = "dodge", col= "black")+
  coord_flip()+
  ggtitle(paste("Breach Types for Sector -",sec) )+
  scale_fill_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1.5)))+
  theme(legend.position = "none") 


sec = "Finance"
FN <- ggplot(dataBreacheInfo[dataBreacheInfo$Sector==sec,], aes(x=BreachType, fill = BreachType)) +
  geom_bar(position = "dodge", col= "black")+
  coord_flip()+
  ggtitle(paste("Breach Types for Sector -",sec) )+
  scale_fill_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1.5)))+
  theme(legend.position = "none") 


sec = "Consumer Services"
CS <- ggplot(dataBreacheInfo[dataBreacheInfo$Sector==sec,], aes(x=BreachType, fill = BreachType)) +
  geom_bar(position = "dodge", col= "black")+
  coord_flip()+
  ggtitle(paste("Breach Types for Sector -",sec) )+
  scale_fill_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1.5)))+
  theme(legend.position = "none") 


sec = "Technology"
ggplot(dataBreacheInfo[dataBreacheInfo$Sector==sec,], aes(x=BreachType, fill = BreachType)) +
  geom_bar(position = "dodge", col= "black")+
  coord_flip()+
  ggtitle(paste("Breach Types for Sector -",sec) )+
  scale_fill_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1.5)))


TC +
econdatalong <- gather(econdata, key="measure", value="value", c("GDP_nom", "GDP_PPP"))




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
  scale_color_brewer(type = "div", palette = "RdYlBu") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1.5)))+
  




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












######Map Updated Code - as suggested by Richard



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

Map_Ouput + facet_wrap(dataBreacheInfo$Sector ~.) + 
  scale_fill_brewer(palette = "blue") +
  scale_color_brewer(palette = "blue") 


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








##############################
#Facet for sector#
dataBreacheInfoFC <- dataBreacheInfo[!(dataBreacheInfo$Sector=='n/a'),]
fc<- ggplot(dataBreacheInfo, aes(x=dataBreacheInfo$BreachType)) +
  geom_bar(position = "dodge")+
  coord_flip()+
  ggtitle("Breach Types Over Years" )+
  scale_fill_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1)))+
  theme(legend.position = "none") 

fc + facet_wrap(dataBreacheInfo$BreachYear ~.) + 
  scale_fill_brewer(palette = "blue") +
  scale_color_brewer(palette = "blue") 


###






##########

ggplot(dataBreacheInfo, aes(x=dataBreacheInfo$BreachYear)) +
  geom_bar(position = "dodge", col= "black", fill=dataBreacheInfo$BreachType)+
#  coord_flip()+
  facet_grid(.~dataBreacheInfo$BreachYear)+
  ggtitle(paste("Breach Types for Year -",sec) )+
  scale_fill_brewer(palette = "RdYlBu") +
  scale_color_brewer(palette = "RdYlBu") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1.5)))+
  theme(legend.position = "none") 



ggplot(dataBreacheInfo, aes(x= dataBreacheInfo$BreachYear, y= dataBreacheInfo$TotalRecords), fill = dataBreacheInfo$BreachYear ) +
  geom_point(stat = "identity",  colour="blue" ) + 
  xlab("Breached Year") +
  ylab("#RecordsBreached") +
  coord_flip() +
 # geom_text(aes(label=Freq), position = position_dodge(1), hjust= -0.2) +
  theme_minimal()+
  ggtitle("Breached Records")+
  scale_fill_brewer(type = "div", palette = "RdYlBu") +
  scale_color_brewer(type = "div", palette = "RdYlBu") +
  theme(legend.position = "none") +
  #scale_x_discrete(labels=c("0" = "Not Breached", "1" = "Breached"))+
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1.5)))+
  geom_point(data=dataBreacheInfo[dataBreacheInfo$TotalRecords > 100000000, ],  aes(x= BreachYear, y= TotalRecords), colour="red", size=4)


# 
# 
# ggplot(dataBreacheInfo, x= dataBreacheInfo$BreachYear, y= dataBreacheInfo$LastSale, fill=Sector ) +
#   geom_point(stat = "identity") + 
#   xlab("Breached Year") +
#   ylab("#LastSale") +
#   coord_flip() +
#   facet_wrap(dataBreacheInfo$Sector~)+
#   # geom_text(aes(label=Freq), position = position_dodge(1), hjust= -0.2) +
#   theme_minimal()+
#   ggtitle("Volume Traded")+
#   scale_fill_brewer(type = "div", palette = "RdYlBu") +
#   scale_color_brewer(type = "div", palette = "RdYlBu") +
#   theme(legend.position = "none") +
#   #scale_x_discrete(labels=c("0" = "Not Breached", "1" = "Breached"))+
#   theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
#   theme(axis.text.y=element_text(size=rel(1.5)))
#  # geom_point(data=dataBreacheInfo[dataBreacheInfo$TotalRecords > 100000000, ],  aes(x= BreachYear, y= TotalRecords), colour="red", size=4)
# 
# 

