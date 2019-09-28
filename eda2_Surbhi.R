install.packages("Rcpp")
library(ggplot2)
library(dplyr)
library(plotly)
library(BCA)
library(wesanderson)
library(gganimate)
library(gifski)

EdaData <- read.csv("~/20190919MergedData__mdStock.csv")

EdaData <- subset(EdaData, select = -c(1))

str(EdaData)

variable.summary(EdaData)

summary(EdaData)

#TDconvert year to date rather than integer/
#Does is make sense that a company would have 1 or 0 median volume traded during a year?
#595 NA's in medVol?
#is all data from kaggle apart from medVol in American dollars?
#is medDiff the change in of the median open and median close prices (=close-open) for a given year? 
#does the medHigh make sense? Is this the highest trading value in american dollars? 
#name summary highlights obvious issues with matching of company names (i.e. Barclays appearing #75)
#IP0Year only available for 10% of the dataset
#Marketcap conversion from @sid
#Date made public to convert to date format
#Only 733 matches for breached companies

#Bring in Breached data set info only####
#Talk to the trend etc (breaches; sectors; years;)

#Merged data set####


face1 <- EdaData %>% filter(!is.na(BreachType))

#BREACH TYPE#
####Total Data Breaches by Type of Breach#### 
ggplot(data = face1) + 
  aes(x = BreachType) +
  geom_bar() +
  labs(title = "Total Data Breaches by Type of Breach",
       x = "Data Breach Type",
       y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = "none")

####facet wrap of number of breaches by types each year####
ggplot(data = face1) + 
  aes(x = BreachType, color = BreachType) +
  geom_bar() +
  scale_color_brewer(palette = "Set3") +
  labs(title = "Data Breach Type by Year",
           x = "Data Breach Type",
           y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  facet_wrap(vars(year)) +
  coord_flip()

####Animate facet wrap above####
ggplot(data = face1) + 
  aes(x = BreachType, color = BreachType) +
  geom_bar() +
  scale_color_brewer(palette = "Set3") +
  labs(title = "Data Breach Type by Year",
       x = "Data Breach Type",
       y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  coord_flip() +
  labs(title = "Year: {frame_time}", x = "Data Breach Type", y = "Number of Breaches") +
  transition_time(year) +
  ease_aes('linear') 

#SECTOR#
####Total Data Breaches by sector#### 
ggplot(data = face1) +
  aes(x = Sector, fill = Sector) +
  geom_bar() +
  scale_fill_viridis_d(option  = "cividis") +
  labs(title = "Number of Breaches by Sector",
       x = "Sector",
       y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = 'none') +
  coord_flip()

####facet wrap of number of breaches by sector each year####
ggplot(data = face1) +
  aes(x = Sector, fill = Sector) +
  geom_bar() +
  scale_fill_viridis_d(option  = "cividis") +
  labs(title = "Number of Breaches by Sector",
       x = "Sector",
       y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = 'none') +
  coord_flip() + 
  labs(title = "Year: {frame_time}", x = "Sector", y = "Number of Breaches") +
  transition_time(year) +
  ease_aes('linear')

####Animate facet wrap above####
ggplot(data = face1) +
  aes(x = Sector, fill = Sector) +
  geom_bar() +
  scale_fill_viridis_d(option  = "cividis") +
  labs(title = "Number of Breaches by Sector",
       x = "Sector",
       y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = 'none') +
  coord_flip()

#COMPANY SIZE#

ggplot(data = face1) +
  aes(x = compSize, fill = compSize) +
  geom_bar() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(title = "Number of Breaches by Company Size",
       x = "Company Size",
       y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = 'none') +
  coord_flip()






p <- ggplot(EdaData, aes(x=Breached, y=medVol, fill=text, color=text)) +
  geom_violin(width=2.1, size=0.2) +
  theme_ipsum() +
  theme(legend.position="none") +
  coord_flip() +
  xlab("") +
  ylab("")

p


p <- EdaData %>% group_by(Breached,year) %>% summarise(medVol = mean(medVol))


ggplot(p, aes(x=year, y=medVol, group=Breached)) +
  geom_line()





