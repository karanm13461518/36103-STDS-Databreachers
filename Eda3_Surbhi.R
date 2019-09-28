install.packages("Rcpp")
library(ggplot2)
library(dplyr)
library(plotly)
library(BCA)
library(wesanderson)
library(gganimate)
library(gifski)
library(rattle)
install.packages("rattle")
install.packages("rattle", dependencies=c("Depends", "Suggests"))

EdaData <- read.csv("~/20190919MergedData__mdStock.csv")

EdaData <- subset(EdaData, select = -c(1))

str(EdaData)

variable.summary(EdaData)

summary(EdaData)


#Bring in Breached data set info only####
#Talk to the trend etc (breaches; sectors; years;)

#Merged data set####


BD <- EdaData %>% filter(!is.na(BreachType))



#BREACH TYPE#
####Total Data Breaches by Type of Breach#### 

ggplot(data = BD) + 
  aes(x = BreachType) +
  geom_bar() +
  scale_fill_brewer(palette = "RdYlBu")
  scale_color_brewer(palette = "RdYlBu") +
  labs(title = "Data Breach Type by Year",
       x = "Data Breach Type",
       y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(vars(year)) +
  coord_flip()


####facet wrap of number of breaches by types each year####
ggplot(data = BD) + 
  aes(x = BreachType, color = BreachType) +
  geom_bar() +
  scale_color_brewer(palette = "Set3") +
  labs(title = "Data Breach Type by Year",
           x = "Data Breach Type",
           y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(vars(year)) +
  coord_flip()

####Animate facet wrap above####
ggplot(data = BD) + 
  aes(x = BreachType) +
  geom_bar() +
  scale_fill_brewer(type = 'div', palette = 'RdYlBu') +
  scale_color_brewer(type ='div', palette = "RdYlBu") +
  labs(title = "Data Breach Type by Year",
       x = "Data Breach Type",
       y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = 'none') +
  coord_flip() +
  labs(title = "Year: {frame_time}", x = "Data Breach Type", y = "Number of Breaches") +
  transition_time(year)
  ease_aes('linear') 

#Including data labels
dbtype <- as.data.frame(table(EdaData$BreachType, EdaData$year))

d <-  arrange(dbtype, desc(Freq))
  
bt <- ggplot(data = d, aes(x=reorder(Var1, Freq), y=Freq, fill=Var1)) + 
    geom_bar(stat = "identity") +
    labs(title = "Total Data Breaches by Type of Breach",
         x = "Data Breach Type",
         y = "Number of Breaches") +
    coord_flip() +
    geom_text(aes(label=Freq), position = position_dodge(1), hjust = -0.2) +
    theme_minimal() +
    theme(legend.position = "none") +
    scale_fill_brewer(type = 'div', palette = "RdYlBu") +
    scale_color_brewer(type ='div', palette = "RdYlBu") +
    labs(title = "Year: {frame_time}") +
    transition_time(Var2) 

animate(bt, nframes=14, fps=0.5)  


ggplot(d, aes(x= Var1, y= Freq, fill = Var1)) +
  geom_bar(stat = "identity" ) +
  xlab("Industry") +
  ylab("#Breaches") +
  coord_flip() +
  geom_text(aes(label=Freq), position = position_dodge(1), hjust= -0.2) +
  guides(fill=guide_legend(title="Industry")) +
  ggtitle("Number of Breaches over Top Industries")+
  theme_minimal()+
  scale_fill_brewer(type = 'div', palette = "RdYlBu") +
  scale_color_brewer(type ='div', palette = "RdYlBu") 
  labs(title = "Year: {frame_time}", x = "Company Size", y = "Number of Breaches") +
  transition_time(year) +
  ease_aes('linear')

  
  
#SECTOR#
####Total Data Breaches by sector#### 
ggplot(data = BD) +
  aes(x = Sector, fill = Sector) +
  geom_bar() +
  scale_fill_viridis_d(option  = "cividis") +
  labs(title = "Number of Breaches by Sector",
       x = "Sector",
       y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = 'none') +
  coord_flip()

####animate of number of breaches by sector each year####
BreachSectorD <- BD %>% filter(Sector !="n/a") 
  
NewBD <- ggplot(data = BreachSectorD) +
  aes(x = Sector, fill = Sector) +
  geom_bar() +
  scale_fill_brewer(palette = "RdYlBu")+
  labs(title = "Number of Breaches by Sector",
       x = "Sector",
       y = "#Breaches") +
  theme_minimal() +
  theme(legend.position = 'none') +
  coord_flip() +
  labs(title = "Year: {frame_time}") +
  transition_time(year) +
  ease_aes('linear')

BreachSector <- animate(NewBD, nframes=14, fps=0.5)  
  
anim_save(BreachSector3, animation = last_animation(), path = NULL, filename = "BreachSec")  

####animate of number of breach type by year####
BType <- BD %>% filter(BreachType !="Unknown") 

NewBT <- ggplot(data = BType) +
  aes(x = BreachType, fill = BreachType) +
  geom_bar() +
  scale_fill_brewer(palette = "RdYlBu")+
  labs(title = "#Breaches",
       x = "Type of Breach",
       y = "#Breaches") +
  theme_minimal() +
  theme(legend.position = 'none') +
  coord_flip() +
  labs(title = "Year: {frame_time}") +
  transition_time(year) +
  ease_aes('linear')

BreachType <- animate(NewBT, nframes=14, fps=0.5)  

anim_save(BreachSector3, animation = last_animation(), path = NULL, filename = "BreachSec")  

####Animate facet wrap above####
ggplot(data = BD) +
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
ggplot(data = BD) +
  aes(x = compSize, fill = compSize) +
  geom_bar() +
  scale_fill_brewer(palette = "YlGnBu") +
  labs(title = "Number of Breaches by Company Size",
       x = "Company Size",
       y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = 'none') +
  coord_flip()

#facet wrap for company size and breaches
ggplot(data = BD) + 
  aes(x = compSize, fill = compSize) +
  geom_bar() +
  scale_color_brewer(palette = "YlGnBu") +
  labs(title = "Number of Breaches by Company Size",
       x = "Company Size",
       y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = 'none') +
  facet_wrap(vars(year)) +
  coord_flip()

#animate for company size and breaches
ggplot(data = BD) + 
  aes(x = compSize, color = compSize) +
  geom_bar() +
  scale_color_brewer(palette = "YlGnBu") +
  labs(title = "Number of Breaches by Company Size",
       x = "Company Size",
       y = "Number of Breaches") +
  theme_minimal() +
  theme(legend.position = 'bottom') +
  facet_wrap(vars(year)) +
  coord_flip()
  labs(title = "Year: {frame_time}", x = "Company Size", y = "Number of Breaches") +
  transition_time(year) +
  ease_aes('linear')



#Plotting breached vs unbreached median volume across time - DO NOT USE

p <- EdaData %>% group_by(Breached,year) %>% summarise(medVol = mean(medVol))

ggplot(p, aes(x=year, y=medVol, group=Breached)) +
  geom_line()


###use for a particular industry - Finance### - DO NOT USE
ind <- EdaData %>% group_by(Breached,year) %>% 
      filter(Sector == "Health Care") %>% 
      summarise(medVol = mean(medVol))

lp1 <- ggplot(ind, aes(x=year, y=medVol, group=Breached)) +
  geom_line(aes(color = Breached)) +
  geom_point(aes(color = Breached)) +
  theme(legend.position = "bottom") +
  theme_minimal()

lp1 + scale_colour_discrete(name  ="Breached",
                            labels=c("Not Breached", "Breached"))



