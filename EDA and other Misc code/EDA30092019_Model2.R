# Siddhi
#Analysis of Model2 data

newData <- read.csv(file.choose())

View(head(newData))

newDataNumeric <- newData %>%
  select_if(is.numeric)
#Removing the records with NA
newDataNumeric <- na.omit(newDataNumeric)
pairs(newDataNumeric[c(3:14)],)


newDataNumericBreached <- newDataNumeric[newDataNumeric$Breached==1,]

pairs(newDataNumericBreached[c(3:14)],)

cor(newDataNumericBreached)

newData$CPAvg <- (newData$CP..03.Months + newData$CP..06.Months +newData$CP..09.Months + newData$CP..12.Months)/4
newData$VolAvg <- (newData$VOL..03.Months + newData$VOL..06.Months +newData$VOL..09.Months + newData$VOL..12.Months)/4

NewBreachedData <- newData[newData$Breached==1,]

NewNonBreachedData <-  newData[newData$Breached==0,]

#Stock Closed price All vs breached

ggplot(NewNonBreachedData, aes(x=NewNonBreachedData$CP..03.Months)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_minimal()+
  ggtitle("Closed Price Standard Deviation Density (Non-Breached)")+
  xlab("Standard Deviation") 


ggplot(newDataNumericBreached, aes(x=newDataNumericBreached$CP..03.Months)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_minimal()+
  ggtitle("Closed Price Standard Deviation Density (Breached)")+
  xlab("Standard Deviation") 








#All company org size
as.data.frame(table(newData$compSize)) %>%
  arrange(desc(Freq)) %>%
  ggplot(aes(x= reorder(Var1, Freq), y= Freq, fill = Var1)) +
  geom_bar(stat = "identity" ) + 
  xlab("Size of Organization") +
  ylab("#Breaches") +
  coord_flip() +
  geom_text(aes(label=Freq), position = position_dodge(1), hjust= -0.2) +
  theme_minimal()+
  ggtitle("Organization Size")+
  scale_fill_brewer(type = "div", palette = "RdYlBu") +
  scale_color_brewer(type = "div", palette = "RdYlBu") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1.5)))+
  theme(legend.position = "none") 


#breached comp Org size
as.data.frame(table(NewBreachedData$compSize)) %>%
  arrange(desc(Freq)) %>%
  ggplot(aes(x= reorder(Var1, Freq), y= Freq, fill = Var1)) +
  geom_bar(stat = "identity" ) + 
  xlab("Size of Organization") +
  ylab("#Breaches") +
  coord_flip() +
  geom_text(aes(label=Freq), position = position_dodge(1), hjust= -0.2) +
  theme_minimal()+
  ggtitle("Breached Organizations")+
  scale_fill_brewer(type = "div", palette = "RdYlBu") +
  scale_color_brewer(type = "div", palette = "RdYlBu") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold"))+
  theme(axis.text.y=element_text(size=rel(1.5)))+
  theme(legend.position = "none") 




fc + facet_wrap(dataBreacheInfo$BreachYear ~.) + 
  scale_fill_brewer(palette = "blue") +
  scale_color_brewer(palette = "blue") 

###############################################



## 0-3 month facet

allDataGraph<- ggplot(newData, aes(x=newData$CP..03.Months)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_minimal()+
  ggtitle("Closed Price - Normalized Standard Deviation (-[0:3]) months) Density")+
  xlab("Standard Deviation") 

allDataGraph + facet_wrap(newData$Breached ~.) + 
  scale_fill_brewer(palette = "blue") +
  scale_color_brewer(palette = "blue") 

nrow(NewBreachedData[NewBreachedData$CP..03.Months>0.25,])
nrow(NewNonBreachedData[NewNonBreachedData$CP..03.Months>0.25,])

skewness(NewBreachedData$CP..03.Months)
skewness(NewNonBreachedData$CP..03.Months)

# 0 : -3 month facet

allDataGraph<- ggplot(newData, aes(x=newData$CP..03.Months)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_minimal()+
  ggtitle("Closed Price - Normalized Standard Deviation (-3 months) Density")+
  xlab("Standard Deviation") 

allDataGraph + facet_wrap(newData$Breached ~.) + 
  scale_fill_brewer(palette = "blue") +
  scale_color_brewer(palette = "blue") 

mean(NewBreachedData$CP..03.Months)
mean(NewNonBreachedData$CP..03.Months)

nrow(NewBreachedData[NewBreachedData$CP..03.Months>0.2,])/722 - 1
nrow(NewNonBreachedData[NewNonBreachedData$CP..03.Months>0.2,])/4186 -1


skewness(NewBreachedData$CP..03.Months)
skewness(NewNonBreachedData$CP..03.Months)

# -3 : -6 month facet

  allDataGraph<- ggplot(newData, aes(x=newData$CP..06.Months)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_minimal()+
  ggtitle("Closed Price - Normalized Standard Deviation (-[3:6] months) Density")+
  xlab("Standard Deviation") 

allDataGraph + facet_wrap(newData$Breached ~.) + 
  scale_fill_brewer(palette = "blue") +
  scale_color_brewer(palette = "blue") 

mean(NewBreachedData$CP..06.Months)
mean(NewNonBreachedData$CP..06.Months)

nrow(NewBreachedData[NewBreachedData$CP..06.Months>0.2,])/722 -1
nrow(NewNonBreachedData[NewNonBreachedData$CP..06.Months>0.2,])/4186 -1


skewness(NewBreachedData$CP..06.Months)
skewness(NewNonBreachedData$CP..06.Months)

# -6 : -9 month facet

allDataGraph<- ggplot(newData, aes(x=newData$CP..09.Months)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_minimal()+
  ggtitle("Closed Price - Normalized Standard Deviation (-[6:9] months) Density")+
  xlab("Standard Deviation") 

allDataGraph + facet_wrap(newData$Breached ~.) + 
  scale_fill_brewer(palette = "blue") +
  scale_color_brewer(palette = "blue") 

mean(NewBreachedData$CP..09.Months)
mean(NewNonBreachedData$CP..09.Months)

nrow(NewBreachedData[NewBreachedData$CP..09.Months>0.2,])/722 -1
nrow(NewNonBreachedData[NewNonBreachedData$CP..09.Months>0.2,])/4186 -1

skewness(NewBreachedData$CP..09.Months)
skewness(NewNonBreachedData$CP..09.Months)


# -9 : -12 month facet

allDataGraph<- ggplot(newData, aes(x=newData$CP..12.Months)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_minimal()+
  ggtitle("Closed Price - Normalized Standard Deviation (-[9:12] months) Density")+
  xlab("Standard Deviation") 

allDataGraph + facet_wrap(newData$Breached ~.) + 
  scale_fill_brewer(palette = "blue") +
  scale_color_brewer(palette = "blue") 

mean(NewBreachedData$CP..12.Months)
mean(NewNonBreachedData$CP..12.Months)

nrow(NewBreachedData[NewBreachedData$CP..12.Months>0.2,])/722 -1
nrow(NewNonBreachedData[NewNonBreachedData$CP..12.Months>0.2,])/4186 -1

skewness(NewBreachedData$CP..12.Months)
skewness(NewNonBreachedData$CP..12.Months)


## CP average


allDataGraph<- ggplot(newData, aes(x=newData$CPAvg)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_minimal()+
  ggtitle("Closed Price - Average Normalized Standard Deviation Density Over a year Prior to Breach")+
  xlab("Standard Deviation") 

allDataGraph + facet_wrap(newData$Breached ~.) + 
  scale_fill_brewer(palette = "blue") +
  scale_color_brewer(palette = "blue") 

mean(NewBreachedData$CPAvg)
mean(NewNonBreachedData$CPAvg)

nrow(NewBreachedData[NewBreachedData$CPAvg>0.08,])/722 -1
nrow(NewNonBreachedData[NewNonBreachedData$CPAvg>0.08,])/4186 -1

skewness(NewBreachedData$CPAvg)
skewness(NewNonBreachedData$CPAvg)


########################################

##Vol 




allDataGraph<- ggplot(newData, aes(x=newData$VolAvg)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)+
  theme_minimal()+
  ggtitle("Volume - Average Normalized volume Density Over a year Prior to Breach")+
  xlab("Volume") 

allDataGraph + facet_wrap(newData$Breached ~.) + 
  scale_fill_brewer(palette = "blue") +
  scale_color_brewer(palette = "blue") 

mean(NewBreachedData$VolAvg)
mean(NewNonBreachedData$VolAvg)

nrow(NewBreachedData[NewBreachedData$VolAvg>1,])/722 -1
nrow(NewNonBreachedData[NewNonBreachedData$VolAvg>1,])/4186 -1

skewness(NewBreachedData$CPAvg)
skewness(NewNonBreachedData$CPAvg)
