library(tidyr)
library(tidyverse)
library(forcats)
library(dplyr)
library(caret)
library(jsonlite)
library(gtrendsR)
library(lubridate)
dataPart1 <- read_csv("data/Privacy_Rights_Clearinghouse-Data-Breaches-Export_2005-2012.csv")
dataPart2 <- read_csv("data/Privacy_Rights_Clearinghouse-Data-Breaches-Export_2013-2019csv.csv")

data <- rbind(dataPart1,dataPart2)
data <- as.data.frame(data)

names(data)
names(data) <- c("DateMadePublic", "Company", "City", "State", "BreachType", "OrgType", "TotalRecords", "Description", "InfoSource","SourceURL", "BreachYear", "Latitude", "Longitude")


data[,c("OrgType", "BreachYear", "BreachType", "Company")]<-lapply(data[,c("OrgType", "BreachYear", "BreachType", "Company")], as.factor)
sapply(data, class)

data <- data %>%
      mutate(BreachType = fct_recode(BreachType,
                                        "Hacking" = "HACK",
                                        "Insider" = "INSD",
                                        "Card Fraud" = "CARD",
                                        "Physical Loss" = "PHYS",
                                        "Portable Device" = "PORT",
                                        "Stationary Device" = "STAT",
                                        "Unintended Disclosure" = "DISC",
                                        "Unknown" = "UNKN"
  ))

data %>%
  count(BreachType)

data <- data %>%
  mutate(OrgType = fct_recode(OrgType,
                              "Financial" = "BSF",
                              "Business Other" = "BSO",
                              "Online Retailer" = "BSR",
                              "Educational" = "EDU",
                              "Government" = "GOV",
                              "Healthcare" = "MED",
                              "NGOs" = "NGO",
                              "Unknown" = "UNKN"))

data %>%
  count(OrgType)

dataHack <- data %>%
  filter(BreachType == "Hacking")

dataMinBreach <- dataHack %>%
  filter(TotalRecords > 3 & TotalRecords < 5)


dataHack %>%
  count(OrgType)

data %>%
  count(Company)

# create a subset of data to get trends for

getTrendsFor <- dataHack[,c("Company", "DateMadePublic", "Description")]
getTrendsFor$Company <- as.character(getTrendsFor$Company)
#getTrendsFor$DateMadePublic <- as.Date(getTrendsFor$DateMadePublic, format = "%B %d, %Y")

trendData <- data.frame()

# for(row in 1:nrow(getTrendsFor)){
for(row in 1:30){ #test loop for only first 30 companies
  
    rowComp <- getTrendsFor[row,"Company"]
    rowDate <- as.Date(getTrendsFor[row,"DateMadePublic"], format="%B %d, %Y")
    dateLower <- ymd(rowDate) %m-% months(3)
    dateUpper <- ymd(rowDate) %m+% months(3)
    dateSpan <- paste(as.character(dateLower), as.character(dateUpper), sep = " ")
    
    rowCompTrends <- gtrends(keyword = rowComp, time = dateSpan, gprop = "web", low_search_volume = FALSE, onlyInterest = FALSE, hl = "en-US")
    
    trendData[row,"Company"] <- rowComp
    trendData[row,"DateMadePublic"] <- rowDate
    trendData$Trends[[row]] <- rowCompTrends
}  
## SCRIPTING - set up a GitHub Repo
## Scripting - loading the data
## and running google trend search for company name
## and Join data
