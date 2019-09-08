## Load the libraries
library(tidyr)
library(tidyverse)
library(forcats)
library(dplyr)
library(caret)
library(jsonlite)
library(gtrendsR)
library(lubridate)
library(tabulizer)
library(countrycode)
library(svMisc)
library(foreach)
library(doParallel)
library(tidystringdist)
library(stringdist)
library(stringr)
library(edgarWebR)
library(spacyr)
#spacy_install()  ### Install if you have not previously Installed this package

registerDoParallel(detectCores())

## Read in the CSV file. Some warning which appear can be ignored.
dataPart1 <- read_csv("data/Privacy_Rights_Clearinghouse-Data-Breaches-Export_2005-2012.csv")
dataPart2 <- read_csv("data/Privacy_Rights_Clearinghouse-Data-Breaches-Export_2013-2019csv.csv")

## Combine the two CSVs and convert to a data frame
data <- rbind(dataPart1,dataPart2)
data <- as.data.frame(data)

## Renmae the column headings for our data breaches dataset
names(data)
names(data) <- c("DateMadePublic", "Company", "City", "State", "BreachType", "OrgType", "TotalRecords", "Description", "InfoSource","SourceURL", "BreachYear", "Latitude", "Longitude")

## Convert OrgType, Breach Yr, Breack Type and Company name to factor
data[,c("OrgType", "BreachYear", "BreachType")]<-lapply(data[,c("OrgType", "BreachYear", "BreachType")], as.factor)
sapply(data, class)

### Initial High level EDA

## Rename factor level in Breach Type

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


## Rename factor level in Org Type
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



######### Listings ###########

listingNASDAQ <- read.csv("data/NASDAQcompanylist.csv")
listingNYSE <- read.csv("data/NYSEcompanylist.csv")

combinedListing <- listingNASDAQ[,-c(8,9)]
combinedListing$Ex <- c("NASDAQ")
listingNYSE$Ex <- c("NYSE")
combinedListing <- rbind(combinedListing, listingNYSE[,-c(8,9)])

combinedListing$Symbol <- as.character(combinedListing$Symbol)
combinedListing$Name <- as.character(combinedListing$Name)
combinedListing$LastSale <- as.numeric(combinedListing$LastSale)
combinedListing$MarketCap <- as.character(combinedListing$MarketCap)
combinedListing$IPOyear <- as.character(combinedListing$IPOyear)
combinedListing$Ex <- as.factor(combinedListing$Ex)

combinedListing <- distinct(combinedListing)

combinedListing <- combinedListing %>%
  filter(combinedListing$MarketCap != "n/a")

#############################################


############## ORG Matching ################

spacy_initialize()
dataTemp <- data

# Create a new column in combinedListing with the company name cleaned up
combinedListing <- cbind(combinedListing, clean = gsub(' Incorporated| Corporated| Corporation', '', combinedListing$Name))
combinedListing$clean <- gsub('the', '', combinedListing$clean, ignore.case = TRUE)
combinedListing$clean <- gsub(', Inc|, Inc.| Inc| Inc.| Corp|, Corp| Corp.|, Corp.| Ltd.| Ltd', '', combinedListing$clean)
combinedListing$clean <- gsub('\\(The\\)|[.]|\'|,', '', combinedListing$clean)
combinedListing$clean <- str_replace_all(combinedListing$clean,"[^a-zA-Z\\s]", " ")

# copy data over from dataTemp - only to use while de-bugging. Can be deleted in final version
data <- dataTemp

# Create a new column in data with the Compnay name cleaned up

data <- cbind(data, clean = gsub(' Incorporated| Corporated| Corporation', '', data$Company))
data$clean <- gsub('the', '', data$clean, ignore.case = TRUE)
data$clean <- gsub(', Inc|, Inc.| Inc| Inc.| Corp|, Corp| Corp.|, Corp.| Ltd.| Ltd', '', data$clean)
data$clean <- gsub('\\(The\\)|[.]|\'|,', '', data$clean)
data$clean <- str_replace_all(data$clean,"[^a-zA-Z\\s]", " ")


# proc time to measure how long the loop runs for.
ptm <- proc.time()

for(rows in 1:nrow(data)){ #for all rows in the databreach set.
  rowComp <- data$clean[rows] # searh on the cleaned company name
  kw <- spacy_extract_entity(rowComp, output = "list", type = "named", multithread = TRUE) # use spacy to extract names entities as a list
  if(length(kw)==1){ # if list is of length 1, then use the comany name as keyword for lookup
    kw <- rowComp
  }
  data$KW[rows] <- kw # add column for kw used in searching

  # dl and 3.5 = 218 hits
  # lcs and 2.5 = 115 hits*
  
  # Match on first word in the listings name against companmy name where company name is less than = 2 words
   myMatch <- amatch(kw, word(combinedListing$clean,1,1), nomatch = 0, maxDist = 1, nthread = 7, method = "lcs")
   
  # match on stock symbols
   myMatch2 <- amatch(kw, combinedListing$Symbol, nomatch = 0,  maxDist = 1, nthread = 7, method = "dl")
   
   # Match on compnay name in combined listings
   myMatch3 <- amatch(kw, combinedListing$clean, nomatch = 0, maxDist = 3.5, nthread = 7, method = "dl")
  
  if(myMatch != 0){
    data$CompanyName[rows] <- combinedListing$Name[myMatch]
    data$stockTicker[rows] <- combinedListing$Symbol[myMatch]
    data$match[rows] <- c("Match 1")
  }
  else if(myMatch == 0 && myMatch2 != 0){
      data$CompanyName[rows] <- combinedListing$Name[myMatch2]
      data$stockTicker[rows] <- combinedListing$Symbol[myMatch2]
      data$match[rows] <- c("Match 2")
  }
  else if(myMatch == 0 && myMatch2 == 0 && myMatch3 != 0){
      data$CompanyName[rows] <- combinedListing$Name[myMatch3]
      data$stockTicker[rows] <- combinedListing$Symbol[myMatch3]
      data$match[rows] <- c("Match 3")
  }
  else{
      data$CompanyName[rows] <- c("Not Listed")
      data$stockTicker[rows] <- NA
      data$match[rows] <- NA
  }

}
proc.time()-ptm

# Create dataframe 'examine' to check the results of the matching excercise.
examine <- data %>%
  filter(stockTicker != "NA") %>%
  select(Company, BreachType, clean, CompanyName, stockTicker, match)
