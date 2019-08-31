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

numCores <- detectCores()
registerDoParallel(numCores)

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
data[,c("OrgType", "BreachYear", "BreachType", "Company")]<-lapply(data[,c("OrgType", "BreachYear", "BreachType", "Company")], as.factor)
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

## Count by Breach Type
data %>%
  count(BreachType)

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
## Count by Org Type
data %>%
  count(OrgType)


## Subset the data for records that only relate to Hacking. We will use this smaller set for further analysis
dataHack <- data %>%
  filter(BreachType == "Hacking")


## Summary of Org Type and Company Name in the subset data.
dataHack %>%
  count(OrgType)

data %>%
  count(Company)

## create a subset of data to get trends for
getTrendsFor <- dataHack[,c("Company", "DateMadePublic","TotalRecords", "Description")]
getTrendsFor$Company <- as.character(getTrendsFor$Company)

## Location of Global Cyper Security Index 2018 PDF file
pdfFile <- 'data/draft-18-00706_Global-Cybersecurity-Index-EV5_print_2.pdf'

## Extract the table
pdfTables <- extract_tables(pdfFile)

## Set up empty data frane
gci2018 <- data.frame()

## Parse the PDF table to extract 
for(i in 15:21){ #items in pdfTables list 15 to 21 are countries by region and their GCI score and ranking
  
    gci2018 <- rbind(gci2018, as.data.frame(pdfTables[[i]]))

}

## rename columns and clean data drame for GCI Index data extracted
names(gci2018)
names(gci2018) <- c("Country", "Score", "Rank")

gci2018 <- gci2018 %>%
  filter(Rank != "Global Rank")

gci2018$Country <- as.character(gci2018$Country)
gci2018$Score <- as.numeric(as.character(gci2018$Score))
gci2018$Rank <- as.numeric(as.character(gci2018$Rank))

gci2018$Country <- lapply(gci2018$Country, function(x){
  gsub("[*]+","",x)
 # gsub(" and ", " & ", x)
})


## Using package countrycodes to normalise country names. As we will join the GCI Index data with the Google Trends data for our breached organisations using the intrerest by country from google trends.
gci2018$Country <- lapply(gci2018$Country, countrycode, "country.name", "iso3c")
gci2018$Country <- as.character(gci2018$Country)

gci2018 <- na.omit(gci2018)

## Set up empty data frame to collect results.
trendData <- data.frame()

rows <- nrow(getTrendsFor)
rowsP1 <- round(rows / 7, 0)



## Loop through each organisation and run Google Trend Query
for(row in 1:nrow(getTrendsFor)){ ## NOTE: Loop will take > 5 hrs to run ~3K Google trend queries - RUN AT YOUR OWN Risk
#for(row in 1:30){ #test loop for only first 30 companies

      ## Set up progress bar  
    progress(row, progress.bar = TRUE)
    
  
    ## Prepare variables to use in the Google Trend Query
    rowComp <- getTrendsFor$Company[row]
    rowDate <- as.Date(getTrendsFor$DateMadePublic[row], format="%B %d, %Y")
    dateLower <- ymd(rowDate) %m-% months(3)
    dateUpper <- ymd(rowDate) %m+% months(3)
    dateSpan <- paste(as.character(dateLower), as.character(dateUpper), sep = " ")
    
    ## Run Google Trend Query
    rowCompTrends <- gtrends(keyword = rowComp, time = dateSpan, gprop = "web", low_search_volume = FALSE, onlyInterest = FALSE, hl = "en-US", cookie_url = "http://trends.google.com/Cookies/NID")
    

    
    ## Get interest by country from Trend data for the organisation
    intByCountry <- rowCompTrends$interest_by_country
    
    ## Use countrycodes package to normalise country names in the intrest by country data from Google Trends
    intByCountry$location <- lapply(intByCountry$location, countrycode, "country.name", "iso3c")
    intByCountry$location <- as.character(intByCountry$location)
    intByCountry$hits <- as.numeric(intByCountry$hits)
    intByCountry <- na.omit(intByCountry)
    intByCountry$hits <- lapply(intByCountry$hits, function(x){
      gsub("<","",x)
    })
    intByCountry$hits <- as.numeric(intByCountry$hits)
    
    ## Merge intrest by country and GCI 2018 data
    intByCountry <- merge(intByCountry, gci2018, by.x = "location", by.y = "Country", all.x = TRUE)
    
    ## Calculate weighted score.
    intByCountry$weightedGCIScore <- intByCountry$hits * intByCountry$Score
    intByCountry <- na.omit(intByCountry)
    
    
    ## Start building the output data frame
    trendData[row, "Company"] <- rowComp
    #trendData[row, "DateMadePublic"] <- rowDate
    #trendData[row, "Description"] <- getTrendsFor$Description[row]
    #trendData[row, "NumRecords"] <- getTrendsFor$TotalRecords[row]
    if(nrow(intByCountry)==0){
      trendData$medianGCIScore[row] <- as.numeric(c(0))
    }
    else{
      tempScore <- intByCountry %>%
        summarise(median(intByCountry$weightedGCIScore))
      trendData$medianGCIScore[row] <- tempScore[[1]]
    } 

} 
