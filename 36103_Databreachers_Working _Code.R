#### Load the libraries
library(tidyr)
library(tidyverse)
library(tidytext)
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
library(stopwords)
library(qdap)
library(tools)
library(DMwR)
library(pROC)
#devtools::install_github(repo = 'mlampros/fuzzywuzzyR')
library(fuzzywuzzyR)

#Added by Richard Zhang
library(Riex)
library(rlang)
library(purrr)
#spacy_install()  ### Install if you have not previously Installed this package

registerDoParallel(detectCores())

#### Functions (Karan)
cleanCompName <- function(compName){
  temp <- gsub(' Incorporated| Corporated| Corporation| Inc.| Inc| Corp| Limited| Ltd| Pty| The', '', compName)
  temp <- gsub('[[:punct:] ]+',' ',temp)
  
  return(temp)
}

repalceNAWithMedian <- function(x) {replace(x, is.na(x), median(x[!is.na(x)]))}


#### Breaches CSV Files (Karan)
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

#### Initial High level EDA

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



######### Listings (Karan) ###########

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

################# Loading and Merging Company informaition from RIEX (Karan) #################

cData1 <- read.csv("data/CompanyStats1.csv")
cData2 <- read.csv("data/df_Company_Stats2new.csv")
cData3 <- read.csv("data/df_Company_Stats3.csv")
cData4 <- read.csv("data/df_Company_Stats4.csv")

riexData <- rbind(cData1, cData2, cData3, cData4)

riexData <- riexData[,c("symbol", "employees")]

names(riexData) <- c("Symbol", "numEmployees")

riexData$numEmployees[is.na(riexData$numEmployees)] <- median(riexData$numEmployees, na.rm = TRUE)

riexData$compSize <- factor(case_when(
  riexData$numEmployees <= 1000 ~ "Small",
  riexData$numEmployees > 1000 & riexData$numEmployees <= 5000 ~ "Medium",
  riexData$numEmployees > 5000 ~ "Large"
))

riexData$numEmployees <- NULL
riexData <- unique(riexData)


############## Get Market Cap (Rohan and Karan) ################

stkFileList <- as.data.frame(list.files("data/stocks", full.names = TRUE))
names(stkFileList) <- c("FileName")
stkFileList$FileName <- as.character(stkFileList$FileName)

mktCap_df <- data.frame()

for(i in 1:nrow(stkFileList)){
  fileName <- stkFileList$FileName[i]
  
  fName <- basename(file_path_sans_ext(fileName))
  mySymbol <- toupper(sub(pattern = "(.*)\\..*$", replacement = "\\1", fName))
  
  checkFile <- file.info(fileName)
  
  if(checkFile$size != 0){
    tempData <- read.csv(fileName)
    
    names(tempData) <- c("Date","Open","High","Low","Close","Volume","OpenInt")
    
    tempData$Open <- as.numeric(as.character(tempData$Open))
    tempData$High <- as.numeric(as.character(tempData$High))
    tempData$Close <- as.numeric(as.character(tempData$Close))
    tempData$Volume <- as.numeric(as.character(tempData$Volume))
    
    tempData$year <- format(as.Date(tempData$Date, format="%Y-%m-%d"),"%Y")
    tempData$dailyDiff <- tempData$Close - tempData$Open
    
    tempData <- tempData %>%
      group_by(year) %>%
      summarise(medVol = median(Volume), medClosePrice = median(Close), 
                medDiff = median(dailyDiff), medHigh = median(High))
    
    tempData$Symbol <- mySymbol
    
    mktCap_df <- rbind(mktCap_df, tempData)
  }
  
}

## Impute NA values with median
mktCap_df$medVol[is.na(mktCap_df$medVol)] <- median(mktCap_df$medVol, na.rm = TRUE)
mktCap_df$medClosePrice[is.na(mktCap_df$medClosePrice)] <- median(mktCap_df$medClosePrice, na.rm = TRUE)
mktCap_df$medDiff[is.na(mktCap_df$medDiff)] <- median(mktCap_df$medDiff, na.rm = TRUE)
mktCap_df$medHigh[is.na(mktCap_df$medHigh)] <- median(mktCap_df$medHigh, na.rm = TRUE)



mktCap_df <- mktCap_df %>%
  filter(year >= 2005)

#mktCap_df_test <- spread(mktCap_df, year, medCap)

############## ORG Matching (Karan) ################

dataTemp <- data
data <- dataTemp

data$clean <- cleanCompName(data$Company)
combinedListing$clean <- cleanCompName(combinedListing$Name)


data$CompanyName <- NA
data$Symbol  <- NA
#data$match <- NA

# proc time to measure how long the loop runs for.
ptm <- proc.time()

for(rows in 1:nrow(data)){ #for all rows in the databreach set.
  
  kw <- data$clean[rows] # searh on the cleaned company name
  
  
  # 
  #   # dl and 3.5 = 218 hits
  #   # lcs and 2.5 = 115 hits*
  #   
  # Match on first word in the listings name against companmy name where company name is less than = 2 words
  myMatch <- amatch(kw, word(combinedListing$clean,1,2), nomatch = 0, maxDist = 2, nthread = 7, method = "lcs")
  
  # match on stock symbols
  myMatch2 <- amatch(kw, combinedListing$Symbol, nomatch = 0,  maxDist = 1, nthread = 7, method = "dl")
  #myMatch2 <- match(kw, combinedListing$Symbol, nomatch = 0)

   
  # Fuzzy match on compnay name in combined listings
  matchMy <- GetCloseMatches(kw, combinedListing$clean, n = 1, cutoff = 0.75)
  myMatchVec <- as.character(as.vector(matchMy))
  indexNo <- which(combinedListing$clean == myMatchVec)
  
  if(myMatch != 0){
    data$CompanyName[rows] <- combinedListing$Name[myMatch]
    data$Symbol[rows] <- combinedListing$Symbol[myMatch]
    #data$match[rows] <- c("Match 1")
  }
  else if(myMatch == 0 && myMatch2 != 0){
    data$CompanyName[rows] <- combinedListing$Name[myMatch2]
    data$Symbol[rows] <- combinedListing$Symbol[myMatch2]
    #data$match[rows] <- c("Match 2")
  }
  else if(myMatch == 0 && myMatch2 == 0 && length(indexNo) != 0){
    data$CompanyName[rows] <- combinedListing$Name[indexNo]
    data$Symbol[rows] <- combinedListing$Symbol[indexNo]
    #data$match[rows] <- c("Match 4")
  }
  
  
}
proc.time()-ptm

################# Merging Data (Karan) #################

# dataSummary <- data %>%
#   filter(is.na(CompanyName) == FALSE) %>%
#   select(Symbol, Company, City, State, BreachType, 
#          TotalRecords, BreachYear, Latitude, Longitude)

dataMerge <- select(data,-c("Description", "InfoSource", "SourceURL", "clean"))
dataMerge$BreachYear <- year(as.Date(as.character(dataMerge$BreachYear), format = "%Y"))

names(dataMerge) <- c("DateMadePublic", "OrigCompany", "City" , "State", "BreachType", "OrgType", "TotalRecords", "BreachYear", "Latitude", "Longitude", "MatchedCompanyName", "Symbol")

riexData$Symbol <- as.character(riexData$Symbol)

listingsMerge <- select(combinedListing, -c("Ex", "clean"))


medStock_df <- mktCap_df[,c("year", "Symbol", "medVol", "medClosePrice", "medDiff", "medHigh")]
medStock_df$year <- as.numeric(medStock_df$year)

listingsMerge <- listingsMerge %>% left_join(riexData, by = "Symbol")

mergedData_mdStock <- medStock_df %>% inner_join(listingsMerge, by = "Symbol")
mergedData_mdStock <- mergedData_mdStock %>% left_join(dataMerge, by = c("Symbol", "year" = "BreachYear"))


mergedData_mdStock$Breached <- ifelse(is.na(mergedData_mdStock$MatchedCompanyName), FALSE, TRUE)
mergedData_mdStock$MatchedCompanyName <- NULL

write.csv(mergedData_mdStock, "CSV_EDA/20190919MergedData__mdStock.csv")

################# Regression (Karan) #################

regData <- select(mergedData_mdStock, -c("Name", "LastSale", "MarketCap", "DateMadePublic", "OrigCompany", "City", "State", "BreachType", "OrgType", "TotalRecords", "Latitude", "Longitude"))

prop.table(table(regData$Breached))

# regData$IPOyear <- as.numeric(as.character(format(as.Date(regData$IPOyear , format="%Y"),"%Y")))
regData$Symbol <- as.factor(regData$Symbol)
regData$compSize <- as.factor(regData$compSize)
regData$year <- as.factor(regData$year)
regData$IPOyear <- as.factor(regData$IPOyear)

regData$Breached <- ifelse(regData$Breached == TRUE, 1, 0)
regData$Breached <- as.factor(regData$Breached)


regData$medClosePrice[is.na(regData$medClosePrice)] <- median(regData$medClosePrice, na.rm = TRUE)
regData$medVol[is.na(regData$medVol)] <- median(regData$medVol, na.rm = TRUE)
regData$medDiff[is.na(regData$medDiff)] <- median(regData$medDiff, na.rm = TRUE)
regData$medHigh[is.na(regData$medHigh)] <- median(regData$medHigh, na.rm = TRUE)


summary(regData)

set.seed(123)
trainIndex <- createDataPartition(regData$year, p = .7,
                                  list = FALSE,
                                  times = 1)

trainSet <- regData[trainIndex,]
testSet <- regData[-trainIndex,]

### SMOTE Training Set

trainSet$Breached <- as.factor(trainSet$Breached)
trainSet$Symbol <- as.factor(trainSet$Symbol)
# trainSet <- SMOTE(Breached ~ year+medVol+medClosePrice+medDiff+medHigh+IPOyear, as.data.frame(trainSet), perc.over = 100)
trainSet <- SMOTE(Breached ~ ., as.data.frame(trainSet), perc.over = 500, n.cores = 7)


prop.table(table(trainSet$Breached))

trnCtrl <- trainControl(method = "cv", number = 5)
glmModel <- train(Breached ~ medVol+medClosePrice+medDiff+medHigh, data = trainSet, method = "glm", family = "binomial", trControl = trnCtrl, na.action = na.exclude)
# glm(Breached ~ ., family = binomial, data = trainSet)

pred <- predict(glmModel$finalModel, testSet, , type = "response")


auc <- roc(testSet$Breached, pred)
print(auc)

#### Richard Zhang ####
#### iex_info_extract combined function
IexInfoExtract <- function(stockCode, iex_sk, dataType)
{
  stock.info.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url_stocks_list <- paste0(url_prefix, "/ref-data/symbols?token=", iex_sk)
  req <- httr::GET(url_stocks_list)
  req_status <- req$status_code
  if (req_status != 200) {
    print("Connection Failed to IEX Cloud API")
    return(stock.info.df)
  }
  stocks_list <- rjson::fromJSON(file = url_stocks_list)
  stocks_list <- sapply(stocks_list, "[[", "symbol")
  stocks_list <- as.list(stocks_list)
  if (toupper(stockCode) %in% stocks_list == TRUE) { 
    print("Stock Info is available in IEX")
  } else {
    print("Stock Info is not available in IEX")
    return(stock.info.df)
  }  
  x_encoded <- urltools::url_encode(paste(toupper(stockCode), collapse = ", "))
  if (dataType != "") {
    type_string = dataType
  } else {
    print("Unexpected requested information type.")
    return(stock.info.df)
  }
  url = paste0(url_prefix, "/stock/", x_encoded, "/", type_string, "?token=", iex_sk)
  url.info <- httr::GET(url)
  url.content <- httr::content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")) {
    print("No output to parse.")
    return(stock.info.df)
  } else {
    stock.info.raw <- rjson::fromJSON(url.content)
  }
  #Convert to single layer list
  if (dataType == "company") { 
    stock.info.list <- stock.info.raw
  } else if(dataType %in% c("stats", "earnings")) {
    stock.info.list <- lapply(rapply(stock.info.raw, enquote, how = "unlist"), eval)
  }
  #Add missing variables
  if (dataType == "stats") {
    stock.info.list <- prepend(stock.info.list, values = as.character(stockCode), before = 1)
    names(stock.info.list)[1] <- "symbol"
  } else if (dataType == "earnings") {
    names_Earning <- names(stock.info.list)
    names_Earning <- str_replace(names_Earning, "earnings.", "")
    names(stock.info.list) <- names_Earning
    if (!("symbol" %in% names_Earning)) {
      stock.info.list <- append(stock.info.list, x_encoded, after = length(stock.info.list))
      names_Earning <- append(names_Earning, "symbol", after = length(stock.info.list))
      names(stock.info.list) <- names_Earning
    }
    if (!("actualEPS" %in% names_Earning)) {
      stock.info.list <- append(stock.info.list, -100000000, after = length(stock.info.list))
      names_Earning <- append(names_Earning, "actualEPS", after = length(stock.info.list))
      names(stock.info.list) <- names_Earning
    }
    if (!("consensusEPS" %in% names_Earning)) {
      stock.info.list <- append(stock.info.list, -100000000, after = length(stock.info.list))
      names_Earning <- append(names_Earning, "consensusEPS", after = length(stock.info.list))
      names(stock.info.list) <- names_Earning
    }
    if (!("announceTime" %in% names_Earning)) {
      stock.info.list <- append(stock.info.list, "", after = length(stock.info.list))
      names_Earning <- append(names_Earning, "announceTime", after = length(stock.info.list))
      names(stock.info.list) <- names_Earning
    }
    if (!("numberOfEstimates" %in% names_Earning)) {
      stock.info.list <- append(stock.info.list, -100000000, after = length(stock.info.list))
      names_Earning <- append(names_Earning, "numberOfEstimates", after = length(stock.info.list))
      names(stock.info.list) <- names_Earning
    }
    if (!("EPSSurpriseDollar" %in% names_Earning)) {
      stock.info.list <- append(stock.info.list, -100000000, after = length(stock.info.list))
      names_Earning <- append(names_Earning, "EPSSurpriseDollar", after = length(stock.info.list))
      names(stock.info.list) <- names_Earning
    }
    if (!("EPSReportDate" %in% names_Earning)) {
      stock.info.list <- append(stock.info.list, "", after = length(stock.info.list))
      names_Earning <- append(names_Earning, "EPSReportDate", after = length(stock.info.list))
      names(stock.info.list) <- names_Earning
    }
    if (!("fiscalPeriod" %in% names_Earning)) {
      stock.info.list <- append(stock.info.list, "", after = length(stock.info.list))
      names_Earning <- append(names_Earning, "fiscalPeriod", after = length(stock.info.list))
      names(stock.info.list) <- names_Earning
    }
    if (!("fiscalEndDate" %in% names_Earning)) {
      stock.info.list <- append(stock.info.list, "", after = length(stock.info.list))
      names_Earning <- append(names_Earning, "fiscalEndDate", after = length(stock.info.list))
      names(stock.info.list) <- names_Earning
    }
    if (!("yearAgo" %in% names_Earning)) {
      stock.info.list <- append(stock.info.list, -100000000, after = length(stock.info.list))
      names_Earning <- append(names_Earning, "yearAgo", after = length(stock.info.list))
      names(stock.info.list) <- names_Earning
    }
    if (!("yearAgoChangePercent" %in% names_Earning)) {
      stock.info.list <- append(stock.info.list, -100000000, after = length(stock.info.list))
      names_Earning <- append(names_Earning, "yearAgoChangePercent", after = length(stock.info.list))
      names(stock.info.list) <- names_Earning
    }
  }
  #Null variable value handling
  if (dataType == "company") {
    if (is_null(stock.info.list$symbol)) {
      stock.info.list$symbol <- ""
    }
    if (is_null(stock.info.list$companyName)) {
      stock.info.list$companyName <- ""
    }
    if (is_null(stock.info.list$exchange)) {
      stock.info.list$exchange <- ""
    }
    if (is_null(stock.info.list$industry)) {
      stock.info.list$industry <- ""
    }
    if (is_null(stock.info.list$website)) {
      stock.info.list$website <- ""
    }
    if (is_null(stock.info.list$CEO)) {
      stock.info.list$CEO <- ""
    }
    if (is_null(stock.info.list$issueType)) {
      stock.info.list$issueType <- ""
    }
    if (is_null(stock.info.list$sector)) {
      stock.info.list$sector <- ""
    }
    if (is_null(stock.info.list$employees)) {
      stock.info.list$employees <- -100000000
    }
    if (is_null(stock.info.list$address)) {
      stock.info.list$address <- ""
    }
    if (is_null(stock.info.list$address2)) {
      stock.info.list$address2 <- ""
    }
    if (is_null(stock.info.list$state)) {
      stock.info.list$state <- ""
    }
    if (is_null(stock.info.list$city)) {
      stock.info.list$city <- ""
    }
    if (is_null(stock.info.list$zip)) {
      stock.info.list$zip <- ""
    }
    if (is_null(stock.info.list$country)) {
      stock.info.list$country <- ""
    }
    if (is_null(stock.info.list$phone)) {
      stock.info.list$phone <- ""
    }
  } else if (dataType == "stats") {
    if (is_null(stock.info.list$week52change)) {
      stock.info.list$week52change <- -100000000
    }
    if (is_null(stock.info.list$week52high)) {
      stock.info.list$week52high <- -100000000
    }
    if (is_null(stock.info.list$week52low)) {
      stock.info.list$week52low <- -100000000
    }
    if (is_null(stock.info.list$marketcap)) {
      stock.info.list$marketcap <- -100000000
    }
    if (is_null(stock.info.list$employees)) {
      stock.info.list$employees <- -100000000
    }
    if (is_null(stock.info.list$day200MovingAvg)) {
      stock.info.list$day200MovingAvg <- -100000000
    }
    if (is_null(stock.info.list$day50MovingAvg)) {
      stock.info.list$day50MovingAvg <- -100000000
    }
    if (is_null(stock.info.list$float)) {
      stock.info.list$float <- -100000000
    }
    if (is_null(stock.info.list$avg10Volume)) {
      stock.info.list$avg10Volume <- -100000000
    }
    if (is_null(stock.info.list$avg30Volume)) {
      stock.info.list$avg30Volume <- -100000000
    }
    if (is_null(stock.info.list$ttmEPS)) {
      stock.info.list$ttmEPS <- -100000000
    }
    if (is_null(stock.info.list$ttmDividendRate)) {
      stock.info.list$ttmDividendRate <- -100000000
    }
    if (is_null(stock.info.list$companyName)) {
      stock.info.list$companyName <- ""
    }
    if (is_null(stock.info.list$sharesOutstanding)) {
      stock.info.list$sharesOutstanding <- -100000000
    }
    if (is_null(stock.info.list$maxChangePercent)) {
      stock.info.list$maxChangePercent <- -100000000
    }
    if (is_null(stock.info.list$year5ChangePercent)) {
      stock.info.list$year5ChangePercent <- -100000000
    }
    if (is_null(stock.info.list$year2ChangePercent)) {
      stock.info.list$year2ChangePercent <- -100000000
    }
    if (is_null(stock.info.list$year1ChangePercent)) {
      stock.info.list$year1ChangePercent <- -100000000
    }
    if (is_null(stock.info.list$ytdChangePercent)) {
      stock.info.list$ytdChangePercent <- -100000000
    }
    if (is_null(stock.info.list$month6ChangePercent)) {
      stock.info.list$month6ChangePercent <- -100000000
    }
    if (is_null(stock.info.list$month3ChangePercent)) {
      stock.info.list$month3ChangePercent <- -100000000
    }
    if (is_null(stock.info.list$month1ChangePercent)) {
      stock.info.list$month1ChangePercent <- -100000000
    }
    if (is_null(stock.info.list$day30ChangePercent)) {
      stock.info.list$day30ChangePercent <- -100000000
    }
    if (is_null(stock.info.list$day5ChangePercent)) {
      stock.info.list$day5ChangePercent <- -100000000
    }
    if (is_null(stock.info.list$nextDividendDate)) {
      stock.info.list$nextDividendDate <- ""
    }
    if (is_null(stock.info.list$dividendYield)) {
      stock.info.list$dividendYield <- -100000000
    }
    if (is_null(stock.info.list$nextEarningsDate)) {
      stock.info.list$nextEarningsDate <- ""
    }
    if (is_null(stock.info.list$exDividendDate)) {
      stock.info.list$exDividendDate <- ""
    }
    if (is_null(stock.info.list$peRatio)) {
      stock.info.list$peRatio <- -100000000
    }
    if (is_null(stock.info.list$beta)) {
      stock.info.list$beta <- -100000000
    }
  } else if (dataType == "earnings") {
    if (is_null(stock.info.list$actualEPS)) {
      stock.info.list$actualEPS <- -100000000
    }
    if (is_null(stock.info.list$consensusEPS)) {
      stock.info.list$consensusEPS <- -100000000
    }
    if (is_null(stock.info.list$announceTime)) {
      stock.info.list$announceTime <- ""
    }
    if (is_null(stock.info.list$numberOfEstimates)) {
      stock.info.list$numberOfEstimates <- -100000000
    }
    if (is_null(stock.info.list$EPSSurpriseDollar)) {
      stock.info.list$EPSSurpriseDollar <- -100000000
    }
    if (is_null(stock.info.list$EPSReportDate)) {
      stock.info.list$EPSReportDate <- ""
    }
    if (is_null(stock.info.list$fiscalPeriod)) {
      stock.info.list$fiscalPeriod <- ""
    }
    if (is_null(stock.info.list$fiscalEndDate)) {
      stock.info.list$fiscalEndDate <- ""
    }
    if (is_null(stock.info.list$yearAgo)) {
      stock.info.list$yearAgo <- -100000000
    }
    if (is_null(stock.info.list$yearAgoChangePercent)) {
      stock.info.list$yearAgoChangePercent <- -100000000
    }
  }
  #Convert to dataframe
  stock.info.df <- tibble::as_tibble(stock.info.list)
  if (dataType == "company") {
    stock.info.df <- stock.info.df %>% dplyr::select(-("tags")) %>% 
      dplyr::distinct()
  } else if (dataType == "earnings") {
    col_Order <- c("symbol", "actualEPS", "consensusEPS", "announceTime", "numberOfEstimates", "EPSSurpriseDollar", "EPSReportDate", "fiscalPeriod", "fiscalEndDate", "yearAgo", "yearAgoChangePercent")
    stock.info.df <- stock.info.df[, col_Order]
  }
  #Replace missing values with NA
  stock.info.df[stock.info.df == ""] <- NA
  stock.info.df[stock.info.df == -100000000] <- NA
  return(stock.info.df)
}

#Key initialisation
sk_Active <- "Replace this with your key"
#Company index listings
Company_List <- combinedListing$Symbol

#### Outputs
df_Company_Info <- data.frame()
df_Company_Stats <- data.frame()
df_Company_Earnings <- data.frame()

for (i in 1:6614) {
  print(i)
  df1 <- data.frame()
  df1 <- IexInfoExtract(stockCode = Company_List[i], iex_sk = sk_Active, dataType = "company")
  if (length(df1) != 0) {
    df_Company_Info <- rbind(df_Company_Info, df1)
  }
}
for (i in 1:6614) {
  print(i)
  df2 <- data.frame()
  df2 <- IexInfoExtract(stockCode = Company_List[i], iex_sk = sk_Active, dataType = "stats")
  if (length(df2) != 0) {
    df_Company_Stats <- rbind(df_Company_Stats, df2)
  }
}
for (i in 1:6614) {
  print(i)
  df3 <- data.frame()
  df3 <- IexInfoExtract(stockCode = Company_List[i], iex_sk = sk_Active, dataType = "earnings")
  if (length(df3) != 0) {
    df_Company_Earnings <- rbind(df_Company_Earnings, df3)
  }
}