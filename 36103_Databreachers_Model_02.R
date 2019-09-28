#### Load the libraries
library(tidyr)
library(corrplot)
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
library(padr)
library(stopwords)
library(qdap)
library(tools)
library(DMwR)
library(pROC)
library(ROCR)
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

## Function to return financial trend data
## Inputs: repDate = date breach reported or point in time = 09/04/2018
##         stockSym = stock ticker for company
##         fileList_df = data frame with list of compnmay stock ticker and associated
##                       stock information text file

getTrendData <- function(repDate, stockSymbol, fileList_df){
  
  # save date and symbols to temp variables
  tempSymbol <- stockSymbol
  tempDate <- repDate
  
  # get index number of company in file list DF and get file path for text file
  fileIndex <- as.numeric(rownames(fileList_df[fileList_df$Symbol==tempSymbol,]))
  tempfile <- fileList_df[fileIndex,2]
  
  # get file information to check for 0 sized files
  checkFile <- file.info(tempfile) 
  
  # loop to read file and prepare summary dataframe
  if(checkFile$size != 0 && nrow(checkFile) !=0){
    
    tempData <- read.csv(tempfile)
    
    # set default names for file read
    names(tempData) <- c("Date","Open","High","Low","Close","Volume","OpenInt")
    
    # convert temp date to date
    tempData$Date <- as.Date(tempData$Date)
    
    # set start and end dates for use in calculations
    startDate <- tempDate
    endDate <- ymd(startDate) %m-% months(12)
    
    # filter stock data to be between start and end dates
    tempData <- tempData %>%
      filter(Date >= endDate & Date <= startDate)
    
    if(nrow(tempData)  == 0){
      dataSummary <- data.frame()
    }
    else{
      # pad dataframe to ensure full date range is accounted for
      tempData <- pad(tempData, interval = "day", start_val = endDate, end_val = startDate)
      
      # calculate daily difference in stock price Close - Open
      tempData$dailyDiff <- tempData$Close - tempData$Open
      
      
      # break up stock entried into 3 month chuncks going back 12 months
      q1EndDate <- ymd(startDate) %m-% months(3)
      q2EndDate <- ymd(startDate) %m-% months(6)
      q3EndDate <- ymd(startDate) %m-% months(9)
      q4EndDate <- endDate
      
      
      # Create a new column Quarter to group on for summaries
      tempData[tempData$Date >= q1EndDate & tempData$Dat <= startDate, "Quarter"] <- "-03 Months"
      tempData[tempData$Date >= q2EndDate & tempData$Dat < q1EndDate, "Quarter"] <- "-06 Months"
      tempData[tempData$Date >= q3EndDate & tempData$Dat < q2EndDate, "Quarter"] <- "-09 Months"
      tempData[tempData$Date >= q4EndDate & tempData$Dat < q3EndDate, "Quarter"] <- "-12 Months"
      
      # create summary for Volume traded as normalised standard deviation (SD / mean)
      summaryVol <- tempData %>%
        group_by(Quarter) %>%
        summarise(sdNormVol = sd(Volume, na.rm = T)/mean(Volume, na.rm = T))
      
      # create summary for meadian daily stock price difference
      summaryDailyDiff <- tempData %>%
        group_by(Quarter) %>%
        summarise(medDailyDiff = median(dailyDiff, na.rm = T))
      
      # create summary for Close Price as normalised standard deviation (SD / mean)
      summaryClosePrice <- tempData %>%
        group_by(Quarter) %>%
        summarise(sdNormClose = sd(Close, na.rm = T)/mean(Close, na.rm = T))
      
      # create wide data frames for summaries and rename colums
      summaryVol <- spread(summaryVol, "Quarter", sdNormVol)
      names(summaryVol) <- c("VOL -03 Months", "VOL -06 Months", "VOL -09 Months", "VOL -12 Months")
      
      summaryDailyDiff <- spread(summaryDailyDiff, "Quarter", medDailyDiff)
      names(summaryDailyDiff) <- c("DD -03 Months", "DD -06 Months", "DD -09 Months", "DD -12 Months")
      
      summaryClosePrice <- spread(summaryClosePrice, "Quarter", sdNormClose)
      names(summaryClosePrice) <- c("CP -03 Months", "CP -06 Months", "CP -09 Months", "CP -12 Months")
      
      # Join summaries and add stock symbol
      dataSummary <- cbind(summaryVol, summaryDailyDiff, summaryClosePrice)
      dataSummary$Symbol <- tempSymbol
    }
  }
  else{
    dataSummary <- data.frame()
  }
  return(dataSummary)
}

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

stockInfo_df <- data.frame()

for(i in 1:nrow(stkFileList)){
  fileName <- stkFileList$FileName[i]
  
  fName <- basename(file_path_sans_ext(fileName))
  mySymbol <- toupper(sub(pattern = "(.*)\\..*$", replacement = "\\1", fName))
  
  #checkFile <- file.info(fileName)
  stockInfo_df[i,"Symbol"] <- mySymbol
  stockInfo_df[i,"File"] <- fileName
  
  
  # if(checkFile$size != 0){
  #   tempData <- read.csv(fileName)
  #   
  #   names(tempData) <- c("Date","Open","High","Low","Close","Volume","OpenInt")
  #   
  #   tempData$Open <- as.numeric(as.character(tempData$Open))
  #   tempData$High <- as.numeric(as.character(tempData$High))
  #   tempData$Close <- as.numeric(as.character(tempData$Close))
  #   tempData$Volume <- as.numeric(as.character(tempData$Volume))
  #   
  #   tempData$year <- format(as.Date(tempData$Date, format="%Y-%m-%d"),"%Y")
  #   tempData$dailyDiff <- tempData$Close - tempData$Open
  #   
  #   tempData <- tempData %>%
  #     group_by(year) %>%
  #     summarise(medVol = median(Volume), medClosePrice = median(Close), 
  #               medDiff = median(dailyDiff), medHigh = median(High), sdVol = sd(Volume, na.rm=T), sdClosePrice = sd(Close, na.rm = T))
  #   
  #   tempData$Symbol <- mySymbol
  #   
  #   mktCap_df <- rbind(mktCap_df, tempData)
  # }
}

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

breachesData <- select(data,-c("Description", "InfoSource", "SourceURL", "clean"))
breachesData$BreachYear <- year(as.Date(as.character(breachesData$BreachYear), format = "%Y"))

names(breachesData) <- c("DateMadePublic", "OrigCompany", "City" , "State", "BreachType", "OrgType", "TotalRecords", "BreachYear", "Latitude", "Longitude", "MatchedCompanyName", "Symbol")

riexData$Symbol <- as.character(riexData$Symbol)

listingsMerge <- select(combinedListing, -c("Ex", "clean"))

# medStock_df <- mktCap_df[,c("year", "Symbol", "medVol", "medClosePrice", "medDiff", "medHigh", "sdClosePriceNorm", "sdVolNorm")]
# medStock_df$year <- as.numeric(medStock_df$year)

listingsMerge <- listingsMerge %>% left_join(riexData, by = "Symbol")

listingsMerge <- select(listingsMerge, c("Symbol", "Sector", "industry", "compSize"))

breachesData <- select(breachesData, c("DateMadePublic", "City", "State", "Symbol"))

merged_df <- listingsMerge %>% left_join(breachesData, by = "Symbol")

merged_df$Breached <- ifelse(is.na(merged_df$DateMadePublic), 0, 1)

merged_df$breachDate <- as.Date(as.character(merged_df$DateMadePublic), "%B %d, %Y")
merged_df$breachDate[is.na(merged_df$breachDate)] <- as.Date("2018-04-09") #Point in time where we have the last stock data


mySummary <- data.frame()

for(i in 1:nrow(merged_df)){
   #i <- 40
  tempSymbol <- merged_df$Symbol[i]
  tempDate <- merged_df$breachDate[i]
  
  tempSummary <- getTrendData(tempDate, tempSymbol, stockInfo_df)
  
  mySummary <- rbind(mySummary, tempSummary)
}

################# Regression (Karan) #################

regData <- merged_df %>% inner_join(mySummary, by = "Symbol")

regData$`VOL -03 Months`[is.na(regData$`VOL -03 Months`)] <- median(regData$`VOL -03 Months`, na.rm = T)
regData$`VOL -06 Months`[is.na(regData$`VOL -06 Months`)] <- median(regData$`VOL -06 Months`, na.rm = T)
regData$`VOL -09 Months`[is.na(regData$`VOL -09 Months`)] <- median(regData$`VOL -09 Months`, na.rm = T)
regData$`VOL -12 Months`[is.na(regData$`VOL -12 Months`)] <- median(regData$`VOL -12 Months`, na.rm = T)

regData$`DD -03 Months`[is.na(regData$`DD -03 Months`)] <- median(regData$`DD -03 Months`, na.rm = T)
regData$`DD -06 Months`[is.na(regData$`DD -06 Months`)] <- median(regData$`DD -06 Months`, na.rm = T)
regData$`DD -09 Months`[is.na(regData$`DD -09 Months`)] <- median(regData$`DD -09 Months`, na.rm = T)
regData$`DD -12 Months`[is.na(regData$`DD -12 Months`)] <- median(regData$`DD -12 Months`, na.rm = T)

regData$`CP -03 Months`[is.na(regData$`CP -03 Months`)] <- median(regData$`CP -03 Months`, na.rm = T)
regData$`CP -06 Months`[is.na(regData$`CP -06 Months`)] <- median(regData$`CP -06 Months`, na.rm = T)
regData$`CP -09 Months`[is.na(regData$`CP -09 Months`)] <- median(regData$`CP -09 Months`, na.rm = T)
regData$`CP -12 Months`[is.na(regData$`CP -12 Months`)] <- median(regData$`CP -12 Months`, na.rm = T)

regData$compSize[is.na(regData$compSize)] <- "Medium"

regData <- select(regData, -c("Symbol", "DateMadePublic", "City", "State", "breachDate"))

prop.table(table(regData$Breached))

# regData$IPOyear <- as.numeric(as.character(format(as.Date(regData$IPOyear , format="%Y"),"%Y")))
regData$compSize <- as.factor(regData$compSize)
regData$Breached <- as.factor(regData$Breached)


summary(regData)

set.seed(42)
trainIndex <- createDataPartition(regData$Breached, p = .7,
                                  list = FALSE,
                                  times = 1)

trainSet <- regData[trainIndex,]
testSet <- regData[-trainIndex,]


trnCtrl <- trainControl(method = "cv", number = 10, summaryFunction = twoClassSummary, verboseIter = FALSE, classProbs = TRUE, allowParallel = TRUE)

trainSet$Breached <- as.factor(trainSet$Breached)
levels(trainSet$Breached) <- c("No", "Yes")

testSet$Breached <- as.factor(testSet$Breached)
levels(testSet$Breached) <- c("No", "Yes")

glmModel <- train(Breached ~ ., data = trainSet, method = "glm", family = "binomial", trControl = trnCtrl, na.action = na.exclude, metric = "ROC")

summary(glmModel)

# create prediction and probablity on training and test datasets
trainSet$predictions = predict(glmModel, newdata = trainSet)
trainSet$probability <- predict(glmModel, newdata = trainSet, type = "prob")

# create Training preciction object to get model performace metrics
train_pred <- prediction(trainSet$probability[,2], trainSet$Breached)

#And now a prediction and probablityobject on the testing data
testSet$predictions = predict(glmModel, newdata = testSet)
testSet$probability <- predict(glmModel, newdata = testSet, type = "prob")

# create Testing preciction object to get model performace metrics
test_pred <- prediction(testSet$probability[,2], testSet$Breached)


# Create initial Confusion Matrix

confMatrix <- confusionMatrix(data = testSet$predictions, reference = testSet$Breached,
                              mode = "everything", positive="Yes")
confMatrix
#tpr and fpr for our training
train_tpr_fpr <- performance(train_pred, "tpr","fpr")
train_auc <- performance(train_pred, "auc")

#tpr and fpr for our testing
test_tpr_fpr <- performance(test_pred, "tpr","fpr")
test_auc <- performance(test_pred, "auc")

# Plot the trainig and testing ROC curves

plot(test_tpr_fpr, main="Testing and Training ROC Curves", col = "blue")
plot(train_tpr_fpr, add = T, col = "red")
legend("bottomright", legend = c("Training","Testing"), col = c("red","blue"), lty = 1, lwd = 2)
abline(0,1, col = "darkgray")
grid()

#AUC figures and analysis
train_auc = unlist(slot(train_auc, "y.values"))
train_auc

# Area under the ROC curve
test_auc = unlist(slot(test_auc, "y.values"))
test_auc


# Programatically determine an optimal cutoff

# Get the performance object
test_sens_spec = performance(test_pred, "sens","spec")

# Make a dataframe to work out threshhold
threshold_df = data.frame(cut = test_sens_spec@alpha.values[[1]], 
                          sens = test_sens_spec@x.values[[1]],
                          spec = test_sens_spec@y.values[[1]])
which.max(threshold_df$sens + threshold_df$spec)

# Subset the dataframe to get the relevant cutoff value
threshold = threshold_df[which.max(threshold_df$sens + threshold_df$spec), "cut"]
threshold

## Final prediction
testSet$prediction_cutoff = "No"
testSet[testSet$probability$Yes >= threshold, "prediction_cutoff"] = "Yes"
testSet$prediction_cutoff <- as.factor(testSet$prediction_cutoff)
levels(testSet$prediction_cutoff) <- c("No","Yes")

# Build final confusion matrix

finalConfMatrix <- confusionMatrix(data = testSet$prediction_cutoff, reference = testSet$Breached,
                                   mode = "everything", positive="Yes")
finalConfMatrix
