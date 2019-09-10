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
library(Riex)
library(urltools)
library(httr)


############################################################
sk <- "sk_29156f8079454246b31981c8645e7cc5"

company <- data$Company[[22]]

test<-iex.company("TSLA", sk)


test<-iex.company("AAPL", sk)

x<-"TSLA"

############################################################



stock.company.df <- data.frame()
url_prefix <- "https://cloud.iexapis.com/stable"
url_stocks_list <- paste0(url_prefix, "/ref-data/symbols?token=",sk)
req <- httr::GET(url_stocks_list)
req_status <- req$status_code
if (req_status != 200) stop ("Connection Failed to IEX Cloud API")
stocks_list <- rjson::fromJSON(file=url_stocks_list)
stocks_list <- sapply(stocks_list, "[[", "symbol")
stocks_list <- as.list(stocks_list)
if (toupper(x) %in% stocks_list == TRUE) print("Stock Info is available in IEX") else stop ("Stock Info is not available in IEX")
x_encoded <- url_encode(paste(toupper(x), collapse=", "))
url = paste0(url_prefix, "/stock/",x_encoded,"/company?token=",sk)
url.info <- httr::GET(url)
url.content <- content(url.info, as = "text", encoding = "UTF-8")
if (identical(text, "")){
  warning("No output to parse.")
} else {
  stock.company <- rjson::fromJSON(url.content)
  stock.company[which(sapply(stock.company, is_empty))] <- NA
}
stock.company.df <- tibble::as_tibble(stock.company)
stock.company.df <- stock.company.df %>%
  dplyr::select(-("tags")) %>%
  dplyr::distinct()

