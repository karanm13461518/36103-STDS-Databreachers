library(Riex)
library(rlang)
library(purrr)

#Key initialisation
sk_Active <- "sk_22b5ae6975a44c84be02141ce302c0fe"
sk_Test <- "Tsk_c4d4a569d5374229a40296fa5e0189ac" #Secret key
iex_sk <- sk_Active
Company_List <- as.character(data$Symbol) # company name - from NASDAQ Listing.R
r <- "1d" #Extract period {"5y", "2y", "1y", "ytd", "6m", "3m", "1m", "1d"}

iex.company_mod <- function (x, iex_sk) 
{
  stock.company.df <- data.frame()
  url_prefix <- "https://cloud.iexapis.com/stable"
  url_stocks_list <- paste0(url_prefix, "/ref-data/symbols?token=", iex_sk)
  req <- httr::GET(url_stocks_list)
  req_status <- req$status_code
  if (req_status != 200) 
    stop("Connection Failed to IEX Cloud API")
  stocks_list <- rjson::fromJSON(file = url_stocks_list)
  stocks_list <- sapply(stocks_list, "[[", "symbol")
  stocks_list <- as.list(stocks_list)
  if (toupper(x) %in% stocks_list == TRUE) 
    print("Stock Info is available in IEX")
  else {
    print("Stock Info is not available in IEX")
    return(stock.company.df)
  }  
  x_encoded <- urltools::url_encode(paste(toupper(x), collapse = ", "))
  url = paste0(url_prefix, "/stock/", x_encoded, "/company?token=", iex_sk)
  url.info <- httr::GET(url)
  url.content <- httr::content(url.info, as = "text", encoding = "UTF-8")
  if (identical(text, "")) {
    warning("No output to parse.")
  }
  else {
    stock.company <- rjson::fromJSON(url.content)
  }
  #Null variable value handling
  if (is_null(stock.company$symbol)) {
    stock.company$symbol <- ""
  }
  if (is_null(stock.company$companyName)) {
    stock.company$companyName <- ""
  }
  if (is_null(stock.company$exchange)) {
    stock.company$exchange <- ""
  }
  if (is_null(stock.company$industry)) {
    stock.company$industry <- ""
  }
  if (is_null(stock.company$website)) {
    stock.company$website <- ""
  }
  if (is_null(stock.company$CEO)) {
    stock.company$CEO <- ""
  }
  if (is_null(stock.company$issueType)) {
    stock.company$issueType <- ""
  }
  if (is_null(stock.company$sector)) {
    stock.company$sector <- ""
  }
  if (is_null(stock.company$employees)) {
    stock.company$employees <- -100000
  }
  if (is_null(stock.company$address)) {
    stock.company$address <- ""
  }
  if (is_null(stock.company$address2)) {
    stock.company$address2 <- ""
  }
  if (is_null(stock.company$state)) {
    stock.company$state <- ""
  }
  if (is_null(stock.company$city)) {
    stock.company$city <- ""
  }
  if (is_null(stock.company$zip)) {
    stock.company$zip <- ""
  }
  if (is_null(stock.company$country)) {
    stock.company$country <- ""
  }
  if (is_null(stock.company$phone)) {
    stock.company$phone <- ""
  }
  stock.company.df <- tibble::as_tibble(stock.company)
  stock.company.df <- stock.company.df %>% dplyr::select(-("tags")) %>% 
    dplyr::distinct()
  return(stock.company.df)
}

#Outputs
df_Company_Info <- data.frame()

for (i in 1:2967) {
  df1 <- data.frame()
  df1 <- iex.company_mod(Company_List[i], sk_Active)
  if (length(df1) != 0) {
  df_Company_Info <- rbind(df_Company_Info, df1)
  }
}

write_csv(df_Company_Info, "G:/Data Science/UTS Courses/36103/Assignment 2/NASDAQ Company Info.csv")
