library(quantmod)
library(PerformanceAnalytics)

# get adjusted close prices for a specified ticker (15 years by default)
get_adj_close <- function(ticker, from = NA, to = Sys.Date(), source = "yahoo", return_type = "log") {
  
  # get historical prices for ticker
  data <- getSymbols(ticker, from = from, to = to, src = source, auto.assign = FALSE)
  
  # get adjusted close prices
  adj_close <- Ad(data)
  
  return(adj_close)
}


# get log returns
get_log_returns <- function(returns) {
  # calculate log returns
  log_returns <- Return.calculate(returns, method = "log")[-1]
  return(log_returns)
}


process_assets <- function(tickers, from = NA, to = Sys.Date()) {
  # get and transform each ticker's log returns in one step
  log_returns_list <- Map(function(tkr) {
    adj <- get_adj_close(tkr, from = from, to = to)
    ret <- get_log_returns(adj)
    colnames(ret) <- tkr
    return(ret)
  }, tickers)
  
  # merge all log returns
  merged_returns <- Reduce(function(x, y) merge(x, y, all = TRUE), log_returns_list)
  
  # fill missing values using forward fill
  filled_returns <- na.locf(merged_returns, na.rm = FALSE)
  
  # filter to start from first row with no NAs
  first_valid_row <- which(apply(!is.na(filled_returns), 1, all))[1]
  if (is.na(first_valid_row)) {
    stop("No row with complete data found across all tickers.")
  }
  
  cleaned_returns <- filled_returns[first_valid_row:nrow(filled_returns), ]
  return(cleaned_returns)
}


# helper: get adjusted close prices for all tickers as a single xts object
get_all_adj_close <- function(tickers, from, to) {
  price_list <- lapply(tickers, function(tkr) {
    adj <- get_adj_close(tkr, from = from, to = to)
    colnames(adj) <- tkr
    return(adj)
  })
  
  merged_prices <- Reduce(function(x, y) merge(x, y, all = TRUE), price_list)
  
  # forward fill missing values
  filled_prices <- na.locf(merged_prices, na.rm = FALSE)
  
  # filter to start from first row with no NAs
  first_valid_row <- which(apply(!is.na(filled_prices), 1, all))[1]
  if (is.na(first_valid_row)) stop("No row with complete data found across all tickers.")
  cleaned_prices <- filled_prices[first_valid_row:nrow(filled_prices), ]
  
  return(cleaned_prices)
}