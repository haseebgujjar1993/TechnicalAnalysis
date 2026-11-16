# haseeb_load_and_stats.R
# BDA400 Assignment 2 | Technical Analysis using R, Preliminary Stage | Haseeb
# Functions to import the stocks listed in portfolio.txt with quantmod and to
# compute basic statistics (moving average, mean, mode, median, standard
# deviation) on each stock's closing prices.

library(quantmod)
library(TTR)

# Read portfolio.txt (one ticker per line) and import each symbol from Yahoo
# Finance into a named list of xts objects, one data frame per stock.
load_stock_data <- function(portfolio_file = "portfolio.txt",
                            from = "2026-01-01", to = Sys.Date()) {
  symbols <- readLines(portfolio_file)
  symbols <- trimws(symbols[nzchar(trimws(symbols))])
  stock_list <- list()
  for (sym in symbols) {
    cat("Importing", sym, "...\n")
    stock_list[[sym]] <- getSymbols(sym, src = "yahoo", from = from, to = to,
                                    auto.assign = FALSE)
  }
  return(stock_list)
}

# Base R has no built-in mode function, so we define one: the most frequent
# closing price after rounding to the nearest dollar (raw closes are almost
# never repeated exactly, so rounding makes the mode meaningful).
price_mode <- function(x) {
  rounded <- round(as.numeric(x))
  freq <- table(rounded)
  as.numeric(names(freq)[which.max(freq)])
}

# Compute the required statistics on the closing prices of one stock and
# return them as a one-row data frame.
calculate_statistics <- function(stock_data, ma_period = 20) {
  close_prices <- Cl(stock_data)
  moving_avg <- TTR::SMA(close_prices, n = ma_period)
  data.frame(
    Mean   = mean(close_prices, na.rm = TRUE),
    Mode   = price_mode(close_prices),
    Median = median(close_prices, na.rm = TRUE),
    StDev  = sd(close_prices, na.rm = TRUE),
    MA20_latest = as.numeric(tail(na.omit(moving_avg), 1))
  )
}

# Build one summary table for the whole portfolio.
portfolio_statistics <- function(stock_list) {
  stats <- do.call(rbind, lapply(stock_list, calculate_statistics))
  stats <- cbind(Symbol = names(stock_list), stats)
  rownames(stats) <- NULL
  return(stats)
}
