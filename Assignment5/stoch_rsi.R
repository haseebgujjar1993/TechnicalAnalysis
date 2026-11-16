# stoch_rsi.R | Stochastic RSI | BDA400 Assignment 5 | Haseeb
# StochRSI normalizes the RSI between 0 and 1. The %K line is the StochRSI
# smoothed with a simple moving average, and %D is an SMA of %K.
# Needs rsi() from rsi.R and sma() from sma.R.

if (!exists("rsi")) source("rsi.R")
if (!exists("sma")) source("sma.R")

stoch_rsi <- function(data, period, k_period, d_period) {
  # Calculate the RSI
  rsi_values <- rsi(data, period)
  # Calculate the StochRSI. The first 'period' RSI values are NA (warm-up),
  # so we ignore the NAs when finding the lowest and highest RSI,
  # and we drop them before smoothing so sma() receives clean data.
  min_rsi <- min(rsi_values, na.rm = TRUE)
  max_rsi <- max(rsi_values, na.rm = TRUE)
  k_values <- (rsi_values - min_rsi) / (max_rsi - min_rsi)
  k_clean <- k_values[!is.na(k_values)]
  # Calculate the %K line (StochRSI smoothed over k_period)
  k_line <- sma(k_clean, k_period)
  # Calculate the %D line (simple moving average of %K over d_period)
  d_line <- sma(k_line, d_period)
  # Return the %K and %D lines as a list
  result <- list(k_line = k_line, d_line = d_line)
  return(result)
}

# Example usage
# data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
# stoch_rsi_result <- stoch_rsi(data, period = 5, k_period = 3, d_period = 3)
# print(stoch_rsi_result)
