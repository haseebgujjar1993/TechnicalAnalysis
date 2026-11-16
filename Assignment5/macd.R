# macd.R | Moving Average Convergence Divergence | BDA400 Assignment 5 | Haseeb
# MACD line = EMA(short) - EMA(long); signal line = EMA of the MACD line;
# histogram = MACD line - signal line. Needs ema() from ema.R.

if (!exists("ema")) source("ema.R")

macd <- function(data, short_period, long_period, signal_period) {
  # Calculate the short-term and long-term exponential moving averages
  short_ema <- ema(data, short_period)
  long_ema <- ema(data, long_period)
  # Calculate the MACD line
  macd_line <- short_ema - long_ema
  # Calculate the signal line (EMA of the MACD line)
  signal_line <- ema(macd_line, signal_period)
  # Calculate the histogram
  histogram <- macd_line - signal_line
  # Return the MACD line, signal line, and histogram as a list
  result <- list(macd_line = macd_line, signal_line = signal_line, histogram = histogram)
  return(result)
}

# Example usage
# data <- c(100, 105, 110, 115, 120, 125, 130)
# macd_result <- macd(data, short_period = 3, long_period = 5, signal_period = 2)
# print(macd_result)
