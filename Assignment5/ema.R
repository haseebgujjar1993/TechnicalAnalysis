# ema.R | Exponential Moving Average | BDA400 Assignment 5 | Haseeb
# The EMA weights recent points more heavily. We seed the series with the first
# data point and then apply the recursive formula with multiplier 2 / (period + 1).

ema <- function(data, period) {
  # Calculate the multiplier for EMA
  multiplier <- 2 / (period + 1)
  # Initialize a vector to store EMA values
  ema_values <- numeric(length(data))
  # Loop through the data
  for (i in 1:length(data)) {
    if (i == 1) {
      # The first EMA value is just the first data point
      ema_values[i] <- data[i]
    } else {
      # EMA(i) = (Price(i) - EMA(i-1)) * multiplier + EMA(i-1)
      ema_values[i] <- (data[i] - ema_values[i - 1]) * multiplier + ema_values[i - 1]
    }
  }
  return(ema_values)
}

# Example usage
# data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# ema_result <- ema(data, period = 3)
# print(ema_result)
