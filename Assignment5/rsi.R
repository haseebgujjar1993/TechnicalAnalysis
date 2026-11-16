# rsi.R | Relative Strength Index | BDA400 Assignment 5 | Haseeb
# RSI = 100 - 100 / (1 + RS) where RS is the ratio of average gains to average
# losses, updated with Wilder's smoothing method as in the pseudocode. The first
# 'period' positions stay NA because there is no complete window for them yet.

rsi <- function(data, period) {
  # Calculate the differences between consecutive data points
  diff_values <- data[2:length(data)] - data[1:(length(data) - 1)]
  # Initialize two vectors to store the gains and losses
  # (a day that is not a gain is a 0 gain, and the same for losses)
  gains <- rep(0, length(diff_values))
  losses <- rep(0, length(diff_values))
  # Calculate gains and losses
  for (i in 1:length(diff_values)) {
    if (diff_values[i] > 0) {
      gains[i] <- diff_values[i]
    } else {
      losses[i] <- abs(diff_values[i])
    }
  }
  # Calculate the average gains and losses for the first 'period' data points
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  # Initialize the RSI vector with NA values
  rsi_values <- rep(NA_real_, length(data))
  # Calculate RSI values using the Wilder's smoothing method
  for (i in (period + 1):length(data)) {
    avg_gain <- (avg_gain * (period - 1) + gains[i - 1]) / period
    avg_loss <- (avg_loss * (period - 1) + losses[i - 1]) / period
    rs <- avg_gain / avg_loss
    rsi_values[i] <- 100 - (100 / (1 + rs))
  }
  return(rsi_values)
}

# Example usage
# data <- c(45, 50, 48, 55, 52, 49, 58, 60, 65, 62)
# rsi_result <- rsi(data, period = 5)
# print(rsi_result)
