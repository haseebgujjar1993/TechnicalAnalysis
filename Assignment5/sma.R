# sma.R | Simple Moving Average | BDA400 Assignment 5 | Haseeb
# We slide a window of 'period' points along the data and average each window,
# using only base R, following the template and pseudocode from the brief.

sma <- function(data, period) {
  # Check if the length of data is less than the specified period
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  # Initialize a vector to store the SMA values
  sma_values <- numeric(0)
  # Calculate SMA for each window of 'period' data points
  for (i in 1:(length(data) - period + 1)) {
    current_window <- data[i:(i + period - 1)]
    mean_value <- sum(current_window) / period
    # Store the mean value in the sma_values vector
    sma_values[i] <- mean_value
  }
  return(sma_values)
}

# Example usage (from the assignment brief)
# data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# sma_result <- sma(data, period = 3)
# print(sma_result)
