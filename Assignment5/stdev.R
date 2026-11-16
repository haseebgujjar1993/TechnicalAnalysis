# stdev.R | Standard Deviation | BDA400 Assignment 5 | Haseeb
# We follow the assignment formula exactly: variance is the MEAN of the squared
# differences (divide by n). Note that R's built-in sd() divides by n - 1
# (sample standard deviation), so the two give slightly different answers.

stdev <- function(data) {
  # Calculate the mean of the data
  mean_value <- sum(data) / length(data)
  # Calculate the differences between the data points and the mean
  diff_values <- numeric(0)
  for (k in 1:length(data)) {
    diff_values[k] <- data[k] - mean_value
  }
  # Calculate the squared differences
  squared_diff <- numeric(0)
  for (k in 1:length(diff_values)) {
    squared_diff[k] <- diff_values[k] * diff_values[k]
  }
  # Calculate the variance (mean of squared differences)
  variance <- sum(squared_diff) / length(squared_diff)
  # Calculate the standard deviation (square root of the variance)
  standard_deviation <- sqrt(variance)
  return(standard_deviation)
}

# Example usage
# data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# stdev_result <- stdev(data)
# print(stdev_result)
