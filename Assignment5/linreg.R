# linreg.R | Linear Regression | BDA400 Assignment 5 | Haseeb
# Least-squares fit of y = b0 + b1 * x over a window of the source series,
# where the window is controlled by regressionLength and regressionOffset
# exactly as described in the pseudocode.

linreg <- function(regressionSource, regressionLength, regressionOffset) {
  # Calculate the total number of elements in the regressionSource
  n <- length(regressionSource)
  # Check if regressionLength is greater than the number of elements
  if (regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  # Check if regressionOffset is greater than or equal to regressionLength
  if (regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  # Calculate the starting and ending index for the regressionSource
  start_index <- max(1, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  # Extract the relevant portion of regressionSource
  source_subset <- regressionSource[start_index:end_index]
  # Calculate the index values for the regression points
  index_values <- 1:length(source_subset)
  # Calculate the means of index values and of the source subset
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  # Calculate the numerator and denominator for the regression formula
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  # Calculate the slope and intercept of the regression line
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  # Calculate the predicted values
  predicted_values <- slope * index_values + intercept
  # Return the slope, intercept, and predicted values as a list
  result <- list(slope = slope, intercept = intercept, predicted_values = predicted_values)
  return(result)
}

# Example usage
# prices <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# linreg_result <- linreg(prices, regressionLength = 6, regressionOffset = 1)
# print(linreg_result)
