# crossunder.R | Crossunder detector | BDA400 Assignment 5 | Haseeb
# Flags the points where arr1 moves from at-or-above arr2 to strictly below it.
# The pseudocode stores the strings "True" / "False" (with "None" for the first
# element, where no comparison is possible), and we follow it exactly.

crossunder <- function(arr1, arr2) {
  # Check if the length of both arrays is the same
  if (length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  # Initialize a vector to store the crossunder signals
  crossunder_signals <- rep("False", length(arr1))
  crossunder_signals[1] <- "None"
  # Check for crossunder signals at each data point
  for (i in 2:length(arr1)) {
    if (arr1[i] < arr2[i] && arr1[i - 1] >= arr2[i - 1]) {
      crossunder_signals[i] <- "True"
    } else {
      crossunder_signals[i] <- "False"
    }
  }
  return(crossunder_signals)
}

# Example usage (from the assignment brief)
# arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)
# arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)
# crossunder_signals <- crossunder(arr1, arr2)
# print(crossunder_signals)
