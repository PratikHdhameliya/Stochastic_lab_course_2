# Load necessary packages
library(haven)  # For reading Stata files
library(dplyr)  # For data manipulation
library(ggplot2)  # For plotting
library(tidyr)  # For data reshaping

# Read the Stata file and create a data frame
df <- read_dta("childrenfinal.dta")
x_data <- as.vector(df$hypage)
y_data <- as.vector(df$zwast)
data <- data.frame(x_data, y_data)

# Define kernel functions
kernel_uniform <- function(u) { ifelse(abs(u) <= 1, 0.5, 0) }
kernel_triangular <- function(u) { ifelse(abs(u) <= 1, 1 - abs(u), 0) }
kernel_gaussian <- function(u) { dnorm(u) }
kernel_epanechnikov <- function(u) { ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0) }

# Function for local polynomial fitting
local_poly_fit <- function(x, response, covariate, bandwidth, kernel, degree) {
  # Calculate residuals
  residual <- covariate - x
  
  # Generate powers of residuals for polynomial terms
  d <- 0:degree  # This gives a vector of degrees from 0 up to 'degree'
  bias_corrected_covariate <- outer(residual, d, FUN = "^")
  
  # Define the weights using the kernel function
  weights <- kernel(residual / bandwidth)
  
  # Fit the model using weights
  fit <- lm(response ~ bias_corrected_covariate + 0, weights = weights)
  
  # Return the fitted model
  return(fit)
}

# Function to apply bandwidth fits
apply_bandwidth_fit <- function(x_data, y_data, bandwidths, degree, kernel) {
  results <- sapply(bandwidths, function(bandwidth) {
    sapply(x_data, function(x) {
      fit <- local_poly_fit(x, response = y_data, covariate = x_data, bandwidth = bandwidth, degree = degree, kernel = kernel)
      return(fit$coefficients[1])  # Assuming the coefficient of interest is at position 1
    })
  }, simplify = "array")
  
  # Set the column names for each bandwidth result
  colnames(results) <- paste("Bandwidth", bandwidths, sep = "_")
  
  # Combine x_data with results into a data frame
  return(data.frame(x_data, results))
}

# Apply the function
results <- apply_bandwidth_fit(x_data, y_data, bandwidth=c(2, 6, 11, 20,50), 1, kernel_uniform)

# Convert results to a data frame
plot_data_bandwidth <- data.frame(x_data, results)

plot_data_long <- pivot_longer(plot_data_bandwidth, 
                               cols = starts_with("Bandwidth"), 
                               names_to = "Bandwidth", 
                               values_to = "Value")

# Base plot
p <- ggplot() +
  geom_line(data = plot_data_long, aes(x = x_data, y = Value, color = Bandwidth), size = 1) +
  geom_point(data = data, aes(x = x_data, y = y_data), color = "black", size = 1,alpha=0.2) +
  labs(title = "Local Polynomial Regression and Data Points",
       x = "X Values",
       y = "Predictoin") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")  # Using a color palette for clarity

# Print the plot
print(p)


# Plot using ggplot2
ggplot(plot_data_long, aes(x = x_data, y = Value, color = Bandwidth)) +
  geom_line() +
  labs(title = "Local Polynomial Regression Across Different Bandwidths",
       x = "x_data", y = "Value", color = "Bandwidth")

# Function to apply kernel fits
apply_kernel_fit <- function(x_data, y_data, bandwidth, degree, kernels) {
  sapply(kernels, function(kernel) {
    sapply(x_data, function(x) {
      fit <- local_poly_fit(x, response = y_data, covariate = x_data, bandwidth = bandwidth, degree = degree, kernel = kernel)
      fit$coefficients[1]  # Assuming the coefficient of interest is at position 1
    })
  })
}

# Define your kernels in a named list for easy reference and modification
kernels <- list(
  "Uniform" = kernel_uniform,
  "Triangular" = kernel_triangular,
  "Gaussian" = kernel_gaussian,
  "Epanechnikov" = kernel_epanechnikov
)

# Apply the function
results <- apply_kernel_fit(x_data, y_data, 4, 1, kernels)

# Convert results to a data frame
plot_data_kernel <- data.frame(x_data, results)

plot_data_kernel <- pivot_longer(plot_data_kernel, 
                                 cols = -x_data,  # Exclude x_data from pivoting
                                 names_to = "Kernel", 
                                 values_to = "Value")

ggplot(plot_data_kernel, aes(x = x_data, y = Value, color = Kernel)) +
  geom_line() +
  labs(title = "Local Polynomial Fits Across Different Kernels",
       x = "x_data", y = "Fitted Value", color = "Kernel")
