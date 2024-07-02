library(haven) 
library(dplyr)
library(ggplot2)
library(tidyr)

# Read data
data <- read_dta("childrenfinal.dta")

# Select relevant columns
selected_data <- dplyr::select(data, hypage, zwast)

# Extract variables
ages <- selected_data$hypage
zwast <- as.numeric(selected_data$zwast)
unique_ages <- sort(unique(ages))

# Find the starting index and frequency of each unique age
starts <- sapply(unique_ages, function(age) which(ages == age)[1])
frequency <- sapply(unique_ages, function(age) length(which(ages == age)))

# Define Epanechnikov kernel
kernel_epanechnikov <- function(u) { ifelse(abs(u) <= 1, 0.75 * (1 - u^2), 0) }

# Local polynomial fitting function
local_poly_fit <- function(response, covariate, bandwidth, degree = 1, kernel, order = 0){
  fit_function <- function(x){
    X <- outer(covariate - x, 0:degree, FUN = "^")
    weights <- kernel((covariate - x) / bandwidth)
    fit <- lm(response ~ X + 0, weights = weights)
    coefficients <- fit$coefficients %>% unname()
    influence <- influence(fit, do.coef = FALSE)$hat
    return(list(coefficients[order + 1] * factorial(order), influence))
  }
  return(fit_function)
}

# Generalized cross-validation function
gcv <- function(h, response, covariate, degree, kernel, order){
  n <- length(response)
  fit_function <- local_poly_fit(response = response, covariate = covariate, bandwidth = h, degree = degree, kernel = kernel, order = order)
  f_hats <- sapply(unique_ages, function(age) fit_function(age)[[1]])
  hats <- sapply(unique_ages, function(age) fit_function(age)[[2]])
  num <- 0
  for (i in 1:length(response)){
    num <- num + (response[i] - f_hats[[covariate[i] + 1]])^2
  }
  sum_ws <- 0
  for(i in 1:length(unique_ages)){
    if(starts[i] %in% names(hats[[i]])){
      ind <- which.max(names(hats[[i]]) == starts[i])
      sum_ws <- sum_ws + frequency[i] * unname(hats[[i]][ind])
    }
  }
  denom <- (1 - 1/n * sum_ws)^2
  return(num / denom)
}

# Degrees for local polynomial fitting
degrees <- c(1, 2, 3, 4)

# Find GCV-optimal bandwidths
bandwidth_opts <- sapply(degrees, function(degree) {
  optimize(function(bandwidth) gcv(h = bandwidth, response = zwast, covariate = ages, degree = degree, kernel = kernel_epanechnikov, order = 0), interval = c(1, 20))$minimum
})

# Generate data frame for plotting
plot_data_gcv <- data.frame(ages = unique_ages)
for (i in 1:4) {
  fit_function <- local_poly_fit(response = zwast, covariate = ages, bandwidth = bandwidth_opts[i], degree = degrees[i], kernel = kernel_epanechnikov, order = 0)
  plot_data_gcv[[paste0("Degree_", degrees[i])]] <- sapply(plot_data_gcv$ages, function(age) fit_function(age)[[1]])
}

plot_data_gcv <- pivot_longer(plot_data_gcv, 
                                           cols = starts_with("Degree_"), 
                                           names_to = "Degree_", 
                                           values_to = "Value")

# Plot GCV-optimal bandwidths
ggplot(data = plot_data_gcv, aes(x = ages, y = Value, color = Degree_)) +
  geom_line(linewidth = 0.8) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
  labs(title = "GCV-optimal bandwidths with four degrees", x = "Age", y = "Intercept")


# Find GCV-optimal bandwidths for the first derivative
bandwidth_opts_first_derivative <- sapply(degrees, function(degree) {
  optimize(function(bandwidth) gcv(h = bandwidth, response = zwast, covariate = ages, degree = degree, kernel = kernel_epanechnikov, order = 1), interval = c(1, 20))$minimum
})

# Generate data frame for plotting
plot_data_first_derivative <- data.frame(ages = unique_ages)
for (i in 1:4) {
  fit_function <- local_poly_fit(response = zwast, covariate = ages, bandwidth = bandwidth_opts_first_derivative[i], degree = degrees[i], kernel = kernel_epanechnikov, order = 1)
  plot_data_first_derivative[[paste0("Degree_", degrees[i])]] <- sapply(plot_data_first_derivative$ages, function(age) fit_function(age)[[1]])
}


plot_data_first_derivative <- pivot_longer(plot_data_first_derivative, 
                               cols = starts_with("Degree_"), 
                               names_to = "Degree_", 
                               values_to = "Value")
# Plot first derivative with respect to four degrees

ggplot(data = plot_data_first_derivative, aes(x = ages, y = Value, color = Degree_)) +
  geom_line(linewidth = 0.8) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +  
  labs(title = "First derivative with respect to four degrees", x = "Age", y = "First derivative")
