set.seed(123)  # Set seed for reproducibility

# Parameters
n <- 100  # Number of samples
lambda <- 13  # Scale parameter for Weibull distribution
k <- 1  # Shape parameter for Weibull distribution

# Generate samples from the Weibull distribution
weibull_samples <- rweibull(n, shape = k, scale = lambda)

library(boot)

# Set the number of bootstrap iterations
n_boot <- 1000

# Function to calculate sample variance
calculate_sample_variance <- function(data) {
  n <- length(data)
  mean_value <- mean(data)
  sum_squared_deviations <- sum((data - mean_value)^2)
  sample_variance <- sum_squared_deviations / (n - 1)
  return(sample_variance)
}

# Perform bootstrap resampling to estimate variances
bootstrapped_variances <- replicate(n_boot, {
  bootstrap_sample <- sample(weibull_samples, replace = TRUE)
  calculate_sample_variance(bootstrap_sample)
})

# Sort the bootstrapped variances
sorted_variances <- sort(bootstrapped_variances)

# Calculate confidence interval bounds for variance
lower_bound <- quantile(sorted_variances, 0.025)
upper_bound <- quantile(sorted_variances, 0.975)

# Perform bootstrap resampling to estimate medians
bootstrapped_medians <- replicate(n_boot, {
  median(sample(weibull_samples, replace = TRUE))
})

# Sort the bootstrapped medians
sorted_medians <- sort(bootstrapped_medians)

# Calculate confidence interval bounds for median
lower_bound_median <- quantile(sorted_medians, 0.025)
upper_bound_median <- quantile(sorted_medians, 0.975)

# Set parameters for Monte Carlo simulation
M <- 1000  # Number of Monte Carlo samples

# Function to perform Monte Carlo simulation
monte_carlo_simulation <- function(M, n) {
  # Initialize vectors to store results
  coverage_variance <- numeric(M)
  coverage_median <- numeric(M)
  interval_length_variance <- numeric(M)
  interval_length_median <- numeric(M)
  
  for (i in 1:M) {
    # Bootstrap resampling for variance confidence interval
    bootstrapped_variances <- replicate(n_boot, {
      bootstrap_sample <- sample(weibull_samples, replace = TRUE)
      calculate_sample_variance(bootstrap_sample)
    })
    sorted_variances <- sort(bootstrapped_variances)
    lower_bound_variance <- quantile(sorted_variances, 0.025)
    upper_bound_variance <- quantile(sorted_variances, 0.975)
    
    # Bootstrap resampling for median confidence interval
    bootstrapped_medians <- replicate(n_boot, {
      median(sample(weibull_samples, replace = TRUE))
    })
    sorted_medians <- sort(bootstrapped_medians)
    lower_bound_median <- quantile(sorted_medians, 0.025)
    upper_bound_median <- quantile(sorted_medians, 0.975)
    
    # Check if true population variance and median are within confidence intervals
    true_variance <- var(weibull_samples)
    true_median <- median(weibull_samples)
    coverage_variance[i] <- (true_variance >= lower_bound_variance && true_variance <= upper_bound_variance)
    coverage_median[i] <- (true_median >= lower_bound_median && true_median <= upper_bound_median)
    
    # Calculate interval lengths
    interval_length_variance[i] <- upper_bound_variance - lower_bound_variance
    interval_length_median[i] <- upper_bound_median - lower_bound_median
  }
  
  # Calculate coverage probabilities
  coverage_probability_variance <- mean(coverage_variance)
  coverage_probability_median <- mean(coverage_median)
  
  # Calculate average interval lengths
  avg_length_variance <- mean(interval_length_variance)
  avg_length_median <- mean(interval_length_median)
  
  return(list(
    coverage_probability_variance = coverage_probability_variance,
    coverage_probability_median = coverage_probability_median,
    avg_length_variance = avg_length_variance,
    avg_length_median = avg_length_median
  ))
}

# Perform Monte Carlo simulation
results <- monte_carlo_simulation(M, n)
# Perform additional simulations with different parameters

results1 <- monte_carlo_simulation(1000, 1000)
results2 <- monte_carlo_simulation(5000, 100)



