library(bootstrap)
set.seed(123)

bcanon_modified <- function(data, statistic, alpha = 0.05, nboot = 1000) {
  # Create an empty vector to store bootstrap statistics
  boot_stats <- numeric(nboot)
  
  # Original statistic
  original_stat <- statistic(data)
  
  # Bootstrap resampling loop
  for (i in 1:nboot) {
    # Randomly delete an element from the data
    bootstrap_sample <- sample(data[-sample(length(data), 1)], replace = TRUE)
    
    # Compute the statistic of interest for the bootstrap sample
    boot_stats[i] <- statistic(bootstrap_sample)
  }
  
  # Compute bias correction and acceleration
  z0 <- original_stat - mean(boot_stats)
  a <- sum((original_stat - boot_stats) * (original_stat - mean(boot_stats))) /
    sum((original_stat - boot_stats)^2)
  
  # Calculate adjusted confidence interval endpoints
  quantiles <- quantile(boot_stats, c(alpha/2, 1 - alpha/2))
  z_alpha2 <- qnorm(alpha/2)
  z_1_alpha2 <- qnorm(1 - alpha/2)
  
  adjustment <- (z0 + z_alpha2) / (1 - a * z0)
  ci_lower <- original_stat + (z0 + adjustment) / (1 - a * z0)
  
  adjustment <- (z0 + z_1_alpha2) / (1 - a * z0)
  ci_upper <- original_stat + (z0 + adjustment) / (1 - a * z0)
  
  # Return the confidence interval along with z0 and a
  return(list(ci_lower = ci_lower, ci_upper = ci_upper, z0 = z0, a = a))
}

monte_carlo_simulation_bca <- function(M, n, alpha) {
  coverage_probability_variance <- numeric(M)
  coverage_probability_median <- numeric(M)
  avg_length_variance <- numeric(M)
  avg_length_median <- numeric(M)
  estimated_z0 <- numeric(M)
  estimated_a <- numeric(M)
  
  for (i in 1:M) {
    # Generate new sample from Weibull distribution
    weibull_samples <- rweibull(n, shape = 1, scale = 13)  # Assuming same parameters as original sample
    
    # Bootstrap accelerated bias-corrected (BCa) confidence intervals for variance
    ci_variance <- bcanon_modified(weibull_samples, var, alpha = alpha, nboot = n_boot)
    ci_median <- bcanon_modified(weibull_samples, median, alpha = alpha, nboot = n_boot)
    
    # Check if true population variance and median are within confidence intervals
    true_variance <- var(weibull_samples)
    true_median <- median(weibull_samples)
    
    # Coverage probability for variance
    coverage_probability_variance[i] <- (true_variance >= ci_variance$ci_lower && true_variance <= ci_variance$ci_upper)
    
    # Coverage probability for median
    coverage_probability_median[i] <- (true_median >= ci_median$ci_lower && true_median <= ci_median$ci_upper)
    
    # Average interval lengths
    avg_length_variance[i] <- ci_variance$ci_upper - ci_variance$ci_lower
    avg_length_median[i] <- ci_median$ci_upper - ci_median$ci_lower
    
    # Estimated z0 and a
    estimated_z0[i] <- ci_variance$z0
    estimated_a[i] <- ci_variance$a
  }
  
  # Calculate coverage probabilities
  coverage_probability_variance <- mean(coverage_probability_variance)
  coverage_probability_median <- mean(coverage_probability_median)
  
  # Calculate average interval lengths
  avg_length_variance <- mean(avg_length_variance)
  avg_length_median <- mean(avg_length_median)
  
  # Calculate average estimated z0 and a
  avg_estimated_z0 <- mean(estimated_z0)
  avg_estimated_a <- mean(estimated_a)
  
  return(list(
    coverage_probability_variance = coverage_probability_variance,
    coverage_probability_median = coverage_probability_median,
    avg_length_variance = avg_length_variance,
    avg_length_median = avg_length_median,
    avg_estimated_z0 = avg_estimated_z0,
    avg_estimated_a = avg_estimated_a
  ))
}

alpha <- 0.05  # Significance level

# Perform Monte Carlo simulation with BCa confidence intervals
results_bca <- monte_carlo_simulation_bca(1000, 100, alpha)


# Create a data frame to store the results
results_df_total <- data.frame(
  Simulation = c("1000 bootstrap replicate of size 100", "1000 bootstrap replicate of size 1000", "5000 bootstrap replicate of size 100", "Bias-corrected confidence intervals"),
  Cov_Prob_Variance = c(results$coverage_probability_variance, results1$coverage_probability_variance, results2$coverage_probability_variance, results_bca$coverage_probability_variance),
  Cov_Prob_Median = c(results$coverage_probability_median, results1$coverage_probability_median, results2$coverage_probability_median, results_bca$coverage_probability_median),
  Avg_Int_Length_Variance = c(results$avg_length_variance, results1$avg_length_variance, results2$avg_length_variance, results_bca$avg_length_variance),
  Avg_Int_Length_Median = c(results$avg_length_median, results1$avg_length_median, results2$avg_length_median, results_bca$avg_length_median),
  Estimated_z0 = c(NA, NA, NA, results_bca$avg_estimated_z0),
  Estimated_a = c(NA, NA, NA, results_bca$avg_estimated_a)
)

# Export the data frame as a CSV file
write.csv(results_df_total, "results_table_for_whole_Part(A).csv", row.names = FALSE)

