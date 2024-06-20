# Read the text file containing the Sleep Heart Health Study data
data <- read.table("shhs1.txt", header = TRUE)  # Change "your_file.txt" to the path of your text file

# Extract the column containing the RDI (Respiratory Disturbance Index) data
data <- data$rdi4p

# Create a histogram of the RDI data
hist(data, breaks = 50, ylim = c(0, 4000), main = "Distribution of RDI in Sleep Heart Health Study", xlab = "Respiratory Disturbance Index per hour ")

# Define a function to calculate sample variance
calculate_sample_variance <- function(data) {
  n <- length(data)
  mean_value <- mean(data)
  sum_squared_deviations <- sum((data - mean_value)^2)
  sample_variance <- sum_squared_deviations / (n - 1)
  return(sample_variance)
}

# Perform bootstrap resampling to estimate variance
bootstrapped_variances <- replicate(n_boot, {
  bootstrap_sample <- sample(data, replace = TRUE)
  calculate_sample_variance(bootstrap_sample)
})

# Sort the bootstrapped variances
sorted_variances <- sort(bootstrapped_variances)

# Calculate confidence interval bounds for variance
lower_bound_variance_shhs1 <- quantile(sorted_variances, 0.025)
upper_bound_variance_shhs1 <- quantile(sorted_variances, 0.975)

# Perform bootstrap resampling to estimate median
bootstrapped_medians <- replicate(n_boot, {
  bootstrap_sample <- sample(data, replace = TRUE)
  median(bootstrap_sample)
})

# Sort the bootstrapped medians
sorted_medians <- sort(bootstrapped_medians)

# Calculate confidence interval bounds for median
lower_bound_median_shhs1 <- quantile(sorted_medians, 0.025)
upper_bound_median_shhs1 <- quantile(sorted_medians, 0.975)

# Calculate BCa Bootstrap CI for median and variance
ci_median_shhs1 <- bcanon_modified(data, median, alpha = alpha, nboot = n_boot)
ci_variance_shhs1 <- bcanon(data, var, alpha = alpha, nboot = n_boot)

# Sort the BCa Bootstrap CI
ci_median_shhs1 <- sort(ci_median_shhs1)
ci_variance_shhs1 <- sort(ci_variance_shhs1$u)

# Extract lower and upper bounds for BCa Bootstrap CI of median and variance
lower_bound_median_shhs1_BCA <- ci_median_shhs1[1]
upper_bound_median_shhs1_BCA <- ci_median_shhs1[2]
lower_bound_variance_shhs1_BCA <- ci_variance_shhs1[1]
upper_bound_variance_shhs1_BCA <- ci_variance_shhs1[length(ci_variance_shhs1)]

# Create a dataframe to store the bounds of median and variance
results_df <- data.frame(
  Statistic = c("Median", "Median", "Variance", "Variance"),
  Bootstrap_Method = c("Basic Bootstrap CI", "BCa Bootstrap CI", "Basic Bootstrap CI", "BCa Bootstrap CI"),
  Lower_Bound = c(lower_bound_median_shhs1, lower_bound_median_shhs1_BCA, lower_bound_variance_shhs1, lower_bound_variance_shhs1_BCA),
  Upper_Bound = c(upper_bound_median_shhs1, upper_bound_median_shhs1_BCA, upper_bound_variance_shhs1, upper_bound_variance_shhs1_BCA)
)

# Write the results dataframe to a CSV file
write.csv(results_df, "results_table_for_Part(B).csv", row.names = FALSE)
