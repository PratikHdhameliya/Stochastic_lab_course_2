set.seed(123)  # Set the seed for reproducibility
# Parameters
N <- 1000  # Number of samples to generate

# Define the standard normal density function f(x)
f <- function(x) {
  exp(-x^2 / 2) / sqrt(2 * pi)
}

# Define the standard Laplace density function g(x)
g <- function(x) {
  exp(-abs(x)) / 2
}

# Define the range of x values
x <- runif(N)

# Find the maximum ratio
c_value <- max(f(x) / g(x))  # Calculate the maximum value of f(x)/g(x) as c

cat("The value of c such that f(x) <= c * g(x) is:", c_value, "\n")
###############################################################################################
# Function to generate Laplace distributed random variables
generate_laplace <- function(N) {
  u <- runif(N)  # Generate uniform random variables
  laplace_samples <- -sign(u - 0.5) * log(1 - 2 * abs(u - 0.5))  # Inverse transform sampling for Laplace distribution
  return(laplace_samples)
}

# Function to generate standard normal distributed random variables using the accept-reject method
generate_normal_accept_reject <- function(N) {
  normal_samples <- numeric(N)  # Initialize vector to store normal samples
  count_accept <- 0  # Initialize acceptance counter
  count_reject <- 0  # Initialize rejection counter
  for (i in 1:N) {
    while (TRUE) {
      x <- generate_laplace(1)  # Generate a Laplace sample
      u <- runif(1)  # Generate a uniform random variable
      if (u <  f(x) / (c_value * g(x))) {  # Check the accept-reject condition
        normal_samples[i] <- x  # Accept the sample
        count_accept <- count_accept + 1  # Increment acceptance counter
        break  # Exit the while loop
      } else {
        count_reject <- count_reject + 1  # Increment rejection counter
      }
    }
  }
  acceptance_probability <- count_accept / N  # Calculate acceptance probability
  rejection_probability <- count_reject / N  # Calculate rejection probability
  return(list(samples = normal_samples, acceptance_probability = acceptance_probability, 
              rejection_probability = rejection_probability))  # Return the results
}

# Generate normal samples using the accept-reject method
result <- generate_normal_accept_reject(N)
normal_samples <- result$samples
acceptance_probability <- result$acceptance_probability

# Compare estimated and theoretical acceptance probabilities
theoretical_acceptance_probability <- c_value

# Create a data frame for the output
output_df <- data.frame(
  Metric = c("Theoretical acceptance probability", "Estimated acceptance probability"),
  Value = c(theoretical_acceptance_probability, acceptance_probability)
)
# Export the data frame to a CSV file
write.csv(output_df, "Comparision of both probability.csv", row.names = FALSE)

# Plot histogram of obtained sample with standard normal density
hist(normal_samples, breaks = 30, freq = FALSE, main = "Histogram of Standard Normal Samples with Density of Standard Normal ", xlab = "Value", ylab = "Frequency",xlim=c(-4,4))  # Plot histogram
curve(f(x), from = -4, to = 4, col = "blue", lwd = 2, add = TRUE, n = 1000, yaxt = "n")  # Plot standard normal density

# Make QQ-plot
qqnorm(normal_samples)  # Plot QQ-plot
qqline(normal_samples, col = "red")  # Add reference line