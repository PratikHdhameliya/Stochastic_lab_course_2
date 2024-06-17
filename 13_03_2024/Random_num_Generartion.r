# Print the current random number generator
print(RNGkind())

# Set the random number generator to Wichmann-Hill
RNGkind(kind = "Wichmann-Hill")

# Print the current random number generator
print(RNGkind())

# Set the seed for reproducibility
set.seed(123) 

# Parameters
N <- 1000
p <- 0.4

#########################################################################################
# Function to generate geometrically distributed random variables using inversion method
generate_geometric <- function(N, p) {
  # Generate uniform random variables
  u <- runif(N)
  # Inversion method
  return(ceiling(log(1 - u) / log(1 - p)))
}

# Generate geometrically distributed random variables using the inversion method
geo_samples_Inverse <- generate_geometric(N, p)

########################################################################################
# Simulate Bernoulli trials
bernoulli_trials <- rbinom(N, size = 1, prob = p)

# Initialize vector to store geometrically distributed random variables
geo_samples_Bernouli <- integer(N)

# Simulate geometrically distributed random variables using the Bernoulli trials
for (i in 1:N) {
  # Count the number of failures until the first success
  num_failures <- 0
  while (bernoulli_trials[i] == 0) {
    num_failures <- num_failures + 1
    bernoulli_trials[i] <- rbinom(1, size = 1, prob = p)
  }
  # Store the number of failures as the geometric variable
  geo_samples_Bernouli[i] <- num_failures
}

###############################################################################################
# Simulate geometrically distributed random variables using rgeom function
geo_samples_Rgeom <- rgeom(N, prob = p)

##############################################################################################
# Display the simulated geometrically distributed random variables
print(geo_samples_Bernouli)
print(geo_samples_Inverse)
print(geo_samples_Rgeom)

# Plot density lines for all three datasets in one plot
plot(density(geo_samples_Bernouli), col = "skyblue", lwd = 2, main = "Empirical Probability Density Functions", xlab = "Value", ylab = "Density", xlim = c(0, max(c(geo_samples_Bernouli, geo_samples_Inverse, geo_samples_Rgeom))))
lines(density(geo_samples_Inverse), col = "lightgreen", lwd = 2)
lines(density(geo_samples_Rgeom), col = "lightcoral", lwd = 2)
# Add legend
legend("topright", legend = c("Bernoulli Inversion", "Geometric Inversion", "rgeom Function"), fill = c("skyblue", "lightgreen", "lightcoral"), lwd = 2)


#switch  to defualt method
RNGkind(kind = "Mersenne-Twister")
print(RNGkind())