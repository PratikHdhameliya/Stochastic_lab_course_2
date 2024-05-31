# Function to create a random sample
# Arguments:
#   n: The total number of elements in the sample population
#   p: The proportion of elements to sample from the population
# Returns:
#   A random sample of indices from the population
create_random_sample <- function(n, p) {
  # Use the sample() function to generate a random sample of indices
  # from 1 to n, with replacement set to FALSE (no duplicates)
  # The size of the sample is calculated as the ceiling of (n * p),
  # ensuring at least one element is sampled if p is not a whole number
  sample(1:n, size = ceiling(n * p), replace = FALSE)
}

# Call the function to create a random sample
# Here, it generates a random sample of 5% of the indices from
# a population of 100 elements
random_num = create_random_sample(100, 0.05)

# Print the random sample
random_num
