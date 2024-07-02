###      PART    A      ###

# Read the data
data <- read.csv("StudentsPerformance.csv")

# Extract math scores
math_scores <- data$math.score


# Example usage:
bandwidth_seq <- c(0.09,0.5, 4.26, 6.8)  # Sequence of bandwidth values
plot_kde_for_bandwidth_seq(math_scores, bandwidth_seq, epanechnikov_kernel)

bandwidth <- 4.26
kernels <- list(gaussian_kernel = gaussian_kernel, 
                epanechnikov_kernel = epanechnikov_kernel,
                uniform_kernel=uniform_kernel,  
                tridiagonal_kernel=tridiagonal_kernel)

plot_kde_for_kernel(math_scores, bandwidth, kernels)
