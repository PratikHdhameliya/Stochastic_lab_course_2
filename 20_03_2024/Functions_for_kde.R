# Define Epanechnikov kernel function
epanechnikov_kernel <- function(u) {
  return(0.75 * (1 - u^2) * (abs(u) <= 1))
}

# Define Tridiagonal kernel function
tridiagonal_kernel <- function(u) {
  return((1/6) * (2 - abs(u)) * (abs(u) <= 2))
}

# Define Uniform kernel function
uniform_kernel <- function(u) {
  return(0.5 * (abs(u) <= 1))
}

# Define Gaussian kernel function
gaussian_kernel <- function(u) {
  return(dnorm(u))
}

# Kernel Density Estimation (KDE) function
kde <- function(x, data, bandwidth, kernel) {
  # Number of data points
  n <- length(data)
  
  # Number of points to estimate
  k <- length(x)
  
  # Initialize result vector
  result <- numeric(k)
  
  # Loop over each point to estimate
  for (i in 1:k) {
    # Calculate kernel density estimate for each point
    result[i] <- sum(kernel((x[i] - data) / bandwidth)) / (n * bandwidth)
  }
  
  return(result)
}

# Function to plot kernel density estimation for a sequence of bandwidth values
plot_kde_for_bandwidth_seq <- function(data, bandwidth_seq, kernel) {
  library(ggplot2)
  
  # Define the range of values for evaluation
  x <- seq(min(data), max(data), length.out = 100)
  
  # Calculate density estimation for each bandwidth
  density_estimation <- lapply(bandwidth_seq, function(bw) kde(x, data, bw, kernel))
  
  # Create a data frame for plotting
  plot_data <- data.frame(x = rep(x, length(bandwidth_seq)),
                          density = unlist(density_estimation),
                          bandwidth = rep(bandwidth_seq, each = length(x)))
  
  # Plot using ggplot2
  ggplot(plot_data, aes(x = x, y = density, color = as.factor(bandwidth))) +
    geom_line() +
    scale_color_manual(values = rainbow(length(bandwidth_seq))) +
    labs(x = "X", y = "Density", color = "Bandwidth") +
    ggtitle("Kernel Density Estimation")
}

# Function to plot kernel density estimation for different kernels
plot_kde_for_kernel <- function(data, bandwidth, kernels) {
  library(ggplot2)
  
  # Define the range of values for evaluation
  x <- seq(min(data), max(data), length.out = 100)
  
  # Calculate density estimation for each kernel
  density_estimation <- lapply(kernels, function(kernel) kde(x, data, bandwidth, kernel))
  
  # Create a data frame for plotting
  plot_data <- data.frame(x = rep(x, length(kernels)),
                          density = unlist(density_estimation),
                          kernel = rep(names(kernels), each = length(x)))
  
  # Plot using ggplot2
  ggplot(plot_data, aes(x = x, y = density, color = kernel)) +
    geom_line() +
    scale_color_manual(values = rainbow(length(kernels))) +
    labs(x = "X", y = "Density", color = "Kernel") +
    ggtitle("Kernel Density Estimation")
}

# Function to compute the cross-validation score for a given bandwidth
CV <- function(h, data, kernel) {
  n <- length(data)
  cv_score <- 0
  
  # Compute the range of x
  range_x <- range(data)
  
  # Compute the integral of f_hat(x;h)^2 with respect to x
  integral <- integrate(function(x) kde(x, data, h, kernel)^2, range_x[1], range_x[2], subdivisions = 10000)$value
  cv_score <- cv_score + integral
  
  for (i in 1:n) {
    xi <- data[i]   
    
    for (j in 1:n) {
      xj <- data[j]
      if (i != j) {
        K <- kernel((xj - xi) / h)
        cv_score <- cv_score - (2 / (n * (n - 1) * h)) * K
      }
    }
  }
  
  return(cv_score)
}

# Function to compute the optimal bandwidth using cross-validation
compute_optimal_bandwidth <- function(data, bandwidth_values, kernel) {
  CV_scores <- numeric(length(bandwidth_values))
  
  # Compute the cross-validation score for each bandwidth value
  for (i in seq_along(bandwidth_values)) {
    CV_scores[i] <- CV(bandwidth_values[i], data, kernel)
  }
  
  # Find the bandwidth value with the minimum cross-validation score
  min_bandwidth <- bandwidth_values[which.min(CV_scores)]
  return(min_bandwidth)
}

# Function to compute the optimal bandwidth using optimization
compute_CV_optim <- function(data, bandwidth_values, kernel) {
  # Use optimization to find the bandwidth value that minimizes the cross-validation score
  result <- optimize(CV, bandwidth_values, data = data, kernel = kernel)$minimum
  
  return(result)
}

# Function to plot kernel density estimation for two datasets with different bandwidths
plot_kde_for_two_datasets <- function(data1, bandwidth1, data2, bandwidth2, kernel) {
  # Define the range of values for evaluation
  dataset1_name <- deparse(substitute(data1))
  dataset2_name <- deparse(substitute(data2))
  x1 <- seq(min(data1), max(data1), length.out = 100)
  x2 <- seq(min(data2), max(data2), length.out = 100)
  
  # Calculate density estimation for each dataset with respective bandwidth
  density_estimation1 <- kde(x1, data1, bandwidth1, kernel)
  density_estimation2 <- kde(x2, data2, bandwidth2, kernel)
  
  # Create data frames for plotting
  plot_data1 <- data.frame(x = x1, density = density_estimation1, dataset = rep("Dataset 1", length(x1)))
  plot_data2 <- data.frame(x = x2, density = density_estimation2, dataset = rep("Dataset 2", length(x2)))
  combined_plot_data <- rbind(plot_data1, plot_data2)
  
  # Plot using ggplot2
  ggplot(combined_plot_data, aes(x = x, y = density, color = dataset)) +
    geom_line() +
    scale_color_manual(values = c("red", "blue"), labels = c(dataset1_name, dataset2_name)) +
    labs(x = "X", y = "Density", color = "Dataset") +
    ggtitle(paste("Kernel Density Estimation for", dataset1_name, "and", dataset2_name)) 
}














































