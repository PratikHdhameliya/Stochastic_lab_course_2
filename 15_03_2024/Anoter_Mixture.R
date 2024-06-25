# Load the readr package
library(readr)

# Read the data from the file
data <- read_csv("mixer_data.csv")
data <- data$x

# Plot a histogram of the data
hist(data)

# Compute the mean and standard deviation of the data
m_1 <- mean(data)
sd <- sd(data)

# Define initial parameter values for optimization
mean_1 <- m_1 - (sd / 4)
mean_2 <- m_1 + (sd / 4)
initial_params <- c(0.5, mean_1, sd, mean_2, sd)

# Perform optimization to find the best parameters
Optimized_data <- perform_optimization(data, initial_params, num_iterations = 50, 
                                       stopping_criteria = c("iterations", "threshold"), 
                                       threshold = 1e-3)

# Compute and plot the marginal density for each set of optimized parameters
Marginal_Density_and_plot(Optimized_data, data)
