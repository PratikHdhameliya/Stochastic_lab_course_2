# Define a function to compute the final output based on given parameters
final_output <- function(params_data, data, params_UP) {
  # Extract parameters for data and User Preference (UP)
  p_data <- params_data[1]          # Probability parameter for data distribution
  mean_1_data <- params_data[2]     # Mean parameter for the first data distribution
  sd_1_data <- params_data[3]       # Standard deviation parameter for the first data distribution
  mean_2_data <- params_data[4]     # Mean parameter for the second data distribution
  sd_2_data <- params_data[5]       # Standard deviation parameter for the second data distribution
  
  p_UP <- params_UP[1]              # Probability parameter for User Preference distribution
  mean_1_UP <- params_UP[2]         # Mean parameter for the first User Preference distribution
  sd_1_UP <- params_UP[3]           # Standard deviation parameter for the first User Preference distribution
  mean_2_UP <- params_UP[4]         # Mean parameter for the second User Preference distribution
  sd_2_UP <- params_UP[5]           # Standard deviation parameter for the second User Preference distribution
  
  # Compute the final output based on the given data, parameters, and User Preference
  output <- sapply(data, function(x) {
    # Compute the numerator for the first distribution
    numerator_1 <- log(p_data * dnorm(x, mean_1_data, sd_1_data)) *
      ((p_UP * dnorm(x, mean_1_UP, sd_1_UP)) / 
         (p_UP * dnorm(x, mean_1_UP, sd_1_UP) + (1 - p_UP) * dnorm(x, mean_2_UP, sd_2_UP)))
    
    # Compute the numerator for the second distribution
    numerator_2 <- log((1 - p_data) * dnorm(x, mean_2_data, sd_2_data)) *
      (((1 - p_UP) * dnorm(x, mean_2_UP, sd_2_UP)) / 
         (p_UP * dnorm(x, mean_1_UP, sd_1_UP) + (1 - p_UP) * dnorm(x, mean_2_UP, sd_2_UP)))
    
    # Return the sum of the two numerators
    return(numerator_1 + numerator_2)
  })
  
  return(-(sum(output)))  # Minimize the negative sum of output
}





#############################################################################################################################################
#########Always keep in mind change upper bound after analyzing data otherwise this silly mistake will not produce proper output#############
#############################################################################################################################################
# Function to perform optimization using Differential Evolution (DE)
perform_optimization <- function(data, initial_params, num_iterations, stopping_criteria = c("iterations", "threshold"), threshold = 1e-3) {
  # Initialize parameters for optimization
  library(DEoptim)
  params_UP <- initial_params
  
  # Initialize list to store optimization results
  optimization_results <- list()
  
  # Perform optimization
  for (i in 1:num_iterations) {
    # Perform DE optimization
    optimized_params <- DEoptim(final_output, data = data, params_UP = params_UP,
                                lower = c(0, 0, 0, 0, 0), upper = c(1, 1000, 1000, 1000, 1000),
                                control = list(trace = FALSE))
    
    # Update parameters with the best parameters found
    params_UP <- optimized_params$optim$bestmem
    
    # Store optimization results
    optimization_results[[i]] <- list(iteration = i, params = params_UP, value = optimized_params$optim$bestval)
    
    # Print iteration number, best parameter values, and objective value
    cat("Iteration:", i, "\n")
    cat("Best parameters:", params_UP, "\n")
    cat("Objective value:", optimized_params$optim$bestval, "\n\n")
    
    # Check stopping criteria
    if ("iterations" %in% stopping_criteria && i >= num_iterations) {
      cat("Reached maximum number of iterations.\n")
      break
    }
    
    if ("threshold" %in% stopping_criteria && i > 1) {
      prev_params <- optimization_results[[i - 1]]$params
      curr_params <- params_UP
      dist <- sqrt(sum((curr_params - prev_params)^2))
      if (dist < threshold) {
        cat("Distance between consecutive parameter vectors below threshold.\n")
        break
      }
    }
  }
  
  # Return optimization results
  return(optimization_results)
}
## Be_ careful with optimization_results it should contain lot of optimized points where optimized parameters stored at 
##params and params should have p value mean_1,sd_1,mean_2,sd_2 yeah that's all I guess :)) 
# Function to compute the marginal density and plot optimization results



Marginal_Density_and_plot <- function(optimization_results, data) {
  # Define a function to compute the marginal density
  Marginal_density <- function(params_data, data) {
    p_data <- params_data[1]        # Probability parameter for data distribution
    mean_1_data <- params_data[2]   # Mean parameter for the first data distribution
    sd_1_data <- params_data[3]     # Standard deviation parameter for the first data distribution
    mean_2_data <- params_data[4]   # Mean parameter for the second data distribution
    sd_2_data <- params_data[5]     # Standard deviation parameter for the second data distribution
    
    # Compute the marginal density for each data point
    output <- sapply(data, function(x) {
      marginal_dens_for_each_point <- log((p_data * dnorm(x, mean_1_data, sd_1_data)) + 
                                            ((1 - p_data) * dnorm(x, mean_2_data, sd_2_data)))
      return(marginal_dens_for_each_point)
    })
    
    return(sum(output))  # Sum of marginal densities
  }
  
  # Compute the marginal density for each optimization result
  output_of_marginal <- numeric(length(optimization_results))
  for (i in seq_along(optimization_results)) {
    output_of_marginal[[i]] <- Marginal_density(params_data = optimization_results[[i]]$params, data = data)
  }
  
  # Extract the objective values from optimization results
  #results <- sapply(optimization_results, function(res) res$value)
  
  # Plot the optimization results with correct x-axis labels
  plot(1:length(output_of_marginal), output_of_marginal, type = "l", col = "blue", lwd = 2,
       xlab = "Iteration", ylab = "Log-likelihood",
       main = "Values of log-likelihood for Optimized Parameter Sets")
  
  # Modify the y-axis labels to display the marginal density
  axis(2, at = 1:length(output_of_marginal), labels = output_of_marginal)
}

