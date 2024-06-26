
generate_from_F<- function(n, N, gamma) {
  # Function to generate samples using the inversion method
  generate_samples <- function(N, gamma) {
    # Generate uniform random variables
    u <- runif(N)
    # Apply the inversion method to generate samples
    return(1 + (1/gamma) * (((-(1/log(u)))^(-gamma)) - 1))
  }
  
  # Initialize a list to store each set of generated samples
  all_samples <- vector("list", n)
  
  # Repeat the sample generation process n times
  for (i in 1:n) {
    all_samples[[i]] <- generate_samples(N, gamma)
  }
  
  # Return the list of all generated sample sets
  return(all_samples)
}

# Parameters
n_runs <- 100   # Number of samples
N_samples <- 2000  # size of samples 
gamma_value <- 0.85  # Gamma parameter value

sample_results <- generate_from_F(n_runs, N_samples, gamma_value)
df_samples <- data.frame(sample_results)

# Initialize the list to store the results
hill_estimator_F <- vector("list", 100)

# Loop over the columns of the data frame
for (i in seq_len(ncol(df_samples))) {
  hill_estimator_F[i] <- Hill_estimator(df_samples[, i], 200)
}

hill_estimator_F<-as.numeric(unlist(hill_estimator_F))

boxplot(hill_estimator_F, main = "Box Plot for Extreme value index", ylab = "Data points",col="skyblue")







#(2)#####################
p_th_quantile <- function(data, k, gamma, p) {
  # Ensure data is numeric
  data <- as.numeric(data)
  
  # Get the number of data points
  n <- length(data)
  
  # Sort data in decreasing order
  sorted_data <- sort(data, decreasing = TRUE)
  
  # Identify the k-th last data point
  last_k <- sorted_data[k]
  
  # Calculate the scale parameter a_n
  a_n <- last_k * gamma
  
  # Calculate multiplier for adjustment
  if (n * p < 0) {
    stop("p times number of data points must be at least 1")
  }
  multiplier <- ((k / (n * p))^gamma - 1) / gamma
  
  # Return the estimated p-th quantile
  return(last_k + (a_n * multiplier))
}

calculate_quantiles_for_df <- function(df, k, gamma_list, p) {
  # Ensure gamma_list length matches the number of columns in df
  if (length(gamma_list) != ncol(df)) {
    stop("Length of gamma_list must be equal to the number of columns in the dataframe.")
  }
  
  # Initialize a list or vector to store the quantile results for each column
  quantile_results <- vector("list", 100)
  
  # Iterate through each column of the data frame
  for (i in seq_along(df)) {
    # Apply the quantile function to each column using the corresponding gamma value
    quantile_results[[i]] <- p_th_quantile(df[,i], k, gamma_list[[i]], p)
    
    # Assign names as string indices from 1 to 100
    names(quantile_results) <- as.character(1:length(quantile_results))
  }
  
  # Return the list of quantiles
  return(quantile_results)
}

# Parameters
k <- 200
gamma_list <- hill_estimator_F  # Specific gamma for each column of dataset
p <- 0.0001

# Calculate the quantiles
quantile_results <- calculate_quantiles_for_df(df_samples, k, gamma_list, p)
quantile_results<-as.numeric(unlist(quantile_results))

boxplot(quantile_results, main = "Box Plot for 1-pth quantile ", ylab = "Data Points",col="skyblue")



#(3)###################

p_cap <- function(data, c, k, gamma) {
  # Ensure data is numeric
  data <- as.numeric(data)
  
  # Validate input
  if(k <= 0 || k > length(data)) {
    stop("Parameter k must be within the range of 1 to the number of elements in data.")
  }
  if(gamma == 0) {
    stop("Gamma should not be zero.")
  }
  
  # Get the number of data points
  n <- length(data)
  
  # Sort data in decreasing order
  sorted_data <- sort(data, decreasing = TRUE)
  
  # Identify the k-th last data point
  last_k <- sorted_data[k]
  
  # Calculate the scale parameter a_n
  a_n <- last_k * gamma
  
  # Compute the multiplier factor considering threshold c
  multiplier <- (max(0, 1 + gamma * ((c - last_k) / a_n)))^(-1 / gamma)
  
  # Calculate the adjusted probability
  return((k / n) * multiplier)
}

calculate_P_cap <- function(df, k, gamma_list, c) {
  # Ensure gamma_list length matches the number of columns in df
  if (length(gamma_list) != ncol(df)) {
    stop("Length of gamma_list must be equal to the number of columns in the dataframe.")
  }
  
  # Initialize a list or vector to store the quantile results for each column
  p_cap_list <- list(ncol(df))
  
  # Iterate through each column of the data frame
  for (i in seq_along(df)) {
    # Apply the quantile function to each column using the corresponding gamma value
    quantile_results[[i]] <- p_cap(df[,i], c, k,gamma_list[[i]])
    
    # Assign names as string indices from 1 to 100
    names(quantile_results) <- as.character(1:length(quantile_results))
  }
  
  # Return the list of quantiles
  return(quantile_results)
}


p_caps<-as.numeric(unlist(calculate_P_cap(df_samples,200,hill_estimator_F,4.171)))


# Create a box plot
boxplot(p_caps, main = "Box Plot for 1-F(417.1)",ylab = "Data points",col="skyblue")


