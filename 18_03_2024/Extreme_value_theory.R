# Load necessary packages
library(readr)
library(lubridate)
library(dplyr)

# Hill estimator function
Hill_estimator <- function(data, k) {
  data <- as.numeric(data)
  sorted_data <- sort(data, decreasing = TRUE)
  top_k <- sorted_data[1:(k - 1)]
  last_k <- sorted_data[k]
  avg <- sum(log(top_k)) / k  - log(last_k)
  return(avg)
}

# Function to plot multiple Hill estimates
hill_estimator_plot_multiple <- function(ks, cauchy_samples, dataset_name) {
  # Initialize list to store Hill estimates for each column
  hill_estimates <- vector("list", 5)
  
  # Calculate Hill estimates for each column
  for (col in 1:5) {
    hill_estimates[[col]] <- sapply(ks, function(k) Hill_estimator(cauchy_samples[, col], k))
  }
  
  # Convert list to data frame
  df <- data.frame(k = rep(ks, times = 5),
                   Hill_Estimate = unlist(hill_estimates),
                   Column = factor(rep(1:5, each = length(ks))))
  
  # Plot line graph
  library(ggplot2)
  ggplot(df, aes(x = k, y = Hill_Estimate, color = Column)) +
    geom_line() +
    labs(x = "k", y = "Hill Estimate", 
         title = paste("Hill Estimates for fives Columns of", dataset_name, "and different Values of k")) +
    scale_color_discrete(name = "Column")
}

# Set the seed for reproducibility
set.seed(123)

# Number of samples
num_samples <- 100

# Sample size
sample_size <- 1000

# Simulate data
cauchy_samples <- matrix(rcauchy(num_samples * sample_size), nrow = sample_size)
frechet_samples <- matrix(rf(num_samples * sample_size, df1 = 1, df2 = 1, ncp = 0), nrow = sample_size)
student_samples <- matrix(rt(num_samples * sample_size, df = 3), nrow = sample_size)

kc <- seq(10, 450)
kf <- seq(10, 850)
ks <- seq(10, 400)

# Example usage
hill_estimator_plot_multiple(kc, cauchy_samples, "Cauchy samples")
hill_estimator_plot_multiple(kf, frechet_samples, "Frechet samples")
hill_estimator_plot_multiple(ks, student_samples, "Student samples")

# Range for each ks value
kc_range <- seq(100, 200)
kf_range <- seq(200, 370)
ks_range <- seq(100, 250)

# Function to calculate Hill estimates matrix
Hill_estimates_matrix <- function(ks, data) {
  hill_estimates <- matrix(NA, nrow = ncol(data), ncol = length(ks))
  for (j in seq_along(ks)) {
    for (i in 1:ncol(data)) {
      hill_estimates[i, j] <- Hill_estimator(data[, i], k = ks[j])
    }
  }
  return(t(hill_estimates))
}

# Function to plot histogram of Hill estimates
Hill_estimates_histogram <- function(data, kc, dataset_name) {
  Cauchy_matrix <- Hill_estimates_matrix(kc, data)
  mean_over_k <- list()
  for (i in 1:ncol(Cauchy_matrix)) {
    mean_over_k[[i]] <- mean(Cauchy_matrix[, i])
  }
  unlisted_mean <- unlist(mean_over_k)
  hist(unlisted_mean, breaks = 10, main = paste("Histogram of Hill Estimates for", dataset_name), xlab = "Average values of Hill Estimator over k")
}

# Plot histograms
Hill_estimates_histogram(cauchy_samples, kc_range, "Cauchy Samples")
Hill_estimates_histogram(frechet_samples, kf_range, "Frechet Samples")
Hill_estimates_histogram(student_samples, ks_range, "Student Samples")









###################################TASK_2###############################
density_function <- function(x, gamma) {
  if (1 + gamma * x <= 0) {
    return(0)  # Return zero density outside of the domain
  }
  return(exp(-((1 + gamma * x) ^ (-1 / gamma)))*((1+gamma*x)^((-1-gamma)/(gamma))))
}

data <- read.csv("39001_gdf.csv")

df<- data$timestamp

data<- data[-(1:19),]

colnames(data)<-c("Date","River_flow_meters_per_second","extra")
data<- data[, !colnames(data) == "extra", drop = FALSE]


# Convert the date column to a Date object
data$Date <- as.Date(data$Date)

# Extract the month from the date column
data$month <- month(data$Date, label = TRUE)

# Define a function to determine the season based on the month
get_season <- function(month) {
  if (month %in% c("Mar", "Apr", "May")) {
    return("Spring")
  } else if (month %in% c("Jun", "Jul", "Aug")) {
    return("Summer")
  } else if (month %in% c("Sep", "Oct", "Nov")) {
    return("Fall")
  } else {
    return("Winter")
  }
}

# Apply the function to the "month" column to get the season
data$season <- sapply(data$month, get_season)

# Filter data for each season
spring_data <- data %>% filter(season == "Spring")
summer_data <- data %>% filter(season == "Summer")
fall_data <- data %>% filter(season == "Fall")
winter_data <- data %>% filter(season == "Winter")

# Convert the "River_flow_meters_per_second" column to numeric for each season
spring_data <- spring_data %>%
  mutate(River_flow_meters_per_second = as.numeric(River_flow_meters_per_second))

summer_data <- summer_data %>%
  mutate(River_flow_meters_per_second = as.numeric(River_flow_meters_per_second))

fall_data <- fall_data %>%
  mutate(River_flow_meters_per_second = as.numeric(River_flow_meters_per_second))

winter_data <- winter_data %>%
  mutate(River_flow_meters_per_second = as.numeric(River_flow_meters_per_second))

# Calculate the mean flow rate for each season
mean_flow_spring <- mean(spring_data$River_flow_meters_per_second, na.rm = TRUE)
mean_flow_summer <- mean(summer_data$River_flow_meters_per_second, na.rm = TRUE)
mean_flow_fall <- mean(fall_data$River_flow_meters_per_second, na.rm = TRUE)
mean_flow_winter <- mean(winter_data$River_flow_meters_per_second, na.rm = TRUE)

mean_flow<-c(mean_flow_spring,mean_flow_summer,mean_flow_fall,mean_flow_winter)

# Calculate residuals for spring data
spring_data$residual <- spring_data$River_flow_meters_per_second - mean_flow_spring
plot(spring_data$residual,
     main = "Spring Data Residual Plot",  
     xlab = "Time 1883-2021(index)",       
     ylab = "Residual")

# Calculate residuals for summer data
summer_data$residual <- summer_data$River_flow_meters_per_second - mean_flow_summer
plot(summer_data$residual,
     main = "Summer Data Residual Plot",  
     xlab = "Time 1883-2021(index)",       
     ylab = "Residual")       

# Calculate residuals for fall data
fall_data$residual <- fall_data$River_flow_meters_per_second - mean_flow_fall
plot(fall_data$residual,
     main = "Fall Data Residual Plot",  
     xlab = "Time 1883-2021(index)",       
     ylab = "Residual")

# Calculate residuals for winter data
winter_data$residual <- winter_data$River_flow_meters_per_second - mean_flow_winter
plot(winter_data$residual,
     main = "Winter Data Residual Plot",  
     xlab = "Time 1883-2021(index)",       
     ylab = "Residual")




#Winter data
winter_data_residual <- moment(winter_data$residual, k=c(100:4000))
plot(winter_data_residual$k, winter_data_residual$estimate, type="l", lwd=1.5,
     main = "Hill Estimator vs. k for Winter Data",  
     xlab = "K values",       
     ylab = "Hill Estimator")
abline(h=0, col="red")

kc<-seq(600,1200)
hill_estimator_winter<- list()

# Calculate Hill estimates for each value of k for  winter
for (i in seq_along(kc)) {
  hill_estimator_winter[[i]] <- Hill_estimator(winter_data$residual, kc[i])
}
gamma_hat_winter <- mean(unlist(hill_estimator_winter))





# Spring Data
spring_data_residual <- moment(spring_data$residual, k = c(50:4000))
plot(spring_data_residual$k, spring_data_residual$estimate, type = "l", lwd = 1.5,
     main = "Hill Estimator vs. k for Spring Data",  
     xlab = "K values",       
     ylab = "Hill Estimator")
abline(h = 0, col = "red")

kc <- seq(750, 1050)
hill_estimator_spring <- list()

# Calculate Hill estimates for each value of k for spring
for (i in seq_along(kc)) {
  hill_estimator_spring[[i]] <- Hill_estimator(spring_data$residual, kc[i])
}

gamma_hat_spring <- mean(unlist(hill_estimator_spring))




# Summer Data
summer_data_residual <- moment(summer_data$residual, k = c(150:4000))
plot(summer_data_residual$k, summer_data_residual$estimate, type = "l", lwd = 1.5,
     main = "Hill Estimator vs. k for Summer Data",  
     xlab = "K values",       
     ylab = "Hill Estimator")
abline(h = 0, col = "red")

kc<-seq(2200,3200)
hill_estimator_summer <- list()

# Calculate Hill estimates for each value of k for summer
for (i in seq_along(kc)) {
  hill_estimator_summer[[i]] <- Hill_estimator(summer_data$residual, kc[i])
}
gamma_hat_summer <- mean(unlist(hill_estimator_summer))




# Fall Data
fall_data_residual <- moment(fall_data$residual, k = c(50:3000))
plot(fall_data_residual$k, fall_data_residual$estimate, type = "l", lwd = 1.5,
     main = "Hill Estimator vs. k for Fall Data",  
     xlab = "K values",       
     ylab = "Hill Estimator")
abline(h = 0, col = "red")

kc<-seq(100,600)
hill_estimator_fall <- list()

# Calculate Hill estimates for each value of k for fall
for (i in seq_along(kc)) {
  hill_estimator_fall[[i]] <- Hill_estimator(fall_data$residual, kc[i])
}
gamma_hat_fall <- mean(unlist(hill_estimator_fall))



seasons <- list(
  winter = list(data = winter_data, gamma_hat = gamma_hat_winter),
  summer = list(data = summer_data, gamma_hat = gamma_hat_summer),
  spring = list(data = spring_data, gamma_hat = gamma_hat_spring),
  fall   = list(data = fall_data  , gamma_hat = gamma_hat_fall)
)

# Process each season
for (season in names(seasons)) {
  # Current season data and gamma
  current_data <- seasons[[season]]$data
  current_gamma <- seasons[[season]]$gamma_hat
  
  # Calculate x values and density values
  x_values <- seq(-1/current_gamma, 20, by = 0.001)
  density_values <- sapply(x_values, function(x) density_function(x, current_gamma))
  
  # Calculate annual maxima and normalize
  annual_maxima <- current_data %>%
    group_by(Year = format(Date, "%Y")) %>%
    summarize(MaxResidual = max(residual, na.rm = TRUE)) %>%
    mutate(NormalizedMaxResidual = scale(MaxResidual)+0.4)
  normalized_maxima <- annual_maxima$NormalizedMaxResidual
  
  # Plotting
  par(mar=c(2, 2, 2, 2))
  plot(x_values, density_values, type = 'l', col = 'blue', lwd = 2,
       main = paste("Density Function and Histogram for", season , " and gamma_hat=", current_gamma),
       xlab = "Annual Maximum Flow (m3/s)", ylab = "Density")
  
  hist(normalized_maxima, breaks = 8, freq = FALSE, add = TRUE, col = rgb(1, 0, 0, 0.5))
}



#####Extra Functions ###################
################if you want to use for df replace Cauchy_samples[,1] by Cauchy_samples #############################
hill_estimator_plot_multiple_2_Accurate <- function(kc, cauchy_samples) {
  # Initialize list to store Hill estimates
  hill_estimator_caushy_1<- list()
  hill_estimator_caushy_2<-list()
  hill_estimator_caushy_3<-list()
  hill_estimator_caushy_4<-list()
  hill_estimator_caushy_5<-list()
  
  # Calculate Hill estimates for each value of k
  for (i in seq_along(kc)) {
    hill_estimator_caushy_1[[i]] <- Hill_estimator(cauchy_samples[,1], kc[i])
    hill_estimator_caushy_2[[i]]<- Hill_estimator(cauchy_samples[,2], kc[i])
    hill_estimator_caushy_3[[i]]<- Hill_estimator(cauchy_samples[,3], kc[i])
    hill_estimator_caushy_4[[i]]<- Hill_estimator(cauchy_samples[,4], kc[i])
    hill_estimator_caushy_5[[i]]<- Hill_estimator(cauchy_samples[,5], kc[i])
  }
  
  # Convert list to data frame
  df_1 <- data.frame(k = kc, Hill_Estimate = unlist(hill_estimator_caushy_1))
  ones_column <- rep(1, nrow(df_1))
  # Combine your data set with the ones column
  df_1 <- cbind(df_1, column = ones_column)
  
  
  df_2 <- data.frame(k = kc, Hill_Estimate = unlist(hill_estimator_caushy_2))
  ones_column <- rep(2, nrow(df_2))
  # Combine your data set with the ones column
  df_2 <- cbind(df_2, column = ones_column)
  
  
  df_3 <- data.frame(k = kc, Hill_Estimate = unlist(hill_estimator_caushy_3))
  ones_column <- rep(3, nrow(df_3))
  # Combine your data set with the ones column
  df_3 <- cbind(df_3, column = ones_column)
  
  
  
  df_4 <- data.frame(k = kc, Hill_Estimate = unlist(hill_estimator_caushy_4))
  ones_column <- rep(4, nrow(df_4))
  # Combine your data set with the ones column
  df_4 <- cbind(df_4, column = ones_column)
  
  
  
  df_5 <- data.frame(k = kc, Hill_Estimate = unlist(hill_estimator_caushy_5))
  ones_column <- rep(5, nrow(df_5))
  # Combine your data set with the ones column
  df_5 <- cbind(df_5, column = ones_column)
  
  library(dplyr)
  combined_df <- rbind(df_1, df_2,df_3,df_4,df_5)
  
  # Plot line graph
  library(ggplot2)
  ggplot(combined_df, aes(x = k, y = Hill_Estimate, color = as.factor(column))) +
    geom_line() +
    labs(x = "k", y = "Hill Estimate", 
         title = "Hill Estimates for Different Columns and Values of k") +
    scale_color_discrete(name = "Column")
}


limiting_distribution <- function(x, gamma) {
  # Ensure 1 + gamma * x is positive
  if (1 + gamma * x <= 0) {
    stop("Invalid input: Ensure that 1 + gamma * x > 0")
  }
  
  # Compute the CDF for the generalized Pareto distribution
  result <- exp(-((1 + gamma * x) ^ (-1 / gamma)))
  
  return(result)
}
