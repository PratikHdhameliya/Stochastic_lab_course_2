# Load necessary libraries
library(readr)
library(ggplot2)
library(dplyr)

# Read student data from CSV file
data <- read_csv("student-mat.csv")

# Define a function to plot density plot and Poisson PMF comparison
plot_density_and_poisson <- function(data, G1, title, xlabal) {
  # Calculate density of the data for G1
  density_data <- density(data[[G1]])
  # Calculate mean value of the data for G1
  mean_value <- mean(data[[G1]])
  
  # Create data frame for Poisson PMF
  x <- 0:20
  poisson_pmf <- data.frame(x = x, pmf = dpois(x, lambda = mean_value))
  
  # Plot both density plot and Poisson PMF
  ggplot() +
    geom_density(data = data, aes_string(x = G1), fill = "skyblue", color = "blue") +
    geom_line(data = poisson_pmf, aes(x = x, y = pmf), color = "red", size = 1) +
    labs(title = paste("Distribution of", title, " and Poisson PMF Comparison "),
         x = paste("Data points of ", xlabal), y = "Density/Probability") +
    scale_y_continuous(sec.axis = sec_axis(~. * length(data[[G1]]), name = "Poisson PMF")) +
    theme_minimal()
}

# Plot density plot and Poisson PMF for G1, G2, and G3
plot_density_and_poisson(data, "G1", "G1", "First-period grade in mathematics")
plot_density_and_poisson(data, "G2", "G2", "Second-period grade in mathematics")
plot_density_and_poisson(data, "G3", "G3", "Final grade in mathematics")




###########Part_B###############################################

# Select columns for modeling, excluding G2 and G3
data_model_1 <- select(data, -G2, -G3)

# Select specific columns for Model 2
data_model_2 <- data_model_1[, c("sex", "Fedu", "studytime", "failures", "schoolsup", "famsup", "goout", "G1")]

# Select specific columns for Model 3
data_model_3 <- data_model_1[, c("sex", "Fedu", "studytime", "failures", "schoolsup", "famsup", "Walc", "G1")]

# Fit Gaussian linear model for Model 1
model_1_gaussian <- glm(G1 ~ ., family = gaussian(link = "identity"), data = data_model_1)

# Print summary of Model 1
summary(model_1_gaussian)

# Calculate deviance, AIC, and BIC for Model 1
deviance <- deviance(model_1_gaussian)
AIC_gaussian_1 <- AIC(model_1_gaussian)
BIC_gaussian_1 <- BIC(model_1_gaussian)

# Calculate Pearson residuals for Model 1
pearson_resid_gaussian_1 <- residuals(model_1_gaussian, type = "pearson")

# Calculate Anscombe residuals for Model 1
var_func <- family(model_1_gaussian)$variance  # Variance function of the GLM
mean_func <- family(model_1_gaussian)$linkinv  # Inverse link function of the GLM
mu <- model_1_gaussian$linear.predictors  # Fitted values (linear predictors)
anscombe_resid_gaussian_1 <- pearson_resid_gaussian_1 / sqrt(var_func(mean_func(mu)))

# Plot Pearson residuals for Model 1
par(mar = c(4, 5, 2, 1)) # Set margins
library(ggplot2)
plot_histogram <- function(data, num_bins = 30, fill_color = "blue", line_color = "black", data_name) {
  # Check if the input data is numeric
  if (!is.numeric(data)) {
    stop("Data must be numeric.")
  }
  
  # Create a data frame from the vector for ggplot2 usage
  df <- data.frame(Residuals = data)
  
  # Create histogram using ggplot2
  p <- ggplot(df, aes(x = Residuals)) +
    geom_histogram(bins = num_bins, fill = fill_color, color = line_color) +  # Set bin color and outline color
    labs(title = paste("Histogram of", data_name), x = "Residual", y = "Frequency") +
    theme_minimal() +  # Clean theme
    theme(panel.grid.major = element_line(color = "gray", linetype = "dotted"),  # Customize major grid lines
          panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"))  # Customize minor grid lines
  
  # Return the plot object
  return(p)
}

# Plot histogram for Pearson residuals of Model 1
hist_plot <- plot_histogram(pearson_resid_gaussian_1, num_bins = 20, fill_color = "blue", line_color = "black", "Pearson residuals")
print(hist_plot)

# Plot histogram for Anscombe residuals of Model 1
hist_plot <- plot_histogram(anscombe_resid_gaussian_1, num_bins = 20, fill_color = "darkblue", line_color = "black", "Anscombe residuals")
print(hist_plot)

# Capture summary of Model 1 as text
model_1 <- capture.output(summary(model_1_gaussian))

# Write the model summary to a text file
writeLines(model_1, "model_1_summary.txt")










# Model 1  Fit Poisson linear model
model_1_poisson <- glm(G1 ~ ., family = poisson(link = "log"), data = data_model_1)

# Summary of Model 1
summary(model_1_poisson)

# Calculate deviance, AIC, and BIC for Model 1
deviance_model_1 <- deviance(model_1_poisson)
AIC_model_1 <- AIC(model_1_poisson)
BIC_model_1 <- BIC(model_1_poisson)

# Calculate Pearson residuals for Model 1
pearson_resid_model_1 <- residuals(model_1_poisson, type = "pearson")

# Calculate Anscombe residuals for Model 1
var_func_model_1 <- family(model_1_poisson)$variance  # Variance function of the GLM
mean_func_model_1 <- family(model_1_poisson)$linkinv  # Inverse link function of the GLM
mu_model_1 <- model_1_poisson$linear.predictors  # Fitted values (linear predictors)
anscombe_resid_model_1 <- pearson_resid_model_1 / sqrt(var_func_model_1(mean_func_model_1(mu_model_1)))

# Plot Pearson residuals for Model 1
#plot(pearson_resid_model_1)
hist_plot <- plot_histogram(pearson_resid_model_1, num_bins = 20, fill_color = "gray", line_color = "white","Pearson residuals")
print(hist_plot)
# Plot Anscombe residuals for Model 1
#plot(anscombe_resid_model_1)

hist_plot <- plot_histogram(anscombe_resid_model_1, num_bins = 20, fill_color = "black", line_color = "white", "Anscombe residuals")
print(hist_plot)












######Part_C##############################################
# Fit Poisson linear model for Model 2
model_2_poisson <- glm(G1 ~ ., family = poisson(link = "log"), data = data_model_2)

# Summary of Model 2
summary(model_2_poisson)

# Calculate deviance, AIC, and BIC for Model 2
deviance_model_2 <- deviance(model_2_poisson)
AIC_model_2 <- AIC(model_2_poisson)
BIC_model_2 <- BIC(model_2_poisson)

# Calculate Pearson residuals for Model 2
pearson_resid_model_2 <- residuals(model_2_poisson, type = "pearson")

# Calculate Anscombe residuals for Model 2
var_func_model_2 <- family(model_2_poisson)$variance  # Variance function of the GLM
mean_func_model_2 <- family(model_2_poisson)$linkinv  # Inverse link function of the GLM
mu_model_2 <- model_2_poisson$linear.predictors  # Fitted values (linear predictors)
anscombe_resid_model_2 <- pearson_resid_model_2 / sqrt(var_func_model_2(mean_func_model_2(mu_model_2)))

# Set plot margins
par(mar = c(1, 1, 1, 2) + 0.01) 

# Plot histogram for Pearson residuals of Model 2
hist_plot <- plot_histogram(pearson_resid_model_2, num_bins = 20, fill_color = "black", line_color = "white","Pearson Residual")
print(hist_plot)

# Plot histogram for Anscombe residuals of Model 2
hist_plot <- plot_histogram(anscombe_resid_model_2, num_bins = 20, fill_color = "brown", line_color = "white","Anscombe Residual")
print(hist_plot)

# Capture summary of Model 2 as text
model_2_summary <- capture.output(summary(model_2_poisson))

# Write the model summary to a text file
writeLines(model_2_summary, "model_2_summary.txt")





# Fit Poisson linear model for Model 3
model_3_poisson <- glm(G1 ~ ., family = poisson(link = "log"), data = data_model_3)

# Capture the summary of Model 3 as text
model_3_summary <- capture.output(summary(model_3_poisson))
summary(model_3_poisson)

# Write the model summary to a text file
writeLines(model_3_summary, "model_3_summary.txt")

# Calculate deviance, AIC, and BIC for Model 3
deviance_model_3 <- deviance(model_3_poisson)
AIC_model_3 <- AIC(model_3_poisson)
BIC_model_3 <- BIC(model_3_poisson)

# Calculate Pearson residuals for Model 3
pearson_resid_model_3 <- residuals(model_3_poisson, type = "pearson")

# Calculate Anscombe residuals for Model 3
var_func_model_3 <- family(model_3_poisson)$variance  # Variance function of the GLM
mean_func_model_3 <- family(model_3_poisson)$linkinv  # Inverse link function of the GLM
mu_model_3 <- model_3_poisson$linear.predictors  # Fitted values (linear predictors)
anscombe_resid_model_3 <- pearson_resid_model_3 / sqrt(var_func_model_3(mean_func_model_3(mu_model_3)))

# Plot histogram for Pearson residuals of Model 3
hist_plot <- plot_histogram(pearson_resid_model_3, num_bins = 20, fill_color = "red", line_color = "white","Pearson residuals")
print(hist_plot)

# Plot histogram for Anscombe residuals of Model 3
hist_plot <- plot_histogram(anscombe_resid_model_3, num_bins = 20, fill_color = "blue", line_color = "white", "Anscombe residuals")
print(hist_plot)

