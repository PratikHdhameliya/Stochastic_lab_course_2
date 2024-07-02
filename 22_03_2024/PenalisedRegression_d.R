# Load necessary libraries
library(glmnet)
library(lars)

# Load the diabetes dataset from 'lars' package
data(diabetes)

# Split the dataset into predictors (X) and response (y)
X <- diabetes$x
y <- diabetes$y

# Convert predictors to a data frame (if necessary for other operations)
X_df <- as.data.frame(X)

# Fit an ordinary least squares model without an intercept
ols_fit <- lm(y ~ X + 0)
ols_coef <- coef(ols_fit)

# Initialize storage vectors
lambda_min_adoptive_values <- numeric(40)
cv_error_values <- numeric(40)
BIC_error_values <- numeric(40)
prediction_error <- numeric(40)

# Loop over i from 1 to 1000
for (i in 1:40) {
  # Perform cross-validated Lasso
  adapt_model_cv <- cv.glmnet(X, y, alpha = 1, penalty.factor = as.vector(abs(ols_coef)), intercept = FALSE, standardize = TRUE)
  
  # Store lambda and CV error
  lambda_min_adoptive_values[i] <- adapt_model_cv$lambda.min
  cv_error_values[i] <- min(adapt_model_cv$cvm)
  
  # Fit model using determined lambda_min
  adapt_model_fit <- glmnet(X, y, alpha = 1, penalty.factor = as.vector(abs(ols_coef)), intercept = FALSE, standardize = TRUE)
  
  # Extract coefficients and compute statistics
  coef_values <- coef(adapt_model_fit, s = adapt_model_cv$lambda.min)[-1]  # Exclude intercept
  prediction_error[i] <- t(y - X %*% coef_values) %*% (y - X %*% coef_values)
  
  # Compute residual sum of squares
  RSS <- sum((y - predict(adapt_model_fit, newx = X, s = adapt_model_cv$lambda.min))^2)
  
  # Compute number of nonzero coefficients
  num_nonzero <- sum(coef_values != 0)
  n<-nrow(X)
  # Compute and store BIC error
  BIC_error_values[i] <- RSS + log(n) * num_nonzero
}

# Calculate and return mean results
results <- data.frame(
  lambda_min =       lambda_min_adoptive_values,
  cv_error =         cv_error_values,
  BIC_error =        BIC_error_values,
  prediction_error = prediction_error
)


lambda_CV<-mean(results$lambda_min)
ggplot(results, aes(x = lambda_min, y = cv_error)) +
  geom_smooth(method = "loess", se = FALSE) +  # Use loess smoothing method
  labs(x = "Lambda", y = "CV Error", title = "Lambda vs CV Error for Diabetes data")






run_DEoptim_BIC <- function(X, y, control_params, iterations = 40) {
  BIC_error <- function(lambda) {
    
    adapt_model_fit <- glmnet(X, y, alpha = 1, penalty.factor = as.vector(abs(ols_coef)), intercept = FALSE, standardize = TRUE)
    
    RSS <- sum((y - predict(adapt_model_fit, newx = X, s = lambda))^2)
    coef_values <- coef(adapt_model_fit, s = lambda)[-1]  
    num_nonzero <- sum(coef_values != 0)
    
    BIC_error_values <- RSS + log(length(y)) * num_nonzero
    return(BIC_error_values)
  }
  
  lambda_min_values <- numeric(iterations)
  BIC_error_values <- numeric(iterations)
  
  for (i in 1:iterations) {
    result <- DEoptim(BIC_error, lower = 0, upper = 100, control = control_params)
    lambda_min_values[i] <- result$optim$bestmem
    BIC_error_values[i] <- result$optim$bestval
  }
  
  results_df_BIC <- data.frame(lambda_min = lambda_min_values, BIC_error = BIC_error_values)
  return(results_df_BIC)
}

# Example usage:
results_BIC <- run_DEoptim_BIC(X, y, control_params = DEoptim.control(itermax = 100, F = 0.05, CR = 0.09),iterations = 40)
lambda_BIC<-mean(results_BIC$lambda_min)
ggplot(results_BIC, aes(x = lambda_min, y = BIC_error)) +
  geom_line() +
  labs(x = "Lambda", y = "BIC Error", title = "Lambda vs BIC Error for Diabetes data")




Prediction_error<-function(lambda){
  
  # Fit model using determined lambda_min
  adapt_model_fit <- glmnet(X, y, alpha = 1, penalty.factor = as.vector(abs(ols_coef)), intercept = FALSE, standardize = TRUE)
  
  coef_values <- coef(adapt_model_fit, s = lambda)[-1]  # Exclude intercept
  prediction_error <- t(y - X %*% coef_values) %*% (y - X %*% coef_values)
  return(prediction_error)
}
Prediction_error_BIC<-Prediction_error(lambda_BIC)
Prediction_error_CV<-Prediction_error(lambda_CV)



results_df <- data.frame(
  lambda_CV = lambda_CV,
  lambda_BIC = lambda_BIC,
  Prediction_error_BIC = Prediction_error_BIC,
  Prediction_error_CV = Prediction_error_CV
)
# Export the data frame to a CSV file
write.csv(results_df, "results_diabetes.csv", row.names = FALSE)
