# Load the glmnet package
install.packages("DEoptim")
library(glmnet)
library(DEoptim)
library(ggplot2)
set.seed(123)
n <- 100
X <- cbind(rep(1, n), seq(1, n), rep(c(1, 0), times = n/2))
epsilon <- rnorm(n)
beta_true <- c(3, 0, 1/n)
y <- X %*% beta_true + epsilon
ols_fit <- lm(y ~ X + 0)
ols_coef <- coef(ols_fit)


#this function runs 1000 times and count error and lambda and give mean value
run_adaptive_lasso_CV <- function(beta_true) {
  
  # Initialize storage vectors
  lambda_min_adoptive_values <- numeric(1000)
  cv_error_values <- numeric(1000)
  BIC_error_values <- numeric(1000)
  prediction_error <- numeric(1000)
  
  # Loop over i from 1 to 1000
  for (i in 1:1000) {
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
  
  return(results)
}


result_CV<-run_adaptive_lasso_CV(beta_true)
lambda_CV<-mean(result_CV$lambda_min)
ggplot(result_CV, aes(x = lambda_min, y = cv_error)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Lambda", y = "CV Error", title = "Lambda vs CV Error")








run_DEoptim_BIC <- function(X, y, control_params, iterations = 1000) {
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
results_BIC <- run_DEoptim_BIC(X, y, control_params = DEoptim.control(itermax = 100, F = 0.05, CR = 0.09),iterations = 1000)
lambda_BIC<-mean(results_BIC$lambda_min)
ggplot(results_BIC, aes(x = lambda_min, y = BIC_error)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Lambda", y = "BIC Error", title = "Lambda vs BIC Error")




Prediction_error<-function(lambda){
  
  # Fit model using determined lambda_min
  adapt_model_fit <- glmnet(X, y, alpha = 1, penalty.factor = as.vector(abs(ols_coef)), intercept = FALSE, standardize = TRUE)
  
  coef_values <- coef(adapt_model_fit, s = lambda)[-1]  # Exclude intercept
  prediction_error <- t(y - X %*% coef_values) %*% (y - X %*% coef_values)
  return(prediction_error)
}
Prediction_error_BIC<-Prediction_error(lambda_BIC)
Prediction_error_CV<-Prediction_error(lambda_CV)


beta_true <- c(1, 0, 2)
y <- X %*% beta_true + epsilon
ols_fit <- lm(y ~ X + 0)
ols_coef <- coef(ols_fit)

result_CV_new_Beta<-run_adaptive_lasso_CV(beta_true)
lambda_CV_new_Beta<-mean(result_CV_new_Beta$lambda_min)
ggplot(result_CV_new_Beta, aes(x = lambda_min, y = cv_error)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Lambda", y = "CV Error", title = "Lambda vs CV Error for another Beta")



results_BIC_new_Beta <- run_DEoptim_BIC(X, y, control_params = DEoptim.control(itermax = 100, F = 0.05, CR = 0.09),iterations = 1000)
lambda_BIC_new_Beta<-mean(results_BIC_new_Beta$lambda_min)
ggplot(results_BIC_new_Beta, aes(x = lambda_min, y = BIC_error)) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(x = "Lambda", y = "BIC Error", title = "Lambda vs BIC Error for another Beta")


Prediction_error_BIC_new_Beta<-Prediction_error(lambda_BIC_new_Beta)
Prediction_error_CV_new_Beta<-Prediction_error(lambda_CV_new_Beta)


# Create a data frame
results_df <- data.frame(
  lambda_CV = lambda_CV,
  lambda_BIC = lambda_BIC,
  Prediction_error_BIC = Prediction_error_BIC,
  Prediction_error_CV = Prediction_error_CV,
  lambda_BIC_new_Beta = lambda_BIC_new_Beta,
  lambda_CV_new_Beta = lambda_CV_new_Beta,
  Prediction_error_BIC_new_Beta = Prediction_error_BIC_new_Beta,
  Prediction_error_CV_new_Beta = Prediction_error_CV_new_Beta
)

# Export the data frame to a CSV file
write.csv(results_df, "results.csv", row.names = FALSE)

