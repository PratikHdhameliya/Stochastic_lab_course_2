# Load necessary libraries
library(glmnet)

# Set seed for reproducibility
set.seed(123)

# Generate data
n <- 100  # Number of samples
p <- 3    # Number of predictors

# Create design matrix X
X <- cbind(rep(1, n),            # Intercept term
           seq(1, n),             # Linear trend
           rep(c(1,0), times = n/2))  # Binary predictor with alternating values

# True coefficients for generating the response variable
beta_true <- c(3, 0, 1/n)

# Generate response variable y with noise
epsilon <- rnorm(n)
y <- X %*% beta_true + epsilon

# Perform Lasso regression with cross-validation
lasso_model_cv <- cv.glmnet(X, y, alpha = 1, family = "gaussian")

# Plot the cross-validation results
plot(lasso_model_cv, ylab = "Mean-Squared Error for Lasso Regression")

# Find lambda value for minimum cross-validation error
lambda_min_lasso <- lasso_model_cv$lambda.min
cat("Lambda for minimum CV error:", lambda_min_lasso, "\n")

# Fit Lasso model with selected lambda
lasso_fit <- glmnet(X, y, alpha = 1, lambda = lambda_min_lasso)
lasso_coef <- coef(lasso_fit)
print(lasso_coef)
lasso_fit <- glmnet(X, y, alpha = 1)
plot(lasso_fit,xvar = "lambda",label = TRUE)



# Normal OLS beta_hat for adaptive lasso
ols_fit <- lm(y ~ X + 0)  # Fit OLS model without intercept
ols_coef <- coef(ols_fit)  # Extract OLS coefficients

# Perform Adaptive Lasso regression with cross-validation
adapt_model_cv <- cv.glmnet(X, y, alpha = 1, penalty.factor = as.vector(abs(ols_coef)), intercept = FALSE)

# Plot the cross-validation results for adaptive Lasso
plot(adapt_model_cv,ylab = "Mean-Squared Error for adaptive Lasso Regression")

# Find lambda value for minimum cross-validation error
lambda_min_adaptive <- adapt_model_cv$lambda.min
cat("Lambda for minimum CV error:", lambda_min_adaptive, "\n")

# Fit Adaptive Lasso model with selected lambda
adapt_lasso_fit <- glmnet(X, y, alpha = 1, lambda = lambda_min_adaptive, penalty.factor = as.vector(abs(ols_coef)), intercept = FALSE)
adapt_lasso_coef <- coef(adapt_lasso_fit)
print(adapt_lasso_coef)


adapt_lasso_fit <- glmnet(X, y, alpha = 1, penalty.factor = as.vector(abs(ols_coef)), intercept = FALSE)
plot(adapt_lasso_fit,xvar = "lambda",label = TRUE)
