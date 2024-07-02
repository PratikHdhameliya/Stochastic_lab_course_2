
data <- read.table("C:/Users/dhame/OneDrive/Documents/prostate.data.txt", header = TRUE, sep = "\t")
data <- subset(data, select = -c(train))
X<-subset(data,select = -c(lpsa))
y<-subset(data,select = c(lpsa))

#load the "lars" package
library(lars)
# Load the "diabetes" dataset
data(diabetes)



ridge_regression_cv <- function(y, X, lambda = 5) {
  X <- as.matrix(X)
  y <- as.matrix(y)
  I <- diag(ncol(X))
  Beta_lambda <- solve(t(X) %*% X + lambda * I) %*% t(X) %*% y
  A <- X %*% (solve(t(X) %*% X + lambda * I) %*% t(X))
  cv <- sum((y - X %*% Beta_lambda) ^ 2 / (1 - diag(A))^2)
  cv <- cv / nrow(X)
  return(list(Beta_lambda, cv))
}

lambda_values <- seq(1, 100)

# Apply ridge_regression_cv function to each lambda value
results <- sapply(lambda_values, function(lambda) ridge_regression_cv(y, X, lambda))

# Convert the results to a data frame
results_df <- data.frame(lambda = lambda_values, CV = unlist(results[2,]))

# Print the results
print(results_df)




# Set smaller margins
par(mar = c(5, 5, 2, 2))  # Adjust margin values as needed
# Plot lambda vs. CV
plot(results_df$lambda, results_df$CV, type = "l", xlab = "Lambda", ylab = "CV")
# Add a red point at the lambda with the minimum CV
min_index <- which.min(results_df$CV)
min_lambda <- results_df$lambda[min_index]
points(min_lambda, results_df$CV[min_index], col = "red", pch = 19)
# Optionally add a title
title(main = "Cross-Validation (CV) vs. Lambda")
