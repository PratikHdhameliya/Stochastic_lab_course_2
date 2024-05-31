# Load the custom functions for creating train and test datasets, generating random samples, and normalizing datasets
source("create_train_test_datasets.R") 
source("create_random_sample.R") 
source("normalize_datasets.R") 

# Access the waiting column of the faithful dataset
data <- faithful$waiting
n <- length(data)

# Generate a random sample to split the dataset into train and test sets
random_num <- create_random_sample(n, 0.5)

# Create train and test datasets using the function
result <- create_train_test_datasets(data, random_num)

# Access train and test sets
train_set <- result$train_set
test_set <- result$test_set

# Normalize the datasets
normalized_data <- normalize_datasets(train_set, test_set)

# Access the normalized train and test sets
normalized_train_set <- normalized_data$normalized_train_set
normalized_test_set <- normalized_data$normalized_test_set

# Print the normalized datasets along with histograms
print("Normalized Train Set:")
hist(normalized_train_set, main = "Histogram of Train data set", xlab = "Normalized train data point", ylab = "Frequency")
print("Normalized Test Set:")
hist(normalized_test_set, main = "Histogram of Test data set", xlab = "Normalized test data point", ylab = "Frequency")
