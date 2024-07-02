# Define the normalize_datasets function
normalize_datasets <- function(train_set, test_set) {
  # Compute mean and standard deviation of the train set
  train_mean <- mean(train_set)
  train_sd <- sd(train_set)
  test_mean <- mean(test_set)
  test_sd <- sd(test_set)
  
  # Apply normalization to train set
  normalized_train_set <- scale(train_set, center = train_mean, scale = train_sd)
  
  # Apply the same transformation to the test set
  normalized_test_set <- scale(test_set, center = test_mean, scale = test_sd)
  
  # Return both normalized datasets
  return(list(normalized_train_set = normalized_train_set, normalized_test_set = normalized_test_set))
}