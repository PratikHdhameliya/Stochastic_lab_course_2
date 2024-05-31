# Function to create train and test datasets from a given dataset
# Arguments:
#   dataset: The dataset from which to create train and test sets
#   indices: The indices specifying which rows of the dataset belong to the train set
# Returns:
#   A list containing the train and test datasets
create_train_test_datasets <- function(dataset, indices) {
  # Extract the train set using the specified indices
  train_set <- dataset[indices]
  
  # Extract the test set using the remaining indices
  test_set <- dataset[-indices]
  
  # Return the train and test sets as a list
  return(list(train_set = train_set, test_set = test_set))
}
