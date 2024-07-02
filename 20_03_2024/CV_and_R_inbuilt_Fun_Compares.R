###      PART    B      ###
# Compute the optimal bandwidth using cross-validation for each dataset

# Extract the reading and writing scores from the data
reading_scores <- data$reading.score
writing_scores <- data$writing.score

# Define the range of bandwidth values to try
bandwidth_values <- c(0.09, 3.28, 4.26, 6.8, 20)

# Compute the optimal bandwidth using cross-validation for math scores
h_math <- compute_CV_optim(math_scores, bandwidth_values, gaussian_kernel)

# Compute the optimal bandwidth using cross-validation for reading scores
h_read <- compute_CV_optim(reading_scores, bandwidth_values, gaussian_kernel)

# Compute the optimal bandwidth using cross-validation for writing scores
h_write <- compute_CV_optim(writing_scores, bandwidth_values, gaussian_kernel)

# Load the required library for bandwidth selection
library(MASS)

# Compute bandwidth using biased cross-validation (bcv) for math scores
h_math_bcv <- bw.bcv(math_scores)

# Compute bandwidth using unbiased cross-validation (ucv) for math scores
h_math_ucv <- bw.ucv(math_scores)

# Compute bandwidth using biased cross-validation (bcv) for reading scores
h_read_bcv <- bw.bcv(reading_scores)

# Compute bandwidth using unbiased cross-validation (ucv) for reading scores
h_read_ucv <- bw.ucv(reading_scores)

# Compute bandwidth using biased cross-validation (bcv) for writing scores
h_write_bcv <- bw.bcv(writing_scores)

# Compute bandwidth using unbiased cross-validation (ucv) for writing scores
h_write_ucv <- bw.ucv(writing_scores)

# Create a summary table containing bandwidth values for each dataset and bandwidth selection method
summary_table <- data.frame(
  Dataset = c("Math", "Reading", "Writing"),
  CV = c(h_math, h_read, h_write),        # Bandwidth values computed using cross-validation
  BCV = c(h_math_bcv, h_read_bcv, h_write_bcv),  # Bandwidth values computed using biased cross-validation
  UCV = c(h_math_ucv, h_read_ucv, h_write_ucv)   # Bandwidth values computed using unbiased cross-validation
)

# Export the summary table to a CSV file named "summary_table.csv"
write.csv(summary_table, file = "summary_table.csv", row.names = FALSE)
