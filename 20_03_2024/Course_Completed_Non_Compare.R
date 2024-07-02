###      PART    c      ###

# Subset the data for completed cases and select only the desired columns
completed_cases <- subset(data, test.preparation.course == "completed" , select = c("math.score", "writing.score", "reading.score"))

# Subset the data for non-completed cases and select only the desired columns
non_completed_cases <- subset(data, test.preparation.course == "none", select = c("math.score", "reading.score", "writing.score"))

# Extract scores for completed and non-completed cases for each subject
math_completed <- completed_cases$math.score
math_non_completed <- non_completed_cases$math.score
reading_completed <- completed_cases$reading.score
reading_non_completed <- non_completed_cases$reading.score
writing_completed <- completed_cases$writing.score
writing_non_completed <- non_completed_cases$writing.score

# Create a list containing datasets for each subject
datasets <- list(math_completed, math_non_completed, 
                 reading_completed, reading_non_completed, 
                 writing_completed, writing_non_completed)

# Compute the optimal bandwidths using unbiased cross-validation (UCV) for each dataset
optimal_bandwidths <- lapply(datasets, compute_CV_optim, bandwidth_values = c(3.0, 6.0), kernel = gaussian_kernel)

# Load the ggplot2 library for plotting
library(ggplot2)

# Plot kernel density estimation for completed and non-completed cases for math scores
plot_kde_for_two_datasets(math_completed, 5.999924, math_non_completed, 5.353564, gaussian_kernel)

# Plot kernel density estimation for completed and non-completed cases for reading scores
plot_kde_for_two_datasets(reading_completed, 5.069659, reading_non_completed, 4.706739, gaussian_kernel)

# Plot kernel density estimation for completed and non-completed cases for writing scores
plot_kde_for_two_datasets(writing_completed, 3.851564, writing_non_completed, 4.8229, gaussian_kernel)
