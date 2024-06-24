# Function to calculate variance and identify low variance variables
calculate_variance <- function(data, year_column, continuous_vars, binary_vars) {
  # # Ensure the specified continuous and binary columns are numeric
  # data <- data %>%
  #   mutate(across(all_of(continuous_vars), ~ as.numeric(as.character(.)))) %>%
  #   mutate(across(all_of(binary_vars), ~ as.numeric(as.character(.))))
  
  # Get unique years from the dataset
  years <- unique(data[[year_column]])
  
  # Initialize a list to store the results
  variance_results <- list()
  low_variance_vars <- list()
  
  # Loop through each year and calculate variance
  for (year in years) {
    # Filter data for the current year
    data_year <- data %>% filter(!!sym(year_column) == year)
    
    # Calculate variance for continuous variables
    continuous_variances <- sapply(data_year[continuous_vars], function(x) {
      if (!all(is.na(x))) {
        return(var(x, na.rm = TRUE))
      } else {
        return(NA)
      }
    })
    
    # Calculate variance for binary variables using p(1-p)
    binary_variances <- sapply(data_year[binary_vars], function(x) {
      if (!all(is.na(x))) {
        p <- mean(x, na.rm = TRUE)
        return(p * (1 - p))
      } else {
        return(NA)
      }
    })
    
    # Combine variances
    variances <- c(continuous_variances, binary_variances)
    
    # Identify variables with variance below 10% (0.1)
    low_var <- names(variances[!is.na(variances) & variances < 0.1])
    
    # Store the results
    variance_results[[as.character(year)]] <- variances
    low_variance_vars[[as.character(year)]] <- low_var
  }
  
  return(list(variance_results = variance_results, low_variance_vars = low_variance_vars))
}
