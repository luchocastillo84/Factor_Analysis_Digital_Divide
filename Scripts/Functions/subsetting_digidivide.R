# Load necessary packages
# install.packages("fastDummies")
# install.packages("dplyr")
library(fastDummies)
library(dplyr)
# Define the function
subsetting_digidivide <- function(data, patt, var2encode = NULL, year, include_weights = FALSE) {
  # Filter the data for the specified year
  data_year <- data %>% filter(year == !!year)
  
  # Select variables based on the pattern
  vars_t <- grep(patt, names(data_year), value = TRUE)
  
  # Subset the data frame based on selected variables
  ict_dd_1 <- data_year[, vars_t, drop = FALSE]
  
  # Perform one-hot encoding on the specified variables
  if (!is.null(var2encode)) {
    ict_dd_1 <- dummy_cols(ict_dd_1, select_columns = var2encode, remove_selected_columns = TRUE, remove_first_dummy = FALSE)
    
    # Convert the resulting dummy variables to factors
    dummy_vars <- grep(paste0("^", var2encode, "_"), names(ict_dd_1), value = TRUE)
    ict_dd_1 <- ict_dd_1 %>% mutate(across(all_of(dummy_vars), as.factor))
  }
  
  # Create subsets for A, S, and U groups
  A_data <- ict_dd_1 %>% dplyr::select(starts_with("A"), matches(paste0("^", var2encode, "_")))
  S_data <- ict_dd_1 %>% dplyr::select(starts_with("S"), matches(paste0("^", var2encode, "_")))
  U_data <- ict_dd_1 %>% dplyr::select(starts_with("U"), matches(paste0("^", var2encode, "_")))
  
  # Optionally include weights
  if (include_weights) {
    if ("weight" %in% names(data_year)) {
      A_data <- cbind(A_data, weight = data_year$weight)
      S_data <- cbind(S_data, weight = data_year$weight)
      U_data <- cbind(U_data, weight = data_year$weight)
    } else {
      warning("Weights column is not available in the data.")
    }
  }
  
  # Remove any duplicates in the selection
  A_data <- A_data[, !duplicated(names(A_data))]
  S_data <- S_data[, !duplicated(names(S_data))]
  U_data <- U_data[, !duplicated(names(U_data))]
  
  # Rename the datasets to include the year
  A_data_name <- paste0("A_data_filtered_", year)
  S_data_name <- paste0("S_data_filtered_", year)
  U_data_name <- paste0("U_data_filtered_", year)
  
  # Return the datasets as a list
  return(list(A_data = A_data, S_data = S_data, U_data = U_data, A_data_name = A_data_name, S_data_name = S_data_name, U_data_name = U_data_name))
}
# # Example usage
# patt <- "^A|^S|^U|^size_rev"
# var2encode <- c("size_rev")
# year <- 2014
# 
# result <- compind_digidivide(ict_combined, patt, var2encode, year)


