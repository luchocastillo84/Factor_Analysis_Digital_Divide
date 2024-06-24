integrate_imputed_data <- function(original_data, imputed_datasets, imputed_cols) {
  
  # Getting the order of the column of the original data
  order_cols <- names(original_data)
  # Getting the indexes of each variable in the original data
  index_original <- match(imputed_cols, names(original_data))
  
  # Initialize a list to store extracted columns from each imputed dataset
  extracted_cols <- list()
  
  # Loop through each imputed dataset
  for(i in 1:length(imputed_datasets)) {
    # Extract the specified columns from the imputed dataset
    extracted_data <- imputed_datasets[[i]][, 1:ncol(imputed_datasets[[i]])]
    extracted_cols[[i]] <- extracted_data
  }
  
  # Bind all the extracted columns together
  bind_imp <- do.call(cbind, extracted_cols)
  
  # Remove the columns in the original dataset that will be replaced
  original_data <- original_data[, -index_original]
  
  # Bind the original data with the imputed columns
  integrated_data <- cbind(original_data, bind_imp)
  
  # Order the columns based on the original dataset
  integrated_data <- integrated_data[, order_cols]
  
  return(integrated_data)
}

# # Example usage:
# imputed_datasets_list <- list(completed_data_ws2, completed_data_cc1, completed_data_ti2, completed_data_mv3, completed_data_cc1a)
# imputed_cols <- c("C8a", "C8b", "C8c", "C8d", "C8e", "C8f", "C8g", "C8h", "C8i", "D2a", "D2b", "D2c", "D2d", 
#                   "D2e", "D2f", "D2g", "D3a", "D3b", "D4a", "D4b", "D4c", "I2c", "I2b", "I2a", "C4", "C6", "J8")
# 
# ict_14_impT <- integrate_imputed_data(ict_14c, imputed_datasets_list, imputed_cols)
