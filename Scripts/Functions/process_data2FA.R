# Define the function
process_data2FA <- function(data, patt, encode = TRUE) {
  # Search for the pattern in the df
  vars_t <- grep(patt, names(data), value = TRUE)
  
  # Subset the data frame based on selected variables
  ict_dd_1 <- data[, vars_t]
  
  # If encode is TRUE, perform one-hot encoding
  if (encode) {
    # Identify columns starting with size_, sme_, and mac_
    encode_cols <- grep("^size_|^sme_|^mac_s", names(ict_dd_1), value = TRUE)
    
    # One-hot encoding for selected columns
    ict_dd_1 <- dummy_cols(ict_dd_1, select_columns = encode_cols, 
                           remove_selected_columns = TRUE, remove_first_dummy = FALSE)
  }
  
  # Convert columns matching pattern to factor
  position <- which(names(ict_dd_1) == "ateco")
  
  # Convert all columns after 'ateco' to factors if 'ateco' is found
  if (length(position) > 0 && position > 0) {
    ict_dd_1[(position + 1):ncol(ict_dd_1)] <- lapply(
      ict_dd_1[(position + 1):ncol(ict_dd_1)], 
      factor)
  }
  
  return(ict_dd_1)
}