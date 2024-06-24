normalize_min_max <- function(df, col_name, new_col_name) {
  # Calculate the minimum and maximum values of the specified column
  min_value <- min(df[[col_name]], na.rm = TRUE)
  max_value <- max(df[[col_name]], na.rm = TRUE)
  
  # Apply min-max normalization
  df[[new_col_name]] <- (df[[col_name]] - min_value) / (max_value - min_value)
  
  # Return the modified dataframe
  return(df)
}
