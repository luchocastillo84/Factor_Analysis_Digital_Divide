process_ict_data <- function(vars_df, ict_df, patterns = "^C9.|^C10.|^D.|^F.|^G.|^H.") {
  
  # Assuming convert_and_clean is a function you have already defined elsewhere
  ict_df <- convert_and_clean(ict_df)
  
  # Identify missing values
  miss_ict <- is.na(ict_df)
  
  # Count missing values for each column
  missing_count <- colSums(miss_ict)
  
  # Calculate miss_per and add it to vars_df
  vars_df$miss_per <- round((apply(ict_df, 2, function(x) sum(is.na(x))) / nrow(ict_df)) * 100, 2)
  
  # The fixed patterns combined with the user provided patterns
  full_pattern <- patterns
  
  # Remove rows where miss_per > 60 or based on the given patterns in acrom
  vars_df_r <- vars_df[!vars_df$miss_per > 60 & !grepl(full_pattern, vars_df$acrom), ]
  
  col_i <- match(vars_df_r$acrom, colnames(ict_df))
  
  # Subset the ict_df data frame based on the matched column indices (excluding NAs)
  ict_df <- ict_df[, na.omit(col_i)]
  
  return(list(vars_df_r, ict_df))
}

# # Usage:
# result <- process_ict_data(var_15, ict_15)
# var_15r <- result[[1]]
# ict_15 <- result[[2]]
