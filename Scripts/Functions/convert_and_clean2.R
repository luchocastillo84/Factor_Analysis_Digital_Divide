# Define the improved function
convert_and_clean2 <- function(data_frame) {
  # Trigger garbage collection at the beginning
  gc()
  
  # Trim white spaces for variable names
  names(data_frame) <- trimws(names(data_frame))
  
  # Trim white spaces only for character columns and replace problematic values
  data_frame <- data_frame %>%
    mutate(across(where(is.character), ~trimws(.))) %>%
    mutate(across(where(is.character), ~replace(., . %in% c(".", "", "NA"), NA_character_)))
  
  # Check for numeric-like character columns and convert them to numeric
  data_frame <- data_frame %>%
    mutate(across(where(is.character), ~ifelse(grepl("^[0-9.]+$", .), as.numeric(.), .)))
  
  # Return the cleaned data frame without converting character columns to factors
  return(data_frame)
}