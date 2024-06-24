convert_and_clean <- function(data_frame) {
  # Trigger garbage collection at the beginning
  gc()
  
  # Trim white spaces for variable names
  names(data_frame) <- trimws(names(data_frame))
  
  # Trim white spaces only for character columns and replace problematic values
  data_frame <- data_frame %>%
    mutate(across(where(is.character), ~trimws(.))) %>%
    mutate(across(where(is.character), ~replace(., . %in% c(".", "", "NA"), NA_character_)))
  
  # Convert only character columns to factors
  data_frame <- data_frame %>%
    mutate(across(where(is.character), as.factor))
  
  return(data_frame)
}