my_scale <- function(data, columns_to_normalize, level = "overall") {
  # Function to normalize specified columns in a dataset
  # 'level' can be 'overall', 'sector', or 'year'
  
  # Normalization function
  normalize <- function(x) {
    var <- preProcess(data.frame(x), method = c("center", "scale"))
    predict(var, data.frame(x))[[1]]
  }
  
  # Apply normalization based on the specified level
  if (level == "overall") {
    data[columns_to_normalize] <- lapply(data[columns_to_normalize], normalize)
  } else {
    data <- data %>% group_by(!!sym(level)) %>%
      mutate_at(vars(one_of(columns_to_normalize)), normalize) %>%
      ungroup()
  }
  
  return(data)
}