ci_generalized_mean1 <- function (x, indic_col, p, na.rm = TRUE) {
  # Extract the numeric data for the specified columns
  x_num <- x[, indic_col]
  
  # Check for non-numeric data
  if (!all(sapply(x_num, is.numeric))) {
    stop("All columns must be numeric.")
  }
  
  # Check for NA values and handle them
  if (na.rm) {
    na_present <- any(is.na(x_num))
    if (na_present) {
      # Provide a warning about NA values
      warning("NA values found. They will be removed before computing the generalized mean.")
      x_num <- na.omit(x_num)
    }
  } else if (any(is.na(x_num))) {
    # If na.rm is FALSE and there are NAs, stop and return an error
    stop("NA values present. Set na.rm=TRUE to remove them or address them in your data.")
  }
  
  # Calculate the generalized mean
  x_elev_p <- x_num^p
  sum_p <- rowSums(x_elev_p, na.rm = na.rm)
  ci_generalized_mean_est <- (sum_p / ncol(x_num))^(1/p)
  
  # Return a list with the results
  r <- list(ci_generalized_mean_est = ci_generalized_mean_est, 
            ci_method = "generalized_mean")
  class(r) <- "CI"
  return(r)
}
