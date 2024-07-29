# Load necessary libraries
library(dplyr)
library(broom)

# Function to perform hypothesis testing for binary variables
perform_chisq_test <- function(data, binary_vars, alpha = 0.05) {
  
  # Ensure the year variable is a factor
  data <- data %>% mutate(year = as.factor(year))
  
  results <- list()
  
  # Perform chi-squared tests for binary variables
  if (!is.null(binary_vars) && length(binary_vars) > 0) {
    for (var in binary_vars) {
      if (var %in% names(data)) {
        table_data <- table(data$year, data[[var]])
        chisq_result <- chisq.test(table_data)
        p_value <- chisq_result$p.value
        test_statistic <- chisq_result$statistic
        decision <- ifelse(p_value < alpha, "Reject Null Hypothesis", "Fail to Reject Null Hypothesis")
        result <- data.frame(
          Variable = var,
          Test = "Chi-Squared Test",
          Test_Statistic = test_statistic,
          P_Value = sprintf("%.4f", p_value),
          Decision = decision
        )
        results <- append(results, list(result))
      } else {
        warning(paste("Variable", var, "not found in the data."))
      }
    }
  } else {
    stop("No binary variables provided.")
  }
  
  # Combine results into a single data frame
  results_df <- do.call(rbind, results)
  
  return(results_df)
}

# Example usage with your data
# test_results_binary <- perform_chisq_test(A_data_combined, binary_vars = c("A1_B2b", "A2_C5a", "A2_C4_low", "A2_C4_high"))
# 
# print(test_results_binary)

