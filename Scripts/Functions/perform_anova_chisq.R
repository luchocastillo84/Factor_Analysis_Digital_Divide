# Load necessary libraries
library(dplyr)
library(broom)

# Function to perform hypothesis testing
perform_hypothesis_test <- function(data, continuous_vars = NULL, binary_vars = NULL, alpha = 0.05) {
  
  # Ensure the year variable is a factor
  data <- data %>% mutate(year = as.factor(year))
  
  results <- list()
  
  # Perform ANOVA for continuous variables
  if (!is.null(continuous_vars)) {
    for (var in continuous_vars) {
      if (var %in% names(data)) {
        formula <- as.formula(paste(var, "~ year"))
        anova_result <- aov(formula, data = data)
        anova_summary <- summary(anova_result)
        p_value <- anova_summary[[1]][["Pr(>F)"]][1]
        test_statistic <- anova_summary[[1]][["F value"]][1]
        decision <- ifelse(p_value < alpha, "Reject Null Hypothesis", "Fail to Reject Null Hypothesis")
        result <- data.frame(
          Variable = var,
          Test = "ANOVA",
          Test_Statistic = test_statistic,
          P_Value = p_value,
          Decision = decision
        )
        results <- append(results, list(result))
      } else {
        warning(paste("Variable", var, "not found in the data."))
      }
    }
  }
  
  # Perform chi-squared tests for binary variables
  if (!is.null(binary_vars)) {
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
          P_Value = p_value,
          Decision = decision
        )
        results <- append(results, list(result))
      } else {
        warning(paste("Variable", var, "not found in the data."))
      }
    }
  }
  
  # Combine results into a single data frame
  results_df <- do.call(rbind, results)
  
  return(results_df)
}

# Example usage with your data
# Replace the variable names with actual names from your data
test_results <- perform_hypothesis_test(A_data_combined, 
                                        continuous_vars = c("A2_A2", "A2_C2", "A2_C6"), 
                                        binary_vars = c("A1_B2b", "A2_C5a", "A2_C4_low", "A2_C4_high"))
print(test_results)
