evaluate_original_data <- function(data, response_var, predictors) {
  # Subset the data to include only the required variables and omit missing values
  data_no_missing <- na.omit(data[, c(response_var, predictors)])
  
  # Create the formula for the model
  formula_str <- paste(response_var, "~", paste(predictors, collapse = " + "))
  formula_obj <- as.formula(formula_str)
  
  # Run the logistic regression model on the data without missing values
  model <- glm(formula_obj, data = data_no_missing, family = binomial())
  
  # Predict using the model
  predicted_prob <- predict(model, type = "response")
  predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
  
  # Accuracy
  acc <- sum(predicted_class == data_no_missing[[response_var]]) / length(data_no_missing[[response_var]])
  
  # Confusion Matrix
  conf_matrix <- confusionMatrix(factor(predicted_class, levels = c(0,1)),
                                 factor(data_no_missing[[response_var]], levels = c(0,1)))
  
  # ROC & AUC
  roc_obj <- roc(data_no_missing[[response_var]], predicted_prob)
  auc <- auc(roc_obj)
  
  return(list("Model" = summary(model), "Accuracy" = acc, "ConfusionMatrix" = conf_matrix, "AUC" = auc))
}

# # Example usage:
# results_original <- evaluate_original_data(ict_14c, "J7", c("I2c", "I2b"))
# 
# # Print the results
# print(paste("Accuracy for Original Model:", results_original$Accuracy))
# print(results_original$ConfusionMatrix)
# print(paste("AUC for Original Model:", results_original$AUC))
