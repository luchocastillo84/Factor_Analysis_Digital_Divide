evaluate_imputed_data <- function(imp_model, data, target, predictors) {
  
  all_results <- list()
  
  # Loop over the 5 imputed datasets
  for(action in 1:5) {
    
    # Get the completed data
    completed_data <- complete(imp_model, action = action)
    completed_data[[target]] <- data[[target]]
    
    # Fit the GLM model
    formula_str <- paste(target, "~", paste(predictors, collapse = " + "))
    model <- glm(as.formula(formula_str), data = completed_data, family = binomial())
    
    # Predictions
    predicted_prob <- predict(model, type = "response")
    predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
    
    # Evaluation metrics
    acc <- mean(predicted_class == completed_data[[target]])
    conf_matrix <- confusionMatrix(factor(predicted_class, levels = c(0,1)), 
                                   factor(completed_data[[target]], levels = c(0,1)))
    roc_obj <- roc(completed_data[[target]], predicted_prob)
    auc_value <- auc(roc_obj)
    
    # Store results for this action value
    all_results[[action]] <- list(action = action,
                                  accuracy = acc, 
                                  confusion_matrix = conf_matrix, 
                                  AUC = auc_value)
  }
  
  # Rank results based on AUC and Accuracy
  ranked_results <- all_results[order(sapply(all_results, function(x) x$AUC), decreasing = TRUE)]
  
  # If multiple results have the same AUC, order them by Accuracy
  max_auc <- max(sapply(ranked_results, function(x) x$AUC))
  if(sum(sapply(ranked_results, function(x) x$AUC == max_auc)) > 1) {
    with_max_auc <- which(sapply(ranked_results, function(x) x$AUC) == max_auc)
    sub_ranked <- ranked_results[with_max_auc]
    sub_ranked <- sub_ranked[order(sapply(sub_ranked, function(x) x$accuracy), decreasing = TRUE)]
    ranked_results[with_max_auc] <- sub_ranked
  }
  
  # Return the best result (which is now the first in the ranked list)
  best_result <- ranked_results[[1]]
  
  cat("Best Result (based on highest AUC and then ACC):\n")
  cat("From imputed dataset number:", best_result$action, "\n")
  cat("AUC:", best_result$AUC, "\n")
  cat("Accuracy:", best_result$accuracy, "\n")
  print(best_result$confusion_matrix)
  
  return(best_result)
}

# # Example usage:
# best_result <- evaluate_imputed_data(imp_model = imp_model_ti1, 
#                                      data = ict_14c, 
#                                      target = "J1", 
#                                      predictors = c("I2c", "I2b"))
