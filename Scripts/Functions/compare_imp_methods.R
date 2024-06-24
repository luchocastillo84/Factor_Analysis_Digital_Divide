compare_imputation_methods <- function(models_list, data, target, predictors) {
  
  results <- list()
  
  # Evaluate each imputation method
  for(model_name in names(models_list)) {
    cat("Evaluating method:", model_name, "\n")
    results[[model_name]] <- evaluate_imputed_data(imp_model = models_list[[model_name]], 
                                                   data = data, 
                                                   target = target, 
                                                   predictors = predictors)
    cat("\n")
  }
  
  # Rank methods based on AUC
  ranked_methods <- results[order(sapply(results, function(x) x$AUC), decreasing = TRUE)]
  
  # If multiple methods have the same AUC, order them by Accuracy
  max_auc <- max(sapply(ranked_methods, function(x) x$AUC))
  if(sum(sapply(ranked_methods, function(x) x$AUC == max_auc)) > 1) {
    with_max_auc <- which(sapply(ranked_methods, function(x) x$AUC) == max_auc)
    sub_ranked <- ranked_methods[with_max_auc]
    sub_ranked <- sub_ranked[order(sapply(sub_ranked, function(x) x$accuracy), decreasing = TRUE)]
    ranked_methods[with_max_auc] <- sub_ranked
  }
  
  # Best method and its metrics
  best_method <- names(ranked_methods)[1]
  best_result <- ranked_methods[[1]]
  
  cat("Best Imputation Method:", best_method, "\n")
  cat("AUC:", best_result$AUC, "\n")
  cat("Accuracy:", best_result$accuracy, "\n")
  print(best_result$confusion_matrix)
  
  return(list(method = best_method, result = best_result))
}

# # Example usage:
# models_to_compare <- list(pmm = imp_model_fm1, rf = imp_model_fm2, cart = imp_model_fm3)
# best_method_result <- compare_imputation_methods(models_list = models_to_compare, 
#                                                  data = ict_14c, 
#                                                  target = "J1", 
#                                                  predictors = c("I2c", "I2b"))
