# ci_factor_mca <- function (x, indic_col, method = "ONE", dim = 3) {
#   require(FactoMineR)
#   
#   # Ensure all columns in indic_col are factors
#   x[, indic_col] <- lapply(x[, indic_col], factor)
#   
#   # Perform MCA with different number of dimensions based on the method
#   if (method == "ONE") {
#     mca_results <- MCA(x[, indic_col], graph = FALSE, ncp = 1)
#     ci_factor_est = mca_results$ind$coord[, 1]
#   } else if (method == "ALL") {
#     ncp_to_use <- min(nrow(x) - 1, length(indic_col))
#     mca_results <- MCA(x[, indic_col], graph = FALSE, ncp = ncp_to_use)
#     variances = mca_results$eig[, 2] / sum(mca_results$eig[, 2])  # proportion of variance
#     ci_factor_est = apply(mca_results$ind$coord %*% diag(variances), 1, sum)
#   } else if (method == "CH") {
#     mca_results <- MCA(x[, indic_col], graph = FALSE, ncp = dim)
#     variances = mca_results$eig[, 2] / sum(mca_results$eig[, 2])[1:dim]  # proportion of variance for selected dimensions
#     ci_factor_est = apply(mca_results$ind$coord[, 1:dim] %*% diag(variances), 1, sum)
#   } else {
#     stop("Please check method!")
#   }
#   
#   inertia_percentages = mca_results$eig[, 2] / sum(mca_results$eig[, 2])
#   
#   r <- list(ci_factor_est = ci_factor_est, inertia_fact = inertia_percentages, 
#             ci_method = "MCA")
#   r$call <- match.call()
#   class(r) <- "CI"
#   return(r)
# }
# 
# 
# 

ci_factor_mca <- function (x, indic_col, method = "ONE", dim = 3, weights = NULL) {
  require(FactoMineR)
  
  # Ensure all columns in indic_col are factors
  x[, indic_col] <- lapply(x[, indic_col], factor)
  
  # Perform MCA with different number of dimensions based on the method
  if (method == "ONE") {
    mca_results <- MCA(x[, indic_col], graph = FALSE, ncp = 1, row.w = weights)
    ci_factor_est = mca_results$ind$coord[, 1]
  } else if (method == "ALL") {
    ncp_to_use <- min(nrow(x) - 1, length(indic_col))
    mca_results <- MCA(x[, indic_col], graph = FALSE, ncp = ncp_to_use, row.w = weights)
    variances = mca_results$eig[, 2] / sum(mca_results$eig[, 2])  # proportion of variance
    ci_factor_est = apply(mca_results$ind$coord %*% diag(variances), 1, sum)
  } else if (method == "CH") {
    mca_results <- MCA(x[, indic_col], graph = FALSE, ncp = dim, row.w = weights)
    variances = mca_results$eig[, 2] / sum(mca_results$eig[, 2])[1:dim]  # proportion of variance for selected dimensions
    ci_factor_est = apply(mca_results$ind$coord[, 1:dim] %*% diag(variances), 1, sum)
  } else {
    stop("Please check method!")
  }
  
  inertia_percentages = mca_results$eig[, 2] / sum(mca_results$eig[, 2])
  
  r <- list(ci_factor_est = ci_factor_est, inertia_fact = inertia_percentages, 
            ci_method = "MCA")
  r$call <- match.call()
  class(r) <- "CI"
  return(r)
}




