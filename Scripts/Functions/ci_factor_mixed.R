# 
# 
# ci_factor_mixed <- function (x, indic_col, method = "ONE", dim = 3) {
#   
#   n_indic <- length(indic_col)
#   n_unit <- nrow(x)
#   
#   if (method == "ONE") {
#     famd_results <- FAMD(x[, indic_col], ncp = 1, graph = FALSE)
#     pesi_fatt = as.matrix(var(famd_results$ind$coord)^2)
#     ci_factor_est = famd_results$ind$coord
#     r <- list(ci_factor_est = ci_factor_est, loadings_fact = pesi_fatt, 
#               ci_method = "FAMD")
#     
#   } else if (method == "ALL") {
#     # Make sure that ncp is not greater than min(n_unit-1, n_indic)
#     ncp_to_use <- min(n_unit - 1, n_indic)
#     famd_results <- FAMD(x[, indic_col], ncp = ncp_to_use, graph = FALSE)
#     # Calculate the weights for each factor
#     pesi_fatt = apply(famd_results$var$coord^2, 2, sum) / ncp_to_use
#     # Compute the composite indicator scores
#     ci_factor_est = famd_results$ind$coord %*% pesi_fatt
#     r <- list(ci_factor_est = ci_factor_est, loadings_fact = pesi_fatt, 
#               ci_method = "FAMD")
#     
#   } else if (method == "CH") {
#     famd_results <- FAMD(x[, indic_col], ncp = dim, graph = FALSE)
#     pesi_fatt = apply(famd_results$var$coord^2, 2, sum) / n_indic
#     pesi_fatt = pesi_fatt[1:dim]
#     ci_factor_est = famd_results$ind$coord[, 1:dim] %*% pesi_fatt
#     r <- list(ci_factor_est = ci_factor_est, loadings_fact = pesi_fatt, 
#               ci_method = "FAMD")
#   } else {
#     stop("Please check method!")
#   }
#   
#   r$call <- match.call()
#   class(r) <- "CI"
#   return(r)
# }
# 
# 

ci_factor_mixed <- function (x, indic_col, method = "ONE", dim = 3, weights = NULL) {
  
  n_indic <- length(indic_col)
  n_unit <- nrow(x)
  
  if (method == "ONE") {
    famd_results <- FAMD(x[, indic_col], ncp = 1, graph = FALSE, row.w = weights)
    pesi_fatt = as.matrix(var(famd_results$ind$coord)^2)
    ci_factor_est = famd_results$ind$coord
    r <- list(ci_factor_est = ci_factor_est, loadings_fact = pesi_fatt, 
              ci_method = "FAMD")
    
  } else if (method == "ALL") {
    # Make sure that ncp is not greater than min(n_unit-1, n_indic)
    ncp_to_use <- min(n_unit - 1, n_indic)
    famd_results <- FAMD(x[, indic_col], ncp = ncp_to_use, graph = FALSE, row.w = weights)
    # Calculate the weights for each factor
    pesi_fatt = apply(famd_results$var$coord^2, 2, sum) / ncp_to_use
    # Compute the composite indicator scores
    ci_factor_est = famd_results$ind$coord %*% pesi_fatt
    r <- list(ci_factor_est = ci_factor_est, loadings_fact = pesi_fatt, 
              ci_method = "FAMD")
    
  } else if (method == "CH") {
    famd_results <- FAMD(x[, indic_col], ncp = dim, graph = FALSE, row.w = weights)
    pesi_fatt = apply(famd_results$var$coord^2, 2, sum) / n_indic
    pesi_fatt = pesi_fatt[1:dim]
    ci_factor_est = famd_results$ind$coord[, 1:dim] %*% pesi_fatt
    r <- list(ci_factor_est = ci_factor_est, loadings_fact = pesi_fatt, 
              ci_method = "FAMD")
  } else {
    stop("Please check method!")
  }
  
  r$call <- match.call()
  class(r) <- "CI"
  return(r)
}
