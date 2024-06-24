library(FactoMineR)
library(factoextra)
library(ggplot2)
library(Compind)


# # Define the function
# comp_index_dd_mixed <- function(data, index = "access", year = 2014) {
#   # Perform FAMD
#   famd_results <- FAMD(data, graph = FALSE, ncp = 8)
#   famd_results_name <- paste0("famd_results_", year, "_", index)
#   assign(famd_results_name, famd_results, envir = .GlobalEnv)
#   
#   # Extract eigenvalues
#   eigenvalues <- get_eigenvalue(famd_results)
#   eigenvalues_name <- paste0("eigenvalues_", year, "_", index)
#   assign(eigenvalues_name, eigenvalues, envir = .GlobalEnv)
#   
#   # Create scree plot
#   scree_plot <- fviz_screeplot(famd_results, addlabels = TRUE, ylim = c(0, 40))
#   scree_plot_name <- paste0("scree_plot_", year, "_", index)
#   assign(scree_plot_name, scree_plot, envir = .GlobalEnv)
#   
#   # Visualize contributions for each variable in the first four dimensions
#   contrib_d1 <- fviz_contrib(famd_results, "var", axes = 1, top = 50)
#   contrib_d2 <- fviz_contrib(famd_results, "var", axes = 2, top = 50)
#   contrib_d3 <- fviz_contrib(famd_results, "var", axes = 3, top = 50)
#   contrib_d4 <- fviz_contrib(famd_results, "var", axes = 4, top = 50)
#   
#   contrib_d1_name <- paste0("contrib_", year, "_", index, "_D1")
#   contrib_d2_name <- paste0("contrib_", year, "_", index, "_D2")
#   contrib_d3_name <- paste0("contrib_", year, "_", index, "_D3")
#   contrib_d4_name <- paste0("contrib_", year, "_", index, "_D4")
#   
#   assign(contrib_d1_name, contrib_d1, envir = .GlobalEnv)
#   assign(contrib_d2_name, contrib_d2, envir = .GlobalEnv)
#   assign(contrib_d3_name, contrib_d3, envir = .GlobalEnv)
#   assign(contrib_d4_name, contrib_d4, envir = .GlobalEnv)
#   
#   # Calculate composite indices using ci_factor_mixed
#   ci_famd <- ci_factor_mixed(data, indic_col = 1:ncol(data), method = "ALL")
#   ci_famd_name <- paste0("ci_famd_", year, "_", index)
#   assign(ci_famd_name, ci_famd, envir = .GlobalEnv)
#   
#   ci <- data.frame(ci_factor_est = ci_famd$ci_factor_est)
#   colnames(ci) <- index  # Rename the column to the value of the index argument
#   ci_name <- paste0("ci_", year, "_", index)
#   assign(ci_name, ci, envir = .GlobalEnv)
#   
#   # Create histogram of the composite indices
#   hist <- ggplot(ci, aes_string(x = index)) + geom_histogram()
#   hist_name <- paste0("hist_", year, "_", index)
#   assign(hist_name, hist, envir = .GlobalEnv)
#   
#   # Return the results as a list
#   return(list(
#     famd_results = famd_results,
#     eigenvalues = eigenvalues,
#     scree_plot = scree_plot,
#     contrib_d1 = contrib_d1,
#     contrib_d2 = contrib_d2,
#     contrib_d3 = contrib_d3,
#     contrib_d4 = contrib_d4,
#     ci_famd = ci_famd,
#     ci = ci,
#     hist = hist
#   ))
# }



# Define the function
comp_index_dd_mixed <- function(data, index = "access", year = 2014, weights = NULL) {
  # Exclude the weight column from indic_col
  indic_col <- setdiff(1:ncol(data), which(names(data) == "weight"))
  
  # Perform FAMD
  famd_results <- FAMD(data[, indic_col], graph = FALSE, ncp = 8, row.w = weights)
  famd_results_name <- paste0("famd_results_", year, "_", index)
  assign(famd_results_name, famd_results, envir = .GlobalEnv)
  
  # Extract eigenvalues
  eigenvalues <- get_eigenvalue(famd_results)
  eigenvalues_name <- paste0("eigenvalues_", year, "_", index)
  assign(eigenvalues_name, eigenvalues, envir = .GlobalEnv)
  
  # Create scree plot
  scree_plot <- fviz_screeplot(famd_results, addlabels = TRUE, ylim = c(0, 40))
  scree_plot_name <- paste0("scree_plot_", year, "_", index)
  assign(scree_plot_name, scree_plot, envir = .GlobalEnv)
  
  # Visualize contributions for each variable in the first four dimensions
  contrib_d1 <- fviz_contrib(famd_results, "var", axes = 1, top = 50)
  contrib_d2 <- fviz_contrib(famd_results, "var", axes = 2, top = 50)
  contrib_d3 <- fviz_contrib(famd_results, "var", axes = 3, top = 50)
  contrib_d4 <- fviz_contrib(famd_results, "var", axes = 4, top = 50)
  
  contrib_d1_name <- paste0("contrib_", year, "_", index, "_D1")
  contrib_d2_name <- paste0("contrib_", year, "_", index, "_D2")
  contrib_d3_name <- paste0("contrib_", year, "_", index, "_D3")
  contrib_d4_name <- paste0("contrib_", year, "_", index, "_D4")
  
  assign(contrib_d1_name, contrib_d1, envir = .GlobalEnv)
  assign(contrib_d2_name, contrib_d2, envir = .GlobalEnv)
  assign(contrib_d3_name, contrib_d3, envir = .GlobalEnv)
  assign(contrib_d4_name, contrib_d4, envir = .GlobalEnv)
  
  # Calculate composite indices using ci_factor_mixed
  ci_famd <- ci_factor_mixed(data, indic_col = indic_col, method = "ALL", weights = weights)
  ci_famd_name <- paste0("ci_famd_", year, "_", index)
  assign(ci_famd_name, ci_famd, envir = .GlobalEnv)
  
  ci <- data.frame(ci_factor_est = ci_famd$ci_factor_est)
  colnames(ci) <- index  # Rename the column to the value of the index argument
  ci_name <- paste0("ci_", year, "_", index)
  assign(ci_name, ci, envir = .GlobalEnv)
  
  # Create histogram of the composite indices
  hist <- ggplot(ci, aes_string(x = index)) + geom_histogram()
  hist_name <- paste0("hist_", year, "_", index)
  assign(hist_name, hist, envir = .GlobalEnv)
  
  # Return the results as a list
  return(list(
    famd_results = famd_results,
    eigenvalues = eigenvalues,
    scree_plot = scree_plot,
    contrib_d1 = contrib_d1,
    contrib_d2 = contrib_d2,
    contrib_d3 = contrib_d3,
    contrib_d4 = contrib_d4,
    ci_famd = ci_famd,
    ci = ci,
    hist = hist
  ))
}

# ### Testing the function 
# 
# ci_famd_A_14w <- ci_factor_mixed(A_data_2014,
#                                  indic_col =  c(1:9),
#                                  method = "CH", dim= 9)
# 
# vis_miss(A_data_2014)
# 
# ci_A_14w <- data.frame(access= ci_famd_A_14w$ci_factor_est)
# 
# ci_A_14w <- normalize_min_max(ci_A_14w, 
#                               col_name = "access", 
#                               new_col_name = "access_n")
# 
# ggplot(ci_A_14w, aes(x= access_n)) + geom_histogram() 
# summary(ci_A_15w)