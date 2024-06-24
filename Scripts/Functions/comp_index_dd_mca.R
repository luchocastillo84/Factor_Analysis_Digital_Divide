# library(FactoMineR)
# library(factoextra)
# library(ggplot2)
# 
# # Define the function
# comp_index_dd_mca <- function(data, index = "skills", year = 2014) {
#   # Perform MCA
#   mca_results <- MCA(data, graph = FALSE, ncp = 8)
#   mca_results_name <- paste0("mca_results_", year, "_", index)
#   assign(mca_results_name, mca_results, envir = .GlobalEnv)
#   
#   # Extract eigenvalues
#   eigenvalues <- get_eigenvalue(mca_results)
#   eigenvalues_name <- paste0("eigenvalues_", year, "_", index)
#   assign(eigenvalues_name, eigenvalues, envir = .GlobalEnv)
#   
#   # Create scree plot
#   scree_plot <- fviz_screeplot(mca_results, addlabels = TRUE, ylim = c(0, 40))
#   scree_plot_name <- paste0("scree_plot_", year, "_", index)
#   assign(scree_plot_name, scree_plot, envir = .GlobalEnv)
#   
#   # Visualize contributions for each variable in the first four dimensions
#   contrib_d1 <- fviz_contrib(mca_results, "var", axes = 1, top = 50)
#   contrib_d2 <- fviz_contrib(mca_results, "var", axes = 2, top = 50)
#   contrib_d3 <- fviz_contrib(mca_results, "var", axes = 3, top = 50)
#   contrib_d4 <- fviz_contrib(mca_results, "var", axes = 4, top = 50)
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
#   # Calculate composite indices using ci_factor_mca
#   ci_mca <- ci_factor_mca(data, indic_col = 1:ncol(data), method = "ALL")
#   ci_mca_name <- paste0("ci_mca_", year, "_", index)
#   assign(ci_mca_name, ci_mca, envir = .GlobalEnv)
#   
#   ci <- data.frame(ci_factor_est = ci_mca$ci_factor_est)
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
#     mca_results = mca_results,
#     eigenvalues = eigenvalues,
#     scree_plot = scree_plot,
#     contrib_d1 = contrib_d1,
#     contrib_d2 = contrib_d2,
#     contrib_d3 = contrib_d3,
#     contrib_d4 = contrib_d4,
#     ci_mca = ci_mca,
#     ci = ci,
#     hist = hist
#   ))
# }
# 
# 
# 
# Define the function
comp_index_dd_mca <- function(data, index = "skills", year = 2014, weights = NULL) {
  # Debugging output to check the data and weights
  print(paste("Year:", year))
  print(paste("Index:", index))
  print(paste("Data dimensions:", dim(data)))
  if (!is.null(weights)) {
    print(paste("Weights length:", length(weights)))
    print(paste("Data rows:", nrow(data)))
  }
  
  # Exclude the weight column from indic_col
  indic_col <- setdiff(1:ncol(data), which(names(data) == "weight"))
  
  # Ensure weights have the same length as the number of rows in the data frame
  if (!is.null(weights) && length(weights) != nrow(data)) {
    stop("The length of weights does not match the number of rows in the data frame.")
  }
  
  # Perform MCA
  mca_results <- MCA(data[, indic_col], graph = FALSE, ncp = 8, row.w = weights)
  mca_results_name <- paste0("mca_results_", year, "_", index)
  assign(mca_results_name, mca_results, envir = .GlobalEnv)
  
  # Extract eigenvalues
  eigenvalues <- get_eigenvalue(mca_results)
  eigenvalues_name <- paste0("eigenvalues_", year, "_", index)
  assign(eigenvalues_name, eigenvalues, envir = .GlobalEnv)
  
  # Create scree plot
  scree_plot <- fviz_screeplot(mca_results, addlabels = TRUE, ylim = c(0, 40))
  scree_plot_name <- paste0("scree_plot_", year, "_", index)
  assign(scree_plot_name, scree_plot, envir = .GlobalEnv)
  
  # Visualize contributions for each variable in the first four dimensions
  contrib_d1 <- fviz_contrib(mca_results, "var", axes = 1, top = 50)
  contrib_d2 <- fviz_contrib(mca_results, "var", axes = 2, top = 50)
  contrib_d3 <- fviz_contrib(mca_results, "var", axes = 3, top = 50)
  contrib_d4 <- fviz_contrib(mca_results, "var", axes = 4, top = 50)
  
  contrib_d1_name <- paste0("contrib_", year, "_", index, "_D1")
  contrib_d2_name <- paste0("contrib_", year, "_", index, "_D2")
  contrib_d3_name <- paste0("contrib_", year, "_", index, "_D3")
  contrib_d4_name <- paste0("contrib_", year, "_", index, "_D4")
  
  assign(contrib_d1_name, contrib_d1, envir = .GlobalEnv)
  assign(contrib_d2_name, contrib_d2, envir = .GlobalEnv)
  assign(contrib_d3_name, contrib_d3, envir = .GlobalEnv)
  assign(contrib_d4_name, contrib_d4, envir = .GlobalEnv)
  
  # Calculate composite indices using ci_factor_mca
  ci_mca <- ci_factor_mca(data, indic_col = indic_col, method = "ALL", weights = weights)
  ci_mca_name <- paste0("ci_mca_", year, "_", index)
  assign(ci_mca_name, ci_mca, envir = .GlobalEnv)
  
  ci <- data.frame(ci_factor_est = ci_mca$ci_factor_est)
  colnames(ci) <- index  # Rename the column to the value of the index argument
  ci_name <- paste0("ci_", year, "_", index)
  assign(ci_name, ci, envir = .GlobalEnv)
  
  # Create histogram of the composite indices
  hist <- ggplot(ci, aes_string(x = index)) + geom_histogram()
  hist_name <- paste0("hist_", year, "_", index)
  assign(hist_name, hist, envir = .GlobalEnv)
  
  # Return the results as a list
  return(list(
    mca_results = mca_results,
    eigenvalues = eigenvalues,
    scree_plot = scree_plot,
    contrib_d1 = contrib_d1,
    contrib_d2 = contrib_d2,
    contrib_d3 = contrib_d3,
    contrib_d4 = contrib_d4,
    ci_mca = ci_mca,
    ci = ci,
    hist = hist
  ))
}


# ### testing the funtion 
# 
# ci_mcaS_14w <- ci_factor_mca(S_data_2014,
#                              indic_col =  c(1:12),
#                              method = "ALL", weights = S_data_2014$weight)
# 
# ci_S_14w <- data.frame(skills= ci_mcaS_14w$ci_factor_est)
# 
# ci_S_14w <- normalize_min_max(ci_S_14w, 
#                               col_name = "skills", 
#                               new_col_name = "skills_n")
# 
# ggplot(ci_S_14w, aes(x= skills_n)) + geom_histogram() 
# 
# summary(ci_S_14w$skills_n)
# 
# mean(ci_S_15$skills)

