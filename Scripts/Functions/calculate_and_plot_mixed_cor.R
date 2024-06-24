library(psych)
library(reshape2)
library(ggplot2)

# Define the function
calculate_and_plot_mixed_cor<- function(data, continuous_indices, binary_indices) {
  # Subset the column names based on the provided indices
  continuous_vars <- colnames(data)[continuous_indices]
  binary_vars <- colnames(data)[binary_indices]
  
  # Ensure continuous variables are numeric
  data[continuous_vars] <- lapply(data[continuous_vars], as.numeric)
  
  # Ensure binary variables are numeric (0 and 1)
  data[binary_vars] <- lapply(data[binary_vars], function(x) as.numeric(as.character(x)))
  
  # Initialize a correlation matrix
  n_vars <- length(continuous_vars) + length(binary_vars)
  cor_matrix <- matrix(NA, nrow = n_vars, ncol = n_vars,
                       dimnames = list(c(continuous_vars, binary_vars),
                                       c(continuous_vars, binary_vars)))
  
  # Calculate correlations for continuous-continuous pairs
  for (i in seq_along(continuous_vars)) {
    for (j in i:seq_along(continuous_vars)) {
      cor_matrix[continuous_vars[i], continuous_vars[j]] <- 
        cor_matrix[continuous_vars[j], continuous_vars[i]] <- 
        cor(data[[continuous_vars[i]]], data[[continuous_vars[j]]], use = "complete.obs")
    }
  }
  
  # Calculate correlations for binary-binary pairs
  for (i in seq_along(binary_vars)) {
    for (j in i:seq_along(binary_vars)) {
      if (i != j) {
        cor_matrix[binary_vars[i], binary_vars[j]] <- 
          cor_matrix[binary_vars[j], binary_vars[i]] <- 
          tetrachoric(data[, c(binary_vars[i], binary_vars[j])])$rho[1, 2]
      } else {
        cor_matrix[binary_vars[i], binary_vars[j]] <- 1
      }
    }
  }
  
  # Calculate correlations for continuous-binary pairs
  for (i in seq_along(continuous_vars)) {
    for (j in seq_along(binary_vars)) {
      cor_matrix[continuous_vars[i], binary_vars[j]] <- 
        cor_matrix[binary_vars[j], continuous_vars[i]] <- 
        biserial(data[[continuous_vars[i]]], data[[binary_vars[j]]])
    }
  }
  
  # Convert correlation matrix to data frame for plotting
  cor_df <- melt(cor_matrix)
  
  # Plot heatmap
  heatmap_plot <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab", 
                         name = "Correlation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1)) +
    coord_fixed() +
    geom_text(aes(label = round(value, 2)), color = "black", size = 3)
  
  # Return the correlation matrix and the heatmap plot
  return(list(correlation_matrix = as.data.frame(cor_matrix), heatmap_plot = heatmap_plot))
}
