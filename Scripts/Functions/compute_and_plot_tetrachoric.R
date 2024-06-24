library(psych)
library(reshape2)
library(ggplot2)

# Define the function
compute_and_plot_tetrachoric <- function(data) {
  # Convert all columns to numeric
  data <- data %>%
    mutate(across(everything(), ~ as.numeric(as.character(.))))
  
  # Calculate the tetrachoric correlation
  tetra_res <- tetrachoric(data)
  rho_matrix <- tetra_res$rho
  
  # Reshape the correlation matrix for ggplot2
  rho_melt <- melt(rho_matrix)
  
  # Plot the heatmap with labels
  heatmap_plot <- ggplot(rho_melt, aes(x = Var1, y = Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1, 1), space = "Lab", 
                         name = "Tetrachoric\nCorrelation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1)) +
    coord_fixed() +
    geom_text(aes(label = round(value, 2)), color = "black", size = 3)
  
  # Return a list containing the correlation matrix and the heatmap
  return(list(correlation_matrix = as.data.frame(rho_matrix), heatmap_plot = heatmap_plot))
}