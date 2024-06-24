library(tidyr)
library(dplyr)
library(ggplot2)

plot_binary_var <- function(data, variable_name, title_name, x_var= "clad4", facet_var = "clad4", ncol = 1) {
  
  # Create a mapping from short variable names to descriptive labels
  var_labels <- list(
    dom1 = "Sectors1",
    rip = "Regions",
    clad4 = "Company Size",
    Revenue_K = "Renevue",
    ateco_1 = "Sectors2"
  )
  
  
  long_var <- data %>%
    dplyr::select(Revenue_K, clad4, rip, ateco_1, all_of(variable_name)) %>%
    pivot_longer(cols = -c(Revenue_K, clad4, rip, ateco_1), 
                 names_to = "variable", values_to = "value")
  
  long_var$clad4 <- factor(long_var$clad4, 
                           levels = c("cl1", "cl2", "cl3"),
                           labels = c("Small", "Medium", "Large"))
  
  long_var$rip <- factor(long_var$rip, 
                         levels = c("ITC", "ITF", "ITG", "ITH", "ITI"),
                         labels = c("Northwest", "South", "Island", 
                                    "Northeast", "Center"))
  
  # Categorize the values
  long_var$categorized <- case_when(
    is.na(long_var$value) ~ "Missing",
    long_var$value == 1 ~ "1",
    long_var$value == 0 ~ "0",
    TRUE ~ as.character(long_var$value)
  )
  
  x_label <- var_labels[[x_var]]
  facet_label <- var_labels[[facet_var]]
  dynamic_title <- paste("Barplot of", title_name, "by", x_label, "and faceted by", facet_label)
  
  # Plotting
  ggplot(long_var, aes(x = !!sym(x_var), fill = categorized)) +
    geom_bar(position = "stack") +
    labs(title = dynamic_title, x = x_label, y = "Count", fill = "Category") +
    facet_wrap(as.formula(paste("~", facet_var)), ncol = ncol, 
               scales = "free_x", labeller = as_labeller(var_labels)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}