# library(tidyr)
# library(dplyr)
# library(ggplot2)
# 
# plot_categorical_var <- function(data, variable_name, title_name, x_var="ateco_1", facet_var="clad4", ncol=1) {
#   
#   # Required columns check
#   required_cols <- c("year", x_var, facet_var, "rip", "ateco_1", variable_name)
#   missing_cols <- setdiff(required_cols, names(data))
#   if(length(missing_cols) > 0) {
#     stop("The following columns are missing from the data: ", paste(missing_cols, collapse=", "))
#   }
#   
#   # Prepare data for plotting
#   long_data <- data %>%
#     dplyr::select(all_of(required_cols)) %>%
#     pivot_longer(cols = -c("year", x_var, facet_var, "rip", "ateco_1"), names_to = "variable", values_to = "value")
#   
#   # Conditionally mutate columns if they exist
#   if("clad4" %in% names(long_data)) {
#     long_data <- long_data %>% 
#       mutate(
#         clad4 = factor(clad4, 
#                        levels = c("cl1", "cl2", "cl3"),
#                        labels = c("Small", "Medium", "Large"))
#       )
#   }
#   if("rip" %in% names(long_data)) {
#     long_data <- long_data %>% 
#       mutate(
#         rip = factor(rip, 
#                      levels = c("ITC", "ITF", "ITG", "ITH", "ITI"),
#                      labels = c("Northwest", "South", "Island", "Northeast", "Center"))
#       )
#   }
#   
#   long_data <- long_data %>% 
#     mutate(categorized = case_when(
#       is.na(value) ~ "Missing",
#       TRUE ~ as.character(value)
#     ))
#   
#   # Define variable labels
#   var_labels <- list(
#     rip = "Regions",
#     clad4 = "Company Size",
#     year = "Year",
#     ateco_1 = "Sectors2"
#   )
#   
#   x_label <- var_labels[[x_var]]
#   facet_label <- var_labels[[facet_var]]
#   dynamic_title <- paste("Barplot of", title_name, "by", x_label, "and faceted by", facet_label)
#   
#   # Plot
#   ggplot(long_data, aes(x = !!sym(x_var), fill = categorized)) +
#     geom_bar(position = "stack") +
#     labs(title = dynamic_title, x = x_label, y = "Count", fill = "Category") +
#     facet_wrap(as.formula(paste("~", facet_var)), ncol = ncol, 
#                scales = "free_x", labeller = as_labeller(var_labels)) +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1))
# }




plot_categorical_var <- function(data, variable_name, title_name, x_var, facet_var, ncol = 1) {
  
  # Check if the provided variables exist in the dataset
  required_cols <- c("year", x_var, facet_var, variable_name)
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("The following columns are missing from the data: ", paste(missing_cols, collapse = ", "))
  }
  
  # Prepare data for plotting
  long_data <- data %>%
    dplyr::select(all_of(required_cols)) %>%
    pivot_longer(cols = -c("year", x_var, facet_var), names_to = "variable", values_to = "value")
  
  # Conditionally mutate columns if they exist and are categorical
  if (x_var %in% names(long_data) && is.factor(data[[x_var]])) {
    long_data <- long_data %>%
      mutate(
        !!sym(x_var) := factor(!!sym(x_var))
      )
  }
  
  if (facet_var %in% names(long_data) && is.factor(data[[facet_var]])) {
    long_data <- long_data %>%
      mutate(
        !!sym(facet_var) := factor(!!sym(facet_var))
      )
  }
  
  long_data <- long_data %>%
    mutate(categorized = case_when(
      is.na(value) ~ "Missing",
      TRUE ~ as.character(value)
    ))
  
  # Dynamic labels for x and facet variables
  x_label <- x_var
  facet_label <- facet_var
  dynamic_title <- paste("Barplot of", title_name, "by", x_label, "and faceted by", facet_label)
  
  # Plot
  p <- ggplot(long_data, aes(x = !!sym(x_var), fill = categorized)) +
    geom_bar(position = "stack") +
    labs(title = dynamic_title, x = x_label, y = "Count", fill = "Category") +
    facet_wrap(as.formula(paste("~", facet_var)), ncol = ncol, scales = "free_x") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p)
}
