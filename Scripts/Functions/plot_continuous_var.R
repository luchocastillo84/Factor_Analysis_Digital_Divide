library(tidyr)
library(dplyr)
library(ggplot2)

plot_continuous_var <- function(data, variable_names, title_name,
                                facet_var = "clad4", x_var = "ateco_1", ncol = 1, log_transform = FALSE) {
  
  # Ensure variable_names is a vector
  if(!is.vector(variable_names)) {
    variable_names <- c(variable_names)
  }
  
  # Create a mapping from short variable names to descriptive labels
  var_labels <- list(
    dom1= "Sectors1", 
    rip = "Regions",
    clad4 = "Company Size",
    ateco_1 = "Sectors2"
  )
  
  # Melt the data
  long_var <- data %>%
    dplyr::select(all_of(c("Revenue_K", x_var, facet_var, "rip", "ateco_1", variable_names))) %>%
    pivot_longer(cols = all_of(variable_names), 
                 names_to = "variable", values_to = "value")
  
  if("clad4" %in% names(long_var)) {
    long_var <- long_var %>% 
      mutate(
        clad4 = factor(clad4, 
                       levels = c("cl1", "cl2", "cl3"),
                       labels = c("Small", "Medium", "Large"))
      )
  }
  
  if("rip" %in% names(long_var)) {
    long_var <- long_var %>% 
      mutate(
        rip = factor(rip, 
                     levels = c("ITC", "ITF", "ITG", "ITH", "ITI"),
                     labels = c("Northwest", "South", "Island", "Northeast", "Center"))
      )
  }
  
  # Apply log transformation if needed
  if(log_transform) {
    long_var$value <- log1p(long_var$value)  # Using log1p for safety against zeros
  }
  
  # Fetch descriptive label from the mapping
  x_label <- var_labels[[x_var]]
  facet_label <- var_labels[[facet_var]]
  dynamic_title <- paste("Boxplot of", title_name, "by", x_label, "and faceted by", facet_label)
  # dynamic_title <- if(is.null(title_name)) {
  #   if(length(variable_names) == 1) {
  #     paste("Distribution of", variable_names, "by", x_label)
  #   } else {
  #     paste("Distribution of Variables by", x_label)
  #   }
  # } else {
  #   title_name
  # }
  
  # Plot with facets by chosen facet_var and x-axis by chosen x_var
  ggplot(long_var, aes(x = !!sym(x_var), y = value)) +
    geom_boxplot(aes(fill = variable)) +
    labs(title = dynamic_title, 
         x = x_label, y = if(log_transform) "Log-transformed Value" else "Value", fill = "Variables") +
    facet_wrap(as.formula(paste("~", facet_var)), ncol = ncol, 
               scales = "free_x", labeller = as_labeller(var_labels)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


library(ggplot2)
library(dplyr)

plot_data_with_missing <- function(data, variable_name) {
  
  # Create an indicator for missing values
  data <- data %>%
    mutate(missing_ind = ifelse(is.na(.data[[variable_name]]), "Missing", "Observed"))
  
  # Plot
  ggplot(data, aes(x = missing_ind, y = ifelse(missing_ind == "Observed", .data[[variable_name]], NA))) +
    geom_boxplot(aes(fill = missing_ind), position = "dodge") +
    labs(title = paste("Distribution of", variable_name, "with Missing Values"),
         y = variable_name,
         x = "Status") +
    scale_fill_manual(values = c("Observed" = "blue", "Missing" = "red")) +
    theme_minimal()
}

# Test the function
# plot_data_with_missing(your_data, "your_variable_name")
