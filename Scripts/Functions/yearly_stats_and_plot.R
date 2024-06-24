# library(dplyr)
# library(ggplot2)
# 
# # Define the integrated function for line plots
# yearly_stats_and_plot <- function(data, line_vars, group_var = "size_rev", statistic = "median") {
#   # Ensure the group_var is a character vector
#   if (!is.character(group_var)) {
#     stop("group_var must be a character vector.")
#   }
#   
#   # Ensure the statistic is either "median" or "mean"
#   if (!statistic %in% c("median", "mean")) {
#     stop("statistic must be either 'median' or 'mean'.")
#   }
#   
#   # Ensure line_vars is provided
#   if (is.null(line_vars)) {
#     stop("line_vars must be provided for line plot.")
#   }
#   
#   # Define the summarise function dynamically
#   stat_fun <- match.fun(statistic)
#   
#   # Define column names based on the chosen statistic
#   access_col <- paste0(statistic, "_access_n")
#   skills_col <- paste0(statistic, "_skills_n")
#   usage_col <- paste0(statistic, "_usage_n")
#   secDD_col <- paste0(statistic, "_secDD_n")
#   
#   # Group by year and the selected group_var
#   grouped_data <- data %>%
#     group_by(across(all_of(c("year", group_var)))) %>%
#     summarise(
#       !!access_col := stat_fun(access_n, na.rm = TRUE),
#       !!skills_col := stat_fun(skills_n, na.rm = TRUE),
#       !!usage_col := stat_fun(usage_n, na.rm = TRUE),
#       !!secDD_col := stat_fun(secDD_n, na.rm = TRUE),
#       .groups = 'drop'
#     )
#   
#   # Define friendly names for the legend and title
#   friendly_names <- c(access_n = "Access", skills_n = "Skills", usage_n = "Usage", secDD_n = "Skills & Usage")
#   
#   # Dynamically set the legend labels
#   legend_labels <- friendly_names[line_vars]
#   
#   # Define the column names based on the statistic
#   var_cols <- paste0(statistic, "_", line_vars)
#   
#   # Define the plot dynamically
#   p <- ggplot(grouped_data, aes(x = year)) +
#     labs(title = paste("Yearly Trends in", paste(legend_labels, collapse = " vs ")),
#          x = "Year", y = "Normalized Index Value",
#          color = "Index") +
#     theme_minimal() +
#     scale_color_manual(values = setNames(c("blue", "darkgreen", "red", "purple"), legend_labels)) +
#     facet_wrap(as.formula(paste("~", group_var)), scales = "fixed") +
#     theme(legend.position = "top",
#           axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
#           axis.text.y = element_text(size = 12),
#           axis.title.x = element_text(size = 15),
#           axis.title.y = element_text(size = 15),
#           plot.title = element_text(size = 16, face = "bold"),
#           strip.text = element_text(size = 14),
#           legend.title = element_text(size = 14),
#           legend.text = element_text(size = 14))
#   
#   # Add geom layers for line plot
#   for (var_col in var_cols) {
#     var_label <- friendly_names[gsub(paste0(statistic, "_"), "", var_col)]
#     p <- p +
#       geom_line(aes_string(y = var_col, color = paste0("'", var_label, "'"), group = 1), size = 1) +
#       geom_point(aes_string(y = var_col, color = paste0("'", var_label, "'")), size = 3) +
#       geom_text(aes_string(y = var_col, label = paste0("sprintf('%.2f', ", var_col, ")")), 
#                 vjust = -0.5, hjust = 0.5, size = 3)
#   }
#   
#   return(p)
# }


# Define the integrated function for line plots
yearly_stats_and_plot <- function(data, line_vars, group_var = NULL, statistic = "median", size = "small", angle = FALSE) {
  # Ensure the statistic is either "median" or "mean"
  if (!statistic %in% c("median", "mean")) {
    stop("statistic must be either 'median' or 'mean'.")
  }
  
  # Ensure line_vars is provided
  if (is.null(line_vars)) {
    stop("line_vars must be provided for line plot.")
  }
  
  # Define the summarise function dynamically
  stat_fun <- match.fun(statistic)
  
  # Define column names based on the chosen statistic
  access_col <- paste0(statistic, "_access_n")
  skills_col <- paste0(statistic, "_skills_n")
  usage_col <- paste0(statistic, "_usage_n")
  secDD_col <- paste0(statistic, "_secDD_n")
  
  # Group by year and the selected group_var if provided
  if (!is.null(group_var)) {
    grouped_data <- data %>%
      group_by(across(all_of(c("year", group_var)))) %>%
      summarise(
        !!access_col := stat_fun(access_n, na.rm = TRUE),
        !!skills_col := stat_fun(skills_n, na.rm = TRUE),
        !!usage_col := stat_fun(usage_n, na.rm = TRUE),
        !!secDD_col := stat_fun(secDD_n, na.rm = TRUE),
        .groups = 'drop'
      )
  } else {
    grouped_data <- data %>%
      group_by(year) %>%
      summarise(
        !!access_col := stat_fun(access_n, na.rm = TRUE),
        !!skills_col := stat_fun(skills_n, na.rm = TRUE),
        !!usage_col := stat_fun(usage_n, na.rm = TRUE),
        !!secDD_col := stat_fun(secDD_n, na.rm = TRUE),
        .groups = 'drop'
      )
  }
  
  # Define friendly names for the legend and title
  friendly_names <- c(access_n = "Access", skills_n = "Skills", usage_n = "Usage", secDD_n = "Skills & Usage")
  
  # Dynamically set the legend labels
  legend_labels <- friendly_names[line_vars]
  
  # Define the column names based on the statistic
  var_cols <- paste0(statistic, "_", line_vars)
  
  # Set theme sizes based on the 'size' argument
  if (size == "big") {
    text_size <- 16
    title_size <- 20
    legend_size <- 16
    strip_size <- 18
    lab_size <- 4
  } else {
    text_size <- 12
    title_size <- 16
    legend_size <- 14
    strip_size <- 14
    lab_size <- 3
  }
  
  # Set angle based on the 'angle' argument
  x_axis_angle <- if (angle) 45 else 0
  
  # Conditional labeller logic
  labeller_val <- if (!is.null(group_var) && group_var %in% c("sme_rev", "sme_emp")) {
    labeller(.cols = c(sme = "SMEs", large = "Large"))
  } else if (!is.null(group_var) && group_var %in% c("size_rev", "size_emp")) {
    labeller(.cols = c(small = "Small", medium = "Medium", large = "Large"))
  } else if (!is.null(group_var) && group_var %in% c("region", "sec_name")) {
    labeller(.cols = label_value)
  } else if (!is.null(group_var) && group_var == "mac_sec") {
    labeller(mac_sec = c(
      Indus = "Industrial",
      Commer = "Commercial",
      Serv = "Services",
      `Cre_Indus_&_ICT` = "Creative Industries & ICTs"
    ))
  } else {
    NULL
  }
  
  # Define the plot dynamically
  p <- ggplot(grouped_data, aes(x = year)) +
    labs(title = paste("Yearly Trends in", paste(legend_labels, collapse = " vs ")),
         x = "Year", y = "Normalized Index Value",
         color = "Index") +
    theme_minimal() +
    scale_color_manual(values = setNames(c("blue", "darkgreen", "red", "purple"), legend_labels)) +
    theme(
      legend.position = "top",
      axis.text.x = element_text(angle = x_axis_angle, vjust = if (angle) 1 else 0.5, size = text_size),
      axis.text.y = element_text(size = text_size),
      axis.title.x = element_text(size = title_size),
      axis.title.y = element_text(size = title_size),
      plot.title = element_text(size = title_size, face = "bold"),
      strip.text = element_text(size = strip_size),
      legend.title = element_text(size = legend_size),
      legend.text = element_text(size = legend_size)
    )
  
  # Add facet_wrap conditionally
  if (!is.null(group_var)) {
    p <- p + facet_wrap(as.formula(paste("~", group_var)), scales = "fixed", labeller = labeller_val)
  }
  
  # Add geom layers for line plot
  for (var_col in var_cols) {
    var_label <- friendly_names[gsub(paste0(statistic, "_"), "", var_col)]
    p <- p +
      geom_line(aes_string(y = var_col, color = paste0("'", var_label, "'"), group = 1), linewidth = 1) +
      geom_point(aes_string(y = var_col, color = paste0("'", var_label, "'")), size = 3) +
      geom_text(aes_string(y = var_col, label = paste0("sprintf('%.2f', ", var_col, ")")), 
                vjust = -0.5, hjust = 0.5, size = lab_size)
  }
  
  return(p)
}
