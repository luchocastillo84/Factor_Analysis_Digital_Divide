library(dplyr)
library(ggplot2)
library(stringr)

calculate_weights <- function(data, weight_scheme, weight_column, ci_index_dd = c("access_n", "secDD_n")) {
  
  # Changing type of variables and cases 
  data$year <- as.numeric(as.character(data$year))
  weight_scheme$size <- tolower(weight_scheme$size)
  data$size <- tolower(data$size)
  
  # Join data based on 'year' and 'size', select relevant columns
  ci_dd_w <- inner_join(data, weight_scheme, by = c("year", "size")) %>%
    dplyr::select(year, region, macro_sector, ateco_1, sec_name, size, access, access_n, secDD, secDD_n, 
           all_of(weight_column))
  
  # Create new columns for weighted indices
  ci_dd_w$windexA <- ci_dd_w[[ci_index_dd[1]]] * ci_dd_w[[weight_column]]
  ci_dd_w$windexSU <- ci_dd_w[[ci_index_dd[2]]] * ci_dd_w[[weight_column]]
  
  # Normalize if not within range 0 to 1
  ci_dd_w <- ci_dd_w %>%
    mutate(
      windexA = if(any(windexA < 0 | windexA > 1, na.rm = TRUE)) {
        (windexA - min(windexA, na.rm = TRUE)) / (max(windexA, na.rm = TRUE) - min(windexA, na.rm = TRUE))
      } else {
        windexA
      },
      windexSU = if(any(windexSU < 0 | windexSU > 1, na.rm = TRUE)) {
        (windexSU - min(windexSU, na.rm = TRUE)) / (max(windexSU, na.rm = TRUE) - min(windexSU, na.rm = TRUE))
      } else {
        windexSU
      }
    )
  
  ci_dd_w$size <-  factor(ci_dd_w$size, levels = c("small", "medium", "large"))
  
  # Summary statistics by year
  summary_stats <- ci_dd_w %>%
    group_by(year, size) %>%
    summarise(mean_windexA = mean(windexA, na.rm = TRUE),
              median_windexA = median(windexA, na.rm = TRUE),
              mean_windexSU = mean(windexSU, na.rm = TRUE),
              median_windexSU = median(windexSU, na.rm = TRUE),
              .groups = 'drop'  # Dropping the groups after summarisation
    )
  
  # Histograms
  p1 <- ggplot(ci_dd_w, aes(x = windexA)) +
    geom_histogram() +
    ggtitle("Histogram of windexA") +
    xlab("windexA") + ylab("Frequency")
  
  p2 <- ggplot(ci_dd_w, aes(x = windexSU)) +
    geom_histogram() +
    ggtitle("Histogram of windexSU") +
    xlab("windexSU") + ylab("Frequency")
  
  # Calculate yearly medians for windexA and windexSU by size
  yearly_data <- ci_dd_w %>%
    group_by(year) %>%
    summarise(
      median_windexA = median(windexA, na.rm = TRUE),
      median_windexSU = median(windexSU, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Line plot with weighted indices
  line_plot <- ggplot(yearly_data, aes(x = year)) +
    geom_line(aes(y = median_windexA, color = "Access"), linetype = "solid", linewidth = 1) +
    geom_point(aes(y = median_windexA), color = "blue", size = 3) +
    geom_line(aes(y = median_windexSU, color = "Skill & Usage"), linetype = "solid", size = 1) +
    geom_point(aes(y = median_windexSU), color = "red", size = 3) +
    labs(title = "Yearly Trends in Access and Second-Level Digital Divide",
         x = "Year", y = "Normalized Index Value",
         color = "Legend Title") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
    scale_x_continuous(breaks = summary_stats$year) +
    scale_color_manual(values = c("blue", "red"), name = "Median")
  
  yearly_size_data <- ci_dd_w %>%
    group_by(year, size) %>%
    summarise(
      median_windexA = median(windexA, na.rm = TRUE),
      median_windexSU = median(windexSU, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Faceted Line plots by size
  line_plot_facetedSize <- ggplot(yearly_size_data, aes(x = year)) +
    geom_line(aes(y = median_windexA, color = "Access"), size = 1) +
    geom_point(aes(y = median_windexA), color = "blue", size = 3) +
    geom_line(aes(y = median_windexSU, color = "Skills & Usage"), size = 1) +
    geom_point(aes(y = median_windexSU), color = "red", size = 3) +
    labs(title = "Yearly Trends in Access vs Usage & Skills",
         x = "Year", y = "Median Weighted Index Value",
         color = "Index") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = c("Access" = "blue", `Skills & Usage` = "red")) +
    facet_wrap(~ size, scales = "fixed")  # Faceting by size
  
  
  # Scatter plot  by size
  scatter_plot <- ggplot(ci_dd_w, aes(x = windexA, y = windexSU, color = size)) +
    geom_point() +
    #facet_wrap(~ year) +  # Facet by year to create separate plots for each year
    geom_vline(xintercept = 0.5, linetype = "dashed") +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    labs(color = "Size") +
    theme_minimal() +
    ggtitle("Weighted Clusters by Size for all Years")
  
  ##  Sacetter Plot: Clusters by Size by Year
  scatter_plot_faceted <- ggplot(ci_dd_w, aes(x = windexA, y = windexSU, color = size)) +
    geom_point() +
    facet_wrap(~ year) +  # Facet by year to create separate plots for each year
    geom_vline(xintercept = 0.5, linetype = "dashed") +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    labs(color = "Size") +
    theme_minimal() +
    labs(title = "Clusters by Size Over Years",
         x = "Access", y = "Skills & Usage",
         color = "Firm Size") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Customize x-axis text size
      axis.text.y = element_text(size = 12),  # Customize y-axis text size
      axis.title.x = element_text(size = 15),  # Customize x-axis title size
      axis.title.y = element_text(size = 15),  # Customize y-axis title size
      plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
      strip.text = element_text(size = 14),  # Customize facet titles
      legend.title = element_text(size = 15),  # Customize legend title size
      legend.text = element_text(size = 14)  # Customize legend text size
    ) 
  
  
  
  #### Regional line plots
  ## data preparation 
  yearly_data_reg <- ci_dd_w %>%
    group_by(year, region) %>%
    summarise(
      median_windexA = median(windexA, na.rm = TRUE),
      median_windexSU = median(windexSU, na.rm = TRUE),
      .groups = 'drop'
    )
  
  ### Line plots 
  
  line_plot_facetedReg <- ggplot(yearly_data_reg, aes(x = year)) +
    geom_line(aes(y = median_windexA, color = "Access"), size = 1) +
    geom_point(aes(y = median_windexA), color = "blue", size = 3) +
    geom_line(aes(y = median_windexSU, color = "Skills & Usage"), size = 1) +
    geom_point(aes(y = median_windexSU), color = "red", size = 3) +
    geom_text(aes(y = median_windexA, label = sprintf("%.2f", median_windexA)), color = "blue", vjust = -0.5, hjust = 0.5, size = 3) +  # Add labels for windexSU
    geom_text(aes(y = median_windexSU, label = sprintf("%.2f", median_windexSU)), color = "red", vjust = 1.5, hjust = 0.5, size = 3) +  # Add labels for wsecDD_N
    labs(title = "Yearly Trends in Access vs Usage & Skills by Region",
         x = "Year", y = "Median Normalized Index Value",
         color = "Index") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = c(Access = "blue", `Skills & Usage` = "red")) +
    facet_wrap(~ region, scales = "fixed") + # Faceting by size, allowing y-axes to vary
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Customize x-axis text size
      axis.text.y = element_text(size = 12),  # Customize y-axis text size
      axis.title.x = element_text(size = 15),  # Customize x-axis title size
      axis.title.y = element_text(size = 15),  # Customize y-axis title size
      plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
      strip.text = element_text(size = 14),  # Customize facet titles
      legend.title = element_text(size = 15),  # Customize legend title size
      legend.text = element_text(size = 14)  # Customize legend text size
    )
  
  ### Sectoral ####
  yearly_data_mac <- ci_dd_w %>%
    group_by(year, macro_sector) %>%
    summarise(
      median_windexA = median(windexA, na.rm = TRUE),
      median_windexSU = median(windexSU, na.rm = TRUE),
      .groups = 'drop'
    )
  
  ### Line plots 
  
  line_plot_facetedMac <- ggplot(yearly_data_mac, aes(x = year)) +
    geom_line(aes(y = median_windexA, color = "Access"), size = 1) +
    geom_point(aes(y = median_windexA), color = "blue", size = 3) +
    geom_line(aes(y = median_windexSU, color = "Skills & Usage"), size = 1) +
    geom_point(aes(y = median_windexSU), color = "red", size = 3) +
    geom_text(aes(y = median_windexA, label = sprintf("%.2f", median_windexA)), color = "blue", vjust = -0.5, hjust = 0.5, size = 3) +
    geom_text(aes(y = median_windexSU, label = sprintf("%.2f", median_windexSU)), color = "red", vjust = 1.5, hjust = 0.5, size = 3) +
    labs(title = "Yearly Trends in Access vs Usage & Skills by Macro Sectors",
         x = "Year", y = "Median Normalized Index Value",
         color = "Index") +
    theme_minimal() +
    scale_color_manual(values = c(Access = "blue", `Skills & Usage` = "red")) +
    facet_wrap(~ macro_sector, scales = "fixed") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  # Customize x-axis text size
      axis.text.y = element_text(size = 12),  # Customize y-axis text size
      axis.title.x = element_text(size = 15),  # Customize x-axis title size
      axis.title.y = element_text(size = 15),  # Customize y-axis title size
      plot.title = element_text(size = 16, face = "bold"),  # Customize plot title
      strip.text = element_text(size = 14),  # Customize facet titles
      legend.title = element_text(size = 14),  # Customize legend title size
      legend.text = element_text(size = 14)  # Customize legend text size
    )
  

  
  
  
  # Return results
  list(ci_dd_w = ci_dd_w, summary_stats = summary_stats, yearly_data = yearly_data, yearly_size_data = yearly_size_data, histogram_windexA = p1, 
       histogram_windexSU = p2, yearly_trends_plot = line_plot, line_plot_facetedSize = line_plot_facetedSize, 
       scatter_plot = scatter_plot, scatter_plot_faceted = scatter_plot_faceted, line_plot_facetedReg = line_plot_facetedReg,
       line_plot_facetedMac = line_plot_facetedMac)
}

# Example usage
# data <- read.csv("your_data.csv")
# weight_scheme <- read.csv("your_weight_scheme.csv")
# results <- calculate_weights(data, weight_scheme, "chosen_weight_column")
