
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(here)
library(visdat)
library(ggplot2)
library(gridExtra)
library(FactoMineR)
library(factoextra)
library(forcats)
library(DescTools)
library(fastDummies)
library(corrplot)
library(Compind)
library(plm)
library(stargazer)
library(caret)
library(lavaan)
library(Matrix)
library(flexmix)
library(Compind)
library(moments)
library(networkD3)
library(circlize)
library(broom)
library(ggpubr)
library(stargazer)

#### 0. Loading the environment and data ####

load(here("Scripts", "Environments",  "size_rev_results_weighted.RData"))


load(here("Data", "Processed", "CI_DD_comb_rev_weighted.rda"))
load(here("Data", "Processed", "CI_DD_comb_rev_weightedS11.rda")) ### Presented in Urbino

load(here("Scripts", "Environments",  "WeightingSchemes.RData"))


ci_dd_combined_rev <- ci_dd_combined_weighted
summary(ci_dd_combined_rev)

#### 0.1. Adding the quadrant varaible ####
# Add a new column for quadrants
combined_data_harmonized <- ci_dd_combined_rev %>%
  mutate(
    quadrant = case_when(
      access_n > 0.5 & secDD_n > 0.5 ~ "Q1 (H_A, H_S&U) )",
      access_n <= 0.5 & secDD_n > 0.5 ~ "Q2 (L_A, H_S&U)",
      access_n <= 0.5 & secDD_n <= 0.5 ~ "Q3 (L_A, L_S&U)",
      access_n > 0.5 & secDD_n <= 0.5 ~ "Q4 (H_A, L_S&U)"
    )
  )


summary(combined_data_harmonized)
combined_data_harmonized$year <- as.numeric(as.character(combined_data_harmonized$year))
# combined_data_harmonized$mac_sec <- as.factor(combined_data_harmonized$mac_sec)
combined_data_harmonized$quadrant <- as.factor(combined_data_harmonized$quadrant)
summary(combined_data_harmonized)


#### 1. Summary Stats for Both Indices ####

summary_table <- create_summary_table(ci_dd_combined_rev)

# # Calculate summary statistics for access_n and secDD_n by year and size 
# summary_stats <- combined_data_harmonized %>%
#   group_by(year) %>%  # Ensure grouping by both year and size if needed
#   summarise(
#     across(c(access_n, secDD_n), 
#            list(mean = ~round(mean(.), 3),
#                 sd = ~round(sd(.), 3),
#                 q1 = ~round(quantile(., 0.25), 3),
#                 median = ~round(median(.), 3),
#                 q3 = ~round(quantile(., 0.75), 3)), 
#            .names = "{.col}_{.fn}")) %>%
#   ungroup()

#### 2. Individual representation of the histograms ####

# Histogram for access_n
p1_access <- ggplot(combined_data_harmonized, aes(x = access_n)) +
  geom_histogram() +
  ggtitle("Histogram of Access") +
  xlab("Access") +
  ylab("Frequency") +
  facet_wrap(~ year, scales = "fixed")

p1_access

# Histogram for skills_n
p2_skills <- ggplot(combined_data_harmonized, aes(x = skills_n)) +
  geom_histogram() +
  ggtitle("Histogram of Skills") +
  xlab("Skills") +
  ylab("Frequency") +
  facet_wrap(~ year, scales = "fixed")
p2_skills

# Histogram for usage_n
p2_usage <- ggplot(combined_data_harmonized, aes(x = usage_n)) +
  geom_histogram() +
  ggtitle("Histogram of Usage") +
  xlab("Usage") +
  ylab("Frequency") +
  facet_wrap(~ year, scales = "fixed")
p2_usage


# Histogram for usage_n
p2_secDD <- ggplot(combined_data_harmonized, aes(x = secDD_n)) +
  geom_histogram() +
  ggtitle("Histogram of Skills & Usage") +
  xlab("Skills & Usage") +
  ylab("Frequency") +
  facet_wrap(~ year, scales = "fixed")
p2_secDD



#### 2.1 Panel of histograms ####

# Reshaping the data for faceting
data_long <- combined_data_harmonized %>%
  pivot_longer(cols = c(access_n, skills_n, usage_n, secDD_n), names_to = "variable", values_to = "value")

# Plotting the faceted histogram
p_combined <- ggplot(data_long, aes(x = value)) +
  geom_histogram() +
  facet_wrap(~variable, scales = "free_y") +  # Freeing the y-axis scales for each facet
  ggtitle("Histograms of Indices") +
  xlab("Value") +
  ylab("Frequency")
p_combined


#### 3. LINE PLOTS ####

#### 3.1. Yearly trend of Access, Skills and Usage ####

line_plot_by_year <- yearly_stats_and_plot(combined_data_harmonized, 
                                           line_vars = c("access_n", "skills_n", "usage_n"), 
                                           statistic = "median")

line_plot_by_year



#### 3.1. SIZE_Rev Yearly lineplots by size by revenue ####

# Generate a line plot with access_n and skills_n
line_plot_by_size <- yearly_stats_and_plot(combined_data_harmonized, 
                                           line_vars = c("access_n", "skills_n", "usage_n"), 
                                           group_var = "size_rev", 
                                           statistic = "median")

line_plot_by_size

#### 3.2. SMEs_Rev Yearly lineplots by size by revenue ####

# Generate a line plot with access_n and skills_n
line_plot_by_smeR <- yearly_stats_and_plot(combined_data_harmonized, 
                                           line_vars = c("access_n", "skills_n", "usage_n"), 
                                           group_var = "sme_rev", 
                                           statistic = "median",
                                           size = "big",
                                           angle = F)

line_plot_by_smeR


#### 3.3. MAC_SEC Yearly lineplots by size by revenue ####

# Generate a line plot with access_n and skills_n
line_plot_by_mac <- yearly_stats_and_plot(combined_data_harmonized, 
                                          line_vars = c("access_n", "skills_n", "usage_n"), 
                                          group_var = "mac_sec", 
                                          statistic = "median")

line_plot_by_mac

#### 3.4. MAC_SEC Yearly lineplots by size by revenue ####

# Generate a line plot with access_n and skills_n
line_plot_by_ateco <- yearly_stats_and_plot(combined_data_harmonized, 
                                            line_vars = c("access_n", "skills_n", "usage_n"), 
                                            group_var = "sec_name", 
                                            statistic = "median")

line_plot_by_ateco


#### 3.5. REGION Yearly lineplots by size by revenue ####

# Generate a line plot with access_n and skills_n
line_plot_by_region <- yearly_stats_and_plot(combined_data_harmonized, 
                                             line_vars = c("access_n", "skills_n", "usage_n"), 
                                             group_var = "region", 
                                             statistic = "median")

line_plot_by_region






