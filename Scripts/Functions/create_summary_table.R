library(dplyr)
library(tidyr)

# Function to create summary table in the desired format
create_summary_table <- function(data) {
  summary_table <- data %>%
    # Reshape data to long format and rename indices
    pivot_longer(cols = c(access_n, skills_n, usage_n), 
                 names_to = "Index", 
                 values_to = "Value") %>%
    mutate(Index = recode(Index,
                          "access_n" = "Digital_Access",
                          "skills_n" = "Digital_Skills",
                          "usage_n" = "Digital_Usage")) %>%
    # Calculate summary statistics
    group_by(year, Index) %>%
    summarise(
      Mean = round(mean(Value, na.rm = TRUE), 2),
      Median = round(median(Value, na.rm = TRUE), 2),
      `Std Dev` = round(sd(Value, na.rm = TRUE), 2),
      .groups = 'drop'  # Drop the grouping after summarising
    ) %>%
    # Reshape the data to long format again to get each year repeated for each statistic
    pivot_longer(cols = c(Mean, Median, `Std Dev`), 
                 names_to = "Statistic", 
                 values_to = "Value") %>%
    pivot_wider(names_from = Statistic, values_from = Value) %>%
    arrange(year, Index)
  
  return(summary_table)
}
# load(here("Data", "Processed", "CI_DD_comb_rev.rda"))
# 
# # Example usage
# summary_table <- create_summary_table(ci_dd_combined)
