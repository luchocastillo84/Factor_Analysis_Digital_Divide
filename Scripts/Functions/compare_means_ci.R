# Function to compare means and confidence intervals
compare_means_ci <- function(data, continuous_vars = NULL, binary_vars = NULL) {
  summary_stats <- data %>%
    group_by(year) %>%
    summarise(
      # Continuous variables
      across(all_of(continuous_vars), list(
        mean = ~mean(., na.rm = TRUE),
        sd = ~sd(., na.rm = TRUE),
        n = ~sum(!is.na(.))
      ), .names = "{.col}_{.fn}"),
      # Binary variables
      across(all_of(binary_vars), list(
        prop = ~mean(., na.rm = TRUE),
        n = ~sum(!is.na(.))
      ), .names = "{.col}_{.fn}")
    ) %>%
    mutate(
      # Confidence intervals for continuous variables
      across(starts_with("mean_"), ~ . - qt(1 - (0.05 / 2), n - 1) * (get(sub("mean", "sd", cur_column())) / sqrt(n)), .names = "ci_lower_{.col}"),
      across(starts_with("mean_"), ~ . + qt(1 - (0.05 / 2), n - 1) * (get(sub("mean", "sd", cur_column())) / sqrt(n)), .names = "ci_upper_{.col}"),
      # Confidence intervals for binary variables
      across(starts_with("prop_"), ~ binom.test(x = . * n, n = n)$conf.int[1], .names = "ci_lower_{.col}"),
      across(starts_with("prop_"), ~ binom.test(x = . * n, n = n)$conf.int[2], .names = "ci_upper_{.col}")
    )
  
  return(summary_stats)
}