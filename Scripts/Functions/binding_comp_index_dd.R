library(dplyr)
library(ggplot2)


# # Assume the normalize_min_max and ci_generalized_mean1 functions are defined elsewhere
# # If not, you need to define them before using this function
# 
# # Define the function
# binding_comp_index_dd <- function(data, year) {
#   # Step 1: Filter and select contextual variables for the specified year
#   context_vars <- data %>% filter(year == !!year) %>% dplyr::select(29:40)
#   
#   # Step 2: Combine context variables with composite indices
#   indices <- cbind(context_vars, 
#                    get(paste0("ci_", year, "_access")), 
#                    get(paste0("ci_", year, "_skills")), 
#                    get(paste0("ci_", year, "_usage")))
#   
#   # Step 3: Normalize the 'access' column
#   indices <- normalize_min_max(indices, col_name = "access", new_col_name = "access_n")
#   hist_access <- ggplot(indices, aes(x = access_n)) + geom_histogram() 
#   
#   # Step 4: Normalize the 'skills' column
#   indices <- normalize_min_max(indices, col_name = "skills", new_col_name = "skills_n")
#   hist_skills <- ggplot(indices, aes(x = skills_n)) + geom_histogram() 
#   
#   # Step 5: Normalize the 'usage' column
#   indices <- normalize_min_max(indices, col_name = "usage", new_col_name = "usage_n")
#   hist_usage <- ggplot(indices, aes(x = usage_n)) + geom_histogram() 
#   
#   # Step 6: Aggregate 'skills' and 'usage' according to theory
#   ci_dd_secdd <- ci_generalized_mean1(indices, indic_col = c("skills_n", "usage_n"), p = 0.5, na.rm = TRUE)
#   
#   # Step 7: Combine the aggregated data with indices
#   ci_dd_SU <- data.frame(secDD = ci_dd_secdd$ci_generalized_mean_est)
#   indices <- cbind(indices, ci_dd_SU)
#   
#   # Step 8: Normalize the 'secDD' column
#   indices <- normalize_min_max(indices, col_name = "secDD", new_col_name = "secDD_n")
#   hist_secDD <- ggplot(indices, aes(x = secDD_n)) + geom_histogram() + facet_wrap(~ size_rev)
#   
#   # Additional plots
#   hist_access_size <- ggplot(indices, aes(x = access_n)) + geom_histogram() + facet_wrap(~ size_rev)
#   hist_skills_size <- ggplot(indices, aes(x = skills_n)) + geom_histogram() + facet_wrap(~ size_rev)
#   hist_usage_size <- ggplot(indices, aes(x = usage_n)) + geom_histogram() + facet_wrap(~ size_rev)
#   scatter_access_secDD <- ggplot(indices, aes(x = access_n, y = secDD_n, color = size_rev)) + geom_point() + facet_wrap(~ size_rev)
#   
#   # Store plots in a list
#   plots <- list(
#     hist_access = hist_access,
#     hist_skills = hist_skills,
#     hist_usage = hist_usage,
#     hist_secDD = hist_secDD,
#     hist_access_size = hist_access_size,
#     hist_skills_size = hist_skills_size,
#     hist_usage_size = hist_usage_size,
#     scatter_access_secDD = scatter_access_secDD
#   )
#   
#   return(list(indices = indices, plots = plots))
# }



# Define the function
binding_comp_index_dd <- function(data, year) {
  # Step 1: Filter and select contextual variables for the specified year
  context_vars <- data %>% filter(year == !!year) %>% dplyr::select(29:40)
  
  # Step 2: Combine context variables with composite indices
  indices <- cbind(context_vars, 
                   get(paste0("ci_", year, "_access")), 
                   get(paste0("ci_", year, "_skills")), 
                   get(paste0("ci_", year, "_usage")))
  
  # Step 3: Normalize the 'access' column
  indices <- normalize_min_max(indices, col_name = "access", new_col_name = "access_n")
  hist_access <- ggplot(indices, aes(x = access_n)) + geom_histogram() 
  
  # Step 4: Normalize the 'skills' column
  indices <- normalize_min_max(indices, col_name = "skills", new_col_name = "skills_n")
  hist_skills <- ggplot(indices, aes(x = skills_n)) + geom_histogram() 
  
  # Step 5: Normalize the 'usage' column
  indices <- normalize_min_max(indices, col_name = "usage", new_col_name = "usage_n")
  hist_usage <- ggplot(indices, aes(x = usage_n)) + geom_histogram() 
  
  # Step 6: Aggregate 'skills' and 'usage' according to theory
  ci_dd_secdd <- ci_generalized_mean1(indices, indic_col = c("skills_n", "usage_n"), p = 0.5, na.rm = TRUE)
  
  # Step 7: Combine the aggregated data with indices
  ci_dd_SU <- data.frame(secDD = ci_dd_secdd$ci_generalized_mean_est)
  indices <- cbind(indices, ci_dd_SU)
  
  # Step 8: Normalize the 'secDD' column
  indices <- normalize_min_max(indices, col_name = "secDD", new_col_name = "secDD_n")
  hist_secDD <- ggplot(indices, aes(x = secDD_n)) + geom_histogram() + facet_wrap(~ size_rev)
  
  # Add the year column back to indices
  indices <- indices %>% mutate(year = year)
  
  # Additional plots
  hist_access_size <- ggplot(indices, aes(x = access_n)) + geom_histogram() + facet_wrap(~ size_rev)
  hist_skills_size <- ggplot(indices, aes(x = skills_n)) + geom_histogram() + facet_wrap(~ size_rev)
  hist_usage_size <- ggplot(indices, aes(x = usage_n)) + geom_histogram() + facet_wrap(~ size_rev)
  scatter_access_secDD <- ggplot(indices, aes(x = access_n, y = secDD_n, color = size_rev)) + geom_point() + facet_wrap(~ size_rev)
  
  # Store plots in a list
  plots <- list(
    hist_access = hist_access,
    hist_skills = hist_skills,
    hist_usage = hist_usage,
    hist_secDD = hist_secDD,
    hist_access_size = hist_access_size,
    hist_skills_size = hist_skills_size,
    hist_usage_size = hist_usage_size,
    scatter_access_secDD = scatter_access_secDD
  )
  
  return(list(indices = indices, plots = plots))
}
