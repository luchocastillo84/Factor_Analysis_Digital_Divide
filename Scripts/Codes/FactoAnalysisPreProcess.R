#### 0. Load the necessary packages #####
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(here)
library(git2r)
library(visdat)
library(naniar)
library(chatgpt)
library(gptstudio)
require(devtools)
library(caret)
library(mice)
library(fastDummies)
library(psych)
library(here)
library(fastDummies)
library(dplyr)
library(forcats)
library(DescTools)
library(fastDummies)
library(corrplot)
library(Compind)
library(lme4)
library(Matrix)
library(ltm)
library(psych)
library(reshape2)


load(file = here("Data", "Processed", "ICT_Combined.rda"))

# Define the columns to exclude explicitly
excluded_columns <- c("ateco")
excluded_columns1 <- c("strata","size_emp", "size_rev", 
                       "sme_emp",  "sme_rev", "sec_name")

ict_combined$A2_A2 <- ict_combined$A2_A2/ 100
ict_combined$A2_C2 <- ict_combined$A2_C2/ 100
ict_combined$A2_C6 <- ict_combined$A2_C6/ 100


patt <- "^A|^S|^U|^size_|^sme_|^mac_|^year"  

ict_combined_tests <- process_data2FA(ict_combined, patt = patt, encode = T )


continuous_vars <- c("A2_A2", "A2_C2")
binary_vars <- setdiff(names(ict_combined_tests), c(continuous_vars, "year"))


ict_combined_tests <- ict_combined_tests %>%
  mutate(across(all_of(continuous_vars), ~ as.numeric(as.character(.)))) %>%
  mutate(across(all_of(binary_vars), ~ as.numeric(as.character(.))))

summary(ict_combined_tests)


variance <- calculate_variance(ict_combined_tests, "year", 
                               continuous_vars, binary_vars)

ict_combined_tests <-  ict_combined_tests[, -5]

summary(ict_combined_tests)



# Function to perform reliabilit y analysis for a given subset of data
perform_reliability_analysis <- function(data) {
  # Subset data for columns starting with uppercase "A", "S", and "U" and exclude specified columns
  A_data <- data %>% dplyr::select(starts_with(c("A","size_rev"))) #%>% dplyr::select(-one_of(excluded_columns))
  S_data <- data %>% dplyr::select(starts_with(c("S_","size_rev"))) #%>% dplyr::select(-one_of(excluded_columns1))
  U_data <- data %>% dplyr::select(starts_with(c("U","size_rev")))
  
  
  # Convert factors to numeric if needed
  A_data <- A_data %>% mutate(across(everything(), ~ as.numeric(as.character(.))))
  S_data <- S_data %>% mutate(across(everything(), ~ as.numeric(as.character(.))))
  U_data <- U_data %>% mutate(across(everything(), ~ as.numeric(as.character(.))))
  
  # Identify continuous and binary variables in A_data
  is_binary <- function(x) {
    unique_vals <- unique(x)
    length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
  }
  
  binary_vars <- names(A_data)[sapply(A_data, is_binary)]
  continuous_vars <- setdiff(names(A_data), binary_vars)
  
  # Subset the data
  A_data_binary <- A_data %>% dplyr::select(all_of(binary_vars))
  A_data_continuous <- A_data %>% dplyr::select(all_of(continuous_vars))
  
  # Perform reliability analysis
  A_continuous_reliability <- psych::alpha(A_data_continuous, check.keys = TRUE)
  A_binary_reliability <- psych::alpha(A_data_binary, check.keys = TRUE)
  A_combined_reliability <- psych::alpha(A_data, check.keys = TRUE)  # Combined reliability for both continuous and binary variables
  
  S_reliability <- psych::alpha(S_data, check.keys = TRUE)
  U_reliability <- psych::alpha(U_data, check.keys = TRUE)
  
  # Return the results
  list(
    A_continuous_reliability = A_continuous_reliability,
    A_binary_reliability = A_binary_reliability,
    A_combined_reliability = A_combined_reliability,
    S_reliability = S_reliability,
    U_reliability = U_reliability
  )
}

# Get unique years from the dataset
years <- unique(ict_combined_tests$year)

# Initialize a list to store the results
results <- list()

# Loop through each year and perform the reliability analysis
for (year in years) {
  data_subset <- ict_combined_tests %>% filter(year == !!year)
  results[[as.character(year)]] <- perform_reliability_analysis(data_subset)
}

# Print the results for each year
for (year in years) {
  cat("\nResults for year", year, ":\n")
  print(results[[as.character(year)]])
}

  