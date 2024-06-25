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

load(file = here("Data", "Environments", "ICT_Combined.rda"))

load(file = here("Data", "Processed", "ICT_Combined.rda"))

# Define the columns to exclude explicitly
excluded_columns <- c("ateco")
excluded_columns1 <- c("strata","size_emp", "size_rev", 
                       "sme_emp",  "sme_rev", "sec_name")

ict_combined$A2_A2 <- ict_combined$A2_A2/ 100
ict_combined$A2_C2 <- ict_combined$A2_C2/ 100
ict_combined$A2_C6 <- ict_combined$A2_C6/ 100

ict_combined <- ict_combined[, -c(7)]

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

# # Print the results for each year
# for (year in years) {
#   cat("\nResults for year", year, ":\n")
#   print(results[[as.character(year)]])
# }


## Correlations 2014 ####

##### Access  ####

source(here("Scripts", "Functions", "subsetting_digidivide.R"))

patt <- "^A|^S|^U|^size_rev"
var2encode <- c("size_rev")
year <- 2014



subsets_2014 <- subsetting_digidivide(data =  ict_combined, 
                                      patt =  patt, 
                                      var2encode = var2encode,
                                      year =  year, 
                                      include_weights = F)

A_data_2014 <- subsets_2014$A_data



# Use lapply to loop over the selected columns and convert them to numeric
# Ensure to reference the correct object and handle data types appropriately
A_data_2014[] <- lapply(A_data_2014, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
})

# Sort the column names in alphabetical order
sorted_col_namesA14 <- sort(names(A_data_2014[, 1:9]))

# Reorder the data frame according to the sorted column names
A_data_2014_sorted <- A_data_2014[, sorted_col_namesA14]

# Calculate the correlation matrix for the sorted columns
cor_matrix_sortedA14 <- cor(A_data_2014_sorted, use = "complete.obs")

# Plot the sorted correlation matrix
corrplot(cor_matrix_sortedA14, method = "square", order = "original", tl.col = "red", tl.srt = 45)



##### Skills  ####

patt <- "^A|^S|^U|^size_rev"
var2encode <- c("size_rev")
year <- 2014


S_data_2014 <- subsets_2014$S_data



# Use lapply to loop over the selected columns and convert them to numeric
# Ensure to reference the correct object and handle data types appropriately
S_data_2014[] <- lapply(S_data_2014, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
})

# Sort the column names in alphabetical order
sorted_col_namesS14 <- sort(names(S_data_2014[, 1:12]))

# Reorder the data frame according to the sorted column names
S_data_2014_sorted <- S_data_2014[, sorted_col_namesS14]

# Calculate the correlation matrix for the sorted columns
cor_matrix_sortedS14 <- cor(S_data_2014_sorted, use = "complete.obs")

# Plot the sorted correlation matrix
corrplot(cor_matrix_sortedS14, method = "square", order = "original", tl.col = "red", tl.srt = 45)



##### Usage  ####

patt <- "^A|^S|^U|^size_rev"
var2encode <- c("size_rev")
year <- 2014


S_data_2014 <- subsets_2014$S_data



# Use lapply to loop over the selected columns and convert them to numeric
# Ensure to reference the correct object and handle data types appropriately
S_data_2014[] <- lapply(S_data_2014, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else as.numeric(x)
})

# Sort the column names in alphabetical order
sorted_col_namesS14 <- sort(names(S_data_2014[, 1:12]))

# Reorder the data frame according to the sorted column names
S_data_2014_sorted <- S_data_2014[, sorted_col_namesS14]

# Calculate the correlation matrix for the sorted columns
cor_matrix_sortedS14 <- cor(S_data_2014_sorted, use = "complete.obs")

# Plot the sorted correlation matrix
corrplot(cor_matrix_sortedS14, method = "square", order = "original", tl.col = "red", tl.srt = 45)











  