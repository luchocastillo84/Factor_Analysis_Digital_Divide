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

## This script contains the average interpolations of the some variables 
# such as:Use of management systems such ERM and CRM. And the homogenization 
# of the employment of ICT personnel.
# This process is made before applying PCA to ensure a more robust results.
#
#
#



##### 0.1 Loading the environment #####
load(here("Scripts", "Environments",  "binding.RData"))

#### 1. Loading the data ##### 
load(here("Data", "Processed", "ICT14.rda")) 
load(here("Data", "Processed", "ICT15.rda"))
load(here("Data", "Processed", "ICT16.rda"))
load(here("Data", "Processed", "ICT17.rda"))
load(here("Data", "Processed", "ICT18.rda"))
load(here("Data", "Processed", "ICT19.rda"))

load(here("Data", "Processed", "ict_skills_14.rda"))
load(here("Data", "Processed", "ict_skills_15.rda"))
load(here("Data", "Processed", "ict_skills_16.rda"))
load(here("Data", "Processed", "ict_skills_17.rda"))
load(here("Data", "Processed", "ict_skills_18.rda"))
load(here("Data", "Processed", "ict_skills_19.rda"))


# Calculate the mean of 'access_n' for each 'ateco_1' category
mean_access_n_by_ateco_1 <- ict_15_impT %>%
  group_by(ateco_1) %>%
  summarise(mean_access = mean(A2_C2, na.rm = TRUE)) # 

table(ict_14_impT$ateco_1)


vis_miss(ict_17_impT, warn_large_data = FALSE)

##### 1.1. Data homogenization ##### 
# Define standard variable names based on one year or a template
standard_names <- colnames(ict_14_impT)  # Assuming ict_14_impT has the correct names

# Assign these names to all dataframes
colnames(ict_15_impT) <- standard_names
colnames(ict_16_impT) <- standard_names
colnames(ict_17_impT) <- standard_names
colnames(ict_18_impT) <- standard_names
colnames(ict_19_impT) <- standard_names

# Add a year variable to each dataset
ict_14_impT$year <- 2014
ict_15_impT$year <- 2015
ict_16_impT$year <- 2016
ict_17_impT$year <- 2017
ict_18_impT$year <- 2018
ict_19_impT$year <- 2019



# Combine all datasets into one
ict_combined <- do.call(rbind, list(ict_14_impT, ict_15_impT, 
                                    ict_16_impT, ict_17_impT, 
                                    ict_18_impT, ict_19_impT))

# Converting the year variable into factor
ict_combined$year <- as.factor(ict_combined$year)






# Explorint the data
str(ict_combined)
summary(ict_combined)
summary(ict_combined$S_B5a)

# Correcting variables that are out of range 
ict_combined$A2_A2 <- ifelse(ict_combined$A2_A2 > 100, 100, ict_combined$A2_A2)
ict_combined$A2_C2 <- ifelse(ict_combined$A2_C2 > 100, 100, ict_combined$A2_C2)
ict_combined$A2_C6 <- ifelse(ict_combined$A2_C6 > 100, 100, ict_combined$A2_C6)




vis_miss(ict_combined, warn_large_data = FALSE)



#### 2. Interpolation of missing variables  ##### 

##### 2.1. S_B5a Employment of ICT personnel ##### 

# EDA to the binned data 
plot_binary_var2(ict_skills_15, "B5a", title_name = "Employment of ICT", 
                x_var = "ateco_1", facet_var = "year", ncol = 2)

ggplot(ict_skills_15, aes(x = year, fill = as.factor(B5a))) +
  geom_bar(position = "fill") 


# This line creates a new variable in the ict_combined dataframe called strata. 
# It is formed by the interaction of three other variables: clad4 (company size),
# ateco_1 (sector), and rip (region). The interaction function combines these into 
# a single factor where each level represents a unique combination of the three variables.
ict_combined$strata <- interaction(ict_combined$clad4, ict_combined$ateco_1, ict_combined$rip)

# Calculate the proportion of 1s for each strata excluding year 2017 
# The block of code using dplyr's filter, group_by, and summarize functions 
# calculates the proportion of 1s in the S_B5a variable for each stratum, excluding 
# the year 2017. The filter(year != "2017") ensures that the year 2017 is not included 
# in this calculation. The group_by(strata) groups the data by the newly created strata 
# variable. Then, summarize(proportion_of_1s = mean(as.numeric(as.character(S_B5a)), na.rm = TRUE)) 
# calculates the mean of S_B5a for each group, which is the proportion 
# of 1s. S_B5a is first converted to character and then to numeric to ensure the 
# correct calculation of the mean.
prop_table1 <- ict_combined %>%
  filter(year != "2017") %>%
  group_by(strata) %>%
  summarize(proportion_of_1s = mean(as.numeric(as.character(S_B5a)), na.rm = TRUE))


# The code calculates the proportions of S_B5a being 1 for each stratum based on 
# the available data from years other than 2017. This is done with the assumption 
# that the patterns in S_B5a are stable over time and that the calculated proportions 
# are a reasonable estimate for 2017 as well.

# Applying Proportions to 2017:
#   
#   The code then applies these calculated proportions to the missing values in 2017. 
#   It doesn't interpolate the 2017 values between 2016 and 2018 but instead assumes 
#   that the proportions of S_B5a in 2017 would follow the same distribution as in other years.


# For each strata, distribute 0s and 1s for year 2017 according to the proportion
set.seed(123) # for reproducibility

for (stratum_level in unique(ict_combined$strata)) {
  # Determine the number of cases to impute for the stratum in 2017
  num_to_impute <- sum(ict_combined$year == "2017" & ict_combined$strata == stratum_level)
  
  if (num_to_impute > 0) {
    # Get the proportion for this stratum
    stratum_prop <- prop_table1$proportion_of_1s[prop_table1$strata == stratum_level]
    
    # Calculate the expected counts of 1s and 0s
    num_1s <- round(num_to_impute * stratum_prop)
    num_0s <- num_to_impute - num_1s
    
    # Create a vector of 1s and 0s
    imputed_values <- c(rep(1, num_1s), rep(0, num_0s))
    
    # Randomize the order to avoid any order effects
    imputed_values <- sample(imputed_values)
    
    # Assign the values to the dataset
    ict_combined$S_B5a[ict_combined$year == "2017" & ict_combined$strata == stratum_level] <- imputed_values
  }
}

# Check the results
table(ict_combined$S_B5a, ict_combined$year)


plot_binary_var2(ict_combined, "S_B5a", title_name = "Employment of ICT", 
                x_var = "ateco_1", facet_var = "year", ncol = 2)



ggplot(ict_combined, aes(x = year, fill = as.factor(S_B5a))) +
  geom_bar(position = "fill") +
  facet_wrap(~ year) +
  theme_minimal()

##### 2.1. B5a - B5g Skills variables ##### 


names(ict_skills_14)
ict_skills_14 <- ict_skills_14 %>% rename(C10a = C9a,
                                          C10c = C9c)

names(ict_skills_15)
ict_skills_15 <- ict_skills_15 %>% rename(C10a = C9a,
                                          C10c = C9c)
names(ict_skills_16)
names(ict_skills_17)
names(ict_skills_18)
names(ict_skills_19)

ict_skills_19 <- ict_skills_19 %>% rename(C4 = c4) 


ict_skills <- rbind(ict_skills_14, ict_skills_15, ict_skills_16, 
                    ict_skills_17, ict_skills_18, ict_skills_19)

ict_skills$year <- as.factor(ict_skills$year)



summary(ict_skills$B5a)

names(ict_combined)

plot_categorical_var(ict_combined, 
                     variable_name = "S_B2a", 
                     title_name = "Skills", 
                     x_var = "ateco", 
                     facet_var = "year", 
                     ncol = 2 )


# This line creates a new variable in the ict_combined dataframe called strata. 
# It is formed by the interaction of three other variables: clad4 (company size),
# ateco_1 (sector), and rip (region). The interaction function combines these into 
# a single factor where each level represents a unique combination of the three variables.

ict_skills$strata <- interaction(ict_skills$clad4, ict_skills$ateco_1, ict_skills$rip)

# This part of the code calculates the proportions of each category 
# (1, 2, and 3) for each variable (B5a to B5g) within each stratum and 
# for the available years (2015, 2016, and 2018). The results are stored 
# in a list (prop_table_list) with each element corresponding to a variable 
# and containing the average proportions for each category within each 
# stratum. This prepares the necessary proportions for the imputation step.

library(dplyr)

# Example dataset
# ict_skills <- your dataset

variables_to_impute <- c("B5a", "B5b", "B5c", "B5d", "B5e", "B5f", "B5g")

# Calculate proportions for each category for each variable in available years
prop_table_list <- list()
for (var in variables_to_impute) {
  prop_table <- ict_skills %>%
    filter(year %in% c("2015", "2016", "2018")) %>%
    group_by(strata, year) %>%
    summarize(
      prop_1 = mean(as.numeric(as.character(get(var))) == 1, na.rm = TRUE),
      prop_2 = mean(as.numeric(as.character(get(var))) == 2, na.rm = TRUE),
      prop_3 = mean(as.numeric(as.character(get(var))) == 3, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(strata) %>%
    summarize(
      avg_prop_1 = mean(prop_1, na.rm = TRUE),
      avg_prop_2 = mean(prop_2, na.rm = TRUE),
      avg_prop_3 = mean(prop_3, na.rm = TRUE)
    )
  prop_table_list[[var]] <- prop_table
}

# This code extends the imputation process to multiple variables (B5a to B5g). 
# For each variable, the same imputation logic is applied: calculating the 
# number of cases to impute, using the proportions from the prop_table to 
# estimate the counts of each category, creating a randomized vector of these 
# counts, and assigning the imputed values to the missing entries in the 
# dataset. This ensures that the imputation process is consistent across 
# all variables within the B5 group, maintaining the integrity of the data 
# across different years and variables.

set.seed(123) # for reproducibility

for (var in variables_to_impute) {
  for (stratum_level in unique(ict_skills$strata)) {
    for (missing_year in c("2014", "2017", "2019")) {
      num_to_impute <- sum(ict_skills$year == missing_year & ict_skills$strata == stratum_level)
      
      if (num_to_impute > 0) {
        stratum_props <- prop_table_list[[var]] %>% filter(strata == stratum_level)
        
        if (nrow(stratum_props) == 1) {
          prop_1 <- stratum_props$avg_prop_1
          prop_2 <- stratum_props$avg_prop_2
          prop_3 <- stratum_props$avg_prop_3
          
          num_1s <- round(num_to_impute * prop_1)
          num_2s <- round(num_to_impute * prop_2)
          num_3s <- num_to_impute - num_1s - num_2s
          
          if (num_3s < 0) num_3s <- 0
          
          imputed_values <- c(rep(1, num_1s), rep(2, num_2s), rep(3, num_3s))
          imputed_values <- sample(imputed_values)
          
          ict_skills[[var]][ict_skills$year == missing_year & ict_skills$strata == stratum_level] <- imputed_values
        }
      }
    }
  }
}

# Convert NA values to 3 for each specified variable
ict_skills <- ict_skills %>%
  mutate(across(all_of(variables_to_impute), ~ ifelse(is.na(.), 3, .)))

# Convert columns 8 to 16 to factors
ict_skills[, 5:16] <- lapply(ict_skills[, 5:16], as.factor)

# Function to create binary variables and convert them to factors
create_binary_vars <- function(df, original_var) {
  df %>%
    mutate(
      !!paste0(original_var, "1") := factor(ifelse(get(original_var) == 1, 1, 0)),
      !!paste0(original_var, "2") := factor(ifelse(get(original_var) == 2, 1, 0)),
      !!paste0(original_var, "3") := factor(ifelse(get(original_var) %in% c(1, 2), 1, 0))
    )
}

sum(as.numeric(as.character(ict_skills$B5b3)))/nrow(ict_skills)*(1- sum(as.numeric(as.character(ict_skills$B5b3)))/nrow(ict_skills))



# Apply the function to each original variable
for (var in variables_to_impute) {
  ict_skills <- create_binary_vars(ict_skills, var)
}


# Function to create binary variables and convert them to factors
create_binary_vars <- function(df, original_var) {
  df %>%
    mutate(
      !!paste0(original_var, "1") := factor(ifelse(get(original_var) == 1, 1, 0)),
      !!paste0(original_var, "2") := factor(ifelse(get(original_var) == 2, 1, 0))
    )
}

# Apply the function to each original variable
for (var in variables_to_impute) {
  ict_skills <- create_binary_vars(ict_skills, var)
}



summary(ict_skills)


##### 2.3. C10a - C10c Social Media ##### 

# This part of the code calculates the proportions of each category 
# (0, 1) for each variable (C10a and C10c) within each stratum and 
# for the available years (2014, 2015, 2016, 2017, and 2019). The results are stored 
# in a list (prop_table_list) with each element corresponding to a variable 
# and containing the average proportions for each category within each 
# stratum. This prepares the necessary proportions for the imputation step.



variables_to_impute1 <- c("C10a", "C10c")

# Calculate proportions for each category for each variable in available years
prop_table_list1 <- list()
for (var in variables_to_impute1) {
  prop_table <- ict_skills %>%
    filter(year %in% c("2014","2015", "2016", "2017", "2019")) %>%
    group_by(strata, year) %>%
    summarize(
      prop_0 = mean(as.numeric(as.character(get(var))) == 0, na.rm = TRUE),
      prop_1 = mean(as.numeric(as.character(get(var))) == 1, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    group_by(strata) %>%
    summarize(
      avg_prop_0 = mean(prop_0, na.rm = TRUE),
      avg_prop_1 = mean(prop_1, na.rm = TRUE)
    )
  prop_table_list1[[var]] <- prop_table
}

# This code extends the imputation process to multiple variables (C10a and C10c). 
# For each variable, the same imputation logic is applied: calculating the 
# number of cases to impute, using the proportions from the prop_table to 
# estimate the counts of each category, creating a randomized vector of these 
# counts, and assigning the imputed values to the missing entries in the 
# dataset. This ensures that the imputation process is consistent across 
# all variables within the B5 group, maintaining the integrity of the data 
# across different years and variables.


set.seed(123) # for reproducibility

for (var in variables_to_impute1) {
  for (stratum_level in unique(ict_skills$strata)) {
    for (missing_year in c("2018")) {
      num_to_impute <- sum(ict_skills$year == missing_year & ict_skills$strata == stratum_level)
      
      if (num_to_impute > 0) {
        stratum_props <- prop_table_list1[[var]] %>% filter(strata == stratum_level)
        
        if (nrow(stratum_props) == 1) {
          prop_0 <- stratum_props$avg_prop_0
          prop_1 <- stratum_props$avg_prop_1
          
          num_1s <- round(num_to_impute * prop_1)
          num_0s <- num_to_impute - num_1s
          
          imputed_values <- c(rep(1, num_1s), rep(0, num_0s))
          imputed_values <- sample(imputed_values)
          
          ict_skills[[var]][ict_skills$year == missing_year & ict_skills$strata == stratum_level] <- imputed_values
        }
      }
    }
  }
}


plot_categorical_var(ict_skills, "C10c", "Skills", x_var = "ateco_1", facet_var = "year", ncol = 2 )

summary(ict_skills %>% filter(year == 2019))

##### 2.4. C10a - C10c Social Media II ##### 


library(dplyr)

# List of binary variables to impute
variables_to_impute1 <- c("C10a", "C10c")

# Define the years with NAs that you want to target
years_with_na <- c(2014, 2015)  # Ensure years are numeric

# Convert specified variables to numeric
ict_skills <- ict_skills %>%
  mutate(across(all_of(variables_to_impute1), ~ as.numeric(as.character(.))))

# Replace NAs with 0s for each specified variable in the targeted years
for (var in variables_to_impute1) {
  for (year in years_with_na) {
    ict_skills <- ict_skills %>%
      mutate(!!var := if_else(year == year & is.na(get(var)), 0, get(var)))
  }
}
ict_skills[, 5:16] <- lapply(ict_skills[, 5:16], as.factor)

# View the first few rows of the modified dataset
head(ict_skills)

# View the first few rows of the modified dataset
summary(ict_skills)






##### 2.3. C4  Internet Speed ##### 


library(dplyr)

# Calculate proportions for each category in available years
prop_table_C4 <- ict_skills %>%
  filter(!is.na(C4)) %>%
  group_by(strata, year) %>%
  summarize(
    prop_1 = mean(C4 == 1, na.rm = TRUE),
    prop_2 = mean(C4 == 2, na.rm = TRUE),
    prop_3 = mean(C4 == 3, na.rm = TRUE),
    prop_4 = mean(C4 == 4, na.rm = TRUE),
    prop_5 = mean(C4 == 5, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  group_by(strata) %>%
  summarize(
    avg_prop_1 = mean(prop_1, na.rm = TRUE),
    avg_prop_2 = mean(prop_2, na.rm = TRUE),
    avg_prop_3 = mean(prop_3, na.rm = TRUE),
    avg_prop_4 = mean(prop_4, na.rm = TRUE),
    avg_prop_5 = mean(prop_5, na.rm = TRUE)
  )


set.seed(123) # for reproducibility

years_with_na <- ict_skills %>%
  filter(is.na(C4)) %>%
  group_by(year) %>%
  summarize(num_na = n())

for (year in years_with_na$year) {
  num_na_year <- years_with_na$num_na[years_with_na$year == year]
  
  for (stratum_level in unique(ict_skills$strata)) {
    num_to_impute <- sum(ict_skills$year == year & ict_skills$strata == stratum_level & is.na(ict_skills$C4))
    
    if (num_to_impute > 0) {
      stratum_props <- prop_table_C4 %>% filter(strata == stratum_level)
      
      if (nrow(stratum_props) == 1) {
        prop_1 <- stratum_props$avg_prop_1
        prop_2 <- stratum_props$avg_prop_2
        prop_3 <- stratum_props$avg_prop_3
        prop_4 <- stratum_props$avg_prop_4
        prop_5 <- stratum_props$avg_prop_5
        
        num_1s <- round(num_to_impute * prop_1)
        num_2s <- round(num_to_impute * prop_2)
        num_3s <- round(num_to_impute * prop_3)
        num_4s <- round(num_to_impute * prop_4)
        num_5s <- num_to_impute - num_1s - num_2s - num_3s - num_4s
        
        if (num_5s < 0) num_5s <- 0
        
        imputed_values <- c(rep(1, num_1s), rep(2, num_2s), rep(3, num_3s), rep(4, num_4s), rep(5, num_5s))
        imputed_values <- sample(imputed_values)
        
        ict_skills$C4[ict_skills$year == year & ict_skills$strata == stratum_level & is.na(ict_skills$C4)] <- imputed_values
      }
    }
  }
}

plot_categorical_var(ict_skills, "C4", "Skills", x_var = "ateco_1", facet_var = "year", ncol = 2 )

summary(ict_skills)


# # Recode the C4 variable and ensure the order of factors is high, medium, low
# ict_skills <- ict_skills %>%
#   mutate(C4 = case_when(
#     C4 %in% c(1, 2) ~ "low",
#     C4 == 3 ~ "medium",
#     C4 %in% c(4, 5) ~ "high"
#   ))

# Recode the variable into low and high internet speed
ict_skills <- ict_skills %>%
  mutate(C4 = case_when(
    C4 %in% c(1, 2, 3) ~ "low",
    C4 %in% c(4, 5) ~ "high"
  ))

# Convert to factor for further analysis
ict_skills$C4 <- factor(ict_skills$C4, levels = c("low", "high"))

# View the first few rows of the modified dataset
head(ict_skills)


summary(ict_skills %>% filter(year == 2019) %>% dplyr::select(C4))

# One-hot encode C4 and remove the original column
ict_skills <- dummy_cols(ict_skills, select_columns = "C4", 
                         remove_first_dummy = FALSE, 
                         remove_selected_columns = TRUE)

# Convert to factor for further analysis
ict_skills[, 30:31] <- lapply(ict_skills[, 30:31], as.factor)

summary(ict_skills)


### including internal personal 
ict_skills_impT1 <- ict_skills %>% dplyr::select(codice, year, B5a1, B5b1,
                                                B5c1, B5d1, B5e1, B5f1, B5g1,
                                                C10a, C10c, C4_low,
                                                C4_high)

standard_names1 <- c("codice", "year", "S_B5a1",	"S_B5b1",	"S_B5c1",	
                     "S_B5d1",	"S_B5e1",	"S_B5f1",	"S_B5g1", "UMK_C10a", 
                     "UMK_C10c", "A2_C4_low", "A2_C4_high") ## these names include the internal 

colnames(ict_skills_impT1) <- standard_names1
names(ict_skills_impT1)

### including external personal 
ict_skills_impT2 <- ict_skills %>% dplyr::select(codice, year, B5a2, B5b2,
                                                B5c2, B5d2, B5e2, B5f2, B5g2,
                                                C10a, C10c, C4_low,
                                                C4_high)

standard_names2 <- c("codice", "year", "S_B5a2",	"S_B5b2",	"S_B5c2",	
                     "S_B5d2",	"S_B5e2",	"S_B5f2",	"S_B5g2", "UMK_C10a", 
                     "UMK_C10c", "A2_C4_low", "A2_C4_high") ### include the external 

colnames(ict_skills_impT2) <- standard_names2
names(ict_skills_impT2)


### including external or internal personal 
ict_skills_impT3 <- ict_skills %>% dplyr::select(codice, year, B5a3, B5b3,
                                                 B5c3, B5d3, B5e3, B5f3, B5g3,
                                                 C10a, C10c, C4_low,
                                                 C4_high)

standard_names3 <- c("codice", "year", "S_B5a3",	"S_B5b3",	"S_B5c3",	
                     "S_B5d3",	"S_B5e3",	"S_B5f3",	"S_B5g3", "UMK_C10a", 
                     "UMK_C10c", "A2_C4_low", "A2_C4_high")

colnames(ict_skills_impT3) <- standard_names3
names(ict_skills_impT3)




save(ict_skills, file = here("Data", "Processed", "ict_skills.rda"))


##### 2.2. A1_C8g announcement of vacancies online ##### 

# EDA to the binned data 
plot_binary_var2(ict_skills, "C8g", 
                title_name = "announcement of vacancies online", 
                x_var = "ateco_1", facet_var = "year", ncol = 2)

ggplot(ict_combined, aes(x = year, fill = as.factor(UMK_C8g))) +
  geom_bar(position = "fill") +
  facet_wrap(~ year) +
  theme_minimal()


# This line creates a new variable in the ict_combined dataframe called strata. 
# It is formed by the interaction of three other variables: clad4 (company size),
# ateco_1 (sector), and rip (region). The interaction function combines these into 
# a single factor where each level represents a unique combination of the three variables.
# ict_combined$strata <- interaction(ict_combined$clad4, ict_combined$ateco_1, ict_combined$rip)

# Calculate the proportion of 1s for each strata excluding year 2018 
# The block of code using dplyr's filter, group_by, and summarize functions 
# calculates the proportion of 1s in the A1_C8g variable for each stratum, excluding 
# the year 2018. The filter(year != "2018") ensures that the year 2018 is not included 
# in this calculation. The group_by(strata) groups the data by the newly created strata 
# variable. Then, summarize(proportion_of_1s = mean(as.numeric(as.character(A1_C8g)), na.rm = TRUE)) 
# calculates the mean of A1_C8g for each group, which is the proportion 
# of 1s. A1_C8g is first converted to character and then to numeric to ensure the 
# correct calculation of the mean.
prop_table2 <- ict_combined %>%
  filter(year != "2018") %>%
  group_by(strata) %>%
  summarize(proportion_of_1s = mean(as.numeric(as.character(UM_C8g)), na.rm = TRUE))


# The code calculates the proportions of A1_C8g being 1 for each stratum based on 
# the available data from years other than 2018. This is done with the assumption 
# that the patterns in A1_C8g are stable over time and that the calculated proportions 
# are a reasonable estimate for 2018 as well.

# Applying Proportions to 2018:
#   
#   The code then applies these calculated proportions to the missing values in 2018. 
#   It doesn't interpolate the 2018 values between 2017 and 2019 but instead assumes 
#   that the proportions of A1_C8g in 2018 would follow the same distribution as in other years.


# For each strata, distribute 0s and 1s for year 2018 according to the proportion
set.seed(123) # for reproducibility

for (stratum_level in unique(ict_combined$strata)) {
  # Determine the number of cases to impute for the stratum in 2017
  num_to_impute <- sum(ict_combined$year == "2018" & ict_combined$strata == stratum_level)
  
  if (num_to_impute > 0) {
    # Get the proportion for this stratum
    stratum_prop <- prop_table2$proportion_of_1s[prop_table2$strata == stratum_level]
    
    # Calculate the expected counts of 1s and 0s
    num_1s <- round(num_to_impute * stratum_prop)
    num_0s <- num_to_impute - num_1s
    
    # Create a vector of 1s and 0s
    imputed_values <- c(rep(1, num_1s), rep(0, num_0s))
    
    # Randomize the order to avoid any order effects
    imputed_values <- sample(imputed_values)
    
    # Assign the values to the dataset
    ict_combined$UM_C8g[ict_combined$year == "2018" & ict_combined$strata == stratum_level] <- imputed_values
  }
}

# Check the results
table(ict_combined$UM_C8g, ict_combined$year)


plot_binary_var2(ict_combined, "UM_C8g", 
                title_name = "announcement of vacancies online", 
                x_var = "ateco_1", facet_var = "year", ncol = 2)



ggplot(ict_combined, aes(x = year, fill = as.factor(UM_C8g))) +
  geom_bar(position = "fill") +
  facet_wrap(~ year) +
  theme_minimal()


##### 2.3. UM_E1 using erp software ##### 

# EDA to the binned data 
plot_binary_var2(ict_combined, "UM_E1", title_name = "using erp software", 
                x_var = "ateco_1", facet_var = "year", ncol = 2)

ggplot(ict_combined, aes(x = year, fill = as.factor(UM_E1))) +
  geom_bar(position = "fill") +
  facet_wrap(~ year) +
  theme_minimal()


# This line creates a new variable in the ict_combined dataframe called strata. 
# It is formed by the interaction of three other variables: clad4 (company size),
# ateco_1 (sector), and rip (region). The interaction function combines these into 
# a single factor where each level represents a unique combination of the three variables.
# ict_combined$strata <- interaction(ict_combined$clad4, ict_combined$ateco_1, ict_combined$rip)

# Calculate the proportion of 1s for each strata excluding year 2018 
# The block of code using dplyr's filter, group_by, and summarize functions 
# calculates the proportion of 1s in the UM_E1 variable for each stratum, excluding 
# the year 2018. The filter(year != "2018") ensures that the year 2018 is not included 
# in this calculation. The group_by(strata) groups the data by the newly created strata 
# variable. Then, summarize(proportion_of_1s = mean(as.numeric(as.character(UM_E1)), na.rm = TRUE)) 
# calculates the mean of UM_E1 for each group, which is the proportion 
# of 1s. UM_E1 is first converted to character and then to numeric to ensure the 
# correct calculation of the mean.
prop_table3 <- ict_combined %>%
  filter(year != "2018") %>%
  group_by(strata) %>%
  summarize(proportion_of_1s = mean(as.numeric(as.character(UM_E1)), na.rm = TRUE))


# The code calculates the proportions of UM_E1 being 1 for each stratum based on 
# the available data from years other than 2018. This is done with the assumption 
# that the patterns in UM_E1 are stable over time and that the calculated proportions 
# are a reasonable estimate for 2018 as well.

# Applying Proportions to 2018:
#   
#   The code then applies these calculated proportions to the missing values in 2018. 
#   It doesn't interpolate the 2018 values between 2017 and 2019 but instead assumes 
#   that the proportions of UM_E1 in 2018 would follow the same distribution as in other years.


# For each strata, distribute 0s and 1s for year 2018 according to the proportion
set.seed(123) # for reproducibility

for (stratum_level in unique(ict_combined$strata)) {
  # Determine the number of cases to impute for the stratum in 2017
  num_to_impute <- sum(ict_combined$year == "2018" & ict_combined$strata == stratum_level)
  
  if (num_to_impute > 0) {
    # Get the proportion for this stratum
    stratum_prop <- prop_table3$proportion_of_1s[prop_table3$strata == stratum_level]
    
    # Calculate the expected counts of 1s and 0s
    num_1s <- round(num_to_impute * stratum_prop)
    num_0s <- num_to_impute - num_1s
    
    # Create a vector of 1s and 0s
    imputed_values <- c(rep(1, num_1s), rep(0, num_0s))
    
    # Randomize the order to avoid any order effects
    imputed_values <- sample(imputed_values)
    
    # Assign the values to the dataset
    ict_combined$UM_E1[ict_combined$year == "2018" & ict_combined$strata == stratum_level] <- imputed_values
  }
}

# Check the results
table(ict_combined$UM_E1, ict_combined$year)


plot_binary_var2(ict_combined, "UM_E1", title_name = "using erp software", 
                x_var = "ateco_1", facet_var = "year", ncol = 2)



ggplot(ict_combined, aes(x = year, fill = as.factor(UM_E1))) +
  geom_bar(position = "fill") +
  facet_wrap(~ year) +
  theme_minimal()



##### 2.4. UM_E2b use operational crm software ##### 

# EDA to the binned data 
plot_binary_var2(ict_combined, "UM_E2b", 
                title_name = "use operational crm software", 
                x_var = "ateco_1", facet_var = "year", ncol = 2)

ggplot(ict_combined, aes(x = year, fill = as.factor(UM_E2b))) +
  geom_bar(position = "fill") +
  facet_wrap(~ year) +
  theme_minimal()


# This line creates a new variable in the ict_combined dataframe called strata. 
# It is formed by the interaction of three other variables: clad4 (company size),
# ateco_1 (sector), and rip (region). The interaction function combines these into 
# a single factor where each level represents a unique combination of the three variables.
# ict_combined$strata <- interaction(ict_combined$clad4, ict_combined$ateco_1, ict_combined$rip)

# Calculate the proportion of 1s for each strata excluding year 2018 
# The block of code using dplyr's filter, group_by, and summarize functions 
# calculates the proportion of 1s in the UM_E2b variable for each stratum, excluding 
# the year 2018. The filter(year != "2018") ensures that the year 2018 is not included 
# in this calculation. The group_by(strata) groups the data by the newly created strata 
# variable. Then, summarize(proportion_of_1s = mean(as.numeric(as.character(UM_E2b)), na.rm = TRUE)) 
# calculates the mean of UM_E2b for each group, which is the proportion 
# of 1s. UM_E2b is first converted to character and then to numeric to ensure the 
# correct calculation of the mean.
prop_table4 <- ict_combined %>%
  filter(year != "2018") %>%
  group_by(strata) %>%
  summarize(proportion_of_1s = mean(as.numeric(as.character(UM_E2b)), na.rm = TRUE))


# The code calculates the proportions of UM_E2b being 1 for each stratum based on 
# the available data from years other than 2018. This is done with the assumption 
# that the patterns in UM_E2b are stable over time and that the calculated proportions 
# are a reasonable estimate for 2018 as well.

# Applying Proportions to 2018:
#   
#   The code then applies these calculated proportions to the missing values in 2018. 
#   It doesn't interpolate the 2018 values between 2017 and 2019 but instead assumes 
#   that the proportions of UM_E2b in 2018 would follow the same distribution as in other years.


# For each strata, distribute 0s and 1s for year 2018 according to the proportion
set.seed(123) # for reproducibility

for (stratum_level in unique(ict_combined$strata)) {
  # Determine the number of cases to impute for the stratum in 2017
  num_to_impute <- sum(ict_combined$year == "2018" & ict_combined$strata == stratum_level)
  
  if (num_to_impute > 0) {
    # Get the proportion for this stratum
    stratum_prop <- prop_table4$proportion_of_1s[prop_table4$strata == stratum_level]
    
    # Calculate the expected counts of 1s and 0s
    num_1s <- round(num_to_impute * stratum_prop)
    num_0s <- num_to_impute - num_1s
    
    # Create a vector of 1s and 0s
    imputed_values <- c(rep(1, num_1s), rep(0, num_0s))
    
    # Randomize the order to avoid any order effects
    imputed_values <- sample(imputed_values)
    
    # Assign the values to the dataset
    ict_combined$UM_E2b[ict_combined$year == "2018" & ict_combined$strata == stratum_level] <- imputed_values
  }
}

# Check the results
table(ict_combined$UM_E2b, ict_combined$year)


plot_binary_var2(ict_combined, "UM_E2b", 
                title_name = "use operational crm software", 
                x_var = "ateco_1", facet_var = "year", ncol = 2)



ggplot(ict_combined, aes(x = year, fill = as.factor(UM_E2b))) +
  geom_bar(position = "fill") +
  facet_wrap(~ year) +
  theme_minimal()


##### 2.5. UM_E2a use analytical crm software ##### 

# EDA to the binned data 
plot_binary_var2(ict_combined, "UM_E2a", 
                title_name = "use analytical crm software", 
                x_var = "ateco_1", facet_var = "year", ncol = 2)

ggplot(ict_combined, aes(x = year, fill = as.factor(UM_E2a))) +
  geom_bar(position = "fill") +
  facet_wrap(~ year) +
  theme_minimal()


# This line creates a new variable in the ict_combined dataframe called strata. 
# It is formed by the interaction of three other variables: clad4 (company size),
# ateco_1 (sector), and rip (region). The interaction function combines these into 
# a single factor where each level represents a unique combination of the three variables.
# ict_combined$strata <- interaction(ict_combined$clad4, ict_combined$ateco_1, ict_combined$rip)

# Calculate the proportion of 1s for each strata excluding year 2018 
# The block of code using dplyr's filter, group_by, and summarize functions 
# calculates the proportion of 1s in the UM_E2a variable for each stratum, excluding 
# the year 2018. The filter(year != "2018") ensures that the year 2018 is not included 
# in this calculation. The group_by(strata) groups the data by the newly created strata 
# variable. Then, summarize(proportion_of_1s = mean(as.numeric(as.character(UM_E2a)), na.rm = TRUE)) 
# calculates the mean of UM_E2a for each group, which is the proportion 
# of 1s. UM_E2a is first converted to character and then to numeric to ensure the 
# correct calculation of the mean.
prop_table5 <- ict_combined %>%
  filter(year != "2018") %>%
  group_by(strata) %>%
  summarize(proportion_of_1s = mean(as.numeric(as.character(UM_E2a)), na.rm = TRUE))


# The code calculates the proportions of UM_E2a being 1 for each stratum based on 
# the available data from years other than 2018. This is done with the assumption 
# that the patterns in UM_E2a are stable over time and that the calculated proportions 
# are a reasonable estimate for 2018 as well.

# Applying Proportions to 2018:
#   
#   The code then applies these calculated proportions to the missing values in 2018. 
#   It doesn't interpolate the 2018 values between 2017 and 2019 but instead assumes 
#   that the proportions of UM_E2a in 2018 would follow the same distribution as in other years.


# For each strata, distribute 0s and 1s for year 2018 according to the proportion
set.seed(123) # for reproducibility

for (stratum_level in unique(ict_combined$strata)) {
  # Determine the number of cases to impute for the stratum in 2017
  num_to_impute <- sum(ict_combined$year == "2018" & ict_combined$strata == stratum_level)
  
  if (num_to_impute > 0) {
    # Get the proportion for this stratum
    stratum_prop <- prop_table5$proportion_of_1s[prop_table5$strata == stratum_level]
    
    # Calculate the expected counts of 1s and 0s
    num_1s <- round(num_to_impute * stratum_prop)
    num_0s <- num_to_impute - num_1s
    
    # Create a vector of 1s and 0s
    imputed_values <- c(rep(1, num_1s), rep(0, num_0s))
    
    # Randomize the order to avoid any order effects
    imputed_values <- sample(imputed_values)
    
    # Assign the values to the dataset
    ict_combined$UM_E2a[ict_combined$year == "2018" & ict_combined$strata == stratum_level] <- imputed_values
  }
}

# Check the results
table(ict_combined$UM_E2a, ict_combined$year)


plot_binary_var2(ict_combined, "UM_E2a", 
                title_name = "use analytical crm software", 
                x_var = "ateco_1", facet_var = "year", ncol = 2)



ggplot(ict_combined, aes(x = year, fill = as.factor(UM_E2a))) +
  geom_bar(position = "fill") +
  facet_wrap(~ year) +
  theme_minimal()

vis_miss(ict_combined, warn_large_data = FALSE)
summary(ict_combined, Inf)


# Rename column changing the var "announcement of vacancies 
# or possibility to apply for employment online" from access 
# to usage using dplyr
# ict_combined <- ict_combined %>%
#   rename(UM_C8g = A1_C8g)


##### Merging the ict_combined with the ict_skills_impT with new more variables 

ict_combined$Codice <- as.numeric(ict_combined$Codice)


summary(ict_combined1)

ict_combined1 <- ict_combined %>% left_join(ict_skills_impT1, 
                                            by = c("Codice" = "codice", "year"))

# Create a vector of the column names in the desired order
new_column_order <- c(
  "Codice", "Ricavi",                            # Columns to go at the beginning
  grep("^A", names(ict_combined1), value = TRUE), # Columns that start with "A"
  grep("^S", names(ict_combined1), value = TRUE), # Columns that start with "S"
  grep("^U", names(ict_combined1), value = TRUE), # Columns that start with "U"
  setdiff(names(ict_combined1),                  # Columns that are not specified to be at the beginning or end
          c("Codice", "Ricavi", 
            grep("^A", names(ict_combined1), value = TRUE),
            grep("^S", names(ict_combined1), value = TRUE),
            grep("^U", names(ict_combined1), value = TRUE),
            "year", "rip", "clad4", "dom4", "ateco_1", "Revenue_K", "strata")),
  "year", "rip", "clad4", "dom4", "ateco_1", "Revenue_K", "strata" # Columns to go at the end
)

# Reorder the dataset columns
ict_combined <- ict_combined1 %>% dplyr::select(all_of(new_column_order))



summary(ict_combined)

##### Firms sizes classification by revenue #####


ict_combined$Ricavi <- as.numeric(as.character(ict_combined$Ricavi))

# Create size_revenue variable
# Adjusting the 'cut' function to include all values above 200 million as 'large'
ict_combined$size_revenue <- cut(ict_combined$Ricavi,
                                 breaks = c(0, 10000000, 50000000, Inf),
                                 labels = c("small", "medium", "large"),
                                 right = FALSE)

table(ict_combined$size_revenue)
table(ict_combined$clad4)

# Check entries around 10 million and 50 million
ict_combined %>%
  # Filtering entries where revenue is between 5 million and 20 million
  filter(Ricavi >= 50000000) %>%
  # Formatting the 'Ricavi' column to display values in a non-scientific format
  mutate(Ricavi = format(Ricavi, scientific = FALSE)) %>%
  # Grouping the data by 'size_revenue' to prepare for counting
  group_by(size_revenue) %>%
  # Summarising to count the number of entries in each 'size_revenue' category
  summarise(
    Count = n(),  # Counting number of entries
    # Displaying a few revenue values as examples
    Example_Revenues = paste(sample(Ricavi, size = min(5, n())), collapse = ", ")
  ) %>%
  # Optionally ungrouping the data if further ungrouped operations are needed
  ungroup()


# Create a contingency table
size_comparison <- table(ict_combined$clad4, ict_combined$size_revenue)

# Display the table
print(size_comparison)

# You might also want to calculate proportions or percentages to better understand the distribution
prop.table(size_comparison, margin = 1)  # Row Proportions
prop.table(size_comparison, margin = 2)  # Column Proportions


# Review the new factor variable
table(ict_combined$size_revenue)  # This will give you the count of each category


# Create a bar plot to visualize the comparison
ggplot(ict_combined, aes(x = clad4, fill = size_revenue)) +
  geom_bar(position = "dodge") +
  labs(title = "Comparison of Company Size Classification",
       x = "Size by Number of Employees",
       y = "Count of Companies",
       fill = "Size by Revenue")


counts_clad4_by_year <- ict_combined %>%
  dplyr::mutate(size = forcats::fct_recode(clad4,
                                           Small = "cl1",
                                           Medium = "cl2",
                                           Large = "cl3")) %>%
  dplyr::group_by(year, size) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(total = sum(count),
                props = round(count / total, 3)) 


counts_sizeR_by_year <- ict_combined %>%
  dplyr::group_by(year, size_revenue) %>%
  dplyr::summarise(count = n(), .groups = 'drop') %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(total = sum(count),
                props = round(count / total, 3)) 



size_wide_by_year <- counts_clad4_by_year %>%
  dplyr::select(-total, -props) %>% 
  pivot_wider(names_from = year, values_from = count)


sizeR_wide_by_year <- counts_sizeR_by_year %>%
  dplyr::select(-total, -props) %>% 
  pivot_wider(names_from = year, values_from = count)


#### Changing names of variable 
options(scipen=999)
ict_combined <- ict_combined %>% 
  rename(region = rip,
         size_emp = clad4,
         size_rev = size_revenue,
         ateco= ateco_1,
         ict = dom4,
         revenue = Ricavi,
         code = Codice)

##### Rename the factors for size 

ict_combined <- ict_combined %>%
    dplyr::mutate(size_emp = forcats::fct_recode(size_emp,
                                             small = "cl1",
                                             medium = "cl2",
                                             large = "cl3"))

#### create the SMEs column 

ict_combined <- ict_combined %>%
  mutate(sme_emp = fct_collapse(size_emp,
                                 sme = c("small", "medium")),
         sme_rev = fct_collapse(size_rev,
                                sme = c("small", "medium")))




##### Create Macro sectors column 

ict_combined <- ict_combined %>%
  mutate(mac_sec = factor(case_when(
    ateco %in% c("C", "F", "D_E") ~ "Indus",     # Industrial sector
    ateco %in% c("G", "L") ~ "Commer",           # Commercial sector
    ateco %in% c("H", "I", "M", "N") ~ "Serv",    # Service sector
    ateco %in% c("J", "S") ~ "Cre_Indus_&_ICT",   # Creative industries and ICT
    TRUE ~ NA_character_                         # Handle any unexpected codes
  ), levels = c("Indus", "Commer", "Serv", "Cre_Indus_&_ICT")))

ict_combined <- ict_combined %>%
  dplyr::mutate(sec_name = fct_recode(ateco,
                                      "Manufacturing" = "C",
                                      "Energy & WT" = "D_E",
                                      "Construction" = "F",
                                      "Wholesale Retail" = "G",
                                      "Transport" = "H",
                                      "Accommodation" = "I",
                                      "Media & Telecom" = "J",
                                      "Real Estate" = "L",
                                      "Prof, Sci & Tech" = "M",
                                      "Rental & Travel" = "N",
                                      "Tech Repair" = "S"))


ict_combined <- ict_combined %>%
  dplyr::mutate(region = fct_recode(region,
                                    Northwest = "ITC",
                                    South = "ITF",
                                    Insular = "ITG",
                                    Northeast = "ITH",
                                    Central = "ITI"))


# Assigning an unique code to each obesrvatino
ict_combined$code <- 1: nrow(ict_combined)

ict_combined <- ict_combined[, -12] ### removing redundant var from Skills


save(ict_combined, file = here("Data", "Processed", "ICT_Combined.rda"))

write.csv(ict_combined, file = here("Data", "Processed", "ICT_Combined.csv"), 
          row.names = FALSE)

#### This process will continue in the script name PCA.CI_DDtot.R
summary(ict_combined)






library(psych)

# Define the columns to exclude explicitly
excluded_columns <- c("ateco")
excluded_columns1 <- c("strata","size_emp", "size_rev", 
                       "sme_emp",  "sme_rev", "sec_name")

ict_combined$A2_A2 <- ict_combined$A2_A2/ 100
ict_combined$A2_C2 <- ict_combined$A2_C2/ 100
ict_combined$A2_C6 <- ict_combined$A2_C6/ 100


patt <- "^A|^S|^U|^size_|^sme_|^mac_|^year"  

ict_combined_tests <- process_data2FA(ict_combined, patt = patt, encode = T )


continuous_vars <- c("A2_A2", "A2_C2", "A2_C6")
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









