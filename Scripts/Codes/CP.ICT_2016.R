# load the necessary packages
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

##### 0.1. Load environment #####

load(here("Scripts", "Environments",  "missing_ict_16.RData"))


#### 1. Load Data ####

##### 1.1. Loads ATECO classification ####
ateco <- read_xlsx(here("Data", "Tags","ateco_survey.xlsx"))

ateco_b17 <- ateco %>% group_by(ateco_B17) %>% 
  summarise(details = paste(unique(details_sec1), collapse = "; "))

ateco_a17 <- ateco %>% group_by(ateco_A17) %>% 
  summarise(details = paste(unique(details_sec2), collapse = "; "))

##### 1.1. Loads sizes by No. of employees ##### 

size <- read_xlsx(here("Data", "Tags","size.xlsx"))

##### 1.2. Loads regions NUTS 1 ##### 

regions <- read_xlsx(here("Data", "Tags","regions_nuts1.xlsx"))

##### 1.3. Loads variables info ##### 
var_16 <- read_xlsx(here("Data", "Tags","vars_16.xlsx"))





##### 1.4. Load the ICT usage survey ##### 
# This line reads a text file "ICT_Microdati_Anno_2016.txt", located in the 
# "Data" folder, inside the project directory. 
# The "here" function is used to specify the relative path to the file. 
# The data from the Excel file is stored in a new R data frame called ICT_2016.
ICT_2016 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2016.txt"), header = TRUE, sep = "\t")
ict_16 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2016.txt"), header = TRUE, sep = "\t")

summary(ICT_2016 %>% dplyr::select( B1, B2a) %>% as.numeric())

numeric_B1 <- as.numeric(ICT_2016$B2a)
sum(numeric_B1, na.rm = TRUE)


numeric_B1 <- as.numeric(as.character(ict_16$B2a))
sum(numeric_B1, na.rm = TRUE)
sum(ict_16$B1)

summary(ict_16)

# Print the result
print(total_B1)

summary(as.numeric(ict_16$A2))

summary(ict_15_impT %>% dplyr::select( A2_A2, A2_C2, A2_C6))



###### 1.4.1. Visualize data,  uncover NAs  ######
vis_miss(ict_16, warn_large_data = FALSE)


### Converting all the hidden values into NA
ict_16 <- convert_and_clean2(ict_16)
summary(ict_16)
### Subsetting the skills varaibles to further imputations

ict_16 <- ict_16 %>%
  mutate(ateco_1 = case_when(
    dom1 %in% c("N01", "N02", "N03", "N04", "N05", "N06", "N07", "N08", "N09") ~ "C",
    dom1 %in% c("N10") ~ "D_E",
    dom1 %in% c("N11") ~ "F",
    dom1 %in% c("N12", "N13", "N14") ~ "G",
    dom1 %in% c("N15", "N16") ~ "H",
    dom1 %in% c("N17", "N18") ~ "I",
    dom1 %in% c("N19", "N20", "N21", "N22") ~ "J",
    dom1 %in% c("N23") ~ "L",
    dom1 %in% c("N24") ~ "M",
    dom1 %in% c("N25", "N26") ~ "N",
    dom1 %in% c("N27") ~ "S",
    TRUE ~ NA_character_
  ), ateco_1 = factor(ateco_1))

## Collapsing the size into three levels
ict_16$clad4 <- forcats::fct_collapse(ict_16$clad4,
                                      cl1 = "cl1",
                                      cl2 = c("cl2", "cl3"),
                                      cl3 = "cl4")

colnames(ICT_2016)

var_map <- read_xlsx(here("Data", "Processed","vars&codes.xlsx"))

vars16 <- var_map$acrom_3

valid_vars16 <- vars16[vars16 %in% colnames(ict_16)]

ICT_2016Rdux <- ict_16[, valid_vars16]

vis_miss(ICT_2016Rdux, warn_large_data = FALSE)

save(ICT_2016Rdux, file = here("Data", "Processed", "ICT_2016Rdux.rda"))




ict_skills_16 <- ict_16 %>% 
  dplyr::select(codice, clad4, ateco_1, rip, C4, C10a, C10c, B5a, 
                B5b, B5c, B5d, B5e, B5f, B5g) %>% 
  mutate(year = 2016,
         year = as.factor(year))

table(ict_skills_16$clad4)
summary(ict_skills_16)


### Check the length of the raw data vs the final data
## for 2014 the raw has 18953 but the final data has 18832
expected_sequence <- 1:19089

# Extract the actual series from the Codice column
actual_series <- ict_16_impT$Codice

# Identify missing values
missing_values <- setdiff(expected_sequence, actual_series)

# Check if there are any missing values and print them
if (length(missing_values) == 0) {
  print("The Codice column contains the full series from 1 to 18953.")
} else {
  print("The Codice column is missing the following values:")
  print(missing_values)
}


ict_skills_16 <- ict_skills_16 %>% 
  filter(!codice %in% missing_values)

save(ict_skills_16, file = here("Data", "Processed", "ict_skills_16.rda"))


### Visualize NA
vis_miss(ict_16, warn_large_data = FALSE)

# creating a copy of ict_14c c meaning cleaned
missing_count <- sapply(ict_16, function(x) sum(is.na(x)))
var_16$missing <- round((sum(is.na(ict_16)) / nrow(ict_16)) * 100,2)
ict_16c <- ict_16

summary(ict_16c$B5a)


# Overwriting the 'B5a' column with the recoded values and converting it back to a factor
ict_16c$B5a <- factor(ifelse(ict_16c$B5a == '1', 0,
                             ifelse(ict_16c$B5a == '2', 1,
                                    ifelse(ict_16c$B5a == '3', NA, NA))),
                      levels = c(0, 1))

summary(ict_16c$B5a)

nlevels(ict_16c$C6)

summary(ict_16)



str(ict_16c)
###### 1.4.2. Change var types  ######
## Converting all variables in factor to identify the var types and write them
# in excel for further use 
# ict_16c <- ict_16c %>%
#   # Convert variable 1 to character
#   mutate_at(vars(1:162), as.factor)

# mplement the Data Type Changes using a for loop using the vars_16
for (i in 1:nrow(var_16)) {
  current_var_name <- var_16$acrom[i]
  current_var_type <- var_16$type[i]
  
  # Check if the variable name from var_16 exists in ict_16c
  if (current_var_name %in% colnames(ict_16c)) {
    # Change the data type based on the type specified in var_16
    if (current_var_type == "binary" || current_var_type == "levels") {
      ict_16c[[current_var_name]] <- as.factor(ict_16c[[current_var_name]])
    } else if (current_var_type == "numeric") {
      ict_16c[[current_var_name]] <- as.numeric(as.character(ict_16c[[current_var_name]]))
    } else if (current_var_type == "character") {
      ict_16c[[current_var_name]] <- as.character(ict_16c[[current_var_name]])
    }
  }
}


# Check the summary and the var types 
summary(ict_16c)
str(ict_16c)

vis_miss(ict_16c, warn_large_data = FALSE)

###### 1.4.3.  Count missingness by var  ######
# Identify missing values
missing_values <- is.na(ict_16c)

# Count missing values for each column
missing_count <- colSums(missing_values)

# Calculate missing percentage for each column
miss_per <- round((missing_count / nrow(ict_16c)) * 100,2)

# Subset columns that have missing values and their missing percentages
missing_info <- data.frame(var = names(ict_16c)[missing_count > 0],
                           miss_per = miss_per[missing_count > 0])

## Creates a df that informs what to do with the variables with high % missing, 
## there are many variables that has low missingness meaning there are some rows
# with high missingness rate 
missing_info <- missing_info %>%
  left_join(var_16, by = c("var" = "acrom")) %>%
  mutate(
    missing_count = round((miss_per / 100) * nrow(ict_16c)), 
    action = case_when(
      miss_per > 60 ~ "remove",
      missing_count < 0.01 * nrow(ict_16c) ~ "remove_row",
      TRUE ~ "inform imputation"
    )
  ) %>%
  select(var, name_EN, type, miss_per, missing_count, action)

###### 1.4.4. Removing high % missing and others  ######

# List of columns to remove
cols_to_remove <- missing_info$var[missing_info$action == "remove"]

# Remove the columns
cols_to_remove_safe <- intersect(cols_to_remove, names(ict_16c))
ict_16c <- ict_16c %>% select(-all_of(cols_to_remove_safe))

vis_miss(ict_16c, warn_large_data = FALSE)

# Remove rows where missing values are > 80%
threshold <- 0.16 * ncol(ict_16c)
ict_16c <- ict_16c %>% 
  filter(rowSums(is.na(.)) < threshold)

vis_miss(ict_16c, warn_large_data = FALSE)


## Creates a df that informs the new set of variables for impute treatment
# Recalculating missing info for remaining columns in ict_16c
new_missing_info <- ict_16c %>%
  summarise_all(function(x) mean(is.na(x) * 100)) %>%
  gather(var, miss_per) %>%
  filter(miss_per > 0)

# Rounding miss_per to 2 decimal places
new_missing_info$miss_per <- round(new_missing_info$miss_per, 2)

# Calculating missing count
new_missing_info$miss_count <- round((new_missing_info$miss_per / 100) * nrow(ict_16c))

# Left join with var_16 for additional variable information
new_missing_info <- new_missing_info %>%
  left_join(var_16, by = c("var" = "acrom")) %>%
  mutate(
    action = case_when(
      miss_per > 60 ~ "remove",
      miss_count < 0.01 * nrow(ict_16c) ~ "inform imputation",
      TRUE ~ "inform imputation"
    )
  ) %>%
  select(var, type, name_EN,  miss_per, miss_count, action)

ict_16c <- ict_16c %>% select(-c(mac, dom2, coeffin))

###### 1.4.5. Creating Ateco column  ######
# Create a new column called 'ateco_1' in the 'ict_16c' data frame and assign it the result of the modification done to the initial 'ict_14c' data frame.
# This modification uses the dplyr package to apply 'mutate' to the data frame and add cases to the column 'ateco_1' based on the values of 'dom1' column.
# Depending on the value categories in 'dom1', 'ateco_1' adds a new letter category to that row.
# If 'dom1' is not in any of the defined categories, 'ateco_1' value will be NA.
ict_16c <- ict_16c %>%
  mutate(ateco_1 = case_when(
    dom1 %in% c("N01", "N02", "N03", "N04", "N05", "N06", "N07", "N08", "N09") ~ "C",
    dom1 %in% c("N10") ~ "D_E",
    dom1 %in% c("N11") ~ "F",
    dom1 %in% c("N12", "N13", "N14") ~ "G",
    dom1 %in% c("N15", "N16") ~ "H",
    dom1 %in% c("N17", "N18") ~ "I",
    dom1 %in% c("N19", "N20", "N21", "N22") ~ "J",
    dom1 %in% c("N23") ~ "L",
    dom1 %in% c("N24") ~ "M",
    dom1 %in% c("N25", "N26") ~ "N",
    dom1 %in% c("N27") ~ "S",
    TRUE ~ NA_character_
  ), ateco_1 = factor(ateco_1))


## This will crate a variables Renenue_K that express the revenue in thousands
options(scipen=999)
ict_16c <- ict_16c %>%
  mutate(
    Revenue_K = ifelse(as.numeric(as.character(ricavi)) != 0, 
                       as.numeric(as.character(ricavi)) / 1000, 0),
    Revenue_K = as.factor(Revenue_K) %>% 
      droplevels()
  )

ict_16c <-  droplevels(ict_16c)

ict_16c0 <- ict_16c
ict_16c <- ict_16c0


## loading the variable map for all the years 
var_map <- read_xlsx(here("Data", "Processed","var_map.xlsx"))
vars_dd16 <- var_map$acrom_3
### filtering variables from the var_map
ict_16c <- ict_16c[, vars_dd16]
colnames(ict_16c) <-  var_map$var_dd3

vis_miss(ict_16c, warn_large_data = FALSE)

## Collapsing the size into three levels
ict_16c$clad4 <- forcats::fct_collapse(ict_16c$clad4,
                                       cl1 = "cl1",
                                       cl2 = c("cl2", "cl3"),
                                       cl3 = "cl4")

summary(ict_16c)

summary(ict_16c$clad4)

low_variance14 <- c("S_B3",	"A2_C1",	"A2_C3",	
                    "A2_C4",	"UC_C9b",	"UMK_C9d",	
                    "UMK_C9e",	"UC_H1",	"UC_H6")

ict_16c <- ict_16c %>% dplyr::select(-all_of(low_variance14))

names(ict_16c)
summary(ict_15c0)
str(ict_16c)


###### 1.4.6. One-hot encoding of variables####

ict_16c <- dummy_cols(ict_16c, select_columns = c("clad4", "rip", 
                                                  "ateco_1","Revenue_K"), 
                      remove_selected_columns = F, remove_first_dummy = T)

ict_16c[,c(25:53)] <- lapply(ict_16c[,c(25:53)], as.factor)

#### 2. VISUALIZATOINS of Size, sector and region ####

# Grouping by 'clad4', 'dom1', and 'region'
levels_clad4 = c("cl1", "cl2", "cl3")
labels_clad4 = c("Small", "Medium", "Large")

ict_16c %>%
  group_by(clad4) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = factor(clad4, levels = levels_clad4, labels = labels_clad4), y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Company Size", x = "Company Size", y = "Count") +
  theme_minimal()



ict_16c %>%
  group_by(ateco_1) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = ateco_1, y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Secors", x = "Sectors", y = "Count") +
  theme_minimal()


levels_rip4 = c("ITC", "ITF", "ITG", "ITH", "ITI")
labels_rip4 = c("Northwest", "South", "Iisland", "Northeast", "Center")

ict_16c %>%
  group_by(rip) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = factor(rip, levels = levels_rip4, labels = labels_rip4), y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Region", x = "Region", y = "Count") +
  theme_minimal()



##### 2.1. ONE VAR VIZ FOR BINARY #####


title_bin <- "A1_C9f"

# take into account which data set ict_14c or ict_14cP
plot_binary_var(ict_16c, 
                variable_name =  title_bin,
                title_name = title_bin,
                x_var = "ateco_1",
                facet_var = "clad4",
                ncol = 1 )


##### 2.2. ONE VAR VIZ FOR CONTINUOUS  #####

title_con <- "A2_C6"

plot_continuous_var(ict_16c, title_con,
                    title_name = title_con,
                    x_var = "rip",
                    facet_var = "clad4",
                    ncol = 1)

ggplot(ict_16c, aes(x= A2_A2)) + geom_histogram() + facet_wrap(~ ateco_1)

var(na.omit(ict_16c$A2_A2))

summary(ict_16c$A2_A2)

##### 2.3. ONE VAR VIZ FOR CATEGORICAL  #####

title_cat <- "S-B5a"

plot_categorical_var(ict_16c, title_cat,
                     title_name = title_cat,
                     x_var = "ateco_1",
                     facet_var = "clad4",
                     ncol = 1)


#### 3. LOW MISSIN % VARS ####

# Step 1: Get column names of ict_16c
col_names <- names(ict_16c)

# Step 2: Match column names with their descriptions from var_map
# Ensure that var_map$var_dd2 and col_names are both of the same type (e.g., character)
descriptions <- var_map$name_EN[match(col_names, var_map$var_dd3)]

# Step 3: Calculate the percentage of missing values for each column in ict_16c
miss_per <- sapply(ict_16c, function(col) {
  sum(is.na(col)) / nrow(ict_16c) * 100
})

# Step 4: Combine everything into a new data frame
missng_dd_16 <- data.frame(
  Name = col_names,
  Desc = descriptions,
  Miss_Per = miss_per)


##### 3.1. Model 2: Imputation with rf #####


pattern_lmv <- "^(ateco_1_|clad4_|rip_|UMK_C8$|A2_C5$|A2_A2$|S_B1$|S_B2a$|S_B5a$)"

## Search for the pattern in the df
lmv_cov <- grep(pattern_lmv, names(ict_16c), value = TRUE)

summary(ict_16c$S_B5a)

## Run the imputation model 
gc()
imp_model_lmv1 <- mice(ict_16c[, lmv_cov],
                       method = "rf", 
                       m = 5, # Number of imputed datasets. 5 is a common choice.
                       maxit = 15) # Number of iterations
## Check the predictor matrix
m <- imp_model_lmv1$predictorMatrix
## Check the traceplots
plot_trace(imp_model_lmv1)

##### 3.2. Model 2: Imputation with cart #####

gc()

## Run the imputation model 
imp_model_lmv2 <- mice(ict_16c[, lmv_cov],
                       method = "cart", 
                       m = 5, # Number of imputed datasets. 5 is a common choice.
                       maxit = 15) # Number of iterations
## Check the predictor matrix
m <- imp_model_lmv2$predictorMatrix
## Check the traceplots
plot_trace(imp_model_lmv2)

##### 3.3. IMPUTATION DIAGNOSTICS LMV #####

###### 3.3.1. Model with original data ######

summary(ict_16c)

evaluate_original_data(ict_16c, "UM_E1", c("UMK_C8", "A2_C5", "S_B5a") )

###### 3.3.2. Best Model  ######

# list of imputed models 
lmv_models <- list(rf = imp_model_lmv1, 
                   cart = imp_model_lmv2)

## Get the best results from the best imputation methods
## across the imputed datasets
lmv_best <- compare_imputation_methods(models_list = lmv_models,
                                       data = ict_16c,
                                       target = "UM_E1",
                                       predictors = c("UMK_C8", "A2_C5", "S_B5a"))
# Retrieve best method 
lmv_best$method
# Retrieve the best imputed dataset
lmv_best$result$action

completed_data_lmv <- complete(imp_model_lmv1, action = 4)



#### 3. WEBSITE USE (WS) ####


# This code will help you identify any patterns or concentrations in the dom1 
# and rip variables among the observations with missing values in the seven ICT 
# variables. The bar plots will visually present the distribution of missing 
# cases across different sectors and regions.
# 
# If there are specific sectors or regions that have a noticeably higher count 
# of missing cases, it could suggest a relationship between those characteristics 
# and the missingness in the ICT variables.

# Companies services offered by the website.  
# Ricavi. Is the revenue by levels. There are 14 levels
# dom4. Represent the 27 economic sectors Ateco before 2017
# rip. Represent the 5 regions in Italy NUTS 1
# C8a. Possibility to place orders or reservations online (e.g. online shopping cart)
# C8c. Access to product catalogues or price lists
# C8g. Announcement of job vacancies or possibility of making job applications online
# C8h. Links or references to the company's social media profiles


##### 3.1. WS Visualizations ###### 

###### 3.1.1. Plot by size ###### 


# Calling Website variables starting with C8_ to impute 
ws_vars <- grep("C9.", names(ict_16c), value = TRUE)

ws_title <- "Website vars"

plot_binary_var(ict_16c, 
                variable_name =  ws_vars,
                title_name = ws_title,
                x_var = "ateco_1",
                facet_var = "clad4",
                ncol = 1 )


###### 3.1.2. Plot by region #####

plot_binary_var(ict_16c, 
                variable_name =  ws_vars,
                title_name = ws_title,
                x_var = "ateco_1",
                facet_var = "rip",
                ncol = 1 )

###### 3.1.3. Plot by revenue  #####

plot_binary_var(ict_16c, 
                variable_name =  ws_vars,
                title_name = ws_title,
                x_var = "ateco_1",
                facet_var = "Revenue_K",
                ncol = 5 )

##### 3.2. WS IMPUTATİON ###### 

# Covariate
# B5a Using IT specialists who are part of the business group
# B1 	Employment of IT specialists
# C9c SM usage by type: multimedia content sharing websites

###### 3.2.1. Model 1: Imputation with logreg #####

## Set the pattern which contains the covariates

pattern_ws <- "^(ateco_1_|clad4_|rip_|A2_A2$|S_B1$|S_B2a$)|C9."

## Search for the pattern in the df
ws_cov <- grep(pattern_ws, names(ict_16c), value = TRUE)

## Run the imputation model 
imp_model_ws1 <- mice(ict_16c[, ws_cov],
                      method = "rf", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations
## Check the predictor matrix
m <- imp_model_ws1$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws1)



###### 3.2.2. Model 2: Imputation with pmm #####

## Run the imputation model 
imp_model_ws2 <- mice(ict_16c[, ws_cov],
                      method = "logreg", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations

## Check the predictor matrix
m <- imp_model_ws2$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws2)

###### 3.2.3. Model 3: Imputation with cart #####
gc()
imp_model_ws3 <- mice(ict_16c[, ws_cov],
                      method = "cart",
                      m = 5, 
                      maxit = 15) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ws3$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws3)

##### 3.3. IMPUTATION DIAGNOSTICS WS #####


summary(ict_16c)

evaluate_original_data(ict_16c, "UC_H9", ws_vars)



ws_models <- list(rf = imp_model_ws1, 
                  logreg = imp_model_ws2, 
                  cart = imp_model_ws2)

ws_best <- compare_imputation_methods(models_list = ws_models,
                                      data = ict_16c,
                                      target = "UC_H9",
                                      predictors = ws_vars)


# Retrieve best method 
ws_best$method
# Retrieve the best imputed dataset
ws_best$result$action

completed_data_ws <- complete(imp_model_ws2, action = 4)



### 5. INTEGRATION IMP DATA SETS   ####

imputed_datasets_list <- list(completed_data_lmv,
                              completed_data_ws)


imputed_cols  <- missng_dd_16 %>%
  filter(Miss_Per >= 3, Miss_Per <= 24.0) %>% # , type == "binary"
  dplyr::pull(Name)

ict_16_impT <- integrate_imputed_data(original_data = ict_16c, 
                                      imputed_datasets_list, 
                                      imputed_cols)

ict_16_impT <- ict_16_impT[, -c( 25:53)]
vis_miss(ict_16_impT, warn_large_data = FALSE)

summary(ict_16_impT)


save(ict_16_impT, file = here("Data", "Processed", "ICT16.rda"))
save(ict_16c0, file = here("Data", "Processed", "ict_16c0.rda"))





