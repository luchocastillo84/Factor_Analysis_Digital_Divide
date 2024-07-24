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
library(forcats)

##### 0.1. Load environment #####

load(here("Scripts", "Environments",  "missing_ict_17.RData"))


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
var_17 <- read_xlsx(here("Data", "Tags","vars_17.xlsx"))

##### 1.4. Load the ICT usage survey ##### 
# This line reads a text file "ICT_Microdati_Anno_2017.txt", located in the 
# "Data" folder, inside the project directory. 
# The "here" function is used to specify the relative path to the file. 
# The data from the Excel file is stored in a new R data frame called ICT_2017.
ICT_2017 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2017.txt"), header = TRUE, sep = "\t")
ict_17 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2017.txt"), header = TRUE, sep = "\t")



summary(as.numeric(ICT_2017$A2_))
summary(as.numeric(ICT_2017$C2_))
summary(as.numeric(ICT_2017$C7_))

summary(ict_17_impT %>% dplyr::select( A2_A2_, A2_C2_, A2_C7_))



ict_17 <- rename(ict_17, clad4 = clad3, ateco_1 = Ateco_1)

###### 1.4.1. Visualize data,  uncover NAs  ######
vis_miss(ict_17, warn_large_data = FALSE)

### Converting all the hidden values into NA
ict_17 <- convert_and_clean2(ict_17)

### Subsetting the skills varaibles to further imputations

colnames(ICT_2017)

var_map <- read_xlsx(here("Data", "Processed","vars&codes.xlsx"))

vars17 <- var_map$acrom_4

valid_vars17 <- vars17[vars17 %in% colnames(ict_17)]

ICT_2017Rdux <- ict_17[, valid_vars17]

vis_miss(ICT_2017Rdux, warn_large_data = FALSE)

save(ICT_2017Rdux, file = here("Data", "Processed", "ICT_2017Rdux.rda"))



summary(ict_17)

ict_skills_17 <- ict_17 %>% 
  dplyr::select(codice_, clad4, ateco_1, rip, C4, C10a, C10c) %>% 
  mutate(B5a = NA, 
         B5b = NA, 
         B5c = NA,
         B5d = NA,
         B5e = NA,
         B5f = NA, 
         B5g = NA,
         year = 2017,
         year = as.factor(year)) %>% 
  rename(codice = codice_)

table(ict_skills_17$clad4)
summary(ict_skills_17)

### Check the length of the raw data vs the final data
## for 2014 the raw has 18953 but the final data has 18832
expected_sequence <- 1:21410

# Extract the actual series from the Codice column
actual_series <- ict_17_impT$Codice

# Identify missing values
missing_values <- setdiff(expected_sequence, actual_series)

# Check if there are any missing values and print them
if (length(missing_values) == 0) {
  print("The Codice column contains the full series from 1 to 18953.")
} else {
  print("The Codice column is missing the following values:")
  print(missing_values)
}


ict_skills_17 <- ict_skills_17 %>% 
  filter(!codice %in% missing_values)

# Combine "D" and "E" into "D_E" and remove the old levels
ict_skills_17$ateco_1 <- fct_collapse(ict_skills_17$ateco_1,
                                    D_E = c("D", "E"))


save(ict_skills_17, file = here("Data", "Processed", "ict_skills_17.rda"))


### Visualize NA
vis_miss(ict_17, warn_large_data = FALSE)

# creating a copy of ict_14c c meaning cleaned
# creating a copy of ict_14c c meaning cleaned
missing_count <- sapply(ict_17, function(x) sum(is.na(x)))
var_17$missing <- round((sum(is.na(ict_17)) / nrow(ict_17)) * 100,2)

ict_17c <- ict_17

nlevels(ict_17c$C6)

summary(ict_17c, Inf)
str(ict_17c)
###### 1.4.2. Change var types  ######
## Converting all variables in factor to identify the var types and write them
# in excel for further use 
# ict_17c <- ict_17c %>%
#   # Convert variable 1 to character
#   mutate_at(vars(1:162), as.factor)

# mplement the Data Type Changes using a for loop using the vars_17
for (i in 1:nrow(var_17)) {
  current_var_name <- var_17$acrom[i]
  current_var_type <- var_17$type[i]
  
  # Check if the variable name from var_17 exists in ict_17c
  if (current_var_name %in% colnames(ict_17c)) {
    # Change the data type based on the type specified in var_17
    if (current_var_type == "binary" || current_var_type == "levels") {
      ict_17c[[current_var_name]] <- as.factor(ict_17c[[current_var_name]])
    } else if (current_var_type == "numeric") {
      ict_17c[[current_var_name]] <- as.numeric(as.character(ict_17c[[current_var_name]]))
    } else if (current_var_type == "character") {
      ict_17c[[current_var_name]] <- as.character(ict_17c[[current_var_name]])
    }
  }
}


# Check the summary and the var types 
summary(ict_17c)
str(ict_17c)

vis_miss(ict_17c, warn_large_data = FALSE)

###### 1.4.3.  Count missingness by var  ######
# Identify missing values
missing_values <- is.na(ict_17c)

# Count missing values for each column
missing_count <- colSums(missing_values)

# Calculate missing percentage for each column
miss_per <- round((missing_count / nrow(ict_17c)) * 100,2)

# Subset columns that have missing values and their missing percentages
missing_info <- data.frame(var = names(ict_17c)[missing_count > 0],
                           miss_per = miss_per[missing_count > 0])

## Creates a df that informs what to do with the variables with high % missing, 
## there are many variables that has low missingness meaning there are some rows
# with high missingness rate 
missing_info <- missing_info %>%
  left_join(var_17, by = c("var" = "acrom")) %>%
  mutate(
    missing_count = round((miss_per / 100) * nrow(ict_17c)), 
    action = case_when(
      miss_per > 60 ~ "remove",
      missing_count < 0.01 * nrow(ict_17c) ~ "remove_row",
      TRUE ~ "inform imputation"
    )
  ) %>%
  dplyr::select(var, name_EN, type, miss_per, missing_count, action)

###### 1.4.4. Removing high % missing and others  ######

# List of columns to remove
cols_to_remove <- missing_info$var[missing_info$action == "remove"]

# Remove the columns
cols_to_remove_safe <- intersect(cols_to_remove, names(ict_17c))
ict_17c <- ict_17c %>% dplyr::select(-all_of(cols_to_remove_safe))

vis_miss(ict_17c, warn_large_data = FALSE)

# Remove rows where missing values are > 80%
threshold <- 0.8 * ncol(ict_17c)
ict_17c <- ict_17c %>% 
  filter(rowSums(is.na(.)) < threshold)

vis_miss(ict_17c, warn_large_data = FALSE)


## Creates a df that informs the new set of variables for impute treatment
# Recalculating missing info for remaining columns in ict_17c
new_missing_info <- ict_17c %>%
  summarise_all(function(x) mean(is.na(x) * 100)) %>%
  gather(var, miss_per) %>%
  filter(miss_per > 0)

# Rounding miss_per to 2 decimal places
new_missing_info$miss_per <- round(new_missing_info$miss_per, 2)

# Calculating missing count
new_missing_info$miss_count <- round((new_missing_info$miss_per / 100) * nrow(ict_17c))

# Left join with var_17 for additional variable information
new_missing_info <- new_missing_info %>%
  left_join(var_17, by = c("var" = "acrom")) %>%
  mutate(
    action = case_when(
      miss_per > 60 ~ "remove",
      miss_count < 0.01 * nrow(ict_17c) ~ "inform imputation",
      TRUE ~ "inform imputation"
    )
  ) %>%
  dplyr::select(var, type, name_EN,  miss_per, miss_count, action)

ict_17c <- ict_17c %>% dplyr::select(-c(mac, coeffin))

# ###### 1.4.5. Creating Ateco column  ######
# # Create a new column called 'Ateco_1' in the 'ict_17c' data frame and assign it the result of the modification done to the initial 'ict_14c' data frame.
# # This modification uses the dplyr package to apply 'mutate' to the data frame and add cases to the column 'Ateco_1' based on the values of 'dom1' column.
# # Depending on the value categories in 'dom1', 'Ateco_1' adds a new letter category to that row.
# # If 'dom1' is not in any of the defined categories, 'Ateco_1' value will be NA.
# ict_17c <- ict_17c %>%
#   mutate(Ateco_1 = case_when(
#     dom1 %in% c("N01", "N02", "N03", "N04", "N05", "N06", "N07", "N08", "N09") ~ "C",
#     dom1 %in% c("N10") ~ "D_E",
#     dom1 %in% c("N11") ~ "F",
#     dom1 %in% c("N12", "N13", "N14") ~ "G",
#     dom1 %in% c("N15", "N16") ~ "H",
#     dom1 %in% c("N17", "N18") ~ "I",
#     dom1 %in% c("N19", "N20", "N21", "N22") ~ "J",
#     dom1 %in% c("N23") ~ "L",
#     dom1 %in% c("N24") ~ "M",
#     dom1 %in% c("N25", "N26") ~ "N",
#     dom1 %in% c("N27") ~ "S",
#     TRUE ~ NA_character_
#   ), Ateco_1 = factor(Ateco_1))
# 

## This will crate a variables Renenue_K that express the revenue in thousands
options(scipen=999)
ict_17c <- ict_17c %>%
  mutate(
    Revenue_K = ifelse(as.numeric(as.character(ricavi_cl)) != 0, 
                       as.numeric(as.character(ricavi_cl)) / 1000, 0),
    Revenue_K = as.factor(Revenue_K) %>% 
      droplevels()
  )

ict_17c <-  droplevels(ict_17c)

ict_17c0 <- ict_17c
ict_17c <- ict_17c0


### Create a empty column to store the interpolation 

## B5a use of employees of the company or persons who are part of the group of companies to carry out ict functions

ict_17c$B5a <- NA

## loading the variable map for all the years 
var_map <- read_xlsx(here("Data", "Processed","var_map.xlsx"))
vars_dd17 <- na.omit(var_map$acrom_4)
### filtering variables from the var_map
ict_17c <- ict_17c[, vars_dd17]
colnames(ict_17c) <-  na.omit(var_map$var_dd4)

setdiff(vars_dd17, colnames(ict_17c))

vis_miss(ict_17c, warn_large_data = FALSE)


summary(ict_17c)

summary(ict_17c$clad4)

low_variance14 <- c("S_B3",	"A2_C1",	"A2_C3",	
                    "A2_C4",	"UC_C9b",	"UMK_C9d",	
                    "UMK_C9e",	"UC_G1",	"UC_G8")

ict_17c <- ict_17c %>% dplyr::select(-all_of(low_variance14))

names(ict_17c)
summary(ict_17c)
str(ict_17c)


###### 1.4.6. One-hot encoding of variables####

ict_17c <- dummy_cols(ict_17c, select_columns = c("clad4", 
                                                  "rip", "ateco_1","Revenue_K"), 
                      remove_selected_columns = F, remove_first_dummy = T)

ict_17c[,c(25:56)] <- lapply(ict_17c[,c(25:56)], as.factor)

#### 2. VISUALIZATOINS of Size, sector and region ####

# Grouping by 'clad4', 'dom1', and 'region'
levels_clad4 = c("cl1", "cl2", "cl3")
labels_clad4 = c("Small", "Medium", "Large")

ict_17c %>%
  group_by(clad4) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = factor(clad4, levels = levels_clad4, labels = labels_clad4), y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Company Size", x = "Company Size", y = "Count") +
  theme_minimal()



ict_17c %>%
  group_by(ateco_1) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = ateco_1, y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Secors", x = "Sectors", y = "Count") +
  theme_minimal()


levels_rip4 = c("ITC", "ITF", "ITG", "ITH", "ITI")
labels_rip4 = c("Northwest", "South", "Iisland", "Northeast", "Center")

ict_17c %>%
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
plot_binary_var(ict_17c, 
                variable_name =  title_bin,
                title_name = title_bin,
                x_var = "ateco_1",
                facet_var = "clad4",
                ncol = 1 )


##### 2.2. ONE VAR VIZ FOR CONTINUOUS  #####

title_con <- "A2_C7_"

plot_continuous_var(ict_17c, title_con,
                    title_name = title_con,
                    x_var = "rip",
                    facet_var = "clad4",
                    ncol = 1)

ggplot(ict_17c, aes(x= log(A2_C7_))) + geom_histogram() + facet_wrap(~ ateco_1)

var(na.omit(ict_17c$A2_C7_))

summary(ict_17c$A2_C7_)

##### 2.3. ONE VAR VIZ FOR CATEGORICAL  #####

title_cat <- "S_B5a"

plot_categorical_var(ict_17c, title_cat,
                     title_name = title_cat,
                     x_var = "ateco_1",
                     facet_var = "clad4",
                     ncol = 1)


#### 3. LOW MISSIN % VARS ####


# Remove rows where missing values are > 80%
threshold <- 0.15* ncol(ict_17c)
ict_17c <- ict_17c %>% 
  filter(rowSums(is.na(.)) < threshold)
summary(ict_17c)

# Step 1: Get column names of ict_17c
col_names <- names(ict_17c)

# Step 2: Match column names with their descriptions from var_map
# Ensure that var_map$var_dd2 and col_names are both of the same type (e.g., character)
descriptions <- var_map$name_EN[match(col_names, var_map$var_dd4)]

# Step 3: Calculate the percentage of missing values for each column in ict_17c
miss_per <- sapply(ict_17c, function(col) {
  sum(is.na(col)) / nrow(ict_17c) * 100
})

# Step 4: Combine everything into a new data frame
missng_dd_17 <- data.frame(
  Name = col_names,
  Desc = descriptions,
  Miss_Per = miss_per)

## Removing single na observations 
na <- which(is.na(ict_17c$A2_C7_))
ict_17c <- ict_17c[-na,]

na <- which(is.na(ict_17c$dom4))
ict_17c <- ict_17c[-na,]

ict_17c <- ict_17c[, -c(42,56)]
ict_17c <- droplevels(ict_17c)

summary(ict_17c$ateco_1)
vis_miss(ict_17c, warn_large_data = FALSE)

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
ws_vars <- grep("C9.", names(ict_17c), value = TRUE)

ws_title <- "Website vars"

plot_binary_var(ict_17c, 
                variable_name =  ws_vars,
                title_name = ws_title,
                x_var = "ateco_1",
                facet_var = "clad4",
                ncol = 1 )


###### 3.1.2. Plot by region #####

plot_binary_var(ict_17c, 
                variable_name =  ws_vars,
                title_name = ws_title,
                x_var = "ateco_1",
                facet_var = "rip",
                ncol = 1 )

###### 3.1.3. Plot by revenue  #####

plot_binary_var(ict_17c, 
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
ws_cov <- grep(pattern_ws, names(ict_17c), value = TRUE)

## Run the imputation model 
imp_model_ws1 <- mice(ict_17c[, ws_cov],
                      method = "rf", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations
## Check the predictor matrix
m <- imp_model_ws1$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws1)



###### 3.2.2. Model 2: Imputation with pmm #####

## Run the imputation model 
imp_model_ws2 <- mice(ict_17c[, ws_cov],
                      method = "logreg", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations

## Check the predictor matrix
m <- imp_model_ws2$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws2)

###### 3.2.3. Model 3: Imputation with cart #####
gc()
imp_model_ws3 <- mice(ict_17c[, ws_cov],
                      method = "cart",
                      m = 5, 
                      maxit = 15) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ws3$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws3)

##### 3.3. IMPUTATION DIAGNOSTICS WS #####


summary(ict_17c)

evaluate_original_data(ict_17c, "UM_D1", ws_vars)



ws_models <- list(rf = imp_model_ws1, 
                  logreg = imp_model_ws2, 
                  cart = imp_model_ws2)

ws_best <- compare_imputation_methods(models_list = ws_models,
                                      data = ict_17c,
                                      target = "UM_D1",
                                      predictors = ws_vars)


# Retrieve best method 
ws_best$method
# Retrieve the best imputed dataset
ws_best$result$action

completed_data_ws <- complete(imp_model_ws2, action = 4)



### 5. INTEGRATION IMP DATA SETS   ####

imputed_datasets_list <- list(completed_data_ws)


imputed_cols  <- missng_dd_17 %>%
  filter(Miss_Per >= 10, Miss_Per <= 24.0) %>% # , type == "binary"
  dplyr::pull(Name)

ict_17_impT <- integrate_imputed_data(original_data = ict_17c, 
                                      imputed_datasets_list, 
                                      imputed_cols)

ict_17_impT <- ict_17_impT[, 1:24]

# Combine "D" and "E" into "D_E" and remove the old levels
ict_17_impT$ateco_1 <- fct_collapse(ict_17_impT$ateco_1,
                                    D_E = c("D", "E"))

# Drop the old factor levels that are now unused
ict_17_impT$ateco_1 <- droplevels(ict_17_impT$ateco_1)

vis_miss(ict_17_impT, warn_large_data = FALSE)

summary(ict_17_impT$ateco_1)


save(ict_17_impT, file = here("Data", "Processed", "ICT17.rda"))
save(ict_17c0, file = here("Data", "Processed", "ict_17c0.rda"))





