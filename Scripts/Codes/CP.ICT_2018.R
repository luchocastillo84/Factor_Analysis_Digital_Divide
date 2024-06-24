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

load(here("Scripts", "Environments",  "missing_ict_18.RData"))


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
var_18 <- read_xlsx(here("Data", "Tags","vars_18.xlsx"))

##### 1.4. Load the ICT usage survey ##### 
# This line reads a text file "ICT_Microdati_Anno_2018.txt", located in the 
# "Data" folder, inside the project directory. 
# The "here" function is used to specify the relative path to the file. 
# The data from the Excel file is stored in a new R data frame called ICT_2018.
ICT_2018 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2018.txt"), header = TRUE, sep = "\t")
ict_18 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2018.txt"), header = TRUE, sep = "\t")

summary(as.numeric(ICT_2018$A3_))
summary(as.numeric(ICT_2018$C2_))
summary(as.numeric(ICT_2018$C6_))

summary(ict_18_impT %>% dplyr::select( A2_A3_, A2_C2_, A2_C6_))



ict_18 <- rename(ict_18, clad4 = clad3, ateco_1 = Ateco_1)

###### 1.4.1. Visualize data,  uncover NAs  ######
vis_miss(ict_18, warn_large_data = FALSE)

### Converting all the hidden values into NA
ict_18 <- convert_and_clean(ict_18)

### Subsetting the skills varaibles to further imputations
ict_skills_18 <- ict_18 %>% 
  dplyr::select(codice_, clad4, ateco_1, rip, C4, B5a, 
                B5b, B5c, B5d, B5e, B5f, B5g) %>% 
  mutate(C10a = NA,
         C10c = NA, 
         year = 2018,
         year = as.factor(year)) %>% 
  rename(codice = codice_)

ict_skills_18 <- ict_skills_18[,c(1:5, 13:14, 6:12,15)]

summary(ict_skills_18)


### Check the length of the raw data vs the final data
## for 2014 the raw has 18953 but the final data has 18832
expected_sequence <- 1:22079

# Extract the actual series from the Codice column
actual_series <- ict_18_impT$Codice

# Identify missing values
missing_values <- setdiff(expected_sequence, actual_series)

# Check if there are any missing values and print them
if (length(missing_values) == 0) {
  print("The Codice column contains the full series from 1 to 18953.")
} else {
  print("The Codice column is missing the following values:")
  print(missing_values)
}


ict_skills_18 <- ict_skills_18 %>% 
  filter(!codice %in% missing_values)

# Combine "D" and "E" into "D_E" and remove the old levels
ict_skills_18$ateco_1 <- fct_collapse(ict_skills_18$ateco_1,
                                      D_E = c("D", "E"))

save(ict_skills_18, file = here("Data", "Processed", "ict_skills_18.rda"))




### Visualize NA
vis_miss(ict_18, warn_large_data = FALSE)

# creating a copy of ict_14c c meaning cleaned
# creating a copy of ict_14c c meaning cleaned
missing_count <- sapply(ict_18, function(x) sum(is.na(x)))
var_18$missing <- round((sum(is.na(ict_18)) / nrow(ict_18)) * 100,2)

ict_18c <- ict_18


summary(ict_18c$B5a)


# Overwriting the 'B5a' column with the recoded values and converting it back to a factor
ict_18c$B5a <- factor(ifelse(ict_18c$B5a == '1', 0,
                             ifelse(ict_18c$B5a == '2', 1,
                                    ifelse(ict_18c$B5a == '3', NA, NA))),
                      levels = c(0, 1))


summary(ict_18)

nlevels(ict_18c$C6)

summary(ict_18c, Inf)
str(ict_18c)
###### 1.4.2. Change var types  ######
## Converting all variables in factor to identify the var types and write them
# in excel for further use 
# ict_18c <- ict_18c %>%
#   # Convert variable 1 to character
#   mutate_at(vars(1:162), as.factor)

# mplement the Data Type Changes using a for loop using the vars_18
for (i in 1:nrow(var_18)) {
  current_var_name <- var_18$acrom[i]
  current_var_type <- var_18$type[i]
  
  # Check if the variable name from var_18 exists in ict_18c
  if (current_var_name %in% colnames(ict_18c)) {
    # Change the data type based on the type specified in var_18
    if (current_var_type == "binary" || current_var_type == "levels") {
      ict_18c[[current_var_name]] <- as.factor(ict_18c[[current_var_name]])
    } else if (current_var_type == "numeric") {
      ict_18c[[current_var_name]] <- as.numeric(as.character(ict_18c[[current_var_name]]))
    } else if (current_var_type == "character") {
      ict_18c[[current_var_name]] <- as.character(ict_18c[[current_var_name]])
    }
  }
}


# Check the summary and the var types 
summary(ict_18c)
str(ict_18c)

vis_miss(ict_18c, warn_large_data = FALSE)


###### 1.4.3.  Count missingness by var  ######
# Identify missing values
missing_values <- is.na(ict_18c)

# Count missing values for each column
missing_count <- colSums(missing_values)

# Calculate missing percentage for each column
miss_per <- round((missing_count / nrow(ict_18c)) * 100,2)

# Subset columns that have missing values and their missing percentages
missing_info <- data.frame(var = names(ict_18c)[missing_count > 0],
                           miss_per = miss_per[missing_count > 0])

## Creates a df that informs what to do with the variables with high % missing, 
## there are many variables that has low missingness meaning there are some rows
# with high missingness rate 
missing_info <- missing_info %>%
  left_join(var_18, by = c("var" = "acrom")) %>%
  mutate(
    missing_count = round((miss_per / 100) * nrow(ict_18c)), 
    action = case_when(
      miss_per > 60 ~ "remove",
      missing_count < 0.01 * nrow(ict_18c) ~ "remove_row",
      TRUE ~ "inform imputation"
    )
  ) %>%
  dplyr::select(var, name_EN, type, miss_per, missing_count, action)

###### 1.4.4. Removing high % missing and others  ######

# List of columns to remove
cols_to_remove <- missing_info$var[missing_info$action == "remove"]

# Remove the columns
cols_to_remove_safe <- intersect(cols_to_remove, names(ict_18c))
ict_18c <- ict_18c %>% dplyr::select(-all_of(cols_to_remove_safe))

vis_miss(ict_18c, warn_large_data = FALSE)

# Remove rows where missing values are > 80%
threshold <- 0.08 * ncol(ict_18c)
ict_18c <- ict_18c %>% 
  filter(rowSums(is.na(.)) < threshold)

vis_miss(ict_18c[, 1:50], warn_large_data = FALSE)
summary(ict_18c)

## Creates a df that informs the new set of variables for impute treatment
# Recalculating missing info for remaining columns in ict_18c
new_missing_info <- ict_18c %>%
  summarise_all(function(x) mean(is.na(x) * 100)) %>%
  gather(var, miss_per) %>%
  filter(miss_per > 0)

# Rounding miss_per to 2 decimal places
new_missing_info$miss_per <- round(new_missing_info$miss_per, 2)

# Calculating missing count
new_missing_info$miss_count <- round((new_missing_info$miss_per / 100) * nrow(ict_18c))

# Left join with var_18 for additional variable information
new_missing_info <- new_missing_info %>%
  left_join(var_18, by = c("var" = "acrom")) %>%
  mutate(
    action = case_when(
      miss_per > 60 ~ "remove",
      miss_count < 0.01 * nrow(ict_18c) ~ "inform imputation",
      TRUE ~ "inform imputation"
    )
  ) %>%
  dplyr::select(var, type, name_EN,  miss_per, miss_count, action)

ict_18c <- ict_18c %>% dplyr::select(-c(mac, coeffin))

# ###### 1.4.5. Creating Ateco column  ######
# # Create a new column called 'Ateco_1' in the 'ict_18c' data frame and assign it the result of the modification done to the initial 'ict_14c' data frame.
# # This modification uses the dplyr package to apply 'mutate' to the data frame and add cases to the column 'Ateco_1' based on the values of 'dom1' column.
# # Depending on the value categories in 'dom1', 'Ateco_1' adds a new letter category to that row.
# # If 'dom1' is not in any of the defined categories, 'Ateco_1' value will be NA.
# ict_18c <- ict_18c %>%
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
ict_18c <- ict_18c %>%
  mutate(
    Revenue_K = ifelse(as.numeric(as.character(ricavi_cl)) != 0, 
                       as.numeric(as.character(ricavi_cl)) / 1000, 0),
    Revenue_K = as.factor(Revenue_K) %>% 
      droplevels()
  )

ict_18c <-  droplevels(ict_18c)

ict_18c0 <- ict_18c
ict_18c <- ict_18c0


### Create a empty column to store the interpolation 

## B5a use of employees of the company or persons who are part of the group of companies to carry out ict functions
# D1
# D2b
# D2a


ict_18c$D1 <- NA
ict_18c$D2b <- NA
ict_18c$D2a <- NA
ict_18c$C9g <- NA

## loading the variable map for all the years 
var_map <- read_xlsx(here("Data", "Processed","var_map.xlsx"))
vars_dd18 <- na.omit(var_map$acrom_5)
### filtering variables from the var_map
ict_18c <- ict_18c[, vars_dd18]
colnames(ict_18c) <-  na.omit(var_map$var_dd5)

setdiff(vars_dd18, colnames(ict_18c))

vis_miss(ict_18c, warn_large_data = FALSE)


summary(ict_18c)

summary(ict_18c$clad4)

low_variance14 <- c("S_B3",	"A2_C1",	"A2_C3",	
                    "A2_C4",	"UC_C9b",	"UMK_C9d",	
                    "UMK_C9e",	"UC_I1a",	"UC_I5")

ict_18c <- ict_18c %>% dplyr::select(-all_of(low_variance14))

names(ict_18c)
summary(ict_18c)
str(ict_18c)

## Removing missing in dom4, ateco_1 and Renenue_K
na <- which(is.na(ict_18c$dom4))
ict_18c <- ict_18c[-na,]

na <- which(is.na(ict_18c$ateco_1))
ict_18c <- ict_18c[-na,]

na <- which(is.na(ict_18c$Revenue_K))
ict_18c <- ict_18c[-na,]




###### 1.4.6. One-hot encoding of variables####

ict_18c <- dummy_cols(ict_18c, select_columns = c("clad4",  "rip", 
                                                  "ateco_1","Revenue_K"), 
                      remove_selected_columns = F, remove_first_dummy = T)

ict_18c[,c(25:53)] <- lapply(ict_18c[,c(25:53)], as.factor)

#### 2. VISUALIZATOINS of Size, sector and region ####

# Grouping by 'clad4', 'dom1', and 'region'
levels_clad4 = c("cl1", "cl2", "cl3")
labels_clad4 = c("Small", "Medium", "Large")

ict_18c %>%
  group_by(clad4) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = factor(clad4, levels = levels_clad4, labels = labels_clad4), y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Company Size", x = "Company Size", y = "Count") +
  theme_minimal()



ict_18c %>%
  group_by(ateco_1) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = ateco_1, y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Secors", x = "Sectors", y = "Count") +
  theme_minimal()


levels_rip4 = c("ITC", "ITF", "ITG", "ITH", "ITI")
labels_rip4 = c("Northwest", "South", "Iisland", "Northeast", "Center")

ict_18c %>%
  group_by(rip) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = factor(rip, levels = levels_rip4, labels = labels_rip4), y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Region", x = "Region", y = "Count") +
  theme_minimal()



##### 2.1. ONE VAR VIZ FOR BINARY #####


title_bin <- "UMK_C9f"

# take into account which data set ict_14c or ict_14cP
plot_binary_var(ict_18c, 
                variable_name =  title_bin,
                title_name = title_bin,
                x_var = "ateco_1",
                facet_var = "clad4",
                ncol = 1 )


##### 2.2. ONE VAR VIZ FOR CONTINUOUS  #####

title_con <- "A2_C6_"

plot_continuous_var(ict_18c, title_con,
                    title_name = title_con,
                    x_var = "rip",
                    facet_var = "clad4",
                    ncol = 1)

ggplot(ict_18c, aes(x= log(A2_C6_))) + geom_histogram() + facet_wrap(~ ateco_1)

var(na.omit(ict_18c$A2_C6_))

summary(ict_18c$A2_C6_)

##### 2.3. ONE VAR VIZ FOR CATEGORICAL  #####

title_cat <- "S_B5a"

plot_categorical_var(ict_18c, title_cat,
                     title_name = title_cat,
                     x_var = "ateco_1",
                     facet_var = "clad4",
                     ncol = 1)



#### 3. LOW MISSIN % VARS ####


# Step 1: Get column names of ict_18c
col_names <- names(ict_18c)

# Step 2: Match column names with their descriptions from var_map
# Ensure that var_map$var_dd2 and col_names are both of the same type (e.g., character)
descriptions <- var_map$name_EN[match(col_names, var_map$var_dd5)]

# Step 3: Calculate the percentage of missing values for each column in ict_18c
miss_per <- sapply(ict_18c, function(col) {
  sum(is.na(col)) / nrow(ict_18c) * 100
})

# Step 4: Combine everything into a new data frame
missng_dd_15 <- data.frame(
  Name = col_names,
  Desc = descriptions,
  Miss_Per = miss_per)


##### 3.1. Model 2: Imputation with rf #####



pattern_lmv <- "^(ateco_1_|clad4_|rip_|UMK_C8$|A2_C5$|A2_A2$|S_B1$|S_B2a$|S_B5a$)"

## Search for the pattern in the df
lmv_cov <- grep(pattern_lmv, names(ict_18c), value = TRUE)

summary(ict_18c$S_B5a)

## Run the imputation model 
gc()
imp_model_lmv1 <- mice(ict_18c[, lmv_cov],
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
imp_model_lmv2 <- mice(ict_18c[, lmv_cov],
                       method = "cart", 
                       m = 5, # Number of imputed datasets. 5 is a common choice.
                       maxit = 15) # Number of iterations
## Check the predictor matrix
m <- imp_model_lmv2$predictorMatrix
## Check the traceplots
plot_trace(imp_model_lmv2)

##### 3.3. IMPUTATION DIAGNOSTICS LMV #####

###### 3.3.1. Model with original data ######

summary(ict_18c, Inf)

evaluate_original_data(ict_18c, "S_B1", c("UMK_C8", "A2_C5", "S_B5a") )

###### 3.3.2. Best Model  ######

# list of imputed models 
lmv_models <- list(rf = imp_model_lmv1, 
                   cart = imp_model_lmv2)

## Get the best results from the best imputation methods
## across the imputed datasets
lmv_best <- compare_imputation_methods(models_list = lmv_models,
                                       data = ict_18c,
                                       target = "S_B1",
                                       predictors = c("UMK_C8", "A2_C5", "S_B5a"))
# Retrieve best method 
lmv_best$method
# Retrieve the best imputed dataset
lmv_best$result$action

completed_data_lmv <- complete(imp_model_lmv2, action = 3)


summary(ict_18c)
vis_miss(ict_18c, warn_large_data = FALSE)

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
# dom4. Represent the 27 economic sectors Ateco before 2018
# rip. Represent the 5 regions in Italy NUTS 1

##### 3.1. WS Visualizations ###### 

###### 3.1.1. Plot by size ###### 


# Calling Website variables starting with C8_ to impute 
ws_vars <- grep("K_C9.|C_C9.", names(ict_18c), value = TRUE)

ws_title <- "Website vars"

plot_binary_var(ict_18c, 
                variable_name =  ws_vars,
                title_name = ws_title,
                x_var = "ateco_1",
                facet_var = "clad4",
                ncol = 1 )


###### 3.1.2. Plot by region #####

plot_binary_var(ict_18c, 
                variable_name =  ws_vars,
                title_name = ws_title,
                x_var = "ateco_1",
                facet_var = "rip",
                ncol = 1 )

###### 3.1.3. Plot by revenue  #####

plot_binary_var(ict_18c, 
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

pattern_ws <- "^(ateco_1_|clad4_|rip_|A2_A2$|S_B1$|S_B2a$)|C_C9.|K_C9"

## Search for the pattern in the df
ws_cov <- grep(pattern_ws, names(ict_18c), value = TRUE)

## Run the imputation model 
imp_model_ws1 <- mice(ict_18c[, ws_cov],
                      method = "rf", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations
## Check the predictor matrix
m <- imp_model_ws1$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws1)



###### 3.2.2. Model 2: Imputation with pmm #####

## Run the imputation model 
imp_model_ws2 <- mice(ict_18c[, ws_cov],
                      method = "logreg", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations

## Check the predictor matrix
m <- imp_model_ws2$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws2)

###### 3.2.3. Model 3: Imputation with cart #####
gc()
imp_model_ws3 <- mice(ict_18c[, ws_cov],
                      method = "cart",
                      m = 5, 
                      maxit = 15) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ws3$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws3)

##### 3.3. IMPUTATION DIAGNOSTICS WS #####


summary(ict_18c)

evaluate_original_data(ict_18c, "UC_I1b", ws_vars)



ws_models <- list(rf = imp_model_ws1, 
                  logreg = imp_model_ws2, 
                  cart = imp_model_ws2)

ws_best <- compare_imputation_methods(models_list = ws_models,
                                      data = ict_18c,
                                      target = "UC_I1b",
                                      predictors = ws_vars)


# Retrieve best method 
ws_best$method
# Retrieve the best imputed dataset
ws_best$result$action

completed_data_ws <- complete(imp_model_ws1, action = 5)


##### 4.1. ACCESS VAR (NUMERIC) ######


###### 4.1.1. Plot by Size #####

ti_vars <-  grep("^A2_C6_", names(ict_18c), value = TRUE)

title_num <- "Type of Invoices"

plot_continuous_var(data =  ict_18c,
                    variable_name =  ti_vars,
                    title_name = title_num,
                    facet_var = "clad4",
                    x_var = "clad4",
                    ncol = 2, 
                    log_transform = F)

###### 4.1.1. Plot by Region #####

plot_continuous_var(data =  ict_18c,
                    variable_name =  ti_vars,
                    title_name = title_num,
                    facet_var = "rip",
                    x_var = "rip",
                    ncol = 2, 
                    log_transform = F)




###### 4.1.1. Plot by Revenue #####


plot_continuous_var(data =  ict_18c,
                    variable_name =  ti_vars,
                    title_name = title_num,
                    facet_var = "Revenue_K",
                    x_var = "clad4",
                    ncol = 2, 
                    log_transform = F)


##### 4.2. TI IMPUTATION (NUMERIC) ###### 

###### 4.2.1. Model 1: Imputation with sample #####

## Set the pattern which contains the covariates
pattern_ti <- "^(ateco_1_|clad4_|rip_|A2_A2$|S_B1$|S_B2a$)|A2_C6_"



## Search for the pattern in the df
ti_cov <- grep(pattern_ti, names(ict_18c), value = TRUE)


## Run the imputation model 
gc()
imp_model_ti1 <- mice(ict_18c[, ti_cov],
                      method = "sample", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ti1$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ti1)

###### 4.2.2. Model 2: Imputation with rf #####


## Run the imputation model 
gc()
imp_model_ti2 <- mice(ict_18c[,ti_cov],
                      method = "rf", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ti2$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ti2)


###### 4.2.3. Model 3: Imputation with cart #####


## Run the imputation model 
gc()
imp_model_ti3 <- mice(ict_18c[,ti_cov],
                      method = "cart", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ti3$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ti3)

##### 4.3. IMPUTATION DIAGNOSTICS TI (NUMERIC) #####

ti_vars1 <- c("A2_C6_")

evaluate_original_data(ict_18c, "A1_B2b", ti_vars1)



ti_models <- list(sample = imp_model_ti1, 
                  rf = imp_model_ti2, 
                  cart = imp_model_ti3)

ti_best <- compare_imputation_methods(models_list = ti_models,
                                      data = ict_18c,
                                      target = "A1_B2b",
                                      predictors = ti_vars1)

# Retrieve best method 
ti_best$method
# Retrieve the best imputed dataset
ti_best$result$action

completed_data_ti <- complete(imp_model_ti3, action = 4)




### 5. INTEGRATION IMP DATA SETS   ####

imputed_datasets_list <- list(completed_data_lmv,
                              completed_data_ws,
                              completed_data_ti)


imputed_cols  <- missing_dd_18 %>%
  filter(Miss_Per >= 6, Miss_Per <= 25.0) %>% # , type == "binary"
  dplyr::pull(Name)

ict_18_impT <- integrate_imputed_data(original_data = ict_18c, 
                                      imputed_datasets_list, 
                                      imputed_cols)
ict_18_impT <- ict_18_impT[, 1:24]

# Combine "D" and "E" into "D_E" and remove the old levels
ict_18_impT$ateco_1 <- fct_collapse(ict_18_impT$ateco_1,
                                    D_E = c("D", "E"))

# Drop the old factor levels that are now unused
ict_18_impT$ateco_1 <- droplevels(ict_18_impT$ateco_1)

vis_miss(ict_18_impT, warn_large_data = FALSE)

summary(ict_18_impT$ateco_1)


save(ict_18_impT, file = here("Data", "Processed", "ICT18.rda"))
save(ict_18c0, file = here("Data", "Processed", "ict_18c0.rda"))





