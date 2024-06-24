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
library(stringr)

##### 0.1. Load environment #####

load(here("Scripts", "Environments",  "missing_ict_19.RData"))


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
var_19 <- read_xlsx(here("Data", "Tags","vars_19.xlsx"))

##### 1.4. Load the ICT usage survey ##### 
# This line reads a text file "ICT_Microdati_Anno_2019.txt", located in the 
# "Data" folder, inside the project directory. 
# The "here" function is used to specify the relative path to the file. 
# The data from the Excel file is stored in a new R data frame called ICT_2019.
ICT_2019 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2019.txt"), header = TRUE, sep = "\t")
ict_19 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2019.txt"), header = TRUE, sep = "\t")


numeric_B1 <- as.numeric(ICT_2019$b1)
sum(numeric_B1, na.rm = TRUE)


numeric_B1 <- as.numeric(as.character(ict_19$b1))
sum(numeric_B1, na.rm = TRUE)
sum(ict_16$B1)



summary(as.numeric(ICT_2019$A3_))
summary(as.numeric(ICT_2019$C2_))
summary(as.numeric(ICT_2019$C6_))

summary(ict_19_impT %>% dplyr::select( A2_A3_, A2_C2_, A2_C6_))





ict_19 <- rename(ict_19, clad4 = clad3, ateco_1 = Ateco_1)
summary(ict_19)

###### 1.4.1. Visualize data,  uncover NAs  ######
vis_miss(ict_19, warn_large_data = FALSE)

### Converting all the hidden values into NA
ict_19 <- convert_and_clean(ict_19)


### Subsetting the skills varaibles to further imputations

summary(ict_19)

ict_skills_19 <- ict_19 %>% 
  dplyr::select(codice_, clad4, ateco_1, rip, c4, C10a, C10c) %>% 
  mutate(B5a = NA, 
         B5b = NA, 
         B5c = NA,
         B5d = NA,
         B5e = NA,
         B5f = NA, 
         B5g = NA,
         year = 2019,
         year = as.factor(year)) %>% 
  rename(codice = codice_)

table(ict_skills_19$clad4)
summary(ict_skills_19)

### Check the length of the raw data vs the final data
## for 2014 the raw has 18953 but the final data has 18832
expected_sequence <- 1:19915

# Extract the actual series from the Codice column
actual_series <- ict_19_impT$Codice

# Identify missing values
missing_values <- setdiff(expected_sequence, actual_series)

# Check if there are any missing values and print them
if (length(missing_values) == 0) {
  print("The Codice column contains the full series from 1 to 18953.")
} else {
  print("The Codice column is missing the following values:")
  print(missing_values)
}


ict_skills_19 <- ict_skills_19 %>% 
  filter(!codice %in% missing_values)

# Combine "D" and "E" into "D_E" and remove the old levels
ict_skills_19$ateco_1 <- fct_collapse(ict_skills_19$ateco_1,
                                      D_E = c("D", "E"))


save(ict_skills_19, file = here("Data", "Processed", "ict_skills_19.rda"))



### Visualize NA
vis_miss(ict_19, warn_large_data = FALSE)

# creating a copy of ict_19c c meaning cleaned
missing_count <- sapply(ict_19, function(x) sum(is.na(x)))
var_19$missing <- round((sum(is.na(ict_19)) / nrow(ict_19)) * 100,2)

# Apply str_to_lower to the first 28 column names
colnames(ict_19)[3:150] <- str_to_lower(colnames(ict_19)[3:150])
colnames(ict_19)[ 3:150] <- str_to_title(colnames(ict_19)[3:150])

ict_19c <- ict_19


summary(ict_19c)

# Combine "D" and "E" into "D_E" and remove the old levels
ict_19c$ateco_1 <- fct_collapse(ict_19c$ateco_1,
                                    D_E = c("D", "E"))

summary(ict_19c$ateco_1)


summary(ict_19$B5a)


nlevels(ict_19c$C6)

summary(ict_19c, Inf)
str(ict_19c)
###### 1.4.2. Change var types  ######
## Converting all variables in factor to identify the var types and write them
# in excel for further use 
# ict_19c <- ict_19c %>%
#   # Convert variable 1 to character
#   mutate_at(vars(1:162), as.factor)

# mplement the Data Type Changes using a for loop using the vars_19
for (i in 1:nrow(var_19)) {
  current_var_name <- var_19$acrom[i]
  current_var_type <- var_19$type[i]
  
  # Check if the variable name from var_19 exists in ict_19c
  if (current_var_name %in% colnames(ict_19c)) {
    # Change the data type based on the type specified in var_19
    if (current_var_type == "binary" || current_var_type == "levels") {
      ict_19c[[current_var_name]] <- as.factor(ict_19c[[current_var_name]])
    } else if (current_var_type == "numeric") {
      ict_19c[[current_var_name]] <- as.numeric(as.character(ict_19c[[current_var_name]]))
    } else if (current_var_type == "character") {
      ict_19c[[current_var_name]] <- as.character(ict_19c[[current_var_name]])
    }
  }
}


# Check the summary and the var types 
summary(ict_19c)
str(ict_19c)

vis_miss(ict_19c, warn_large_data = FALSE)


###### 1.4.3.  Count missingness by var  ######
# Identify missing values
missing_values <- is.na(ict_19c)

# Count missing values for each column
missing_count <- colSums(missing_values)

# Calculate missing percentage for each column
miss_per <- round((missing_count / nrow(ict_19c)) * 100,2)

# Subset columns that have missing values and their missing percentages
missing_info <- data.frame(var = names(ict_19c)[missing_count > 0],
                           miss_per = miss_per[missing_count > 0])

## Creates a df that informs what to do with the variables with high % missing, 
## there are many variables that has low missingness meaning there are some rows
# with high missingness rate 
missing_info <- missing_info %>%
  left_join(var_19, by = c("var" = "acrom")) %>%
  mutate(
    missing_count = round((miss_per / 100) * nrow(ict_19c)), 
    action = case_when(
      miss_per > 60 ~ "remove",
      missing_count < 0.01 * nrow(ict_19c) ~ "remove_row",
      TRUE ~ "inform imputation"
    )
  ) %>%
  dplyr::select(var, name_EN, type, miss_per, missing_count, action)

###### 1.4.4. Removing high % missing and others  ######

# List of columns to remove
cols_to_remove <- missing_info$var[missing_info$action == "remove"]

# Remove the columns
cols_to_remove_safe <- intersect(cols_to_remove, names(ict_19c))
ict_19c <- ict_19c %>% dplyr::select(-all_of(cols_to_remove_safe))

vis_miss(ict_19c, warn_large_data = FALSE)

# Remove rows where missing values are > 80%
threshold <- 0.15 * ncol(ict_19c)
ict_19c <- ict_19c %>% 
  filter(rowSums(is.na(.)) < threshold)

vis_miss(ict_19c[, 1:135], warn_large_data = FALSE)
summary(ict_19c)

## Creates a df that informs the new set of variables for impute treatment
# Recalculating missing info for remaining columns in ict_19c
new_missing_info <- ict_19c %>%
  summarise_all(function(x) mean(is.na(x) * 100)) %>%
  gather(var, miss_per) %>%
  filter(miss_per > 0)

# Rounding miss_per to 2 decimal places
new_missing_info$miss_per <- round(new_missing_info$miss_per, 2)

# Calculating missing count
new_missing_info$miss_count <- round((new_missing_info$miss_per / 100) * nrow(ict_19c))

# Left join with var_19 for additional variable information
new_missing_info <- new_missing_info %>%
  left_join(var_19, by = c("var" = "acrom")) %>%
  mutate(
    action = case_when(
      miss_per > 60 ~ "remove",
      miss_count < 0.01 * nrow(ict_19c) ~ "inform imputation",
      TRUE ~ "inform imputation"
    )
  ) %>%
  dplyr::select(var, type, name_EN,  miss_per, miss_count, action)

ict_19c <- ict_19c %>% dplyr::select(-c(mac, coeffin))

# ###### 1.4.5. Creating Ateco column  ######
# # Create a new column called 'Ateco_1' in the 'ict_19c' data frame and assign it the result of the modification done to the initial 'ict_14c' data frame.
# # This modification uses the dplyr package to apply 'mutate' to the data frame and add cases to the column 'Ateco_1' based on the values of 'dom1' column.
# # Depending on the value categories in 'dom1', 'Ateco_1' adds a new letter category to that row.
# # If 'dom1' is not in any of the defined categories, 'Ateco_1' value will be NA.
# ict_19c <- ict_19c %>%
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
ict_19c <- ict_19c %>%
  mutate(
    Revenue_K = ifelse(as.numeric(as.character(ricavi_cl)) != 0, 
                       as.numeric(as.character(ricavi_cl)) / 1000, 0),
    Revenue_K = as.factor(Revenue_K) %>% 
      droplevels()
  )

ict_19c <-  droplevels(ict_19c)

ict_19c0 <- ict_19c
ict_19c <- ict_19c0


## loading the variable map for all the years 
var_map <- read_xlsx(here("Data", "Processed","var_map.xlsx"))
vars_dd19 <- na.omit(var_map$acrom_6)
### filtering variables from the var_map
ict_19c <- ict_19c[, vars_dd19]
colnames(ict_19c) <-  na.omit(var_map$var_dd6)

setdiff(vars_dd19, colnames(ict_19c))

vis_miss(ict_19c, warn_large_data = FALSE)


summary(ict_19c)

summary(ict_19c$clad4)

low_variance14 <- c("S_B3",	"A2_C1",	"A2_C3",	
                    "A2_C4",	"UC_C8b",	"UMK_C8d",	
                    "UMK_C8e",	"UC_F1a",	"UC_F8")

ict_19c <- ict_19c %>% dplyr::select(-all_of(low_variance14))

names(ict_19c)
summary(ict_19c)
str(ict_19c)

## Removing missing in dom4, ateco_1 and Renenue_K
na <- which(is.na(ict_19c$ricavi_cl))
ict_19c <- ict_19c[-na,]

na <- which(is.na(ict_19c$ateco_1))
ict_19c <- ict_19c[-na,]

na <- which(is.na(ict_19c$Revenue_K))
ict_19c <- ict_19c[-na,]

na <- which(is.na(ict_19c$UC_F1b))
ict_19c <- ict_19c[-na,]



###### 1.4.6. One-hot encoding of variables####

ict_19c <- dummy_cols(ict_19c, select_columns = c("clad4",  "rip", 
                                                  "ateco_1","Revenue_K"), 
                      remove_selected_columns = F, remove_first_dummy = T)

ict_19c[,c(25:53)] <- lapply(ict_19c[,c(25:53)], as.factor)

#### 2. VISUALIZATOINS of Size, sector and region ####

# Grouping by 'clad4', 'dom1', and 'region'
levels_clad4 = c("cl1", "cl2", "cl3")
labels_clad4 = c("Small", "Medium", "Large")

ict_19c %>%
  group_by(clad4) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = factor(clad4, levels = levels_clad4, labels = labels_clad4), y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Company Size", x = "Company Size", y = "Count") +
  theme_minimal()



ict_19c %>%
  group_by(ateco_1) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = ateco_1, y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Secors", x = "Sectors", y = "Count") +
  theme_minimal()


levels_rip4 = c("ITC", "ITF", "ITG", "ITH", "ITI")
labels_rip4 = c("Northwest", "South", "Iisland", "Northeast", "Center")

ict_19c %>%
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
plot_binary_var(ict_19c, 
                variable_name =  title_bin,
                title_name = title_bin,
                x_var = "ateco_1",
                facet_var = "clad4",
                ncol = 1 )


##### 2.2. ONE VAR VIZ FOR CONTINUOUS  #####

title_con <- "A2_C6_"

plot_continuous_var(ict_19c, title_con,
                    title_name = title_con,
                    x_var = "ateco_1",
                    facet_var = "clad4",
                    ncol = 1)

ggplot(ict_19c, aes(x= A2_C6_)) + geom_histogram() + facet_wrap(~ ateco_1)
range(ict_19c$A2_C6_, na.rm = T)

var(na.omit(ict_19c$A2_C6_))

summary(ict_19c$A2_C6_)

##### 2.3. ONE VAR VIZ FOR CATEGORICAL  #####

title_cat <- "S_B5a"

plot_categorical_var(ict_19c, title_cat,
                     title_name = title_cat,
                     x_var = "ateco_1",
                     facet_var = "clad4",
                     ncol = 1)



#### 3. LOW MISSIN % VARS ####


# Step 1: Get column names of ict_19c
col_names <- names(ict_19c)

# Step 2: Match column names with their descriptions from var_map
# Ensure that var_map$var_dd2 and col_names are both of the same type (e.g., character)
descriptions <- var_map$name_EN[match(col_names, var_map$var_dd6)]

# Step 3: Calculate the percentage of missing values for each column in ict_19c
miss_per <- sapply(ict_19c, function(col) {
  sum(is.na(col)) / nrow(ict_19c) * 100
})

# Step 4: Combine everything into a new data frame
missing_dd_19 <- data.frame(
  Name = col_names,
  Desc = descriptions,
  Miss_Per = miss_per)



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
# dom4. Represent the 27 economic sectors Ateco before 2019
# rip. Represent the 5 regions in Italy NUTS 1

##### 3.1. WS Visualizations ###### 

###### 3.1.1. Plot by size ###### 


# Calling Website variables starting with C8_ to impute 
ws_vars <- grep("_C8.", names(ict_19c), value = TRUE)

ws_title <- "Website vars"

plot_binary_var(ict_19c, 
                variable_name =  ws_vars,
                title_name = ws_title,
                x_var = "ateco_1",
                facet_var = "clad4",
                ncol = 1 )


###### 3.1.2. Plot by region #####

plot_binary_var(ict_19c, 
                variable_name =  ws_vars,
                title_name = ws_title,
                x_var = "ateco_1",
                facet_var = "rip",
                ncol = 1 )

###### 3.1.3. Plot by revenue  #####

plot_binary_var(ict_19c, 
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

pattern_ws <- "^(ateco_1_|clad4_|rip_|A2_A2$|S_B1$|S_B2a$)|_C8."

## Search for the pattern in the df
ws_cov <- grep(pattern_ws, names(ict_19c), value = TRUE)

## Run the imputation model 
imp_model_ws1 <- mice(ict_19c[, ws_cov],
                      method = "rf", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations
## Check the predictor matrix
m <- imp_model_ws1$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws1)



###### 3.2.2. Model 2: Imputation with pmm #####

## Run the imputation model 
imp_model_ws2 <- mice(ict_19c[, ws_cov],
                      method = "logreg", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations

## Check the predictor matrix
m <- imp_model_ws2$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws2)

###### 3.2.3. Model 3: Imputation with cart #####
gc()
imp_model_ws3 <- mice(ict_19c[, ws_cov],
                      method = "cart",
                      m = 5, 
                      maxit = 15) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ws3$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws3)

##### 3.3. IMPUTATION DIAGNOSTICS WS #####


summary(ict_19c)

evaluate_original_data(ict_19c, "UM_D1", ws_vars)



ws_models <- list(rf = imp_model_ws1, 
                  logreg = imp_model_ws2, 
                  cart = imp_model_ws2)

ws_best <- compare_imputation_methods(models_list = ws_models,
                                      data = ict_19c,
                                      target = "UM_D1",
                                      predictors = ws_vars)


# Retrieve best method 
ws_best$method
# Retrieve the best imputed dataset
ws_best$result$action

completed_data_ws <- complete(imp_model_ws2, action = 3)


##### 4.1. ACCESS VAR (NUMERIC) ######


###### 4.1.1. Plot by Size #####

ti_vars <-  grep("^A2_C6_", names(ict_19c), value = TRUE)

title_num <- "Type of Invoices"

plot_continuous_var(data =  ict_19c,
                    variable_name =  ti_vars,
                    title_name = title_num,
                    facet_var = "clad4",
                    x_var = "clad4",
                    ncol = 2, 
                    log_transform = F)

###### 4.1.1. Plot by Region #####

plot_continuous_var(data =  ict_19c,
                    variable_name =  ti_vars,
                    title_name = title_num,
                    facet_var = "rip",
                    x_var = "rip",
                    ncol = 2, 
                    log_transform = F)




###### 4.1.1. Plot by Revenue #####


plot_continuous_var(data =  ict_19c,
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
ti_cov <- grep(pattern_ti, names(ict_19c), value = TRUE)


## Run the imputation model 
gc()
imp_model_ti1 <- mice(ict_19c[, ti_cov],
                      method = "sample", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ti1$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ti1)

###### 4.2.2. Model 2: Imputation with rf #####


## Run the imputation model 
gc()
imp_model_ti2 <- mice(ict_19c[,ti_cov],
                      method = "rf", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ti2$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ti2)


###### 4.2.3. Model 3: Imputation with cart #####


## Run the imputation model 
gc()
imp_model_ti3 <- mice(ict_19c[,ti_cov],
                      method = "cart", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ti3$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ti3)

##### 4.3. IMPUTATION DIAGNOSTICS TI (NUMERIC) #####

ti_vars1 <- c("A2_C6_")
summary(ict_19c)

evaluate_original_data(ict_19c, "UM_D1", ti_vars1)



ti_models <- list(sample = imp_model_ti1, 
                  rf = imp_model_ti2, 
                  cart = imp_model_ti3)

ti_best <- compare_imputation_methods(models_list = ti_models,
                                      data = ict_19c,
                                      target = "UM_D1",
                                      predictors = ti_vars1)

# Retrieve best method 
ti_best$method
# Retrieve the best imputed dataset
ti_best$result$action

completed_data_ti <- complete(imp_model_ti2, action = 3)




### 5. INTEGRATION IMP DATA SETS   ####

imputed_datasets_list <- list(completed_data_ws,
                              completed_data_ti)


imputed_cols  <- missing_dd_19 %>%
  filter(Miss_Per >= 6, Miss_Per <= 25.0) %>% # , type == "binary"
  dplyr::pull(Name)

ict_19_impT <- integrate_imputed_data(original_data = ict_19c, 
                                      imputed_datasets_list, 
                                      imputed_cols)
ict_19_impT <- ict_19_impT[, 1:24]



vis_miss(ict_19_impT, warn_large_data = FALSE)

summary(ict_19_impT$ateco_1)


save(ict_19_impT, file = here("Data", "Processed", "ICT19.rda"))
save(ict_19c0, file = here("Data", "Processed", "ict_19c0.rda"))





