#### 0. Load the necessary packages ####
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(here)
library(git2r)
library(visdat)
library(ggplot2)
library(naniar)
library(chatgpt)
library(gptstudio)
require(devtools)
library(finalfit)
library(mice)
library(gridExtra)
library(FactoMineR)
library(factoextra)
library(ggmice)
library(ggmcmc)
library(nnet)
library(caret)
library(pROC)
library(fastDummies)
library(janitor)
library(stringr)

##### 0.1. Load environment #####

load(here("Scripts", "Environments",  "missing_ict_14.RData"))


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
var_14 <- read_xlsx(here("Data", "Tags","vars_14.xlsx"))


##### 1.4. Load the ICT usage survey ##### 
# This line reads a text file "ICT_Microdati_Anno_2014.txt", located in the 
# "Data" folder, inside the project directory. 
# The "here" function is used to specify the relative path to the file. 
# The data from the Excel file is stored in a new R data frame called ICT_2014.
ICT_2014 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2014.txt"), 
                     header = TRUE, sep = "\t")







summary(as.numeric(ICT_2014$C2))

summary(as.numeric(ict_14cP$C2))

### Converting all the hidden values into NA
ict_15 <- convert_and_clean2(ict_15)
summary(as.numeric(ICT_2015$A2))

raw_A2 <- ICT_2015 %>% dplyr::select(Codice, A2)
summary(as.numeric(raw_A2$A2))
nrow(raw_A2)



A2_c <- ict_15 %>% dplyr::select(Codice, A2)
summary(as.numeric(A2_c$A2))
nrow(A2_c)


C2_c <- ict_15 %>% dplyr::select(Codice, C2)
summary(as.numeric(C2_c$C2))
nrow(C2_c)

C6_c <- ict_15 %>% dplyr::select(Codice, C6)
summary(as.numeric(C6_c$C6))
nrow(C6_c)

conti_vars <- cbind(Codice=A2_c$Codice, A2_A2= A2_c$A2, A2_C2 = C2_c$C2, A2_C6 = C6_c$C6)

conti_vars <- as.data.frame(conti_vars)
summary_stats1

ict_14 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2014.txt"), 
                   header = TRUE, sep = "\t")

###### 1.4.1. Visualize data,  uncover NAs  ######
vis_miss(ict_14, warn_large_data = FALSE)

### Converting all the hidden values into NA
ict_14 <- convert_and_clean2(ict_14)

### Subsetting the skills varaibles to further imputations

ict_14 <- ict_14 %>%
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
table(ict_14$clad4)
ict_14$clad4 <- forcats::fct_collapse(ict_14$clad4,
                                      cl1 = "cl1",
                                      cl2 = c("cl2", "cl3"),
                                      cl3 = "cl4")

colnames(ICT_2014)

var_map <- read_xlsx(here("Data", "Processed","vars&codes.xlsx"))

vars14 <- var_map$acrom_1

valid_vars <- vars14[vars14 %in% colnames(ict_14)]

ICT_2014Rdux <- ict_14[, valid_vars]

vis_miss(ICT_2014Rdux, warn_large_data = FALSE)

save(ICT_2014Rdux, file = here("Data", "Processed", "ICT_2014Rdux.rda"))


ict_skills_14 <- ict_14 %>% 
  dplyr::select(Codice, clad4, ateco_1, rip, C4, C9a, C9c) %>% 
  mutate(B5a = NA, 
         B5b = NA, 
         B5c = NA,
         B5d = NA,
         B5e = NA,
         B5f = NA, 
         B5g = NA,
         year = 2014,
         year = as.factor(year)) %>% 
  rename(codice = Codice)

table(ict_skills_14$clad4)
summary(ict_skills_14)


### Check the length of the raw data vs the final data
## for 2014 the raw has 18953 but the final data has 18832
expected_sequence <- 1:18953

# Extract the actual series from the Codice column
actual_series <- ict_14_impT$Codice

# Identify missing values
missing_values <- setdiff(expected_sequence, actual_series)

# Check if there are any missing values and print them
if (length(missing_values) == 0) {
  print("The Codice column contains the full series from 1 to 18953.")
} else {
  print("The Codice column is missing the following values:")
  print(missing_values)
}


ict_skills_14 <- ict_skills_14 %>% 
  filter(!codice %in% missing_values)


save(ict_skills_14, file = here("Data", "Processed", "ict_skills_14.rda"))



### Visualize NA
vis_miss(ict_14, warn_large_data = FALSE)

# creating a copy of ict_14c c meaning cleaned
ict_14c <- ict_14


summary(ict_14c)

###### 1.4.2. Change var types  ######
## Converting all variables in factor to identify the var types and write them
# in excel for further use 
# ict_14c <- ict_14c %>%
#   # Convert variable 1 to character
#   mutate_at(vars(1:84), as.factor)

# mplement the Data Type Changes using a for loop using the vars_14
for (i in 1:nrow(var_14)) {
  current_var_name <- var_14$acrom[i]
  current_var_type <- var_14$type[i]
  
  # Check if the variable name from var_14 exists in ict_14c
  if (current_var_name %in% colnames(ict_14c)) {
    # Change the data type based on the type specified in var_14
    if (current_var_type == "binary" || current_var_type == "levels") {
      ict_14c[[current_var_name]] <- as.factor(ict_14c[[current_var_name]])
    } else if (current_var_type == "numeric") {
      ict_14c[[current_var_name]] <- as.numeric(ict_14c[[current_var_name]])
    } else if (current_var_type == "character") {
      ict_14c[[current_var_name]] <- as.character(ict_14c[[current_var_name]])
    }
  }
}


# Check the summary and the var types 
summary(ict_14c)


###### 1.4.3.  Count missingness by var  ######
# Identify missing values
missing_values <- is.na(ict_14c)

# Count missing values for each column
missing_count <- colSums(missing_values)


# Calculate missing percentage for each column
miss_per <- round((missing_count / nrow(ict_14c)) * 100,2)

# Subset columns that have missing values and their missing percentages
missing_info <- data.frame(var = names(ict_14c)[missing_count > 0],
                           miss_per = miss_per[missing_count > 0])

## Creates a df that informs what to do with the variables with high % missing, 
## there are many variables that has low missingness meaning there are some rows
# with high missingness rate 
missing_info <- missing_info %>%
  left_join(var_14, by = c("var" = "acrom")) %>%
  mutate(
    missing_count = round((miss_per / 100) * nrow(ict_14c)), 
    action = case_when(
      miss_per > 60 ~ "remove",
      missing_count < 0.01 * nrow(ict_14c) ~ "remove_row",
      TRUE ~ "inform imputation"
    )
  ) %>%
  dplyr::select(var, name_EN, type, miss_per, missing_count, action)

###### 1.4.4. Removing high % missing and others  ######

# List of columns to remove
cols_to_remove <- missing_info$var[missing_info$action == "remove"]

# Remove the columns
cols_to_remove_safe <- intersect(cols_to_remove, names(ict_14c))
ict_14c <- ict_14c %>% select(-all_of(cols_to_remove_safe))

vis_miss(ict_14c, warn_large_data = FALSE)

# Remove rows where missing values are > 80%
threshold <- 0.8 * ncol(ict_14c)
ict_14c <- ict_14c %>% 
  filter(rowSums(is.na(.)) < threshold)

vis_miss(ict_14c, warn_large_data = FALSE)


## Creates a df that informs the new set of variables for impute treatment
# Recalculating missing info for remaining columns in ict_14c
new_missing_info <- ict_14c %>%
  summarise_all(function(x) mean(is.na(x) * 100)) %>%
  gather(var, miss_per) %>%
  filter(miss_per >= 0)

# Rounding miss_per to 2 decimal places
new_missing_info$miss_per <- round(new_missing_info$miss_per, 2)

# Calculating missing count
new_missing_info$miss_count <- round((new_missing_info$miss_per / 100) * nrow(ict_14c))

# Left join with var_14 for additional variable information
new_missing_info <- new_missing_info %>%
  left_join(var_14, by = c("var" = "acrom")) %>%
  mutate(
    action = case_when(
      miss_per > 60 ~ "remove",
      miss_count == 0 ~ "complete", # Add this condition
      miss_count < 0.01 * nrow(ict_14c) ~ "inform imputation",
      TRUE ~ "inform imputation"
    )
  ) %>%
  dplyr::select(var, type, name_EN,  miss_per, miss_count, action)


## Removing mac, dom2 and coeffin columns that does not give any info
ict_14c <- ict_14c %>% select(-c(mac, dom2, coeffin))

###### 1.4.5. Creating Ateco column  ######
# Create a new column called 'ateco_1' in the 'ict_14c' data frame and assign it the result of the modification done to the initial 'ict_14c' data frame.
# This modification uses the dplyr package to apply 'mutate' to the data frame and add cases to the column 'ateco_1' based on the values of 'dom1' column.
# Depending on the value categories in 'dom1', 'ateco_1' adds a new letter category to that row.
# If 'dom1' is not in any of the defined categories, 'ateco_1' value will be NA.
ict_14c <- ict_14c %>%
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
  )) %>%
  mutate(ateco_1 = as.factor(ateco_1))


## This will crate a variables Renenue_K that express the revenue in thousands
options(scipen=999)
ict_14c <- ict_14c %>%
  mutate(
    Revenue_K = ifelse(as.numeric(as.character(Ricavi)) != 0, 
                       as.numeric(as.character(Ricavi)) / 1000, 0),
    Revenue_K = as.factor(Revenue_K) %>% 
      droplevels()
  )

ict_14c <-  droplevels(ict_14c)

## Size cl2 and cl3 aggregation 
summary(ict_14c$clad4)

ict_14c$clad4 <- forcats::fct_collapse(ict_14c$clad4,
                                       cl1 = "cl1",
                                       cl2 = c("cl2", "cl3"),
                                       cl3 = "cl4")


summary(ict_14c$clad4)



names(ict_14c)
summary(ict_14c)
str(ict_14c)

## Check point ict_14c0 is the version before one-hot ecoding
ict_14c0 <- ict_14c
# ict_14c <- ict_14c0

###### 1.4.6. One-hot encoding of variables####

ict_14c <- dummy_cols(ict_14c, select_columns = c("dom1", "clad4", 
                                                  "rip","Revenue_K"), 
                      remove_selected_columns = F, remove_first_dummy = T)

ict_14c[,c(76:120)] <- lapply(ict_14c[,c(76:120)], as.factor)

#### 2. VISUALIZATOINS of Size, sector and region ####

# Grouping by 'clad4', 'dom1', and 'region'
levels_clad4 = c("cl1", "cl2", "cl3")
labels_clad4 = c("Small", "Medium", "Large")

ict_14c %>%
  group_by(clad4) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = factor(clad4, levels = levels_clad4, labels = labels_clad4), y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Company Size", x = "Company Size", y = "Count") +
  theme_minimal()



ict_14c %>%
  group_by(ateco_1) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = ateco_1, y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Secors", x = "Sectors", y = "Count") +
  theme_minimal()


levels_rip4 = c("ITC", "ITF", "ITG", "ITH", "ITI")
labels_rip4 = c("Northwest", "South", "Iisland", "Northeast", "Center")

ict_14c %>%
  group_by(rip) %>%
  summarise(count = n()) %>% 
  ggplot(aes(x = factor(rip, levels = levels_rip4, labels = labels_rip4), y = count)) + 
  geom_bar(stat="identity") +
  geom_text(aes(label=count), vjust=-0.5) +
  labs(title = "Observations per Region", x = "Region", y = "Count") +
  theme_minimal()


##### 2.1. ONE VAR VIZ FOR BINARY #####


title_bin <- "B3"

# take into account which data set ict_14c or ict_14cP
plot_binary_var(ict_14c, 
                variable_name =  title_bin,
                title_name = title_bin,
                x_var = "ateco_1",
                facet_var = "clad4",
                ncol = 1 )


##### 2.2. ONE VAR VIZ FOR CONTINUOUS  #####

title_con <- "C6"

plot_continuous_var(ict_14c, title_con,
                    title_name = title_con,
                    x_var = "rip",
                    facet_var = "clad4",
                    ncol = 1)



##### 2.3. ONE VAR VIZ FOR CATEGORICAL  #####

title_cat <- "D4a"

plot_categorical_var(ict_14c, title_cat,
                     title_name = title_cat,
                     x_var = "ateco_1",
                     facet_var = "clad4",
                     ncol = 1)

#### 3. LOW MISSIN % VARS ####

# The strategy is to first impute these low missing percentage variables 
# so they can be use as covariates for the high missing percentage variables

# These variables have low missingness percentage ranging from 0.01 to 6.64%

# B5b	binary	Use of people specialized in IT subjects external to the group
# C3	binary	Connection type: fixed broadband - 
# C5a	binary	Connection type: mobile broadband (broadband: connection using the 3G or 4G mobile phone network)
# C5b	binary	Connection Type: Non-Broadband Mobile (GPRS, SPRS, Edge, GPRS)
# C7	binary	Website
# C9a	binary	SM use by type: social networks (e.g. Facebook, Linkedln, Xing, Viadeo, Yammer)
# C9b	binary	SM usage by type: company blog or microblog (e.g. Twitter, Present.ly)
# C9c	binary	SM usage by type: multimedia content sharing websites (e.g. YouTube, Flickr, Picasa, SlideShare)
# C9d	binary	SM usage by typology: Wiki-type tools based on knowledge sharing
# D1	binary	Use of cloud computing services on the Internet
# D5a	binary	Cloud Usage Restrictions: Security Breach Risks
# D5b	binary	Cloud usage limitations: Problems accessing data or software
# D5c	binary	Limitations on cloud use: difficulty in transferring data (portability) in the event of a change of supplier or termination of the cloud service
# D5d	binary	Cloud Usage Limitations: Uncertainty about data location
# D5e	binary	Limitations on cloud use: uncertainty about the legislative framework, jurisdiction and competence in the event of disputes
# D5f	binary	Cloud Usage Limitations: High cost of purchasing cloud services
# D5g	binary	Cloud Usage Limitations: Inadequate knowledge of cloud services


# B5b
# single imputation 
index <- which(is.na(ict_14c$B5b))[1]

ict_14c$B5b[index] <- 1


##### 3.1. Model 1: Imputation with rf #####
## Filtering Low missing variables (lmv)
low_miss_vars <- new_missing_info %>%
  filter(miss_per >= 0.02, miss_per <= 7.0, type == "binary") %>% # , type == "binary"
  dplyr::pull(var)

pattern_lmv <- "^(dom1_|clad4_|rip_|E2|E1$|J1$|J7$)"

## Search for the pattern in the df
lmv_cov <- grep(pattern_lmv, names(ict_14c), value = TRUE)

summary(ict_14c$B1)

## Run the imputation model 
gc()
imp_model_lmv1 <- mice(ict_14c[, c(low_miss_vars, lmv_cov)],
                      method = "rf", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations
## Check the predictor matrix
m <- imp_model_lmv1$predictorMatrix
## Check the traceplots
plot_trace(imp_model_lmv1)

##### 3.2. Model 2: Imputation with pmm #####

gc()

## Run the imputation model 
imp_model_lmv2 <- mice(ict_14c[, c(low_miss_vars, lmv_cov)],
                      method = "pmm", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations
## Check the predictor matrix
m <- imp_model_lmv2$predictorMatrix
## Check the traceplots
plot_trace(imp_model_lmv2)

##### 3.3. IMPUTATION DIAGNOSTICS LMV #####

###### 3.3.1. Model with original data ######

evaluate_original_data(ict_14c, "B1", low_miss_vars )

###### 3.3.2. Best Model  ######

# list of imputed models 
lmv_models <- list(rf = imp_model_lmv1, 
                  pmm = imp_model_lmv2)

## Get the best results from the best imputation methods
## across the imputed datasets
lmv_best <- compare_imputation_methods(models_list = lmv_models,
                           data = ict_14c,
                           target = "B1",
                           predictors = low_miss_vars)
# Retrieve best method 
lmv_best$method
# Retrieve the best imputed dataset
lmv_best$result$action

completed_data_lmv <- complete(imp_model_lmv1, action = 4)

###### 3.3.3. Partial Integration of only LMV   ######

# Completed data fromthe the best method
imp_data1 <- list(completed_data_lmv)

# Integrate the imputed data into the original data

ict_14cP <- integrate_imputed_data(original_data = ict_14c, 
                                   imp_data1, 
                                      low_miss_vars)

vis_miss(ict_14cP, warn_large_data = FALSE)


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
# C8b. Online traceability of the order
# C8c. Access to product catalogues or price lists
# C8d. Ability to customize site content for regular visitors
# C8e. Ability for site visitors to customize or design products
# C8f. Privacy Policy Notices, Privacy Certification Mark, or Site Security Certification
# C8g. Announcement of job vacancies or possibility of making job applications online
# C8h. Links or references to the company's social media profiles
# C8i. Possibility to submit complaints online (via email, web form, etc.)

##### 3.1. WS Visualizations ###### 

###### 3.1.1. Plot by size ###### 


# Calling Website variables starting with C8_ to impute 
ws_vars <- grep("^C8.", names(ict_14c), value = TRUE)

ws_title <- "Website vars"

plot_binary_var(ict_14c, 
                variable_name =  ws_vars,
                title_name = ws_title,
                x_var = "dom1",
                facet_var = "clad4",
                ncol = 1 )


###### 3.1.2. Plot by region #####

plot_binary_var(ict_14c, 
                variable_name =  ws_vars,
                title_name = ws_title,
                x_var = "dom1",
                facet_var = "rip",
                ncol = 1 )

###### 3.1.3. Plot by revenue  #####

plot_binary_var(ict_14c, 
                variable_name =  ws_vars,
                title_name = ws_title,
                x_var = "dom1",
                facet_var = "Revenue_K",
                ncol = 5 )

##### 3.2. WS IMPUTATİON ###### 

# Covariate
# B5a Using IT specialists who are part of the business group
# B1 	Employment of IT specialists
# C9c SM usage by type: multimedia content sharing websites

###### 3.2.1. Model 1: Imputation with logreg #####

## Set the pattern which contains the covariates
pattern_ws <- "^(dom1_|clad4_|rip_|B1$|B5a$|C9c$)"

## Search for the pattern in the df
ws_cov <- grep(pattern_ws, names(ict_14cP), value = TRUE)

## Run the imputation model 
imp_model_ws1 <- mice(ict_14cP[, c(ws_vars, ws_cov)],
                      method = "rf", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations
## Check the predictor matrix
m <- imp_model_ws1$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws1)



###### 3.2.2. Model 2: Imputation with pmm #####

## Run the imputation model 
imp_model_ws2 <- mice(ict_14cP[, c(ws_vars, ws_cov)],
                      method = "pmm", 
                      m = 5, # Number of imputed datasets. 5 is a common choice.
                      maxit = 15) # Number of iterations

## Check the predictor matrix
m <- imp_model_ws2$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws2)

###### 3.2.3. Model 3: Imputation with cart #####
gc()
imp_model_ws3 <- mice(ict_14cP[, c(ws_vars, ws_cov)],
                      method = "cart",
                      m = 5, 
                      maxit = 15) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ws3$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ws3)

##### 3.3. IMPUTATION DIAGNOSTICS WS #####


evaluate_original_data(ict_14cP, "J1", ws_vars)



ws_models <- list(rf = imp_model_ws1, 
                  pmm = imp_model_ws2, 
                  cart = imp_model_ws2)

ws_best <- compare_imputation_methods(models_list = ws_models,
                           data = ict_14cP,
                           target = "J1",
                           predictors = ws_vars)

# Retrieve best method 
ws_best$method
# Retrieve the best imputed dataset
ws_best$result$action

completed_data_ws <- complete(imp_model_ws1, action = 2)

#### 4. CLOUD COMPUTING USE (BINARY) #####

# This code will help you identify any patterns or concentrations in the dom1 
# and rip variables among the observations with missing values in the seven ICT 
# variables. The bar plots will visually present the distribution of missing 
# cases across different sectors and regions.
# 
# If there are specific sectors or regions that have a noticeably higher count 
# of missing cases, it could suggest a relationship between those characteristics 
# and the missingness in the ICT variables.


##### 4.1. CC Visualizations (BINARY) ###### 

###### 4.1.1. Plot by region #####

# Calling Website variables starting with C8_ to impute 
cc_vars <- grep("^D2.|D3.", names(ict_14c), value = TRUE)

cc_title <- "Cloud Computing"

plot_binary_var(ict_14cP, 
                variable_name =  cc_vars,
                title_name = cc_title,
                x_var = "dom1",
                facet_var = "clad4",
                ncol = 1 )


###### 4.1.2. Plot by region #####

plot_binary_var(ict_14cP, 
                variable_name =  cc_vars,
                title_name = cc_title,
                x_var = "dom1",
                facet_var = "rip",
                ncol = 1 )

###### 4.1.3. Plot by revenue  #####

plot_binary_var(ict_14cP, 
                variable_name =  cc_vars,
                title_name = cc_title,
                x_var = "dom1",
                facet_var = "Revenue_K",
                ncol = 5 )

##### 4.2. CC IMPUTATION (BIANRY) ######

# dom1, clad4, rip, D2a, D2a, D2b, D2c, D2d, D2e, D2g, D3a, D3b
# Which cloud computing services used on the Internet are purchased by the company:
# D2a. Email services
# D2b. Office software (e.g., word processors, spreadsheets)
# D2c. Enterprise database hosting
# D2d. File storage
# D2e. Finance and accounting software applications
# D2f. CRM (Customer Relationship Management) software applications to manage information relating to your customers
# D2g. Computing power to run the company's software

# How the cloud computing services used on the Internet purchased by the company are provided.
# D3a. Through cloud service provider servers that are not reserved exclusively for the company
# D3b. Through servers of the cloud service provider which are reserved exclusively for the company

# C7. Website
# B5a Using IT specialists who are part of the business group
# C9c SM usage by type: multimedia content sharing websites

###### 4.2.1. Model 1: Imputation with rf #####

## Set the pattern which contains the covariates
pattern_cc <- "^(dom1_|clad4_|rip_|C7$|B5a$|C9c$)"

## Search for the pattern in the df
cc_cov <- grep(pattern_cc, names(ict_14cP), value = TRUE)


## Run the imputation model 
gc()
imp_model_cc1 <- mice(ict_14cP[,c(cc_vars, cc_cov)],
                      method = "rf", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_cc1$predictorMatrix
## Check the traceplots
plot_trace(imp_model_cc1)


###### 4.2.2. Model 2: Imputation with pmm #####

## Run the imputation model 
gc()
imp_model_cc2 <- mice(ict_14cP[,c(cc_vars, cc_cov)],
                      method = "pmm", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_cc2$predictorMatrix
## Check the traceplots
plot_trace(imp_model_cc2)





###### 4.2.3. Model 3: Imputation with cart #####

gc()
imp_model_cc3 <- mice(ict_14cP[,c(cc_vars, cc_cov)],
                      method = "cart", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.


## Check the predictor matrix
m <- imp_model_cc3$predictorMatrix
## Check the traceplots
plot_trace(imp_model_cc3)


##### 4.3. IMPUTATION DIAGNOSTICS CC (BINARY) #####


evaluate_original_data(ict_14cP, "B1", cc_vars)



cc_models <- list(rf = imp_model_cc1, 
                  pmm = imp_model_cc2, 
                  cart = imp_model_cc2)

cc_best <- compare_imputation_methods(models_list = cc_models,
                                      data = ict_14cP,
                                      target = "B1",
                                      predictors = cc_vars)

# Retrieve best method 
cc_best$method
# Retrieve the best imputed dataset
cc_best$result$action

completed_data_cc <- complete(imp_model_cc1, action = 4)

#### 5. CLOUD COMPUTING BENEFITS (CATEGORY) #####

cc_vars1 <- grep("^D4.", names(ict_14cP), value = TRUE)


##### 5.1. CC Visualizations (CATEGORY) ###### 

###### 5.1.1. Plot by Size #####

plot_categorical_var(ict_14cP, cc_vars1,
                     title_name = cc_title,
                     x_var = "ateco_1",
                     facet_var = "clad4",
                     ncol = 1)


###### 5.1.2. Plot by region #####

plot_categorical_var(ict_14cP, cc_vars1,
                     title_name = cc_title,
                     x_var = "ateco_1",
                     facet_var = "rip",
                     ncol = 1)

###### 5.1.3. Plot by Revenue #####

plot_categorical_var(ict_14cP, cc_vars1,
                     title_name = cc_title,
                     x_var = "ateco_1",
                     facet_var = "Revenue_K",
                     ncol = 5)


##### 5.2. CC IMPUTATION (CATEGORY) ###### 

###### 5.2.1. Model 1: Imputation with polyreg #####

## Set the pattern which contains the covariates
pattern_cc1 <- "^(dom1_|clad4_|rip_|J7$|C7$|E2a$|C9c)"


## Search for the pattern in the df
cc1_cov <- grep(pattern_cc1, names(ict_14cP), value = TRUE)


## Run the imputation model 
gc()
imp_model_cc1a <- mice(ict_14cP[,c(cc_vars1, "C4", cc1_cov)],
                      method = "polyreg", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_cc1a$predictorMatrix
## Check the traceplots
plot_trace(imp_model_cc1a)


###### 5.2.2. Model 2: Imputation with polr #####

## Run the imputation model 
gc()
imp_model_cc2a <- mice(ict_14cP[,c(cc_vars1, "C4", cc1_cov)],
                      method = "polr", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_cc2a$predictorMatrix
## Check the traceplots
plot_trace(imp_model_cc2a)


###### 5.2.3. Model 3: Imputation with pmm #####

gc()
imp_model_cc3a <- mice(ict_14cP[,c(cc_vars1, "C4", cc_cov)],
                      method = "pmm", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.


## Check the predictor matrix
m <- imp_model_cc3a$predictorMatrix
## Check the traceplots
plot_trace(imp_model_cc3a)


##### 5.3. IMPUTATION DIAGNOSTICS CC (BINARY) #####


evaluate_original_data(ict_14cP, "D5a", c(cc_vars1, "C4"))



cc1_models <- list(rf = imp_model_cc1a, 
                  pmm = imp_model_cc2a, 
                  cart = imp_model_cc2a)

cc1_best <- compare_imputation_methods(models_list = cc1_models,
                                      data = ict_14cP,
                                      target = "B1",
                                      predictors = c(cc_vars1, "C4"))

# Retrieve best method 
cc1_best$method
# Retrieve the best imputed dataset
cc1_best$result$action

completed_data_cc1 <- complete(imp_model_cc3a, action = 5)



#### 6. TYPE OF INVIOCES (NUMERIC) ######

# I2a. Percentage of invoices sent to other companies or PAs in standard electronic 
#     format suitable for automatic data processing (e.g. EDI, UBL, XML)	29.55
# I2b. Percentage of invoices sent to other businesses or PAs in electronic 
#     format not suitable for automatic processing (e.g. email, email attachments in PDF format)	29.55
# I2c	Percentage of invoices sent to other companies or PAs in paper format	29.55

##### 6.1. TI Visualizations (NUMERIC) ######


###### 6.1.1. Plot by Size #####

ti_vars <-  grep("^I2.", names(ict_14cP), value = TRUE)

title_num <- "Type of Invoices"

plot_continuous_var(data =  ict_14cP,
                    variable_name =  ti_vars,
                    title_name = title_num,
                    facet_var = "clad4",
                    x_var = "clad4",
                    ncol = 2, 
                    log_transform = F)

###### 6.1.1. Plot by Region #####

plot_continuous_var(data =  ict_14cP,
                    variable_name =  ti_vars,
                    title_name = title_num,
                    facet_var = "rip",
                    x_var = "rip",
                    ncol = 2, 
                    log_transform = F)




###### 6.1.1. Plot by Revenue #####


plot_continuous_var(data =  ict_14cP,
                    variable_name =  ti_vars,
                    title_name = title_num,
                    facet_var = "Revenue_K",
                    x_var = "clad4",
                    ncol = 2, 
                    log_transform = F)


##### 6.2. TI IMPUTATION (NUMERIC) ###### 

###### 6.2.1. Model 1: Imputation with sample #####

## Set the pattern which contains the covariates
pattern_ti <- "^(dom1_|clad4_|rip_|I2b$|I2c$|C6$|C7$|E2a$|C9c)"



## Search for the pattern in the df
ti_cov <- grep(pattern_ti, names(ict_14cP), value = TRUE)


## Run the imputation model 
gc()
imp_model_ti1 <- mice(ict_14cP[, ti_cov],
                       method = "sample", 
                       m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ti1$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ti1)

###### 6.2.2. Model 2: Imputation with rf #####


## Run the imputation model 
gc()
imp_model_ti2 <- mice(ict_14cP[,ti_cov],
                      method = "rf", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ti2$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ti2)


###### 6.2.3. Model 3: Imputation with cart #####


## Run the imputation model 
gc()
imp_model_ti3 <- mice(ict_14cP[,ti_cov],
                      method = "cart", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ti3$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ti3)

##### 6.3. IMPUTATION DIAGNOSTICS TI (NUMERIC) #####

ti_vars1 <- c("I2b","I2c", "C6")

evaluate_original_data(ict_14cP, "J7", ti_vars1)



ti_models <- list(sample = imp_model_ti1, 
                   rf = imp_model_ti2, 
                   cart = imp_model_ti3)

ti_best <- compare_imputation_methods(models_list = ti_models,
                                       data = ict_14cP,
                                       target = "J7",
                                       predictors = ti_vars1)

# Retrieve best method 
ti_best$method
# Retrieve the best imputed dataset
ti_best$result$action

completed_data_ti <- complete(imp_model_ti3, action = 5)


##### 6.4. TI I2a IMPUTATION (NUMERIC) ###### 

###### 6.4.1. Model 1: Imputation with sample #####

## Set the pattern which contains the covariates
pattern_ti1 <- "^(dom1_|clad4_|rip_|I2a$|C7$|E2a$|C9c)"



## Search for the pattern in the df
ti_cov1 <- grep(pattern_ti1, names(ict_14cP), value = TRUE)


## Run the imputation model 
gc()
imp_model_ti1a <- mice(ict_14cP[, ti_cov1],
                      method = "sample", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ti1a$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ti1a)

###### 6.4.2. Model 2: Imputation with rf #####


## Run the imputation model 
gc()
imp_model_ti2a <- mice(ict_14cP[,ti_cov1],
                      method = "rf", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ti2a$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ti2a)


###### 6.2.3. Model 3: Imputation with cart #####


## Run the imputation model 
gc()
imp_model_ti3a <- mice(ict_14cP[,ti_cov1],
                      method = "cart", 
                      m = 5, maxit = 15 ) # Number of imputed datasets. 5 is a common choice.

## Check the predictor matrix
m <- imp_model_ti3a$predictorMatrix
## Check the traceplots
plot_trace(imp_model_ti3a)

##### 6.3. IMPUTATION DIAGNOSTICS TI (NUMERIC) #####

ti_vars1a <- c("I2a")

evaluate_original_data(ict_14cP, "J7", ti_vars1a)



ti_models1 <- list(sample = imp_model_ti1a, 
                  rf = imp_model_ti2a, 
                  cart = imp_model_ti3a)

ti_best1 <- compare_imputation_methods(models_list = ti_models1,
                                      data = ict_14cP,
                                      target = "J7",
                                      predictors = ti_vars1a)

# Retrieve best method 
ti_best1$method
# Retrieve the best imputed dataset
ti_best1$result$action

completed_data_ti1 <- complete(imp_model_ti2a, action = 5)


# single imputation 
index <- which(is.na(ict_14c$B5b))[1]

ict_14c$B5b[index] <- 1

### 7. INTEGRATION IMP DATA SETS   ####

imputed_datasets_list <- list(completed_data_lmv, 
                              completed_data_ws, 
                              completed_data_cc,
                              completed_data_cc1,
                              completed_data_ti, 
                              completed_data_ti1)


imputed_cols  <- new_missing_info %>%
  filter(miss_per >= 0.02, miss_per <= 60.0, var != "J8") %>% # , type == "binary"
  dplyr::pull(var)

ict_14_impT <- integrate_imputed_data(original_data = ict_14c, 
                                       imputed_datasets_list, 
                                       imputed_cols)

vis_miss(ict_14_impT, warn_large_data = FALSE)

summary(ict_14_impT)

# Find the index of columns with missing values
miss_cols <- which(apply(ict_14_impT, 2, function(col) any(is.na(col))))

# Exclude these columns from the dataframe
ict_14_impT <- ict_14_impT[, -miss_cols]


## loading the variable map for all the years 
var_map <- read_xlsx(here("Data", "Processed","var_map.xlsx"))
vars_dd14 <- var_map$acrom_1[!is.na(var_map$acrom_1)]

vars_dd14[1:28] <- str_to_title(vars_dd14[1:28])
# Apply str_to_lower to the first 28 column names
colnames(ict_14_impT)[1:67] <- str_to_lower(colnames(ict_14_impT)[1:67])
colnames(ict_14_impT)[ 1:67] <- str_to_title(colnames(ict_14_impT)[1:67])
### filtering variables from the var_map
ict_14_impT <- ict_14_impT[, -69]
ict_14_impT <- ict_14_impT[, vars_dd14]
colnames(ict_14_impT) <-  na.omit(var_map$var_dd1)



setdiff(vars_dd14, colnames(ict_14_impT))


low_variance14 <- c("S_B3",	"A2_C1",	"A2_C3",	
                    "A2_C4",	"UC_C8b",	"UMK_C8d",	
                    "UMK_C8e",	"UC_J1",	"UC_J5")

ict_14_impT <- ict_14_impT %>% dplyr::select(-all_of(low_variance14))

summary(ict_14_impT$ateco_1)

ict_14_impT$ateco_1 <- forcats::fct_recode(ict_14_impT$ateco_1,
                                           D_E = "D-E")



vis_miss(ict_14_impT, warn_large_data = FALSE)

save(ict_14_impT, file = here("Data", "Processed", "ICT14.rda"))
save(ict_14c0, file = here("Data", "Processed", "ict_14c0.rda"))





