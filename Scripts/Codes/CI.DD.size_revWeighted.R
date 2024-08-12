#### 0. Load the necessary packages ####
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(here)
library(visdat)
library(ggplot2)
library(gridExtra)
library(FactoMineR)
library(factoextra)
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

##### 0.1. Load environment #####

load(here("Scripts", "Environments",  "CI_DD_size_revWeighted.RData"))
source(here("Scripts", "Functions",  "subsetting_digidivide.R"))
source(here("Scripts", "Functions",  "comp_index_dd_mixed.R"))
source(here("Scripts", "Functions",  "ci_factor_mixed.R"))
source(here("Scripts", "Functions",  "ci_factor_mca.R"))
source(here("Scripts", "Functions",  "ci_generalized_mean1.R"))
source(here("Scripts", "Functions",  "comp_index_dd_mca.R"))
source(here("Scripts", "Functions",  "compare_means_ci.R"))
source(here("Scripts", "Functions",  "myscale.R"))
source(here("Scripts", "Functions",  "norm_min_max.R"))
source(here("Scripts", "Functions", "binding_comp_index_dd.R"))



# save.image("~/Library/CloudStorage/Dropbox/Tesis/2021/Italy/Urbino/Data/Repos/Clusterwise_DD_Italy/Scripts/Environments/CI_DD_size_rev.RData")

#### 1. Load the ICT usage survey ##### 

### this process comes fromt he script name CP.Binding.R
load(here("Data", "Processed", "ICT_Combined.rda"))
summary(ict_combined)

ict_combined$A2_A2 <- ict_combined$A2_A2/ 100
ict_combined$A2_C2 <- ict_combined$A2_C2/ 100
ict_combined$A2_C6 <- ict_combined$A2_C6/ 100
vis_miss(ict_combined, warn_large_data = F, facet = year)

summary(ict_combined$A2_C2)

## Scheme 1: year,  region, sec_name, size_emp
## Scheme 2: year,  region, sec_name, sme_rev
## Scheme 3: year,  region, mac_sec, sme_rev
## Scheme 4: year,  region, sec_name
## Scheme 5: year, region, mac_sec
## Scheme 6: year, region, size_rev
## Scheme 7: year, region, sme_rev
## Scheme 8: year, region, Revenue_K
## Scheme 9: year, region, Revenue_K, size_rev
## Scheme 10: year, Revenue_K, sme_rev
## Scheme 11: year,  region, Revenue_K,  sec_name, size_rev

# Calculate weights based on relevant contextual variables
weights <- ict_combined %>%
  group_by(year,  region, Revenue_K,  sec_name, size_rev) %>%  # Adjust these variables as needed
  summarise(count = n(), .groups = 'drop') %>%
  mutate(total = sum(count),
         prop = count / total,
         weight = 1 / prop)

# Normalize weights within each group (year)
weights <- weights %>%
  group_by(year) %>%
  mutate(weight = weight / sum(weight) * n())

# Merge weights back into the original dataset
ict_combined1 <- ict_combined %>%
  left_join(weights %>% dplyr::select(year,  region, Revenue_K,  sec_name, size_rev, weight), 
            by = c("year",  "region", "Revenue_K",  "sec_name", "size_rev"))


# Define the function
summarize_variable_by_year <- function(data, year, variable) {
  # Use enquo to capture the variable name
  variable <- enquo(variable)
  
  # Compute the summary statistics
  summary_stats <- data %>%
    filter(year == !!year) %>%
    summarise(
      mean = mean(!!variable, na.rm = TRUE),
      median = median(!!variable, na.rm = TRUE),
      sd = sd(!!variable, na.rm = TRUE),
      min = min(!!variable, na.rm = TRUE),
      max = max(!!variable, na.rm = TRUE)
    )
  
  # Return the summary statistics
  return(summary_stats)
}

# Example usage
summarize_variable_by_year(ict_combined1, 2015, A2_A2)


var(ict_combined$A2_C6)
## remove A2_C6 due to low variance
ict_combined1 <- ict_combined1[, -c(7)]





#### 2. ICT 2014 ##### 

patt <- "^A|^S|^U|^size_rev"
var2encode <- c("size_rev")
year <- 2014


subsets_2014 <- subsetting_digidivide(data =  ict_combined1, 
                                      patt =  patt, 
                                      var2encode =  var2encode, 
                                      year =  year, 
                                      include_weights = T)


### Access  
A_data_2014 <- subsets_2014$A_data


# correlations of the subset access
# A_cor_14 <- calculate_and_plot_mixed_cor(A_data_2014, c(1,3), c(2,4:9))
# A_cor_14$correlation_matrix
# print(A_cor_14$heatmap_plot)


### Skills 
S_data_2014 <- subsets_2014$S_data
# # correlations of the subset skills
# S_cor_14 <- compute_and_plot_tetrachoric(S_data_2014)
# S_cor_14$correlation_matrix
# print(S_cor_14$heatmap_plot)

### Usage 
U_data_2014 <- subsets_2014$U_data
# # correlations of the subset usage
# U_cor_14 <- compute_and_plot_tetrachoric(U_data_2014)
# U_cor_14$correlation_matrix
# print(U_cor_14$heatmap_plot)


##### 2.1. ACCESS 2014 ##### 

results_access_2014w <- comp_index_dd_mixed(data =  A_data_2014, 
                                           index = "access", 
                                           year = 2014,
                                           weights = A_data_2014$weight)
# summary(results_access_2014w$famd_results$ind$contrib[,2])
eigenvalues_2014_access
eigen_A2014 <- round(as.data.frame(eigenvalues_2014_access),5)
write_xlsx(eigen_A2014, here("Data", "Processed", "eigen_A2014.xlsx"))

scree_plot_2014_access
contrib_2014_access_D1
contrib_2014_access_D2
contrib_2014_access_D3
contrib_2014_access_D4
# View(ci_2014_access)
hist_2014_access


##### 2.2. SKILLS 2014 ##### 

results_skills_2014w <- comp_index_dd_mca(data =  S_data_2014, 
                                         index = "skills", 
                                         year = 2014,
                                         weights = S_data_2014$weight)

eigenvalues_2014_skills
eigen_S2014 <- round(as.data.frame(eigenvalues_2014_skills),5)
write_xlsx(eigen_S2014, here("Data", "Processed", "eigen_S2014.xlsx"))
scree_plot_2014_skills
contrib_2014_skills_D1
contrib_2014_skills_D2
contrib_2014_skills_D3
contrib_2014_skills_D4
# View(ci_2014_skills)
hist_2014_skills



##### 2.3. USAGE 2014 ##### 

results_usage_2014w <- comp_index_dd_mca(data = U_data_2014, 
                                        index = "usage", 
                                        year = 2014,
                                        weights = U_data_2014$weight)
eigenvalues_2014_usage
eigen_U2014 <- round(as.data.frame(eigenvalues_2014_usage),5)
write_xlsx(eigen_U2014, here("Data", "Processed", "eigen_U2014.xlsx"))

scree_plot_2014_usage
contrib_2014_usage_D1
contrib_2014_usage_D2
contrib_2014_usage_D3
contrib_2014_usage_D4
# View(ci_2014_usage)
hist_2014_usage

##### 2.4. Binding access, skills and Usage ##### 

results_comb_compind_2014w <- binding_comp_index_dd(ict_combined1, 2014)

indices_2014w <- results_comb_compind_2014w$indices
summary(indices_2014w)
results_comb_compind_2014w$plots$hist_access
results_comb_compind_2014w$plots$hist_skills
results_comb_compind_2014w$plots$hist_usage
results_comb_compind_2014w$plots$hist_secDD
results_comb_compind_2014w$plots$hist_access_size
results_comb_compind_2014w$plots$hist_skills_size
results_comb_compind_2014w$plots$hist_usage_size
results_comb_compind_2014w$plots$scatter_access_secDD


#### 3. ICT 2015 ##### 

patt <- "^A|^S|^U|^size_rev|weight"
var2encode <- c("size_rev")
year <- 2015

subsets_2015 <- subsetting_digidivide(data =  ict_combined1, 
                                      patt =  patt, 
                                      var2encode =  var2encode, 
                                      year =  year, 
                                      include_weights = T)

A_data_2015 <- subsets_2015$A_data
S_data_2015 <- subsets_2015$S_data
U_data_2015 <- subsets_2015$U_data

##### 3.1. ACCESS 2015 ##### 

results_access_2015w <- comp_index_dd_mixed(data =  A_data_2015, 
                                           index = "access", 
                                           year = 2015,
                                           weights = A_data_2015$weight)
eigenvalues_2015_access
scree_plot_2015_access
contrib_2015_access_D1
contrib_2015_access_D2
contrib_2015_access_D3
contrib_2015_access_D4
# View(ci_2015_access)
hist_2015_access


##### 3.2. SKILLS 2015 ##### 

results_skills_2015w <- comp_index_dd_mca(data =  S_data_2015, 
                                         index = "skills", 
                                         year = 2015,
                                         weights = S_data_2015$weight)
eigenvalues_2015_skills
scree_plot_2015_skills
contrib_2015_skills_D1
contrib_2015_skills_D2
contrib_2015_skills_D3
contrib_2015_skills_D4
# View(ci_2015_skills)
hist_2015_skills

##### 3.3. USAGE 2015 ##### 

results_usage_2015w <- comp_index_dd_mca(data =  U_data_2015, 
                                        index = "usage", 
                                        year = 2015,
                                        weights = U_data_2015$weight)
eigenvalues_2015_usage
scree_plot_2015_usage
contrib_2015_usage_D1
contrib_2015_usage_D2
contrib_2015_usage_D3
contrib_2015_usage_D4
# View(ci_2015_usage)
hist_2015_usage

##### 3.4. Binding access, skills and Usage ##### 

results_comb_compind_2015w <- binding_comp_index_dd(ict_combined1, 2015)

indices_2015w <- results_comb_compind_2015w$indices
summary(indices_2015w)
results_comb_compind_2015w$plots$hist_access
results_comb_compind_2015w$plots$hist_skills
results_comb_compind_2015w$plots$hist_usage
results_comb_compind_2015w$plots$hist_secDD
results_comb_compind_2015w$plots$hist_access_size
results_comb_compind_2015w$plots$hist_skills_size
results_comb_compind_2015w$plots$hist_usage_size
results_comb_compind_2015w$plots$scatter_access_secDD


#### 4. ICT 2016 ##### 

patt <- "^A|^S|^U|^size_rev"
var2encode <- c("size_rev")
year <- 2016

subsets_2016 <- subsetting_digidivide(data = ict_combined1,
                                      patt =  patt, 
                                      var2encode =  var2encode, 
                                      year =  year,
                                      include_weights = T)


A_data_2016 <- subsets_2016$A_data
S_data_2016 <- subsets_2016$S_data
U_data_2016 <- subsets_2016$U_data
summary(U_data_2016)

##### 4.1. ACCESS 2016 ##### 

results_access_2016w <- comp_index_dd_mixed(data =  A_data_2016, 
                                           index = "access", 
                                           year = 2016,
                                           weights = A_data_2016$weight)
eigenvalues_2016_access
scree_plot_2016_access
contrib_2016_access_D1
contrib_2016_access_D2
contrib_2016_access_D3
contrib_2016_access_D4
# View(ci_2016_access)
hist_2016_access

##### 4.2. SKILLS 2016 ##### 

results_skills_2016w <- comp_index_dd_mca(data =  S_data_2016, 
                                         index = "skills", 
                                         year = 2016,
                                         weights = S_data_2016$weight)
eigenvalues_2016_skills
scree_plot_2016_skills
contrib_2016_skills_D1
contrib_2016_skills_D2
contrib_2016_skills_D3
contrib_2016_skills_D4
# View(ci_2016_skills)
hist_2016_skills

##### 4.3. USAGE 2016 ##### 

results_usage_2016w <- comp_index_dd_mca(data =  U_data_2016, 
                                        index = "usage", 
                                        year = 2016,
                                        weights = U_data_2016$weight)
eigenvalues_2016_usage
scree_plot_2016_usage
contrib_2016_usage_D1
contrib_2016_usage_D2
contrib_2016_usage_D3
contrib_2016_usage_D4
# View(ci_2016_usage)
hist_2016_usage

##### 4.4. Binding access, skills and Usage ##### 

results_comb_compind_2016w <- binding_comp_index_dd(ict_combined1, 2016)

indices_2016w <- results_comb_compind_2016w$indices
summary(indices_2015w)
results_comb_compind_2016w$plots$hist_access
results_comb_compind_2016w$plots$hist_skills
results_comb_compind_2016w$plots$hist_usage
results_comb_compind_2016w$plots$hist_secDD
results_comb_compind_2016w$plots$hist_access_size
results_comb_compind_2016w$plots$hist_skills_size
results_comb_compind_2016w$plots$hist_usage_size
results_comb_compind_2016w$plots$scatter_access_secDD


#### 5. ICT 2017 ##### 

patt <- "^A|^S|^U|^size_rev"
var2encode <- c("size_rev")
year <- 2017

subsets_2017 <- subsetting_digidivide(data =  ict_combined1, 
                                      patt =  patt, 
                                      var2encode =   var2encode, 
                                      year =  year,
                                      include_weights = T)

A_data_2017 <- subsets_2017$A_data
S_data_2017 <- subsets_2017$S_data
U_data_2017 <- subsets_2017$U_data

##### 5.1. ACCESS 2017 ##### 

results_access_2017w <- comp_index_dd_mixed(data =  A_data_2017, 
                                           index = "access", 
                                           year = 2017,
                                           weights = A_data_2017$weight)
eigenvalues_2017_access
scree_plot_2017_access
contrib_2017_access_D1
contrib_2017_access_D2
contrib_2017_access_D3
contrib_2017_access_D4
# View(ci_2017_access)
hist_2017_access

##### 5.2. SKILLS 2017 ##### 

results_skills_2017w <- comp_index_dd_mca(data =  S_data_2017, 
                                         index = "skills", 
                                         year = 2017,
                                         weights = S_data_2017$weight)
eigenvalues_2017_skills
scree_plot_2017_skills
contrib_2017_skills_D1
contrib_2017_skills_D2
contrib_2017_skills_D3
contrib_2017_skills_D4
# View(ci_2017_skills)
hist_2017_skills

##### 5.3. USAGE 2017 ##### 

results_usage_2017w <- comp_index_dd_mca(data =  U_data_2017, 
                                        index = "usage", 
                                        year = 2017,
                                        weights = U_data_2017$weight)
eigenvalues_2017_usage
scree_plot_2017_usage
contrib_2017_usage_D1
contrib_2017_usage_D2
contrib_2017_usage_D3
contrib_2017_usage_D4
# View(ci_2017_usage)
hist_2017_usage

##### 5.4. Binding access, skills and Usage ##### 

results_comb_compind_2017w <- binding_comp_index_dd(ict_combined1, 2017)

indices_2017w <- results_comb_compind_2017w$indices
summary(indices_2017w)
results_comb_compind_2017w$plots$hist_access
results_comb_compind_2017w$plots$hist_skills
results_comb_compind_2017w$plots$hist_usage
results_comb_compind_2017w$plots$hist_secDD
results_comb_compind_2017w$plots$hist_access_size
results_comb_compind_2017w$plots$hist_skills_size
results_comb_compind_2017w$plots$hist_usage_size
results_comb_compind_2017w$plots$scatter_access_secDD

#### 6. ICT 2018 ##### 

patt <- "^A|^S|^U|^size_rev"
var2encode <- c("size_rev")
year <- 2018

subsets_2018 <- subsetting_digidivide(data =  ict_combined1, 
                                      patt =  patt, 
                                      var2encode =  var2encode, 
                                      year =  year,
                                      include_weights = T)

A_data_2018 <- subsets_2018$A_data
S_data_2018 <- subsets_2018$S_data
U_data_2018 <- subsets_2018$U_data

##### 6.1. ACCESS 2018 ##### 

results_access_2018w <- comp_index_dd_mixed(data =  A_data_2018, 
                                           index = "access", 
                                           year = 2018,
                                           weights = A_data_2018$weight)
eigenvalues_2018_access
scree_plot_2018_access
contrib_2018_access_D1
contrib_2018_access_D2
contrib_2018_access_D3
contrib_2018_access_D4
# View(ci_2018_access)
hist_2018_access

##### 6.2. SKILLS 2018 ##### 

results_skills_2018w <- comp_index_dd_mca(data =  S_data_2018, 
                                         index = "skills", 
                                         year = 2018,
                                         weights = S_data_2018$weight)
eigenvalues_2018_skills
scree_plot_2018_skills
contrib_2018_skills_D1
contrib_2018_skills_D2
contrib_2018_skills_D3
contrib_2018_skills_D4
# View(ci_2018_skills)
hist_2018_skills

##### 6.3. USAGE 2018 ##### 

results_usage_2018w <- comp_index_dd_mca(data =  U_data_2018, 
                                        index = "usage", 
                                        year = 2018,
                                        weights = U_data_2018$weight)
eigenvalues_2018_usage
scree_plot_2018_usage
contrib_2018_usage_D1
contrib_2018_usage_D2
contrib_2018_usage_D3
contrib_2018_usage_D4
# View(ci_2018_usage)
hist_2018_usage

##### 6.4. Binding access, skills and Usage ##### 

results_comb_compind_2018w <- binding_comp_index_dd(ict_combined1, 2018)

indices_2018w <- results_comb_compind_2018w$indices
summary(indices_2018w)
results_comb_compind_2018w$plots$hist_access
results_comb_compind_2018w$plots$hist_skills
results_comb_compind_2018w$plots$hist_usage
results_comb_compind_2018w$plots$hist_secDD
results_comb_compind_2018w$plots$hist_access_size
results_comb_compind_2018w$plots$hist_skills_size
results_comb_compind_2018w$plots$hist_usage_size
results_comb_compind_2018w$plots$scatter_access_secDD

#### 7. ICT 2019 ##### 

patt <- "^(A|S|U|size_rev|weight)"
var2encode <- c("size_rev")
year <- 2019

subsets_2019 <- subsetting_digidivide(data =  ict_combined1, 
                                      patt =  patt, 
                                      var2encode = var2encode,
                                      year = year,
                                      include_weights = T)

A_data_2019 <- subsets_2019$A_data
S_data_2019 <- subsets_2019$S_data
U_data_2019 <- subsets_2019$U_data

##### 7.1. ACCESS 2019 ##### 

results_access_2019w <- comp_index_dd_mixed(data =  A_data_2019, 
                                           index = "access", 
                                           year = 2019,
                                           weights = A_data_2019$weight)
eigenvalues_2019_access
scree_plot_2019_access
contrib_2019_access_D1
contrib_2019_access_D2
contrib_2019_access_D3
contrib_2019_access_D4
# View(ci_2019_access)
hist_2019_access

##### 7.2. SKILLS 2019 ##### 

results_skills_2019w <- comp_index_dd_mca(data =  S_data_2019, 
                                         index = "skills", 
                                         year = 2019,
                                         weights = S_data_2019$weight)
eigenvalues_2019_skills
scree_plot_2019_skills
contrib_2019_skills_D1
contrib_2019_skills_D2
contrib_2019_skills_D3
contrib_2019_skills_D4
# View(ci_2019_skills)
hist_2019_skills

##### 7.3. USAGE 2019 ##### 

results_usage_2019w <- comp_index_dd_mca(data =  U_data_2019, 
                                        index = "usage", 
                                        year = 2019,
                                        weights = U_data_2019$weight)
eigenvalues_2019_usage
scree_plot_2019_usage
contrib_2019_usage_D1
contrib_2019_usage_D2
contrib_2019_usage_D3
contrib_2019_usage_D4
# View(ci_2019_usage)
hist_2019_usage

##### 7.4. Binding access, skills and Usage ##### 

results_comb_compind_2019w <- binding_comp_index_dd(ict_combined1, 2019)

indices_2019w <- results_comb_compind_2019w$indices
summary(indices_2019w)
results_comb_compind_2019w$plots$hist_access
results_comb_compind_2019w$plots$hist_skills
results_comb_compind_2019w$plots$hist_usage
results_comb_compind_2019w$plots$hist_secDD
results_comb_compind_2019w$plots$hist_access_size
results_comb_compind_2019w$plots$hist_skills_size
results_comb_compind_2019w$plots$hist_usage_size
results_comb_compind_2019w$plots$scatter_access_secDD



ci_dd_combined_weighted <- do.call(rbind, list(indices_2014w, indices_2015w, 
                                      indices_2016w, indices_2017w, 
                                      indices_2018w, indices_2019w))
vis_miss(ci_dd_combined_weighted, warn_large_data =  F)

## Saving the combined composite index data frame as an R object 
save(ci_dd_combined_weighted, file = here("Data", "Processed", "CI_DD_comb_rev_weighted.rda"))
summary(ci_dd_combined_weighted)


## Scheme 1 : year,  region, sec_name, size_emp
## Scheme 2: year,  region, sec_name, sme_rev
## Scheme 3: year,  region, mac_sec, sme_rev
## Scheme 4: year,  region, sec_name
## Scheme 5: year, region, mac_sec
## Scheme 6: year, region, size_rev
## Scheme 7: year, region, sme_rev
## Scheme 8: year, region, Revenue_K presentable
## Scheme 9: year, region, Revenue_K, size_rev
## Scheme 10: year, Revenue_K, sem_rev
## Scheme 11: year,  region, Revenue_K,  sec_name, size_rev <- presentable
