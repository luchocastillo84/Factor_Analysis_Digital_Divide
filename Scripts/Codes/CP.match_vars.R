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
library(dplyr)
library(tokenizers)
library(SnowballC)

load(here("Scripts", "Environments",  "matching_vars.RData"))


var_14 <- read_xlsx(here("Data", "Tags","vars_14.xlsx"))

ict_14 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2014.txt"), 
                   header = TRUE, sep = "\t")
ict_14 <- ict_14 %>%
  mutate(ateco_1 = case_when(
    dom1 %in% c("N01", "N02", "N03", "N04", "N05", "N06", "N07", "N08", "N09") ~ "C",
    dom1 %in% c("N10") ~ "D-E",
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
  ))


options(scipen=999)
ict_14$Revenue_K <- ict_14$Ricavi/1000


patt14 <- "^C9.|^D.|^F.|^G.|^H.|^I.|coeffin$|mac|dom2"


results14 <- process_ict_data(var_14, ict_14, patterns = patt14)

var_14r <- results14[[1]]
ict_14 <- results14[[2]]
ict_14$Revenue_K <- ict_14$Ricavi/1000


vis_miss(ict_14, warn_large_data = FALSE)

var_14tot <- c("A2",	"B1",	"B2a",	"B2b",	"B3",	"B5a",
               "C1",	"C2",	"C3",	"C4",	"C5a",	"C6",	"C7",	
               "C8a",	"C8b",	"C8c",	"C8d",	"C8e",	"C8g",	
               "C8h",	"E1",	"E2b",	"E2a",	"J1",	"J7",	"J5",)







var_15 <- read_xlsx(here("Data", "Tags","vars_15.xlsx"))

ict_15 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2015.txt"), 
                   header = TRUE, sep = "\t")

patt15 <- "^C9.|^C10.|^D.|^F.|^G.|^H."

results15 <- process_ict_data(var_15, ict_15, patterns = patt15)

var_15r <- results15[[1]]
ict_15 <- results15[[2]]


var_16 <- read_xlsx(here("Data", "Tags","vars_16.xlsx"))

ict_16 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2016.txt"), 
                   header = TRUE, sep = "\t")

patt16 <- "^D.|^F.|^G."

results16 <- process_ict_data(var_16, ict_16, patterns = patt16)

var_16r <- results16[[1]]
ict_16 <- results16[[2]]


var_17 <- read_xlsx(here("Data", "Tags","vars_17.xlsx"))

ict_17 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2017.txt"), 
                   header = TRUE, sep = "\t")

patt17 <- "^C10.|^C11.|^E.|^F.|^H."

results17 <- process_ict_data(var_17, ict_17, patterns = patt17)

var_17r <- results17[[1]]
ict_17 <- results17[[2]]


var_18 <- read_xlsx(here("Data", "Tags","vars_18.xlsx"))

ict_18 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2018.txt"), 
                   header = TRUE, sep = "\t")

patt18 <- "^D.|^E.|^F.|^G.|^H.|^J."

results18 <- process_ict_data(var_18, ict_18, patterns = patt18)

var_18r <- results18[[1]]
ict_18 <- results18[[2]]


var_19 <- read_xlsx(here("Data", "Tags","vars_19.xlsx"))

ict_19 <- read.csv(here("Data", "Raw","ICT_Microdati_Anno_2019.txt"), 
                   header = TRUE, sep = "\t")

vis_miss(ict_19, warn_large_data = FALSE)
patt19 <- "^C10.|^C11.|^e."

results19 <- process_ict_data(var_19, ict_19, patterns = patt19)

var_19r <- results19[[1]]
ict_19 <- results19[[2]]





exception_patterns <- "^(codice|ricavi|clad|rip|ateco|dom|coeffin)"

var_19$module <- ifelse(!grepl(exception_patterns, var_19$acrom),
                              toupper(substr(var_19$acrom, 1, 1)),
                        var_19$acrom)




list_of_dfs <- list(var_14r,var_15r,var_16r,var_17r,var_18r,var_19r)

list_of_dfs[[1]]



# install.packages("stopwords")
library(stopwords)
library(dplyr)
library(tokenizers)
library(SnowballC)

# Define a function to preprocess each data frame
preprocess_df <- function(df) {
  # Convert to lowercase
  df <- df %>% mutate(across(c(name_IT, name_EN), tolower))
  
  # Remove non-alphanumeric characters
  df <- df %>% mutate(across(c(name_IT, name_EN), function(x) gsub("[^[:alnum:][:space:]]", "", x)))
  
  # Tokenization
  df$name_IT_tokens <- tokenize_words(df$name_IT)
  df$name_EN_tokens <- tokenize_words(df$name_EN)
  
  # Remove stop words
  stop_words_it <- stopwords::stopwords(language = "it")
  stop_words_en <- stopwords::stopwords(language = "en")
  
  df$name_IT_tokens <- lapply(df$name_IT_tokens, function(tokens) tokens[!tokens %in% stop_words_it])
  df$name_EN_tokens <- lapply(df$name_EN_tokens, function(tokens) tokens[!tokens %in% stop_words_en])
  
  # Stemming
  df$name_IT_tokens <- lapply(df$name_IT_tokens, wordStem, language = "it") # Using Italian for 'name_IT'
  df$name_EN_tokens <- lapply(df$name_EN_tokens, wordStem, language = "en") # Using English for 'name_EN'
  
  return(df)
}


# Assuming you have a list of data frames called df_list
# Apply preprocessing to each data frame in the list
df_list_preprocessed <- lapply(list_of_dfs, preprocess_df)


# Step 1: Convert 'name_EN_tokens' to a single string of unique and sorted tokens, then rename 'acrom'
df_list_renamed <- lapply(1:length(df_list_preprocessed), function(i, df_list) {
  df <- df_list[[i]]
  new_name <- paste0("acrom_", i)
  df %>% 
    dplyr::mutate(name_EN_tokens_str = sapply(name_EN_tokens, function(tokens) {
      unique_tokens <- unique(tokens) # Extract unique tokens
      paste(sort(unique_tokens), collapse = " ") # Sort and concatenate
    })) %>%
    dplyr::select(name_EN_tokens_str, acrom) %>%
    rename(!!new_name := acrom)
}, df_list = df_list_preprocessed)


# Step 1: Convert 'name_IT_tokens' to a single string of unique and sorted tokens, then rename 'acrom'
df_list_renamed <- lapply(1:length(df_list_preprocessed), function(i, df_list) {
  df <- df_list[[i]]
  new_name <- paste0("acrom_", i)
  df %>% 
    dplyr::mutate(name_IT_tokens_str = sapply(name_IT_tokens, function(tokens) {
      unique_tokens <- unique(tokens) # Extract unique tokens
      paste(sort(unique_tokens), collapse = " ") # Sort and concatenate
    })) %>%
    dplyr::select(name_IT_tokens_str, acrom) %>%
    rename(!!new_name := acrom)
}, df_list = df_list_preprocessed)

# Step 2: Join data frames
joined_df <- Reduce(function(x, y) left_join(x, y, by = "name_IT_tokens_str"), df_list_renamed)



joined_df <- Reduce(custom_inner_join, df_list_preprocessed)

merge_dataframes <- function(df_list_preprocessed, reference_index = 1, column = "name_EN_tokens") {
  
  # Create the token string column based on the chosen column (name_EN_tokens or name_IT_tokens)
  token_string <- paste0(column, "_str")
  
  # Convert tokens to a single string of unique and sorted tokens, then rename 'acrom'
  df_list_renamed <- lapply(1:length(df_list_preprocessed), function(i, df_list) {
    df <- df_list[[i]]
    new_name <- paste0("acrom_", i)
    
    df %>% 
      dplyr::mutate(!!token_string := sapply(.data[[column]], function(tokens) {
        unique_tokens <- unique(tokens) # Extract unique tokens
        paste(sort(unique_tokens), collapse = " ") # Sort and concatenate
      })) %>%
      dplyr::select(!!token_string, acrom) %>%
      rename(!!new_name := acrom)
  }, df_list = df_list_preprocessed)
  
  # Shift reference dataframe to the beginning of the list
  df_list_renamed <- c(list(df_list_renamed[[reference_index]]), df_list_renamed[-reference_index])
  
  # Join data frames
  joined_df <- Reduce(function(x, y) left_join(x, y, by = token_string), df_list_renamed)
  
  return(joined_df)
}

# Example usage:
result_df_EN <- merge_dataframes(df_list_preprocessed, reference_index = 6, column = "name_IT_tokens")

result_df_EN <- inner_join(result_df_EN[, c(2:7)], var_19t[, c(2,5)], by= c("acrom_6" = "acrom")) 

result_df_EN <- result_df_EN[, c(7, 1:6)]

write_xlsx(result_df_EN, here("Data", "Processed", "result_df_EN.xlsx"))

comm_vars_tot <- c("A2",	"B1",	"B2a",	"B2b",	"B3",	"B5a",	"C1",	"C2",	"C3",	
                   "C4",	"C5a",	"C6",	"C7",	"C8a",	"C8b",	"C8c",	"C8d",	"C8e",	
                   "C8g",	"C8h",	"E1",	"E2b",	"E2a",	"J1",	"J7",	"J5", )



list_of_dfs <- list(var_14r,var_15r,var_16r,var_17r,var_18r,var_19r)
# Sample usage
var_14t <- df_list_preprocessed[[1]]
var_15t <- df_list_preprocessed[[2]]
var_16t <- df_list_preprocessed[[3]]
var_17t <- df_list_preprocessed[[4]]
var_18t <- df_list_preprocessed[[5]]
var_19t <- df_list_preprocessed[[6]]
combined_results <- data.frame(name_EN_tokens = var_14t$name_EN_tokens)  # Assuming df_2015 as the reference

for(i in 2:length(df_list_preprocessed)) {
  combined_results[paste0('acrom_', i)] <- find_matches(var_14t, list(df_list_preprocessed[[i]]))
}

# Now, 'combined_results' should have the desired structure



clean_name <- function(name) {
  gsub("[[:punct:]]", "", tolower(name)) # Remove punctuation and convert to lower case
}

# Apply this function to the Italian names in each data frame
var_14$name_IT <- sapply(var_14$name_IT, clean_name)
var_15$name_IT <- sapply(var_15$name_IT, clean_name)
var_16$name_IT <- sapply(var_16$name_IT, clean_name)
var_17$name_IT <- sapply(var_17$name_IT, clean_name)
var_18$name_IT <- sapply(var_18$name_IT, clean_name)
var_19$name_IT <- sapply(var_19$name_IT, clean_name)

names_14 <- var_14$name_IT
names_15 <- var_15$name_IT
names_16 <- var_16$name_IT
names_17 <- var_17$name_IT
names_18 <- var_18$name_IT
names_19 <- var_19$name_IT

# Find common names
common_names <- Reduce(intersect, list(names_14, names_15, names_16, names_17, names_18, names_19))









