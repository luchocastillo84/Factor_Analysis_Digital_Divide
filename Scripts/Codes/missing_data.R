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

load( here("Scripts", "Environments", "missing_values.RData"))


load( here("Data", "Processed", "ICT_2014Rdux.rda"))
load( here("Data", "Processed", "ICT_2015Rdux.rda"))
load( here("Data", "Processed", "ICT_2016Rdux.rda"))
load( here("Data", "Processed", "ICT_2017Rdux.rda"))
load( here("Data", "Processed", "ICT_2018Rdux.rda"))
load( here("Data", "Processed", "ICT_2019Rdux.rda"))

ICT_2014Rdux$B5a <- NA
ICT_2017Rdux$B5a <- NA
ICT_2019Rdux$B5a <- NA
ICT_2017Rdux$C10a <- NA
ICT_2017Rdux$C10c <- NA
ICT_2017Rdux$C9g <- NA
ICT_2017Rdux$D1 <- NA
ICT_2017Rdux$D2a <- NA
ICT_2017Rdux$D2b <- NA
ICT_2018Rdux$C10a <- NA
ICT_2018Rdux$C10c <- NA
ICT_2018Rdux$C9g <- NA

ICT_2017Rdux <- ICT_2017Rdux[, c(1:6, 25,8:24)]
ICT_2018Rdux <- ICT_2018Rdux[, c(1:13, 23:25,14:22)]


vis_miss(ICT_2014Rdux, warn_large_data = FALSE)
vis_miss(ICT_2015Rdux, warn_large_data = FALSE)
vis_miss(ICT_2016Rdux, warn_large_data = FALSE)
vis_miss(ICT_2017Rdux, warn_large_data = FALSE)
vis_miss(ICT_2018Rdux, warn_large_data = FALSE)
vis_miss(ICT_2019Rdux, warn_large_data = FALSE)


ncol(ICT_2014Rdux)
ncol(ICT_2015Rdux)
ncol(ICT_2016Rdux)
ncol(ICT_2017Rdux)
ncol(ICT_2018Rdux)
ncol(ICT_2019Rdux)
vis_dat(ICT_2014Rdux)




