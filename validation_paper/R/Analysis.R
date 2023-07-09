
# Name your cohort 
cohort <- 'USA' #Change this to reflect your cohort 

#Load Cleaned Data - Make sure this is set to load the cleaned data file and contains ALL cleaned variables needed for analysis. See scoring first if dataset needs to be cleaned and scored. The data should be housed in 'EDGI_Exercise_cleaned/data'

RData_file <- paste0("data/EDGI_exercise_cleaned_", cohort, ".RData") # Should NOT Need to change if you did the scoring step correctly

load(RData_file) # should NOT need to change if you did scoring step correctly

library(expss)
library(bookdown)
library(dplyr)
library(haven)
library(sjmisc)
library(rmarkdown)
library(knitr)
library(psych)
library(ggplot2)
library(ggrepel)
library(wesanderson)
library(tidyr)
library(caret)
library(broom)
library(nnet)
library(pscl)
library(boot)
library(tibble)
library(psych)
library(caret)
library(viridis)  
library(kableExtra)
library(cgwtools)
library(ggsignif)
library(ggpubr)
library(purrr)

df_file <- paste0("validation_paper/tabs/Result_dfs_", cohort, ".RData") 

# Run 0-3 and Save output
source("validation_paper/R/0.Descriptives.R")
source("validation_paper/R/1.Defs_Sensitivity_NPV.R")
source("validation_paper/R/2.1.CET.R")
source("validation_paper/R/2.2.EDEQ.R")
source("validation_paper/R/2.3.1.MPS_compulsive.R")
source("validation_paper/R/2.3.2.OCI_compulsive.R")
source("validation_paper/R/3.Dx_groups.R")




