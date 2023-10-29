

# Define a list of required packages (in alphabetical order)
required_packages <- c(
  "broom", "boot", "caret", "cgwtools", "dplyr", "effsize",
  "emmeans", "expss", "flextable", "ggplot2", "ggpubr", "ggrepel", "gmodels",
  "gtsummary", "haven", "kableExtra", "knitr", "labelled", "nnet", "patchwork",
  "pscl", "psych", "purrr", "rmarkdown", "sjmisc", "tidyr", "tibble", "viridis",
  "wesanderson"
)

# Install and load the required packages
for (package in required_packages) {
  # Check if the package is not already installed
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

# Load the required packages
library(broom)
library(boot)
library(caret)
library(cgwtools)
library(dplyr)
library(effsize)
library(emmeans)
library(expss)
library(flextable)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(gmodels)
library(gtsummary)
library(haven)
library(kableExtra)
library(knitr)
library(labelled)
library(nnet)
library(patchwork)
library(pscl)
library(psych)
library(purrr)
library(rmarkdown)
library(sjmisc)
library(tidyr)
library(tibble)
library(viridis)
library(wesanderson)


# Name your cohort 
cohort <- 'USA' #Change this to reflect your cohort 

#Load Cleaned Data - Make sure this is set to load the cleaned data file and contains ALL cleaned variables needed for analysis. See scoring first if dataset needs to be cleaned and scored. The data should be housed in 'EDGI_Exercise_cleaned/data'

RData_file <- paste0("data/EDGI_exercise_cleaned_", cohort, ".RData") # Should NOT Need to change if you did the scoring step correctly and are running from inside a project. May need to add additional path if running from outside a project.


load(RData_file) # should NOT need to change if you did scoring step correctly


df_file <- paste0("validation_paper/tabs/Result_dfs_", cohort, ".RData") 

# Run 0-3 and Save output
source("validation_paper/R/0.Descriptives.R")
source("validation_paper/R/1.Defs_Sensitivity_NPV.R")
source("validation_paper/R/2.1.CET.R")
source("validation_paper/R/2.2.EDEQ.R")
source("validation_paper/R/2.3.1.MPS.R")
source("validation_paper/R/2.3.2.OCI.R")
source("validation_paper/R/3.Dx_groups.R")

# Note - this analysis script does not include demographics (currently in 00.Demographics.R for the US cohort), as demographic information collected differs across cohorts.


