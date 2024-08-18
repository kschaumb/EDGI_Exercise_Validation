# BEFORE STARTING
## 1.  Place your data in a folder called 'data/' in the head of the repository.
## 2.  Open the 'EDGI_Exercise_Analyses.Rproj' file and make sure you are running things from INSIDE of the project
## 3.  Make sure scoresheets for each measure are in a folder called 'scoring/' in the head of the repository in a single excel sheet with a tab for each measure


# Install the remotes package if it's not already installed
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}

# Load the remotes package
library(remotes)

# List of required packages 
required_packages <- c("scorekeeper", "sjmisc", "dplyr", "tidyr", "readxl", "cgwtools", "expss", "haven")

# Install and load the required packages
for (package in required_packages) {
  # Install scorekeeper from GitHub if it's not already installed
  if (package == "scorekeeper" && !requireNamespace(package, quietly = TRUE)) {
    remotes::install_github("embark-lab/scorekeeper")
  }
  # Install other packages from CRAN
  else if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

# Load the required packages
library(scorekeeper)
library(sjmisc)
library(dplyr)
library(tidyr)
library(readxl)
library(cgwtools)
library(expss)
library(haven)


## Point to your Raw Data here: 
EDGI_SE_raw <- read_sas("data/Sweden/edgi_se_data.sas7bdat", encoding = "latin1")

## Name your Cohort
cohort <- 'SE' #CHANGE THIS FOR EACH COHORT



## Point to Scoresheets here: 
ED100k_score <- read_excel("scoring/SE_Scoring/EDGI_exercise_scoresheet_SE.xlsx", 'ED100k')
EDEQ_score <- read_excel("scoring/SE_Scoring/EDGI_exercise_scoresheet_SE.xlsx", 'EDEQ')
CET_score <- read_excel("scoring/SE_Scoring/EDGI_exercise_scoresheet_SE.xlsx", 'CET')
OCI12_score <- read_excel("scoring/SE_Scoring/EDGI_exercise_scoresheet_SE.xlsx", 'OCI12')
FrostMPS_score <- read_excel("scoring/SE_Scoring/EDGI_exercise_scoresheet_SE.xlsx", 'FrostMPS')

# ED100k Scoring
ED100k<- scorekeep(EDGI_SE_raw, ED100k_score)
ED100k_scored <- ED100k [[10]]

# Add various weight suppression variables; clean outlier BMI variables
ED100k_scored <- ED100k_scored |> 
  mutate (highestbmi = round(we_hi_kg/(height^2), 2))  |> 
  mutate (lowestbmi = round(wt_lo_kg/(height^2), 2)) |>
  mutate( ED100k_wt_suppress_high_current = we_hi_kg*.453 - wt_cur_kg*.453) |> 
  mutate( ED100k_wt_suppress_high_AN = we_hi_kg*.453 - wt_loan_kg*.453) |> 
  mutate( ED100k_wt_suppress_current_AN = wt_cur_kg*.453 - wt_loan_kg*.453) |> 
  mutate( ED100k_wt_suppress_high_low = we_hi_kg*.453 - wt_lo_kg*.453) |> 
  mutate( ED100k_wt_suppress_curr_low = wt_cur_kg*.453 - wt_lo_kg*.453) |> 
  mutate( ED100k_bmi_suppress_high_curr = highestbmi -  currentbmi) |> 
  mutate( ED100k_bmi_suppress_high_low = highestbmi- lowestbmi) |> 
  mutate( ED100k_bmi_suppress_high_AN = round(we_hi_kg/(height^2),2) - an_bmi) |> 
  mutate( ED100k_bmi_suppress_current_AN = currentbmi - an_bmi) |> 
  mutate( ED100k_bmi_suppress_current_low = currentbmi - lowestbmi) 

# EDEQ Scoring 
EDEQ_scored <- scorekeep(EDGI_SE_raw, EDEQ_score)
EDEQ_scored <- EDEQ_scored [[5]]

# CET Scoring
CET_scored <- scorekeep(EDGI_SE_raw, CET_score)
CET_scored <- CET_scored [[6]]

# OCI Scoring
OCI12_scored <- scorekeep(EDGI_SE_raw, OCI12_score)
OCI12_scored <- OCI12_scored [[6]]

# Frost MPS Scoring 
FrostMPS_scored <- scorekeep(EDGI_SE_raw, FrostMPS_score)
FrostMPS_scored <- FrostMPS_scored [[3]]

# Combine data frames and save
Data <- full_join(CET_scored,ED100k_scored)
Data <- full_join(Data, EDEQ_scored)
Data <- full_join(Data, OCI12_scored)
Data <- full_join(Data, FrostMPS_scored)

EDGI_exercise_cleaned <- Data |> 
  rename_all(function(x) gsub('.factor', '_factor', x)) |> 
  rename(ED100k_ex_int_sumNA = ED100k_ex_interfere_sum_NA_percent) |> #renames to shorter for SAS friendly variable name
  rename(ED100k_ex_int_weightedsum = ED100k_ex_interfere_sum_weighted_sum) #renames to shorter for SAS friendly variable name



# Make case hierarchy and recode restrictive and binge spectrum mixed cases
EDGI_exercise_cleaned <- EDGI_exercise_cleaned |>
  mutate (case_status = case_when(an_case == 1 & bn_case == 0 & bed_case == 0 ~ 'AN',
                                  an_case == 0 & bn_case == 1 & bed_case == 0 ~ 'BN',
                                  an_case == 0 & bn_case == 0 & bed_case == 1 ~ 'BED',
                                  an_case == 1 & (bn_case == 1 | bed_case == 1) ~ 'AN Mixed',
                                  an_case == 0 & bn_case ==1 & bed_case ==1 ~ 'BN-BED Mixed',
                                  an_case == 0 & bn_case == 0 & bed_case == 0 ~ 'Control' )) |>
  mutate (case_heirarchy = case_when (an_case == 1 ~ 'AN',
                                      bn_case == 1 ~ 'BN',
                                      bed_case == 1 ~ 'BED',
                                      an_case == 0 & bn_case == 0 & bed_case == 0 ~ 'Control'))


# Generate the file name with the cohort name
RData_file <- paste0("data/EDGI_exercise_cleaned_", cohort, ".RData") 

save(EDGI_exercise_cleaned, file = RData_file) # Save RData File


rm(list = setdiff(ls(), "cohort"))
