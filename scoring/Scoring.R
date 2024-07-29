#Loads packages
# Install scorekeeper 
# devtools::install_github("embark-lab/scorekeeper")
library(scorekeeper)
#Load additional packages
library(sjmisc)
library(haven)
library(dplyr)
library(tidyr)
library(readxl)
library(cgwtools)
library(expss)

# load .sas7 data file 
EDGI_raw <- read_sas("data/exercise_240610.sas7bdat")
# save as R Data file
save(EDGI_raw, file = "data/EDGI_raw.RData")

## Point to your Raw Data here: 
load('data/EDGI_raw.RData') 

## Point to Scoresheets here: 
ED100k_score <- read_excel("scoring/EDGI_exercise_scoresheet.xlsx", 'ED100k')
EDEQ_score <- read_excel("scoring/EDGI_exercise_scoresheet.xlsx", 'EDEQ')
CET_score <- read_excel("scoring/EDGI_exercise_scoresheet.xlsx", 'CET')
OCI12_score <- read_excel("scoring/EDGI_exercise_scoresheet.xlsx", 'OCI12')
FrostMPS_score <- read_excel("scoring/EDGI_exercise_scoresheet.xlsx", 'FrostMPS')

# ED100k Scoring
ED100k_scored <- scorekeep(EDGI_raw, ED100k_score)
ED100k_scored <- ED100k_scored[[10]]

# Add various weight suppression variables; clean outlier BMI variables
ED100k_scored <- ED100k_scored |> 
  mutate (highestbmi = round(703*wt_hi_lb/(height^2), 2))  |> 
  mutate (lowestbmi = round(703*wt_lo_lb/(height^2), 2)) |>
  mutate( ED100k_wt_suppress_high_current = wt_hi_lb - wt_cur_lb) |> 
  mutate( ED100k_wt_suppress_high_AN = wt_hi_lb - wt_loan_lb) |> 
  mutate( ED100k_wt_suppress_current_AN = wt_cur_lb - wt_loan_lb) |> 
  mutate( ED100k_wt_suppress_high_low = wt_hi_lb - wt_lo_lb) |> 
  mutate( ED100k_wt_suppress_curr_low = wt_cur_lb - wt_lo_lb) |> 
  mutate( ED100k_bmi_suppress_high_curr = highestbmi -  currentbmi) |> 
  mutate( ED100k_bmi_suppress_high_low = highestbmi- lowestbmi) |> 
  mutate( ED100k_bmi_suppress_high_AN = round(703*wt_hi_lb/(height^2),2) - an_bmi) |> 
  mutate( ED100k_bmi_suppress_current_AN = currentbmi - an_bmi) |> 
  mutate( ED100k_bmi_suppress_current_low = currentbmi - lowestbmi) 

# EDEQ Scoring 
EDEQ_scored <- scorekeep(EDGI_raw, EDEQ_score)
EDEQ_scored <- EDEQ_scored[[5]]

# CET Scoring
CET_scored <- scorekeep(EDGI_raw, CET_score)
CET_scored <- CET_scored[[6]]

# OCI Scoring
OCI12_scored <- scorekeep(EDGI_raw, OCI12_score)
OCI12_scored <- OCI12_scored[[6]]

# Frost MPS Scoring 
FrostMPS_scored <- scorekeep(EDGI_raw, FrostMPS_score)
FrostMPS_scored <- FrostMPS_scored [[3]]

# Combine data frames and save
Data <- full_join(CET_scored,ED100k_scored)
Data <- full_join(Data, EDEQ_scored)
Data <- full_join(Data, OCI12_scored)
Data <- full_join(Data, FrostMPS_scored)

EDGI_exercise_cleaned <- Data |> 
  mutate (ex_age = na_if(ex_age, 233)) |> # change age of person who said they were 233 years old
  rename_all(function(x) gsub('.factor', '_factor', x)) |> 
  rename(ED100k_ex_int_sumNA = ED100k_ex_interfere_sum_NA_percent) |> #renames to shorter for SAS friendly variable name
  rename(ED100k_ex_int_weightedsum = ED100k_ex_interfere_sum_weighted_sum) #renames to shorter for SAS friendly variable name

write_sas(EDGI_exercise_cleaned, 'data/exercise_cleaned.sas7bdat') # Save SAS file
save(EDGI_exercise_cleaned, file = "data/EDGI_exercise_cleaned.RData") # Save RData File



