library(dplyr)
library(kableExtra)
library(gtsummary)
library(cgwtools)

cohort <- 'SE' #Change this to reflect your cohort
df_file <- paste0("validation_paper/tabs/Result_dfs_demographics", cohort, ".RData") 
load("data/EDGI_exercise_cleaned_SE.RData") # Load clean data


Demo_df <- EDGI_exercise_cleaned %>% 
  select(age, currentbmi, case_status, ED100k_gender_dummy)

Demo_df$ED100k_gender_dummy <- haven::as_factor(Demo_df$ED100k_gender_dummy)


Table_1 <- Demo_df %>% 
  tbl_summary(
    type=list(
              ED100k_gender_dummy ~ "categorical",
              case_status ~ "categorical",
              age ~ "continuous",
              currentbmi ~ "continuous"),
    label = list(
                 ED100k_gender_dummy ~ "Gender Identity",
                 case_status ~ "Eating Disorder Diagnosis",
                 age ~ "Age",
                 currentbmi ~ "Current BMI"),
    statistic = list(all_continuous() ~"{mean} ({sd})",
                     all_categorical() ~"{n}   ({p})"),
    digits=list(all_continuous() ~c(2,2),
                all_categorical() ~c(0,1))
  ) |> 
  modify_header(label = "**Variable**") |> 
  modify_caption("Participant characteristics")  |> 
  bold_labels()

save(Table_1, file = df_file)

