
Demo_df <- EDGI_exercise_cleaned %>% 
  select(ethnicity, ED100k_gender_dummy, sex, ED100k_race_dummy_2_factor, age, currentbmi, case_status)

Demo_df$ethnicity <- factor(Demo_df$ethnicity, levels=c("1","2"), 
                            labels = c("Hispanic", "Non-Hispanic"))
Demo_df$ED100k_gender_dummy <- to_factor(Demo_df$ED100k_gender_dummy)
Demo_df$sex <- factor(Demo_df$sex, levels = c('1', '2','3'), 
                      labels = c ('Male', 'Female', 'Intersex'))


Table_1 <- Demo_df %>% 
  tbl_summary(
    type=list(ethnicity~ "categorical",
              sex ~ "categorical",
              ED100k_gender_dummy ~ "categorical",
              ED100k_race_dummy_2_factor ~ "categorical",
              case_status ~ "categorical",
              age ~ "continuous",
              currentbmi ~ "continuous"),
    label = list(ethnicity ~ "Ethnicity",
                 sex ~ "Biological Sex",
                 ED100k_gender_dummy ~ "Gender Identity",
                 ED100k_race_dummy_2_factor ~ "Race",
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

resave(Table_1, file = df_file)

