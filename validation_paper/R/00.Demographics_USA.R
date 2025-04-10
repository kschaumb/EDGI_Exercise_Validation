cohort <- 'US' #Change this to reflect your cohort
df_file <- paste0("validation_paper/tabs/Result_dfs_demographics", cohort, ".RData") 
load("data/EDGI_exercise_cleaned_US.RData") # Load clean data

# impute edeq14
EDGI_exercise_cleaned$edeq14 <- ifelse(EDGI_exercise_cleaned$edeq13 == 0, 0, EDGI_exercise_cleaned$edeq14)


Demo_df <- EDGI_exercise_cleaned %>% 
  select(ethnicity, ED100k_gender_dummy_2, sex, age, ED100k_race_dummy_2_factor, currentbmi, case_status, race, edeq_global, edeq_binge_days_28, edeq_fast_freq_28_ordinal, edeq_ex_driven_freq_28, edeq_laxative_freq_28, edeq_vomit_freq_28, edeq14)

# add together binge, fast, driven exerise, laxative, and vomitting days, then dichotomize into present or absent 
Demo_df <- Demo_df %>% 
  mutate(
    edeq_binge_28 = ifelse(edeq14 < 4 , 0, 1),
    edeq_fast_freq_28_ordinal = ifelse(edeq_fast_freq_28_ordinal < 2, 0, 1),
    edeq_ex_driven_freq_28 = ifelse(edeq_ex_driven_freq_28 < 4, 0, 1),
    edeq_laxative_freq_28 = ifelse(edeq_laxative_freq_28 < 4, 0, 1),
    edeq_vomit_freq_28 = ifelse(edeq_vomit_freq_28 < 4, 0, 1)
  ) %>%
  rowwise() %>%
  mutate(
    valid_count = sum(!is.na(c_across(c(
      edeq_binge_28,
      edeq_fast_freq_28_ordinal,
      edeq_ex_driven_freq_28,
      edeq_laxative_freq_28,
      edeq_vomit_freq_28
    )))),
    current_ED_behaviors = if (valid_count >= 4) {
      if (sum(c_across(c(
        edeq_binge_28,
        edeq_fast_freq_28_ordinal,
        edeq_ex_driven_freq_28,
        edeq_laxative_freq_28,
        edeq_vomit_freq_28
      )), na.rm = TRUE) > 0) 1 else 0
    } else {
      NA
    }
  ) %>%
  ungroup() %>%
  select(-valid_count)  # optional: remove helper column

# make a current ED behaviors exempting exercise

Demo_df <- Demo_df %>% 
  rowwise() %>%
  mutate(
    valid_count = sum(!is.na(c_across(c(
      edeq_binge_28,
      edeq_fast_freq_28_ordinal,
      edeq_laxative_freq_28,
      edeq_vomit_freq_28
    )))),
    current_ED_behaviors_no_exercise = if (valid_count >= 3) {
      if (sum(c_across(c(
        edeq_binge_28,
        edeq_fast_freq_28_ordinal,
        edeq_laxative_freq_28,
        edeq_vomit_freq_28
      )), na.rm = TRUE) > 0) 1 else 0
    } else {
      NA
    }
  ) %>%
  ungroup() %>%
  select(-valid_count) # optional: remove helper column


# make a variable called case_status_2 that is just ED and control
Demo_df <- Demo_df %>% 
  mutate(case_status_2 = case_when(case_status == 'Control' ~ 'Control',
                                    case_status == 'AN' ~ 'ED',
                                    case_status == 'BN' ~ 'ED',
                                    case_status == 'BED' ~ 'ED',
                                    case_status == 'AN Mixed' ~ 'ED',
                                    case_status == 'BN-BED Mixed' ~ 'ED')) 

Demo_df$ethnicity <- factor(Demo_df$ethnicity, levels=c("1","2"), 
                            labels = c("Hispanic", "Non-Hispanic"))
Demo_df$ED100k_gender_dummy_2 <- to_factor(Demo_df$ED100k_gender_dummy_2)
Demo_df$race <- factor(Demo_df$race, levels = c('1', '2','3','4','5','6','99','-9'), 
                       labels = c ('White', 'Black/African American', 'Asian', 'Pacific Islander', 'Native American', '> 1 race', 'Other', 'Not Reported'))

Demo_df$sex <- factor(Demo_df$sex, levels = c('1', '2','3'), 
                      labels = c ('Male', 'Female', 'Intersex'))
Demo_df$ED100k_race_dummy_2_factor <- as.character(Demo_df$ED100k_race_dummy_2_factor)

Demo_df$current_ED_behaviors <- factor(Demo_df$current_ED_behaviors, levels = c('0', '1'), 
                                          labels = c ('No', 'Yes'))
Demo_df$current_ED_behaviors_no_exercise <- factor(Demo_df$current_ED_behaviors_no_exercise, levels = c('0', '1'),
                                                  labels = c ('No', 'Yes'))

# make case_status a factor
Demo_df$case_status <- factor(Demo_df$case_status, levels = c('Control', 'AN', 'BN', 'BED', 'AN Mixed', 'BN-BED Mixed'), 
                               labels = c ('Control', 'AN', 'BN', 'BED', 'AN Mixed', 'BN-BED Mixed'))

library(dplyr)



# Define a function to summarize continuous variables
summarize_continuous <- function(data, var, label) {
  data %>%
    summarize(mean = mean(!!sym(var), na.rm = TRUE),
              sd = sd(!!sym(var), na.rm = TRUE)) %>%
    mutate(statistic = paste0(round(mean, 2), " (", round(sd, 2), ")")) %>%
    mutate(Label = label, Level = "") %>%
    select(Label, Level, statistic)
}


# For ED behavior variables only: exclude NA from denominator
summarize_categorical <- function(data, var, label) {
  data %>%
    count(!!sym(var)) %>%
    mutate(
      non_na_total = sum(n[!is.na(!!sym(var))]),
      percentage = ifelse(is.na(!!sym(var)), NA, n / non_na_total * 100),
      statistic = ifelse(is.na(!!sym(var)),
                         paste0(n, " (NA)"),
                         paste0(n, " (", round(percentage, 1), "%)")
      ),
      Label = label,
      Level = ifelse(is.na(!!sym(var)), "Missing", as.character(!!sym(var)))
    ) %>%
    select(Label, Level, statistic)
}

Demo_df_cases <- Demo_df |> 
  filter(case_status_2 == 'ED') 

Demo_df_controls <- Demo_df |> 
  filter(case_status_2 == 'Control')

# Summarize the data frame with labels
Table_1_cases <- bind_rows(
  summarize_categorical(Demo_df_cases, "race", "Race"),
  summarize_categorical(Demo_df_cases, "ethnicity", "Ethnicity"),
  summarize_categorical(Demo_df_cases, "ED100k_gender_dummy_2", "Gender identity"),
  summarize_categorical(Demo_df_cases, "sex", "Sex at birth"),
  summarize_categorical(Demo_df_cases, "case_status", "Eating disorder diagnosis"),
  summarize_categorical(Demo_df_cases, "current_ED_behaviors", "Current regular ED behaviors [EDE-Q]"),
  summarize_categorical(Demo_df_cases, "current_ED_behaviors_no_exercise", "Current regular ED behaviors [EDE-Q] (excl. exercise)"),
  summarize_continuous(Demo_df_cases, "age", "Age"),
  summarize_continuous(Demo_df_cases, "currentbmi", "Current BMI"),
  summarize_continuous(Demo_df_cases, "edeq_global", "EDE-Q global score")
)

# Rename columns to match your desired output
Table_1_cases <- Table_1_cases %>%
  rename(`Statistic (US) cases` = statistic) %>%
  rename(Variable = Label) %>%
  select(Variable, Level, `Statistic (US) cases`) 

# Summarize the data frame with labels for controls
Table_1_controls <- bind_rows(
  summarize_categorical(Demo_df_controls, "race", "Race"),
  summarize_categorical(Demo_df_controls, "ethnicity", "Ethnicity"),
  summarize_categorical(Demo_df_controls, "ED100k_gender_dummy_2", "Gender identity"),
  summarize_categorical(Demo_df_controls, "sex", "Sex at birth"),
  summarize_categorical(Demo_df_controls, "case_status", "Eating disorder diagnosis"),
  summarize_continuous(Demo_df_controls, "age", "Age"),
  summarize_continuous(Demo_df_controls, "currentbmi", "Current BMI")
)

# Rename columns to match your desired output
Table_1_controls <- Table_1_controls %>%
  rename(`Statistic (US) controls` = statistic) %>%
  rename(Variable = Label) %>%
  select(Variable, Level, `Statistic (US) controls`)

# Combine the two tables
Table_1 <- full_join(Table_1_cases, Table_1_controls, by = c("Variable", "Level"))

# Add the overall total N as the first row

total_n_cases <- nrow(Demo_df_cases) 
total_n_controls <- nrow(Demo_df_controls)

Table_1 <- bind_rows(
  tibble(Variable = "N", Level = '', `Statistic (US) cases` = as.character(total_n_cases), 
         `Statistic (US) controls` = as.character(total_n_controls)),
  Table_1
)



# Print the summary table
print(Table_1)


save(Table_1, file = df_file)


