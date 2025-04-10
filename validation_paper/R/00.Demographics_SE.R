library(dplyr)

# ---- Setup ----
cohort <- 'SE'
df_file <- paste0("validation_paper/tabs/Result_dfs_demographics", cohort, ".RData")
load("data/EDGI_exercise_cleaned_SE.RData") # Load clean data

# ---- Impute edeq14 if needed ----
EDGI_exercise_cleaned$edeq14 <- ifelse(EDGI_exercise_cleaned$edeq13 == 0, 0, EDGI_exercise_cleaned$edeq14)

# recode edeq_global to NA if it is > 6 (impossible)
EDGI_exercise_cleaned$edeq_global[EDGI_exercise_cleaned$edeq_global > 6] <- NA

# ---- Select variables ----
Demo_df <- EDGI_exercise_cleaned %>%
  select(ED100k_gender_dummy_2, sex, age, currentbmi, case_status,
         edeq_global, edeq_binge_days_28, edeq_fast_freq_28_ordinal,
         edeq_ex_driven_freq_28, edeq_laxative_freq_28, edeq_vomit_freq_28, edeq14)

# ---- Derive ED behavior variables ----
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
    } else NA
  ) %>%
  ungroup() %>%
  select(-valid_count) %>%
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
    } else NA
  ) %>%
  ungroup() %>%
  select(-valid_count)

# ---- Recode and label ----
Demo_df <- Demo_df %>%
  mutate(
    case_status_2 = case_when(
      case_status == 'Control' ~ 'Control',
      TRUE ~ 'ED'
    ),
    ED100k_gender_dummy_2 = to_factor(ED100k_gender_dummy_2),
    sex = factor(sex, levels = c('1', '2', '3'), labels = c('Male', 'Female', 'Intersex')),
    current_ED_behaviors = factor(current_ED_behaviors, levels = c('0', '1'), labels = c('No', 'Yes')),
    current_ED_behaviors_no_exercise = factor(current_ED_behaviors_no_exercise, levels = c('0', '1'), labels = c('No', 'Yes')),
    case_status = factor(case_status, levels = c('Control', 'AN', 'BN', 'BED', 'AN Mixed', 'BN-BED Mixed'))
  )

# ---- Summary functions ----



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

summarize_continuous <- function(data, var, label) {
  data %>%
    summarize(
      mean = mean(!!sym(var), na.rm = TRUE),
      sd = sd(!!sym(var), na.rm = TRUE)
    ) %>%
    mutate(
      statistic = paste0(round(mean, 2), " (", round(sd, 2), ")"),
      Label = label,
      Level = ""
    ) %>%
    select(Label, Level, statistic)
}

# ---- Split into groups ----
Demo_df_cases <- Demo_df %>% filter(case_status_2 == 'ED')
Demo_df_controls <- Demo_df %>% filter(case_status_2 == 'Control')

# ---- Summarize cases ----
Table_1_cases <- bind_rows(
  summarize_categorical(Demo_df_cases, "ED100k_gender_dummy_2", "Gender identity"),
  summarize_categorical(Demo_df_cases, "sex", "Sex at birth"),
  summarize_categorical(Demo_df_cases, "case_status", "Eating disorder diagnosis"),
  summarize_categorical(Demo_df_cases, "current_ED_behaviors", "Current regular ED behaviors [EDE-Q]"),
  summarize_categorical(Demo_df_cases, "current_ED_behaviors_no_exercise", "Current regular ED behaviors [EDE-Q] (excl. exercise)"),
  summarize_continuous(Demo_df_cases, "age", "Age"),
  summarize_continuous(Demo_df_cases, "currentbmi", "Current BMI"),
  summarize_continuous(Demo_df_cases, "edeq_global", "EDE-Q global score")
) %>%
  rename(Variable = Label, `Statistic (Sweden) cases` = statistic)

# ---- Summarize controls ----
Table_1_controls <- bind_rows(
  summarize_categorical(Demo_df_controls, "ED100k_gender_dummy_2", "Gender identity"),
  summarize_categorical(Demo_df_controls, "sex", "Sex at birth"),
  summarize_categorical(Demo_df_controls, "case_status", "Eating disorder diagnosis"),
  summarize_continuous(Demo_df_controls, "age", "Age"),
  summarize_continuous(Demo_df_controls, "currentbmi", "Current BMI")
) %>%
  rename(Variable = Label, `Statistic (Sweden) controls` = statistic)

# ---- Combine and finalize ----
Table_1 <- full_join(Table_1_cases, Table_1_controls, by = c("Variable", "Level"))

# Add sample sizes
Table_1 <- bind_rows(
  tibble(
    Variable = "N", Level = "",
    `Statistic (Sweden) cases` = as.character(nrow(Demo_df_cases)),
    `Statistic (Sweden) controls` = as.character(nrow(Demo_df_controls))
  ),
  Table_1
)

# ---- Save output ----
print(Table_1)
save(Table_1, file = df_file)
