library(dplyr)
library(kableExtra)
library(gtsummary)
library(cgwtools)

cohort <- 'SE' #Change this to reflect your cohort
df_file <- paste0("validation_paper/tabs/Result_dfs_demographics", cohort, ".RData") 
load("data/EDGI_exercise_cleaned_SE.RData") # Load clean data


Demo_df <- EDGI_exercise_cleaned %>% 
  select(age, sex, currentbmi, case_status, ED100k_gender_dummy_2)

Demo_df$ED100k_gender_dummy_2 <- to_factor(Demo_df$ED100k_gender_dummy_2)

Demo_df$sex <- factor(Demo_df$sex, levels = c('1', '2','3'), 
                      labels = c ('Male', 'Female', 'Intersex'))
library(dplyr)

# Define a function to summarize categorical variables with labels
summarize_categorical <- function(data, var, label) {
  data %>%
    count(!!sym(var)) %>%
    mutate(percentage = n / sum(n) * 100) %>%
    mutate(statistic = paste0(n, " (", round(percentage, 1), "%)")) %>%
    mutate(Label = label, Level = !!sym(var)) %>%
    select(Label, Level, statistic)
}

# Define a function to summarize continuous variables
summarize_continuous <- function(data, var, label) {
  data %>%
    summarize(mean = mean(!!sym(var), na.rm = TRUE),
              sd = sd(!!sym(var), na.rm = TRUE)) %>%
    mutate(statistic = paste0(round(mean, 2), " (", round(sd, 2), ")")) %>%
    mutate(Label = label, Level = "") %>%
    select(Label, Level, statistic)
}

# Summarize the data frame with labels
Table_1 <- bind_rows(
  summarize_categorical(Demo_df, "ED100k_gender_dummy_2", "Gender Identity"),
  summarize_categorical(Demo_df, "case_status", "Eating Disorder Diagnosis"),
  summarize_continuous(Demo_df, "age", "Age"),
  summarize_continuous(Demo_df, "currentbmi", "Current BMI")
)

# Rename columns to match your desired output
Table_1 <- Table_1 %>%
  rename(`Statistic (Sweden)` = statistic) %>%
  rename(Variable = Label) %>%
  select(Variable, Level, `Statistic (Sweden)`) 

# Print the summary table
print(Table_1)

# Add the overall total N as the first row

total_n <- nrow(Demo_df)

Table_1 <- bind_rows(
  tibble(Variable = "Total N", Level = "", `Statistic (Sweden)` = as.character(total_n)),
  Table_1
)

# Print the summary table
print(Table_1)



save(Table_1, file = df_file)

