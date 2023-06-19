
# Below makes the 5 case status diagnosis groups

DG_prop_table <- as.data.frame(table(EDGI_exercise_cleaned[c('an_case', 'bn_case', 'bed_case')])) |>
  mutate (prop = round(Freq/sum(Freq)*100, 2)) |> 
  filter (Freq != 0) 

#Make case hierarchy and recode restrictive and binge spectrum mixed cases
EDGI_exercise_cleaned <- EDGI_exercise_cleaned |> 
  mutate (case_status = case_when(an_case == 1 & bn_case == 0 & bed_case == 0 ~ 'AN',
                                  an_case == 0 & bn_case == 1 & bed_case == 0 ~ 'BN',
                                  an_case == 0 & bn_case == 0 & bed_case == 1 ~ 'BED', 
                                  an_case == 1 & (bn_case == 1 | bed_case == 1) ~ 'AN Mixed',
                                  an_case == 0 & bn_case ==1 & bed_case ==1 ~ 'BN-BED Mixed' )) |> 
  mutate (case_heirarchy = case_when (an_case == 1 ~ 'AN', 
                                      bn_case == 1 ~ 'BN', 
                                      bed_case == 1 ~ 'BED'))

dx_frqs <- as.data.frame(frq(EDGI_exercise_cleaned$case_status))

# label: fig-dxgroups
# fig-cap: Percentage within each diganostic group reporting differing exercise constructs
ct <- table(EDGI_exercise_cleaned$case_status, EDGI_exercise_cleaned$ED100k_ex_compulsive)
ct2 <- table(EDGI_exercise_cleaned$case_status, EDGI_exercise_cleaned$ED100k_ex_compulsive_strict_3mo)
ct3 <- table(EDGI_exercise_cleaned$case_status, EDGI_exercise_cleaned$ED100k_ex_addictive)
ct4 <- table(EDGI_exercise_cleaned$case_status, EDGI_exercise_cleaned$ED100k_ex_excessive)
ct5 <- table(EDGI_exercise_cleaned$case_status, EDGI_exercise_cleaned$ED100k_ex_compensatory)
ct6 <- table(EDGI_exercise_cleaned$case_status, EDGI_exercise_cleaned$ED100k_ex_maladaptive_1)

dx_row_percents_1 <- as.data.frame(prop.table (ct, 1)) |> 
  filter (Var2 == 1) |> 
  select(c(1,3)) |> 
  rename(Compulsive = Freq)
dx_row_percents_2 <- as.data.frame(prop.table (ct2, 1)) |> 
  filter (Var2 == 1) |> 
  select(c(1,3)) |> 
  rename(`Regular Compulsive` = Freq)
dx_row_percents_3 <- as.data.frame(prop.table (ct3, 1)) |> 
  filter (Var2 == 1) |> 
  select(c(1,3)) |> 
  rename(`Addictive` = Freq)
dx_row_percents_4 <- as.data.frame(prop.table (ct4, 1)) |> 
  filter (Var2 == 1) |> 
  select(c(1,3)) |> 
  rename(`Excessive` = Freq)
dx_row_percents_5 <- as.data.frame(prop.table (ct5, 1)) |> 
  filter (Var2 == 1) |> 
  select(c(1,3)) |> 
  rename(`Compensatory` = Freq)
dx_row_percents_6 <- as.data.frame(prop.table (ct6, 1)) |> 
  filter (Var2 == 1) |> 
  select(c(1,3)) |> 
  rename(`Maladaptive (Broad)` = Freq)

dx_row_percents <- full_join(dx_row_percents_1, dx_row_percents_2) 
dx_row_percents <- full_join(dx_row_percents, dx_row_percents_3) 
dx_row_percents <- full_join(dx_row_percents, dx_row_percents_4) 
dx_row_percents <- full_join(dx_row_percents, dx_row_percents_5)
dx_row_percents <- full_join(dx_row_percents, dx_row_percents_6) |> 
  rename(`Diagnosis Group` = Var1) 



dx_row_percents <- pivot_longer(dx_row_percents, cols =  c('Compulsive', 'Regular Compulsive', 'Addictive', 'Excessive', 'Compensatory', 'Maladaptive (Broad)'))

ggplot(dx_row_percents, aes(x = name, y = value*100, fill = `name` )) +
  geom_col()+
  facet_wrap (~ `Diagnosis Group`) +
  labs(title = 'Percentages by Diagnosis Group and Exercise Group', x = 'Exercise Group', y = 'Percentage (within Diagnosis Group)') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(legend.position = 'none') +
  geom_text(aes(x = name, y = (value*100) - 5, label = paste0(round(value*100, 0), '%')), size = rel(3))

dx_groups_fig <- paste0("validation_paper/figs/dx_groups_", cohort, ".png")
ggsave(file = dx_groups_fig)


Dx_table <- EDGI_exercise_cleaned %>%
  select(case_status, cet_clinical, ED100k_ex_addictive, ED100k_ex_compensatory, ED100k_ex_compulsive, ED100k_ex_compulsive_strict, ED100k_ex_excessive, ED100k_ex_maladaptive_1)

Dx_table$case_status <- factor(Dx_table$case_status)

# Create an empty list to store the models
model_list <- list()
OR_list <- list()

formulas <- c("ED100k_ex_compensatory ~ case_status",
              "ED100k_ex_compulsive ~ case_status",
              "ED100k_ex_addictive ~ case_status",
              "ED100k_ex_compulsive_strict ~ case_status",
              "ED100k_ex_excessive ~ case_status",
              "ED100k_ex_maladaptive_1 ~ case_status")

dv_labels <- c("Compensatory", "Compulsive", "Addictive", "Regular Compulsive", "Excessive", "Maladaptive")

# Loop through the formulas
for (i in seq_along(formulas)) {
  # Fit the multinomial regression model
  model <- multinom(as.formula(formulas[i]), data = Dx_table)
  model$DV <- dv_labels[i]
  
  # Store the model in the list
  model_list[[length(model_list) + 1]] <- model
}

# Create an empty data frame to store the results
results_df <- data.frame()

# Loop through the models in the model_list
for (i in seq_along(model_list)) {
  # Convert the model to tidy format
  tidy_model <- tidy(model_list[[i]], conf.int = TRUE, conf.level = 0.9982)
  
  # Add a column for the DV label
  tidy_model$DV <- model_list[[i]]$DV
  
  # Append the tidy model results to the overall results data frame
  results_df <- bind_rows(results_df, tidy_model)
}

results_df <- results_df |> 
  mutate(`Odds Ratio` = exp(estimate)) |> 
  filter(term != '(Intercept)') |> 
  mutate(`Case Status` = dplyr::recode(term, "case_statusAN Mixed" = "AN Mixed", 'cased_statusBED' = 'BED', 'case_statusBN' = 'BN', 'case_statusBN-BED Mixed' = 'BN-BED Mixed', 'case_statusBED' = 'BED')) |> 
  select (-c(term, y.level, statistic)) 

results_df$p.value <- sprintf('%.2e', results_df$p.value)

results_df <- results_df[, c(6,8,7,1:5)] 
results_df[, sapply(results_df, is.numeric)] <- round(results_df[, sapply(results_df, is.numeric)], digits = 3)

Dx_groups_file <- paste0("validation_paper/tabs/dx_multinom_", cohort, ".RData")
save(results_df, file = Dx_groups_file)

rm(list = ls())