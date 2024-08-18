load(RData_file) # should NOT need to change if you did scoring step correctly

EDGI_exercise_cleaned <- EDGI_exercise_cleaned %>% 
  filter(case_status != 'Control') 

Current_Exercise <- EDGI_exercise_cleaned |> 
  select(record_id, cet_total_weighted_sum, cet_mood, cet_enjoy, cet_avoid, cet_rigid, cet_wtcontrol, cet_clinical, edeq_ex_driven_freq_28, ED100k_ex_compulsive_current2)  |>  filter(!is.na(ED100k_ex_compulsive_current2)) |> 
  mutate(`Total` = scale(cet_total_weighted_sum),
         `Mood Improve` = scale(cet_mood),
         `Enjoyment` = scale(cet_enjoy), 
         `Avoidance` = scale (cet_avoid), 
         `Rigidity` = scale (cet_rigid),
         `Wt Control` = scale(cet_wtcontrol))

# standardize outcome variables
Current_Exercise <- Current_Exercise |> pivot_longer(cols = c(Total:`Wt Control`), names_to = 'variable', values_to = 'val') # pivot longer with current exercise as one column, variable name as second column, value as third
# rename current maladaptive exercise 

median_stds <- Current_Exercise |> 
  group_by (ED100k_ex_compulsive_current2, variable) |> 
  summarize (med_val = median(val, na.rm = TRUE))

CET_boxplot_stats <- Current_Exercise |> 
  group_by(ED100k_ex_compulsive_current2, variable) |> 
  summarize(
    Q1 = quantile(val, 0.25, na.rm = TRUE),
    Median = median(val, na.rm = TRUE),
    Mean = mean(val, na.rm = TRUE),
    Q3 = quantile(val, 0.75, na.rm = TRUE),
    Min = min(val, na.rm = TRUE),
    Max = max(val, na.rm = TRUE), 
    se = sd(val, na.rm = TRUE)/sqrt(n())
  )

# Save the boxplot statistics
resave(CET_boxplot_stats, file = df_file)


ggplot(Current_Exercise, aes(x = as.factor(ED100k_ex_compulsive_current2), y = val, color = as.factor(ED100k_ex_compulsive_current2))) +
  facet_wrap(~variable)+
  geom_jitter(size = 0.3) +
  geom_boxplot(alpha = 0.5, color= 'black') +
  scale_x_discrete(labels = c('No Hx', 'Hx only', 'Current')) +
  labs(title = paste('Median CET standard scores based on ED100k Maladaptive exercise', x = 'Maladaptive Exercise [ED100k]', cohort), y = 'CET Standardized Score') + 
  theme(legend.position = 'none', text = element_text(size = 18)) +
  geom_text(data = median_stds, aes(x = as.factor(ED100k_ex_compulsive_current2), y = med_val + 0.3, label = round(med_val, 1)), size = 5, color = 'black', fontface ='bold') +
  scale_color_manual(values = wes_palette("Darjeeling1"))

CET_median_file <- paste0("validation_paper/figs/MedianCET_", cohort, ".png") 
ggsave(CET_median_file)  

# label: tbl-CETANOVA
# tbl-cap: Omnibus ANOVA results comparing groups with current, history only, and no history of compulsive exercise on the ED100k on CET subscales



Enjoyment <- aov(cet_enjoy ~ as.factor(ED100k_ex_compulsive_current2), data = EDGI_exercise_cleaned)
Mood_Improve <- aov(cet_mood ~ as.factor(ED100k_ex_compulsive_current2), data = EDGI_exercise_cleaned)
Avoidance <- aov(cet_avoid ~ as.factor(ED100k_ex_compulsive_current2), data = EDGI_exercise_cleaned)
Rigidity <- aov(cet_rigid ~ as.factor(ED100k_ex_compulsive_current2), data = EDGI_exercise_cleaned)
Wt_Control <- aov(cet_wtcontrol ~ as.factor(ED100k_ex_compulsive_current2), data = EDGI_exercise_cleaned)
Total <- aov(cet_total ~ as.factor(ED100k_ex_compulsive_current2), data = EDGI_exercise_cleaned)


# List of ANOVA objects
anova_objects <- list(Enjoyment, Mood_Improve, Avoidance, Rigidity, Wt_Control, Total)

# List of ANOVA objects
object_names <- c("Enjoyment", "Mood Improve", "Avoidance", "Rigidity", "Wt Control", 'Total')  # Replace with the names of your ANOVA objects

# Combine summaries into a table with object names
summary_table <- bind_rows(mapply(function(obj, name) {
  summary <- tidy(obj)
  summary$Model <- name
  return(summary)
}, anova_objects, object_names, SIMPLIFY = FALSE))

CET_ANOVA_df_unfiltered <- summary_table
resave(CET_ANOVA_df_unfiltered, file = df_file)


# Move "Model" column to the first column
summary_table <- summary_table %>%
  select(Model, everything())

# Print the summary table
summary_table <- summary_table %>% 
  mutate(term = dplyr::recode(term,  'as.factor(ED100k_ex_compulsive_current2)' = 'ED100k History/Current Compulsive Exercise', 'Residuals' = 'Residual'))

summary_table$p.value <- sprintf('%.3e', summary_table$`p.value`)
summary_table$sumsq <- round(summary_table$sumsq, 2)
summary_table$meansq <- round(summary_table$meansq, 2)
summary_table$statistic <- round(summary_table$statistic, 2)


CET_ANOVA_df <- summary_table
resave(CET_ANOVA_df, file = df_file)


# label: tbl-CETcontrasts
# tbl-cap: Contrasts for each of the five CET subscales and the CET Total across those with and without history and current compulsive exercise as reported on the ED100k

dummy_variables <- c("ex_current_dummy1", "ex_current_dummy2", "ex_current_dummy3")

EDGI_exercise_cleaned <- EDGI_exercise_cleaned %>% 
  mutate(ex_current_dummy1 = case_when(ED100k_ex_compulsive_current2 == 0 ~ 0, 
                                       ED100k_ex_compulsive_current2 == 1 ~ 1),
         ex_current_dummy2 = case_when(ED100k_ex_compulsive_current2 == 0 ~ 0, 
                                       ED100k_ex_compulsive_current2 == 2 ~ 1),
         ex_current_dummy3 = case_when(ED100k_ex_compulsive_current2 == 1 ~ 0, 
                                       ED100k_ex_compulsive_current2 == 2 ~ 1))

Tukey_Enjoyment <- as.data.frame(TukeyHSD(Enjoyment)$`as.factor(ED100k_ex_compulsive_current2)`)
Tukey_Enjoyment <- cbind(dummy_variables, Tukey_Enjoyment) %>% 
  mutate(Variable = 'Enjoy')

Tukey_Mood_Improve <- as.data.frame(TukeyHSD(Mood_Improve)$`as.factor(ED100k_ex_compulsive_current2)`)
Tukey_Mood_Improve<- cbind(dummy_variables, Tukey_Mood_Improve) %>% 
  mutate(Variable = 'Mood Improve')

Tukey_Avoidance <- as.data.frame(TukeyHSD(Avoidance)$`as.factor(ED100k_ex_compulsive_current2)`)
Tukey_Avoidance<- cbind(dummy_variables, Tukey_Avoidance) %>% 
  mutate(Variable = 'Avoidance')

Tukey_Rigidity <- as.data.frame(TukeyHSD(Rigidity)$`as.factor(ED100k_ex_compulsive_current2)`)
Tukey_Rigidity<- cbind(dummy_variables, Tukey_Rigidity) %>% 
  mutate(Variable = 'Rigidity')

Tukey_Wt_Control <- as.data.frame(TukeyHSD(Wt_Control)$`as.factor(ED100k_ex_compulsive_current2)`)
Tukey_Wt_Control<- cbind(dummy_variables, Tukey_Wt_Control) %>% 
  mutate(Variable = 'Wt Control')

Tukey_Total <- as.data.frame(TukeyHSD(Total)$`as.factor(ED100k_ex_compulsive_current2)`)
Tukey_Total<- cbind(dummy_variables, Tukey_Total) %>% 
  mutate(Variable = 'Total')


Tukey_join <- full_join(Tukey_Enjoyment, Tukey_Mood_Improve)
Tukey_join <- full_join(Tukey_join, Tukey_Rigidity)
Tukey_join <- full_join(Tukey_join, Tukey_Wt_Control)
Tukey_join <- full_join(Tukey_join, Tukey_Avoidance)
Tukey_join <- full_join(Tukey_join, Tukey_Enjoyment)
Tukey_join <- full_join(Tukey_join, Tukey_Total)


# List of variables
variables <- c("cet_enjoy", "cet_mood", 'cet_rigid', 'cet_avoid', 'cet_wtcontrol', 'cet_total')

# Empty matrix to store the results
results <- matrix(0, nrow = length(variables), ncol = length(dummy_variables))

# Loop through the variables and dummy variables
for (i in seq_along(variables)) {
  for (j in seq_along(dummy_variables)) {
    result <- effsize::cohen.d(formula = EDGI_exercise_cleaned[[variables[i]]] ~ as.factor(EDGI_exercise_cleaned[[dummy_variables[j]]]),
                      na.rm = TRUE, 
                      pooled = TRUE)$estimate
    results[i, j] <- result
  }
}

CET_Subscales <- c('Enjoy', 'Mood Improve', 'Rigidity', 'Avoidance', 'Wt Control', 'Total')
colnames(results) <- c('ex_current_dummy1', 'ex_current_dummy2', 'ex_current_dummy3')

CET_effects <- as.data.frame(results) 
CET_effects <- cbind(CET_Subscales, CET_effects)

CET_effects<- pivot_longer (CET_effects, c(ex_current_dummy1, ex_current_dummy2, ex_current_dummy3), names_to = 'dummy_variables') %>% 
  rename('CohensD' = value) %>% 
  rename('Variable' = CET_Subscales)

Contrasts <- full_join(Tukey_join, CET_effects) %>% 
  mutate(dummy_variables = dplyr::recode(dummy_variables, 'ex_current_dummy1' = 'No vs Hx of Exercise', 'ex_current_dummy2' = 'No vs. Current Exercise', 'ex_current_dummy3' = 'Hx vs. Current Exercise')) %>% 
  rename('Contrast' = dummy_variables)  

y <- c('diff', 'lwr', 'upr', 'CohensD')
Contrasts[y] <- lapply(Contrasts[y], function(x) round(x, 3))
Contrasts$`p adj` <- sprintf('%.2e', Contrasts$`p adj`)

# Move Variable column to the first column
Contrasts <- Contrasts %>%
  select(-Variable) %>%
  mutate(Variable = Contrasts$Variable) %>%
  select(Variable, everything()) %>% 
  select(-`p adj`) |> 
  mutate (alpha = 0.05/18) |> 
  mutate(z_score  = qnorm(1 - alpha/2)) |> 
  mutate (se = (diff - lwr)/1.96) |> 
  mutate (new_lwr = round(diff-(z_score*se), 3)) |> 
  mutate(new_upr = round(diff + (z_score*se),3))

Contrasts$CI <- paste(Contrasts$new_lwr, Contrasts$new_upr, sep = ', ')
addparens <- function(x){paste('(',x,')')}
Contrasts$CI <- addparens(Contrasts$CI)
Contrasts$diff<- paste(Contrasts$diff, Contrasts$CI, sep = ' ')

Contrasts <- Contrasts %>% 
  select(-c(CI, lwr, upr, new_lwr, new_upr, alpha, z_score, se)) %>% 
  rename ('Difference' = diff)

CET_contrast_file <- Contrasts 
resave(CET_contrast_file, file = df_file)

