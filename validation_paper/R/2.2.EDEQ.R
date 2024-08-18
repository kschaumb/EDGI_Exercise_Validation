
mean_edeq_ex <- EDGI_exercise_cleaned|> 
  group_by (ED100k_ex_compulsive_current2) |> 
  filter(!is.na(ED100k_ex_compulsive_current2)) |> 
  summarize (mean_edeq_ex = mean(edeq_ex_driven_freq_28, na.rm = TRUE))

EDGI_exercise_cleaned_2 <- EDGI_exercise_cleaned |> 
  filter(!is.na(ED100k_ex_compulsive_current2)) 

EDEQ_boxplot_stats <- EDGI_exercise_cleaned_2 |> 
  group_by(ED100k_ex_compulsive_current2) |> 
  summarize(
    Q1 = quantile(edeq_ex_driven_freq_28, 0.25, na.rm = TRUE),
    Median = median(edeq_ex_driven_freq_28, na.rm = TRUE),
    Mean = mean(edeq_ex_driven_freq_28, na.rm = TRUE),
    Q3 = quantile(edeq_ex_driven_freq_28, 0.75, na.rm = TRUE),
    Min = min(edeq_ex_driven_freq_28, na.rm = TRUE),
    Max = max(edeq_ex_driven_freq_28, na.rm = TRUE),
    se = sd(edeq_ex_driven_freq_28, na.rm = TRUE)/sqrt(n())
  )

# Save the boxplot statistics
resave(EDEQ_boxplot_stats, file = df_file)


ggplot(mean_edeq_ex, aes(x = as.factor(ED100k_ex_compulsive_current2), y = mean_edeq_ex))  +
  geom_point()+
  geom_boxplot(data = EDGI_exercise_cleaned_2, aes( x = as.factor(ED100k_ex_compulsive_current2), y = edeq_ex_driven_freq_28, fill = as.factor(ED100k_ex_compulsive_current2))) +
  labs(title = paste('ED100k Maladapitve Exercise vs EDEQ Exercise Frequency (Past 28 days)', cohort), x = 'Maladapitve History [ED100k]', y = 'Number of Driven Exercise Episodes in Past 28 days [EDEQ]') + 
  scale_x_discrete(labels = c('No History', 'History, Not Current', 'Current')) +
  geom_text(data = mean_edeq_ex, aes(x  = ED100k_ex_compulsive_current2+1.2, y = mean_edeq_ex+3, label = paste0('M =', round(mean_edeq_ex,2))), size = 8)+
  theme(legend.position = 'none', text = element_text(size =20)) +
  scale_fill_manual(values = wes_palette("Darjeeling1"))

EDEQ_freq_file <- paste0("validation_paper/figs/EDEQ_freq_", cohort, ".png") 

ggsave(EDEQ_freq_file, width = 12, height = 10)

# label: tbl-EDEQZin
# tbl-cap: Zero-inflated Model Coefficients for ED100k Compulsive Exercise Predicting EDEQ Driven Exercise Days (past 28 days)

EDGI_exercise_cleaned <- EDGI_exercise_cleaned %>% 
  mutate(ex_current_dummy4 = case_when(ED100k_ex_compulsive_current2 == 0 ~ 0, 
                                       ED100k_ex_compulsive_current2 == 1 ~ 1,
                                       ED100k_ex_compulsive_current2 == 2 ~ 0),
         ex_current_dummy5 = case_when(ED100k_ex_compulsive_current2 == 0 ~ 0, 
                                       ED100k_ex_compulsive_current2 == 1 ~ 0,
                                       ED100k_ex_compulsive_current2 == 2 ~ 1))

EDEQ_zin1 <- zeroinfl(edeq_ex_driven_freq_28 ~ ex_current_dummy4 + ex_current_dummy5 | ex_current_dummy4 + ex_current_dummy5,
                      data = EDGI_exercise_cleaned)

#Figure out how to remove the 'model' variable before saving - has raw data in it
EDEQ_ZIN_full_models <- EDEQ_zin1
EDEQ_ZIN_full_models[["model"]] <- NA
resave(EDEQ_ZIN_full_models, file = df_file)


EDEQ_zin1_coefs <- summary(EDEQ_zin1)$coefficients

Counts <- as.data.frame(EDEQ_zin1_coefs$count) |> 
  mutate(`Model Part` = 'Count') |> 
  mutate(IRR = exp(Estimate)) |> 
  rownames_to_column (var = 'Term')  

Zeros <- as.data.frame(EDEQ_zin1_coefs$zero) |> 
  mutate(`Model Part` = 'Zero') |> 
  mutate(OR = exp(Estimate)) |> 
  rownames_to_column (var = 'Term') 



Zeros <- Zeros [, c(1, 6, 2,3, 7, 4:5)] 
Counts <- Counts [, c(1, 6, 2,3, 7, 4:5)]

ZIN_table <- full_join(Counts, Zeros) 

ZIN_table <- ZIN_table [, c(1,2,3,4,5,8,6:7)] 

ZIN_table$`Pr(>|z|)` <- sprintf('%.2e', ZIN_table$`Pr(>|z|)`)

vars <- c('Estimate', 'IRR', 'OR', 'Std. Error', 'z value')

ZIN_table[, vars] <- round(ZIN_table[, vars], digits = 3)

EDEQ_ZIN_table<- ZIN_table |> 
  filter(Term != 'Log(theta)') |> 
  mutate(Term = dplyr::recode(Term, 'ex_current_dummy4' = 'History vs. No History', 'ex_current_dummy5' = 'Current vs. No History'))

resave(EDEQ_ZIN_table, file = df_file)
