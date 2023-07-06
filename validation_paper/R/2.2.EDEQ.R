# label: fig-EDEQplot
# fig-cap:  ED100k Compulsive Exercise vs. EDEQ Exercise Frequency

mean_edeq_ex <- EDGI_exercise_cleaned|> 
  group_by (ED100k_ex_compulsive_current2) |> 
  filter(!is.na(ED100k_ex_compulsive_current2)) |> 
  summarize (mean_edeq_ex = mean(edeq_ex_driven_freq_28, na.rm = TRUE))


ggplot(mean_edeq_ex, aes( x = as.factor(ED100k_ex_compulsive_current2), y = mean_edeq_ex))  +
  geom_point()+
  geom_boxplot(data = EDGI_exercise_cleaned, aes( x = as.factor(ED100k_ex_compulsive_current2), y = edeq_ex_driven_freq_28, fill = as.factor(ED100k_ex_compulsive_current2))) +
  labs(title = 'ED100k Compulsive Exercise vs EDEQ Exercise Frequency (Past 28 days)', x = 'Complusive Exercise [ED100k]', y = 'Number of Driven Exercise Episodes in Past 28 days [EDEQ]') + 
  scale_x_discrete(labels = c('No History', 'History, Not Current', 'Current')) +
  theme(legend.position = 'none') +
  geom_text(data = mean_edeq_ex, aes(x  = ED100k_ex_compulsive_current2+1.2, y = mean_edeq_ex+3, label = paste0('M =', round(mean_edeq_ex,2)), size = 3)) + 
  scale_fill_manual(values = wes_palette("Darjeeling1"))

EDEQ_freq_file <- paste0("validation_paper/figs/EDEQ_freq_", cohort, ".png") 

ggsave(EDEQ_freq_file)

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
