
ed100k_ex_items <- c('exercise', 'ED100k_ex7_compensatory_factor', 'ED100k_ex9_maladaptive_hxplusc', 'ED100k_ex_dur_factor', 'ED100k_ex_freq_factor', 'ex_friend_2_factor', 'ex_ill_2_factor', 'ex_diet_2_factor', 'ex_distress_2_factor', 'ex_compel_2_factor', 'ex_age', 'ex_age_last')


ed100k_ex_defs <- c('ED100k_ex1_Q1_broad',
                    'ED100k_ex2_Q1_narrow', 
                    'ED100k_ex3_compulsive_broad', 
                    'ED100k_ex4_compulsive_narrow', 
                    'ED100k_ex5_addictive', 
                    'ED100k_ex6_excessive', 
                    'ED100k_ex7_compensatory', 
                    'ED100k_ex8_maladaptive_current', 
                    'ED100k_ex9_maladaptive_hxplusc')

ed100k_ex_data <- EDGI_exercise_cleaned %>% 
  filter(case_status != 'Control') |> 
  select(ed100k_ex_items) 

ed100k_ex_data$exercise <- factor(ed100k_ex_data$exercise,
                                  levels = c(1, 2, 3),
                                  labels = c("no", "sometimes", "more often"))

ed100k_ex_data$ED100k_ex9_maladaptive_hxplusc <- factor(ed100k_ex_data$ED100k_ex9_maladaptive_hxplusc,
                                                       levels = c(0, 1, 2),
                                                       labels = c("No History", "History, Not Current", "Current"))


factor_frequencies <- lapply(ed100k_ex_data, function(x) {
  if (is.factor(x)) {
    x <- addNA(x)
    tbl <- table(x, useNA = "ifany")
    percent <- prop.table(tbl) * 100
    data.frame(Response = names(tbl), Freq = as.vector(tbl), Percent = as.vector(percent))
  } else {
    NULL
  }
})


factor_frequencies <- factor_frequencies[sapply(factor_frequencies, Negate(is.null))]

ED100k_items_table <- bind_rows(factor_frequencies, .id = "Variable") %>% 
  mutate(Variable = dplyr::recode(Variable, 'exercise' = '1. Exercised excessively', 'ED100k_ex7_compensatory_factor' = '12. Compensatory Exercise', 'ED100k_ex9_maladaptive_hxplusc' = '9. Current Maladaptive Exercise', 'ED100k_ex_dur_factor' = '7. Exercise Duration', 'ED100k_ex_freq_factor' = '8. Exercise Frequency', 'ex_friend_2_factor' = '4. Interfering with Friendship', 'ex_ill_2_factor' = '5. Exercising when ill', 'ex_diet_2_factor' = '6. Modified Diet if unable to Exercise', 'ex_distress_2_factor' = '3. Distressed when unable to exercise', 'ex_compel_2_factor' = '2. Compelled to Exercise')) %>% 
  sort_asc(Variable) %>% 
  mutate(Response = as.character(Response),  # Ensure Response is a character
         Response = case_when(
           Response == 'NaN' ~ 'Dont Know/Prefer Not to Answer',
           Response == 'no' ~ 'No',
           Response == 'sometimes' ~ 'Sometimes',
           Response == 'more often' ~ 'More Often',
           is.na(Response) ~ 'Missing',  # Handle NAs
           TRUE ~ Response)) %>%
  mutate(Percent = round(Percent, 2))

save(ED100k_items_table, file = df_file)

