
# label: tbl-ed100kItems
# tbl-cap: Item-level descriptives for ED100k Exercise Items


ed100k_items <- c('exercise', 'ED100k_ex_compensatory_factor', 'ED100k_ex_compulsive_current2', 'ED100k_ex_dur_factor', 'ED100k_ex_freq_factor', 'ex_friend_2_factor', 'ex_ill_2_factor', 'ex_diet_2_factor', 'ex_distress_2_factor', 'ex_compel_2_factor', 'ex_age', 'ex_age_last')

ed100k_ex_data <- EDGI_exercise_cleaned %>% 
  select(ed100k_items)

ed100k_ex_data$exercise <- factor(ed100k_ex_data$exercise,
                                  levels = c(1, 2, 3),
                                  labels = c("no", "sometimes", "more often"))

ed100k_ex_data$ED100k_ex_compulsive_current2 <- factor(ed100k_ex_data$ED100k_ex_compulsive_current2,
                                                       levels = c(0, 1, 2),
                                                       labels = c("No History", "History, Not Current", "Current"))


factor_frequencies <- lapply(ed100k_ex_data, function(x) {
  if (is.factor(x)) {
    tbl <- table(x)
    percent <- prop.table(tbl) * 100
    data.frame(Response = names(tbl), Freq = as.vector(tbl), Percent = as.vector(percent))
  } else {
    NULL
  }
})


factor_frequencies <- factor_frequencies[sapply(factor_frequencies, Negate(is.null))]

library(haven)

ed100k_items_table <- bind_rows(factor_frequencies, .id = "Variable") %>% 
  mutate(Variable = dplyr::recode(Variable, 'exercise' = '1. Exercised excessively', 'ED100k_ex_compensatory_factor' = 'Q12. Compensatory Exercise', 'ED100k_ex_compulsive_current2' = '9. Current Exercise', 'ED100k_ex_dur_factor' = '7. Exercise Duration', 'ED100k_ex_freq_factor' = '8. Exercise Frequency', 'ex_friend_2_factor' = '4. Interfering with Friendship', 'ex_ill_2_factor' = '5. Exercising when ill', 'ex_diet_2_factor' = '6. Modified Diet if unable to Exercise', 'ex_distress_2_factor' = '3. Distressed when unable to exercise', 'ex_compel_2_factor' = '2. Compelled to Exercise')) %>% 
  sort_asc(Variable) %>% 
  mutate(Response = dplyr::recode(Response, 'NaN' = 'Missing', 'no' = 'No', 'sometimes' = 'Sometimes', 'more often' = 'More Often')) %>% 
  mutate(Percent = round(Percent, 2))

ED100k_items_file <- paste0("validation_paper/tabs/ED100k_items_", cohort, ".RData") 
save(ed100k_items_table, file = ED100k_items_file)
