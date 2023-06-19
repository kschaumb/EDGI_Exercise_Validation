 

traits <- c('ED100k_ex_compulsive', 'ED100k_ex_compulsive_strict_3mo', 'ED100k_ex_addictive', 'ED100k_ex_excessive', 'ED100k_ex_compensatory', 'ED100k_ex_maladaptive_1')
traits_clean <- c('Compulsive', 'Regular Compulsive', 'Addictive', 'Excessive', 'Compensatory', 'Maladaptive (Broad)')

full_sample <- EDGI_exercise_cleaned %>%
  select(all_of(traits))

exercisers <- EDGI_exercise_cleaned %>%
  filter(ED100k_exercise_icb > 0) %>%
  select(all_of(traits))

compulsive_exercisers <- EDGI_exercise_cleaned %>%
  filter(ED100k_ex_compulsive > 0) %>%
  select(all_of(traits))

regular_exercisers <- EDGI_exercise_cleaned %>%
  filter(ED100k_exercise_icb > 1) %>%
  select(all_of(traits))

regular_compulsive_exercisers <- EDGI_exercise_cleaned %>%
  filter(ED100k_ex_compulsive_strict_3mo == 1) %>%
  select(all_of(traits))

addictive_exercisers <- EDGI_exercise_cleaned %>%
  filter(ED100k_ex_addictive == 1) %>%
  select(all_of(traits))

excessive_exercisers <- EDGI_exercise_cleaned %>%
  filter(ED100k_ex_excessive == 1) %>%
  select(all_of(traits))

compensatory_exercisers <- EDGI_exercise_cleaned %>%
  filter(ED100k_ex_compensatory== 1) %>%
  select(all_of(traits))

samples <- list(full_sample, exercisers, regular_exercisers, compulsive_exercisers, regular_compulsive_exercisers, addictive_exercisers, excessive_exercisers, compensatory_exercisers)
sample_names <- c('1. Full Sample', '2. Exercisers (Q1)', '3. Regular Exercisers (Q1)', '4. Compulsive Exercisers', '5. Regular Compulsive Exercisers', '6. Addictive Exercisers', '7. Excessive Exercisers', '8. Compensatory Exercisers')

calculate_valid_percent <- function(sample, trait) {
  valid_percent <- sum(!is.na(sample[[trait]]) & sample[[trait]] == 1) / sum(!is.na(sample[[trait]])) * 100
  return(valid_percent)
}

result_df <- data.frame(stringsAsFactors = FALSE)

for (i in seq_along(samples)) {
  sample <- samples[[i]]
  valid_percent <- sapply(traits, calculate_valid_percent, sample = sample)
  result_df <- rbind(result_df, c(names(samples)[i], t(valid_percent)))
}

# Assign column names directly
colnames(result_df) <- traits_clean

# Name the rows with sample names
rownames(result_df) <- sample_names
result_df <- result_df[, c(6, 1:5)]

result_df <- round(result_df, 2)

result_df$Sample <- rownames(result_df)

# recodes 100% values as NA
result_df[result_df == 100] <- NA 


# Reshape the data frame into long format
result_df_long <- tidyr::pivot_longer(result_df, cols = -Sample, names_to = "Trait", values_to = "Percentage")

trait_order <- c("Maladaptive (Broad)", "Compulsive", "Regular Compulsive", 'Addictive', 'Excessive', 'Compensatory')  # Replace with the desired order of traits
result_df_long$Trait <- factor(result_df_long$Trait, levels = trait_order)

# Create the heatmap using ggplot and geom_tile
ggplot(result_df_long, aes(x = Sample, y = Trait)) +
  geom_tile(aes(fill = Percentage), color = "white") +
  geom_text(aes(label = sprintf("%.2f", Percentage)), color = "white") +
  scale_fill_viridis_c(option = "viridis", begin = 0.1, end = 0.85, direction = -1) +
  labs(x = "Sample", y = "Exercise Construct") +
  ggtitle(stringr::str_wrap("Percentage within subsamples endorsing each exercise construct", width = 45))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

heatmap_file <- paste0("validation_paper/figs/ex_heatmap_", cohort, ".png") 
ggsave(heatmap_file)  


# label: fig-Q1sensitivity
# fig-cap: Accuracy of Q1 in Detecting Compulsive, Addictive, and Excessive Exercise

regular_exercisers <- EDGI_exercise_cleaned |> 
  filter(ED100k_exercise_icb == 2)
regular_compulsive_exercisers <- EDGI_exercise_cleaned |> 
  filter (ED100k_ex_compulsive_strict == 1)

#makes 3 month duration statistic for inclusion in paragrph below
regular_exercisers <- regular_exercisers %>% 
  mutate(ex_dur_3m = case_when(ED100k_ex_dur > 2 ~ 1,
                               ED100k_ex_dur <= 2 ~ 0))


Any.Addictive <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_ex_any), as.factor(EDGI_exercise_cleaned$ED100k_ex_addictive), positive = '1')

Any.Compulsive <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_ex_any), as.factor(EDGI_exercise_cleaned$ED100k_ex_compulsive), positive = '1')

Any.Excessive <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_ex_any), as.factor(EDGI_exercise_cleaned$ED100k_ex_excessive), positive = '1')

Any.Compensatory <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_ex_any), as.factor(EDGI_exercise_cleaned$ED100k_ex_compensatory), positive = '1')

Any.RegCompulsive <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_ex_any), as.factor(EDGI_exercise_cleaned$ED100k_ex_compulsive_strict), positive = '1')


EDGI_exercise_cleaned <-  EDGI_exercise_cleaned %>% 
  mutate (ED100k_exercise_regular = case_when(ED100k_exercise_icb == 2~ 1,
                                              ED100k_exercise_icb < 2 ~ 0))

Regular.Addictive <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_exercise_regular), as.factor(EDGI_exercise_cleaned$ED100k_ex_addictive), positive = '1')

Regular.Compulsive <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_exercise_regular), as.factor(EDGI_exercise_cleaned$ED100k_ex_compulsive), positive = '1')

Regular.Excessive <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_exercise_regular), as.factor(EDGI_exercise_cleaned$ED100k_ex_excessive), positive = '1')

Regular.Compensatory <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_exercise_regular), as.factor(EDGI_exercise_cleaned$ED100k_ex_compensatory), positive = '1')

Regular.RegCompulsive <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_exercise_regular), as.factor(EDGI_exercise_cleaned$ED100k_ex_compulsive_strict), positive = '1')



# List of objects containing confusion matrix output
confusion_matrices_list <- list(Any.Addictive, Any.Compulsive, Any.RegCompulsive, Any.Excessive, Any.Compensatory, Regular.Addictive, Regular.Compulsive, Regular.RegCompulsive, Regular.Excessive, Regular.Compensatory)

# Vector to store PPV values
sensitivity_values <- numeric(length(confusion_matrices_list))
specificity_values <- numeric(length(confusion_matrices_list))
accuracy_values <- numeric(length(confusion_matrices_list))
ppv_values <- numeric(length(confusion_matrices_list))
npv_values <- numeric(length(confusion_matrices_list))

# Character vector to store object names
object_names <- character(length(confusion_matrices_list))

# Assign custom names to the objects in the list
names(confusion_matrices_list) <- c('Any.Addictive', 'Any.Compul', 'Any.RegCompul' , 'Any.Excessive', 'Any.Compensate', 'Regular.Addictive', 'Regular.Compul', 'Regular.RegCompul', 'Regular.Excessive', 'Regular.Compensate')

# Extract PPV values and object names
for (i in 1:length(confusion_matrices_list)) {
  sensitivity_values[i] <- confusion_matrices_list[[i]]$byClass[1]
  specificity_values[i] <- confusion_matrices_list[[i]]$byClass[2]
  ppv_values[i] <- confusion_matrices_list[[i]]$byClass[3]
  npv_values[i] <- confusion_matrices_list[[i]]$byClass[4]
  accuracy_values[i] <- confusion_matrices_list[[i]]$overall[1]
  object_names[i] <- names(confusion_matrices_list)[i]
}

confusion_table <- data.frame(Object = object_names, sensitivity = sensitivity_values, specificity = specificity_values, ppv = ppv_values, npv = npv_values, accuracy= accuracy_values)

confusion_table$`Q1 Criteria` <- sapply(strsplit(as.character(confusion_table$Object), ".", fixed = TRUE), "[[", 1)
confusion_table$`Exercise Type` <- sapply(strsplit(as.character(confusion_table$Object), ".", fixed = TRUE), "[[", 2)

confusion_table <- confusion_table %>% 
  select(-c(Object))


confusion_table <- cbind(confusion_table[, c(6, 7)], confusion_table[, -c(6, 7)]) #Move variable names to columns 1-2

# Round to two decimals
confusion_table[] <- lapply(confusion_table, function(x) if(is.numeric(x)) round(x, 3) else x)
confusion_table$`Q1 Criteria` <- dplyr::recode(confusion_table$`Q1 Criteria`, Regular = 'More Often') 
confusion_table <- confusion_table %>% arrange(`Exercise Type`)
confusion_table_long <- pivot_longer(confusion_table, !c(`Q1 Criteria`, `Exercise Type`), names_to = 'metric', values_to = 'value')

ggplot(confusion_table_long, aes(x = `metric`, y = value, fill = `metric`)) +
  geom_col(position = 'dodge')+
  facet_grid(`Exercise Type` ~ `Q1 Criteria`) +
  labs(title = 'Confusion Matrix Componenets by Answer to Q1', x = 'Metric', y = 'Value') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(legend.position = 'none') +
  geom_text(aes(x  = metric, y = (value)-.1, label = paste0(round(value,3))), size = rel(3)) + 
  scale_fill_manual(values = wes_palette("Moonrise3")) 
# Increase the plot area by changing the margins

Q1_sensitivity_file <- paste0("validation_paper/figs/Q1_sensitivity_", cohort, ".png") 
ggsave(Q1_sensitivity_file)  

# label: fig-npv
# fig-cap: Confusion matrix components of compulsive and maladaptive history vs. current CET and EDEQ exercise

Maladaptive.CET <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_ex_maladaptive_1), as.factor(EDGI_exercise_cleaned$cet_clinical), positive = '1')

Compulsive.CET <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_ex_compulsive), as.factor(EDGI_exercise_cleaned$cet_clinical), positive = '1')

EDGI_exercise_cleaned <- EDGI_exercise_cleaned %>%
  mutate(edeq_ex_any = case_when(edeq_ex_driven_freq_28 > 0 ~ 1, 
                                 edeq_ex_driven_freq_28 == 0 ~ 0))

Maladaptive.EDEQ <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_ex_maladaptive_1), as.factor(EDGI_exercise_cleaned$edeq_ex_any), positive = '1')

Compulsive.EDEQ <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_ex_compulsive), as.factor(EDGI_exercise_cleaned$edeq_ex_any), positive = '1')

EDGI_exercise_cleaned <- EDGI_exercise_cleaned %>%
  mutate(edeq_ex_weekly = case_when(edeq_ex_driven_freq_28 > 3 ~ 1, 
                                    edeq_ex_driven_freq_28 == 0 ~ 0))

Maladaptive.EDEQ_weekly <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_ex_maladaptive_1), as.factor(EDGI_exercise_cleaned$edeq_ex_weekly), positive = '1')

Compulsive.EDEQ_weekly <- caret::confusionMatrix(as.factor(EDGI_exercise_cleaned$ED100k_ex_compulsive), as.factor(EDGI_exercise_cleaned$edeq_ex_weekly), positive = '1')


# List of objects containing confusion matrix output
confusion_matrices_list <- list(Maladaptive.CET, Compulsive.CET, Maladaptive.EDEQ, Compulsive.EDEQ, Maladaptive.EDEQ_weekly, Compulsive.EDEQ_weekly)

# Vector to store PPV values
sensitivity_values <- numeric(length(confusion_matrices_list))
specificity_values <- numeric(length(confusion_matrices_list))
accuracy_values <- numeric(length(confusion_matrices_list))
ppv_values <- numeric(length(confusion_matrices_list))
npv_values <- numeric(length(confusion_matrices_list))

# Character vector to store object names
object_names <- character(length(confusion_matrices_list))

# Assign custom names to the objects in the list
names(confusion_matrices_list) <- c('Maladaptive.CET_clinical', 'Compulsive.CET_clinical', 'Maladaptive.EDEQ_any', 'Compulsive.EDEQ_any', 'Maladaptive.EDEQ_weekly', 'Compulsive.EDEQ_weekly')

# Extract PPV values and object names
for (i in 1:length(confusion_matrices_list)) {
  sensitivity_values[i] <- confusion_matrices_list[[i]]$byClass[1]
  specificity_values[i] <- confusion_matrices_list[[i]]$byClass[2]
  ppv_values[i] <- confusion_matrices_list[[i]]$byClass[3]
  npv_values[i] <- confusion_matrices_list[[i]]$byClass[4]
  accuracy_values[i] <- confusion_matrices_list[[i]]$overall[1]
  object_names[i] <- names(confusion_matrices_list)[i]
}

confusion_table <- data.frame(Object = object_names, sensitivity = sensitivity_values, specificity = specificity_values, ppv = ppv_values, npv = npv_values, accuracy= accuracy_values)

confusion_table$`Q1 Criteria` <- sapply(strsplit(as.character(confusion_table$Object), ".", fixed = TRUE), "[[", 1)
confusion_table$`Exercise Type` <- sapply(strsplit(as.character(confusion_table$Object), ".", fixed = TRUE), "[[", 2)

confusion_table <- confusion_table %>% 
  select(-c(Object))


confusion_table <- cbind(confusion_table[, c(6, 7)], confusion_table[, -c(6, 7)]) #Move variable names to columns 1-2

# Round to two decimals
confusion_table[] <- lapply(confusion_table, function(x) if(is.numeric(x)) round(x, 3) else x)
confusion_table$`Q1 Criteria` <- dplyr::recode(confusion_table$`Q1 Criteria`, Regular = 'More Often') 
confusion_table <- confusion_table %>% arrange(`Exercise Type`)
confusion_table_long <- pivot_longer(confusion_table, !c(`Q1 Criteria`, `Exercise Type`), names_to = 'metric', values_to = 'value')

ggplot(confusion_table_long, aes(x = `metric`, y = value, fill = `metric`)) +
  geom_col(position = 'dodge')+
  facet_grid(`Exercise Type` ~ `Q1 Criteria`) +
  labs(title = 'Compulsive and Maladaptive Exercise History vs CET and EDEQ', x = 'Metric', y = 'Value') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(legend.position = 'none') +
  geom_text(aes(x  = metric, y = (value)-.1, label = paste0(round(value,3))), size = rel(3)) + 
  scale_fill_manual(values = wes_palette("Moonrise3")) 

NPV_file <- paste0("validation_paper/figs/NPV_", cohort, ".png") 

ggsave(NPV_file)  

