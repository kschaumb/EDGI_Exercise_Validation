 

traits <- c('ED100k_ex1_Q1_broad',
            'ED100k_ex2_Q1_narrow', 
            'ED100k_ex3_compulsive_broad', 
            'ED100k_ex4_compulsive_narrow', 
            'ED100k_ex5_addictive', 
            'ED100k_ex6_excessive', 
            'ED100k_ex7_compensatory', 
            'ED100k_ex8_maladaptive_current')

apply_filter_select <- function(data, condition) {
  data %>%
    filter(!!sym(condition) == 1) %>%
    select(all_of(traits))
}

# Use lapply to loop through the list of filter conditions and create new data frames
result_list <- lapply(traits, function(trait) {
  apply_filter_select(EDGI_exercise_cleaned, trait)
})

samples <- result_list
sample_names <- c('1. Q1 Broad', '2. Q1 Narrow', '3. Compulsive/Driven Braod', '4. Compulsive/Driven Narrow', '6. Addictive', '6. Excessive', '7. Compensatory', '8. Current Maladaptive')

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
colnames(result_df) <- sample_names

# Name the rows with sample names
rownames(result_df) <- sample_names

result_df <- round(result_df, 2)
result_df$Sample <- rownames(result_df)

# recodes 100% values as NA
result_df[result_df == 100] <- NA  



# Reshape the data frame into long format
result_df_long <- tidyr::pivot_longer(result_df, cols = -Sample, names_to = "Trait", values_to = "Percentage")

result_df_long$Trait <- factor(result_df_long$Trait)

# Create the heatmap using ggplot and geom_tile
ggplot(result_df_long, aes(x = Sample, y = Trait)) +
  geom_tile(aes(fill = Percentage), color = "white") +
  geom_text(aes(label = sprintf("%.2f", Percentage)), color = "white", size = 6) +
  scale_fill_viridis_c(option = "viridis", begin = 0.1, end = 0.85, direction = -1) +
  labs(x = "Sample", y = "Exercise Construct") +
  ggtitle(stringr::str_wrap("Percentage within subsamples endorsing each exercise construct", width = 45))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), text = element_text(size =18))

# Save Heatmap Df
Heatmap_df <- result_df_long
resave(Heatmap_df, file = df_file)

heatmap_png_file <- paste0("validation_paper/figs/ex_heatmap_", cohort, ".png") 
ggsave(heatmap_png_file)  


# label: fig-Q1sensitivity
# fig-cap: Accuracy of Q1 in Detecting Additional Scoring Approaches

traits_cm <- c('ED100k_ex3_compulsive_broad', 
               'ED100k_ex4_compulsive_narrow', 
               'ED100k_ex5_addictive', 
               'ED100k_ex6_excessive', 
               'ED100k_ex7_compensatory', 
               'ED100k_ex8_maladaptive_current')

cm_list_1 <- list()
cm_list_2 <- list()

# Use a loop to calculate the confusion matrices for both traits
for (trait in traits_cm) {
  # First, calculate the confusion matrix for ED100k_ex1_Q1_broad
  confusion_matrix_1 <- caret::confusionMatrix(
    as.factor(EDGI_exercise_cleaned$ED100k_ex1_Q1_broad),
    as.factor(EDGI_exercise_cleaned[[trait]]),
    positive = '1'
  )
  cm_list_1[[trait]] <- confusion_matrix_1
  
  # Next, calculate the confusion matrix for ED100k_ex2_Q1_narrow
  confusion_matrix_2 <- caret::confusionMatrix(
    as.factor(EDGI_exercise_cleaned$ED100k_ex2_Q1_narrow),
    as.factor(EDGI_exercise_cleaned[[trait]]),
    positive = '1'
  )
  cm_list_2[[trait]] <- confusion_matrix_2
}

# List of objects containing confusion matrix output
confusion_matrices_list <- c(cm_list_1, cm_list_2)

# Vector to store PPV values
sensitivity_values <- numeric(length(confusion_matrices_list))
specificity_values <- numeric(length(confusion_matrices_list))
accuracy_values <- numeric(length(confusion_matrices_list))
ppv_values <- numeric(length(confusion_matrices_list))
npv_values <- numeric(length(confusion_matrices_list))

# Character vector to store object names
object_names <- character(length(confusion_matrices_list))

# Assign custom names to the objects in the list
names(confusion_matrices_list) <- c('Q1 Broad.3-Compulsive[B]', 'Q1 Broad.4-Compulsive[N]', 'Q1 Broad.5-Addictive' , 'Q1 Broad.6-Excessive', 'Q1 Broad.7-Compensate', 'Q1 Broad.8-Current', 'Q1 Narrow.3-Compulsive[B]', 'Q1 Narrow.4-Compulsive[N]', 'Q1 Narrow.5-Addictive' , 'Q1 Narrow.6-Excessive', 'Q1 Narrow.7-Compensate', 'Q1 Narrow.8-Current')

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

confusion_table$`Q1` <- sapply(strsplit(as.character(confusion_table$Object), ".", fixed = TRUE), "[[", 1)
confusion_table$`Exercise Type` <- sapply(strsplit(as.character(confusion_table$Object), ".", fixed = TRUE), "[[", 2)

confusion_table <- confusion_table %>% 
  select(-c(Object))

confusion_table <- cbind(confusion_table[, c(6, 7)], confusion_table[, -c(6, 7)]) #Move variable names to columns 1-2

# Round to two decimals
confusion_table[] <- lapply(confusion_table, function(x) if(is.numeric(x)) round(x, 3) else x)
confusion_table <- confusion_table %>% arrange(`Exercise Type`)
confusion_table_long <- pivot_longer(confusion_table, !c(`Q1`, `Exercise Type`), names_to = 'metric', values_to = 'value')

# Save Q1 Sensitivity Confusion Matrix
Q1_sensitivity_confusion_df <- confusion_table_long
resave(Q1_sensitivity_confusion_df, file = df_file)


ggplot(confusion_table_long, aes(x = `metric`, y = value, fill = `metric`)) +
  geom_col(position = 'dodge')+
  facet_grid(`Exercise Type` ~ `Q1`) +
  labs(title = 'Confusion Matrix Componenets Q1', x = 'Metric', y = 'Value') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 16)) +
  theme(legend.position = 'none', text = element_text(size = 16)) +
  geom_text(aes(x  = metric, y = (value)-.1, label = paste0(round(value,3))), size = rel(5)) + 
  scale_fill_manual(values = wes_palette("Moonrise3")) 
# Increase the plot area by changing the margins

Q1_sensitivity_file <- paste0("validation_paper/figs/Q1_sensitivity_", cohort, ".png") 
ggsave(Q1_sensitivity_file)  

# label: fig-npv
# fig-cap: Confusion matrix components of compulsive and maladaptive history vs. current CET and EDEQ exercise

EDGI_exercise_cleaned <- EDGI_exercise_cleaned %>%
  mutate(edeq_ex_weekly = case_when(edeq_ex_driven_freq_28 > 3 ~ 1, 
                                    edeq_ex_driven_freq_28 == 0 ~ 0),
         edeq_ex_any = case_when(edeq_ex_driven_freq_28 > 0 ~ 1, 
                                 edeq_ex_driven_freq_28 == 0 ~ 0))

traits_aim2 <- c('ED100k_ex1_Q1_broad',
            'ED100k_ex2_Q1_narrow', 
            'ED100k_ex6_excessive', 
            'ED100k_ex7_compensatory', 
            'ED100k_ex8_maladaptive_current')

cm_list_cet <- list()
cm_list_edeq_any <- list()
cm_list_edeq_weekly <- list()

# Use a loop to calculate the confusion matrices for both traits
for (trait in traits_aim2) {
  # First, calculate the confusion matrix for ED100k_ex1_Q1_broad
  confusion_matrix_1 <- caret::confusionMatrix(
    as.factor(EDGI_exercise_cleaned$cet_clinical),
    as.factor(EDGI_exercise_cleaned[[trait]]),
    positive = '1'
  )
  cm_list_cet[[trait]] <- confusion_matrix_1
  
  # Next, calculate the confusion matrix for ED100k_ex2_Q1_narrow
  confusion_matrix_2 <- caret::confusionMatrix(
    as.factor(EDGI_exercise_cleaned$edeq_ex_any),
    as.factor(EDGI_exercise_cleaned[[trait]]),
    positive = '1'
  )
  cm_list_edeq_any[[trait]] <- confusion_matrix_2
  
confusion_matrix_3 <- caret::confusionMatrix(
    as.factor(EDGI_exercise_cleaned$edeq_ex_weekly),
    as.factor(EDGI_exercise_cleaned[[trait]]),
    positive = '1'
  )
  cm_list_edeq_weekly[[trait]] <- confusion_matrix_3
}

cm_list_aim2 <- c(cm_list_cet, cm_list_edeq_any, cm_list_edeq_weekly)

# Vector to store PPV values
sensitivity_values <- numeric(length(cm_list_aim2))
specificity_values <- numeric(length(cm_list_aim2))
accuracy_values <- numeric(length(cm_list_aim2))
ppv_values <- numeric(length(cm_list_aim2))
npv_values <- numeric(length(cm_list_aim2))

# Character vector to store object names
object_names <- character(length(cm_list_aim2))

# Assign custom names to the objects in the list
trait_names_aim2 <- c('1-Q1 Broad',
                 '2-Q1 Narrow', 
                 '6-Excessive', 
                 '7-Compensatory', 
                 '8-Current')
dvs_aim2 <- c('CET Clinical', 'EDEQ Any', 'EDEQ Weekly')
combinations <- expand.grid(traits = trait_names_aim2, dvs = dvs_aim2, stringsAsFactors = FALSE)
combination_names <- paste(combinations$traits, combinations$dvs, sep = '.')

names(cm_list_aim2) <- combination_names

# Extract PPV values and object names
for (i in 1:length(cm_list_aim2)) {
  sensitivity_values[i] <- cm_list_aim2[[i]]$byClass[1]
  specificity_values[i] <- cm_list_aim2[[i]]$byClass[2]
  ppv_values[i] <- cm_list_aim2[[i]]$byClass[3]
  npv_values[i] <- cm_list_aim2[[i]]$byClass[4]
  accuracy_values[i] <- cm_list_aim2[[i]]$overall[1]
  object_names[i] <- names(cm_list_aim2)[i]
}

confusion_table <- data.frame(Object = object_names, sensitivity = sensitivity_values, specificity = specificity_values, ppv = ppv_values, npv = npv_values, accuracy= accuracy_values)

confusion_table$`Q1 Criteria` <- sapply(strsplit(as.character(confusion_table$Object), ".", fixed = TRUE), "[[", 1)
confusion_table$`Exercise Type` <- sapply(strsplit(as.character(confusion_table$Object), ".", fixed = TRUE), "[[", 2)

confusion_table <- confusion_table %>% 
  select(-c(Object))


confusion_table <- cbind(confusion_table[, c(6, 7)], confusion_table[, -c(6, 7)]) #Move variable names to columns 1-2

# Round to two decimals
confusion_table[] <- lapply(confusion_table, function(x) if(is.numeric(x)) round(x, 2) else x)
confusion_table_long <- pivot_longer(confusion_table, !c(`Q1 Criteria`, `Exercise Type`), names_to = 'metric', values_to = 'value')

# Save CET and EDEQ Confusion Matrix
CET_EDEQ_confusion_df <- confusion_table_long
resave(CET_EDEQ_confusion_df, file = df_file)

ggplot(confusion_table_long, aes(x = `metric`, y = value, fill = `metric`)) +
  geom_col(position = 'dodge')+
  facet_grid(`Exercise Type` ~ `Q1 Criteria`) +
  labs(title = 'ED100k Exercise Scoring Algorithms predicting CET and EDEQ Current Exercise', x = 'Metric', y = 'Value') + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 16)) +
  theme(legend.position = 'none', text = element_text(size = 16)) +
  geom_text(aes(x  = metric, y = (value)-.1, label = paste0(round(value,3))), size = rel(5)) + 
  scale_fill_manual(values = wes_palette("Moonrise3")) 

NPV_file <- paste0("validation_paper/figs/NPV_", cohort, ".png") 

ggsave(NPV_file)  

