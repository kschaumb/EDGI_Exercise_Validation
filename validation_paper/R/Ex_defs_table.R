
library(expss)
library(dplyr)
library(haven)
library(sjmisc)
library(psych)
library(ggplot2)
library(tidyr)
library(caret)

load('data/EDGI_exercise_cleaned.RData') 
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

library(ggplot2)
library(viridis)  # Load the 'viridis' package for color palette

# Assuming you have the 'result_df' data frame

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

ggsave('validation_paper/figs/ex_heatmap.png')  
