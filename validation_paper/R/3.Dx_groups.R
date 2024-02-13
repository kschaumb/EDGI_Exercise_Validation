traits_aim3 <- c('ED100k_ex1_Q1_broad',
                 'ED100k_ex2_Q1_narrow', 
                 'ED100k_ex6_excessive', 
                 'ED100k_ex7_compensatory', 
                 'ED100k_ex8_maladaptive_current')

dx_frqs <- as.data.frame(frq(EDGI_exercise_cleaned$case_status))

# create an empty list to store the tables
tables_list <- list()

# loop over the column names
for (col_name in traits_aim3) {
  # create a table and add it to the list
  tables_list[[col_name]] <- table(EDGI_exercise_cleaned$case_status, EDGI_exercise_cleaned[[col_name]])
}

# Named vector mapping column names to output names
names_vector <- c("ED100k_ex1_Q1_broad" = "1. Q1 Any",
                 'ED100k_ex2_Q1_narrow' = "2. Q1 Regular", 
                 'ED100k_ex6_excessive' = '6. Excessive', 
                 'ED100k_ex7_compensatory' = '7. Compensatory', 
                 'ED100k_ex8_maladaptive_current' = '8. Current')

# Create an empty list to store the data frames
dx_row_percents <- list()

# Loop over the names vector
for (col_name in names(names_vector)) {
  # Create a data frame and add it to the list
  dx_row_percents[[col_name]] <- as.data.frame(prop.table(tables_list[[col_name]], 1)) |>
    filter(Var2 == 1) |>
    select(c(1,3)) |>
    rename(!!names_vector[col_name] := Freq)
}

# Convert list of data frames to a list that Reduce() can handle
df_list_as_list <- as.list(dx_row_percents)

# Use Reduce to iteratively join all data frames in the list
dx_row_percents <- Reduce(function(df1, df2) full_join(df1, df2), df_list_as_list) 

# Rename column
dx_row_percents <- dx_row_percents |>
  rename(`Diagnosis Group` = Var1)




dx_row_percents <- pivot_longer(dx_row_percents, cols =  c('1. Q1 Any', '2. Q1 Regular', '6. Excessive', '7. Compensatory', '8. Current'))

Dx_percents <- dx_row_percents
resave(Dx_percents, file = df_file)

diagnosis_order <- c("AN", "AN Mixed", "BN", "BN-BED Mixed", "BED")
construct_order <- c('1. Q1 Any', '2. Q1 Regular', '6. Excessive', '7. Compensatory', '8. Current')
# Convert the diagnosis group variable to a factor with the desired order
dx_row_percents$`Diagnosis Group` <- factor(dx_row_percents$`Diagnosis Group`, levels = diagnosis_order)
dx_row_percents$name <- factor(dx_row_percents$name, levels = construct_order)

resave(dx_row_percents, file = df_file)

## Run Models
Dx_table <- EDGI_exercise_cleaned %>%
  select(case_status, all_of(traits_aim3))

Dx_table$case_status <- factor(Dx_table$case_status)

# Create an empty list to store the models
model_list <- list()
OR_list <- list()

formulas <- c("ED100k_ex1_Q1_broad ~ case_status",
              "ED100k_ex2_Q1_narrow ~ case_status",
              "ED100k_ex6_excessive ~ case_status",
              "ED100k_ex7_compensatory ~ case_status",
              "ED100k_ex8_maladaptive_current ~ case_status")
dv_labels <- c('1. Q1 Any', '2. Q1 Regular', '6. Excessive', '7. Compensatory', '8. Current')

# Loop through the formulas
for (i in seq_along(formulas)) {
  # Fit the multinomial regression model
  model <- glm(as.formula(formulas[i]), data = Dx_table, family = binomial)
  model$DV <- dv_labels[i]
  
  # Store the model in the list
  model_list[[length(model_list) + 1]] <- model
}

dx_glm_models <- model_list 

posthoc_results <- list()
for (i in seq_along(model_list)) {
  posthoc_results[[i]] <- emmeans(model_list[[i]], pairwise ~ case_status, type = "response", adjust = 'bonferroni')
  posthoc_results[[i]]$DV <- dv_labels[i]
  
}

for (i in 1:length(model_list)) { 
dx_glm_models[[i]][["data"]] <- NA
dx_glm_models[[i]][["model"]] <- NA

}

resave(dx_glm_models, file = df_file)
resave(posthoc_results, file = df_file)


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
  mutate(`CI Low` = exp(conf.low)) |> 
  mutate(`CI High` = exp(conf.high)) |> 
  filter(term != '(Intercept)') |> 
  mutate(`Case Status` = dplyr::recode(term, "case_statusAN Mixed" = "AN Mixed", 'cased_statusBED' = 'BED', 'case_statusBN' = 'BN', 'case_statusBN-BED Mixed' = 'BN-BED Mixed', 'case_statusBED' = 'BED')) |> 
  select (-c(term, statistic)) 

Dx_groups_results_df <- results_df
resave(Dx_groups_results_df, file = df_file)


contrasts_df <- data.frame()

for (i in seq_along(posthoc_results)) {
  # Convert the model to tidy format
  contrasts <- as.data.frame(posthoc_results[[i]]$contrasts)
  
  # Add a column for the DV label
  contrasts$DV <- posthoc_results[[i]]$DV
  
  # Append the tidy model results to the overall results data frame
  contrasts_df <- bind_rows(contrasts_df, contrasts)
}

dx_contrasts_df <- contrasts_df |> 
  select(-c(df, null)) 

  
dx_contrasts_df <- dx_contrasts_df[, c("DV", "contrast", "odds.ratio", "SE", 'p.value')] 
dx_contrasts_df$Group1 <- sub("/.*", "", dx_contrasts_df$contrast)
dx_contrasts_df$Group2 <- sub(".*?/", "", dx_contrasts_df$contrast)
dx_contrasts_df$Group2 <- gsub("\\(|\\)", "", dx_contrasts_df$Group2)

resave(dx_contrasts_df, file = df_file)


annotation_df <- dx_contrasts_df |> 
  select(c(DV, contrast, Group1, Group2, `p.value`)) 

annotation_df$y = dplyr::recode(annotation_df$contrast, 
       'AN / AN Mixed' = 50, 
       'AN / BN' = 55,
       'AN / (BN-BED Mixed)' = 60,
       'AN / BED' = 65, 
       'AN Mixed / BN' = 70, 
       'AN Mixed / (BN-BED Mixed)' = 75, 
       'AN Mixed / BED' = 80,
       'BN / (BN-BED Mixed)' = 85,
       'BED / BN' = 90,
       'BED / (BN-BED Mixed)' = 95
       )

annotation_df$x = dplyr::recode(annotation_df$Group1, 
                         'AN ' = '1',
                         'AN Mixed ' = '2', 
                         'BN ' = '3',
                         'BN-BED Mixed ' = '4',
                         'BED ' = '5'
)

annotation_df$xend = dplyr::recode(annotation_df$Group2, 
                         ' AN' = '1',
                         ' AN Mixed' = '2', 
                         ' BN' = '3',
                         ' BN-BED Mixed' = '4',
                         ' BED' = '5')


dx_plots <- list()

for (var in construct_order) {
  
plot_data <- dx_row_percents |> 
  filter (name == var)
annotations = annotation_df |> 
  filter (DV == var,
          as.numeric(`p.value`) < 0.05) 

plot <- ggplot(plot_data, aes(x = `Diagnosis Group`, y = value*100, fill = `Diagnosis Group` )) +
  geom_col() +
geom_signif(annotation = as.numeric(annotations$`p.value`),
            xmin = as.numeric(annotations$x),
            xmax = as.numeric(annotations$xend),
            y_position = annotations$y) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  theme(legend.position = 'none') +
  geom_text(aes(x = `Diagnosis Group`, y = (value*100) - 5, label = paste0(round(value*100, 0), '%')), size = rel(3)) + 
  labs(y = 'Percentage (within Diagnosis Group)')

dx_plots[[var]] <- plot

}

dx_plot_1 <- Reduce(`+`, dx_plots)

dx_plot_1 + 
  plot_layout(ncol = 3) +
  plot_annotation(title = "Exercise Construct by Diagnosis Group", theme = theme(plot.title = element_text(hjust = 0.5))) 

dx_plot_1
dx_groups_fig_file <- paste0("validation_paper/figs/dx_groups_annotated", cohort, ".png")
ggsave(file = dx_groups_fig_file)


ggplot(dx_row_percents, aes(x = `Diagnosis Group`, y = value*100, fill = `Diagnosis Group` )) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, size = 14)) +
  theme(legend.position = 'none') +
  theme(text = element_text(size = 16)) +
  geom_text(aes(x = `Diagnosis Group`, y = (value*100) - 5, label = paste0(round(value*100, 0), '%')), size = rel(4)) + 
  labs(y = 'Percentage (within Diagnosis Group') +
facet_wrap (~ `name`) +
  labs(title = 'Percentages by Diagnosis Group and Exercise Construct', x = 'Diagnosis Group', y = 'Percentage (within Diagnosis Group)') 



dx_groups_fig_file_2 <- paste0("validation_paper/figs/dx_groups_", cohort, ".png")
ggsave(file = dx_groups_fig_file_2, height = 8, width = 10)

