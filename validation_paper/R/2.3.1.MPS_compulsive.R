
# Below makes the 5 case status diagnosis groups
case_status_vars <- c('AN', 'AN Mixed', 'BN', 'BN-BED Mixed', 'BED')
mps_vars <- c('mps_ps', 'mps_cm', 'mps_da')
diagnosis_order <- c("AN", "AN Mixed", "BN", "BN-BED Mixed", "BED")


# Define the labels for the levels
level_labels <- c("No Compulsive Exercise", "Compulsive Exercise")

# Define custom labels for the OCI variables
mps_labels <- c(
  mps_ps = "Personal Standards",
  mps_cm = "Concern over Mistakes",
  mps_da = "Doubts about Actions")


# Create an empty list to store the nested data
nested_data <- list()

# Perform filtering, t-tests, and calculate additional statistics in nested loops
for (status in case_status_vars) {
  nested_data[[status]] <- list()
  
  for (var in mps_vars) {
    filtered_data <- EDGI_exercise_cleaned %>%
      filter(case_status == status)
    
    t_test <- t.test(filtered_data[[var]] ~ filtered_data$ED100k_ex_compulsive)
    
    levels <- c(0,1)
    level_stats <- list()
    
    for (level in levels) {
      level_data <- filtered_data %>%
        filter(ED100k_ex_compulsive == level)
      
      level_stats[[as.character(level)]] <- list(
        mean = mean(level_data[[var]], na.rm = TRUE),
        sd = sd(level_data[[var]], na.rm = TRUE),
        n = length(level_data[[var]]),
        se = sd(level_data[[var]], na.rm = TRUE)/sqrt(length(level_data[[var]]))
      )
    }
    
    nested_data[[status]][[var]] <- list(
      t_test_result = t_test,
      p_value = t_test$p.value,
      level_stats = level_stats
    )
  }
}

MPS_compulsive_data <- nested_data
resave(MPS_compulsive_data, file = df_file)


tidy_results <- data.frame()
tidy_result <- data.frame()

# Loop through case_status and variable
for (status in case_status_vars) {
  for (var in mps_vars) {
    # Extract the t-test result
    t_test_result <- MPS_compulsive_data[[status]][[var]]$t_test_result
    
    # Tidy the t-test result and add it to the tidy_results data frame
    tidy_result <- broom::tidy(t_test_result)
    tidy_result$status <- status
    tidy_result$variable <- var
    
    tidy_results <- rbind(tidy_results, tidy_result)
  }
}





# Iterate over case statuses
for (status in case_status_vars) {
  # Iterate over mps variables
  for (var in mps_vars) {
    # Extract sd from mps_compulsive_data
    sd1 <- MPS_compulsive_data[[status]][[var]]$level_stats$`0`$sd
    sd2 <- MPS_compulsive_data[[status]][[var]]$level_stats$`1`$sd
    
    # Find the rows matching the current case status and mps variable
    rows <- tidy_results$status == status & tidy_results$variable == var
    
    # Update sd1 and sd2 in the matching rows
    tidy_results[rows, c("sd1", "sd2")] <- list(sd1, sd2)
  }
}
tidy_results <- tidy_results %>%
  mutate(cohens_d = abs((as.numeric(estimate1) - as.numeric(estimate2))/ sqrt((sd1^2 + sd2^2) / 2)))

# Format p-values using scientific notation with three significant digits
tidy_results$formatted_pvalue <- signif(tidy_results$p.value, digits = 3)

# Convert p-values to scientific notation
tidy_results$formatted_pvalue <- sprintf("%.2e", tidy_results$p.value)

# Format other columns
tidy_results <- tidy_results %>%
  mutate(across(where(is.numeric), ~ sprintf("%.3f", .)),
         across(where(is.character), as.character))


tidy_results <- tidy_results |> 
  select(estimate, statistic, status, variable, cohens_d, formatted_pvalue)

tidy_results <- tidy_results |> 
  rename(`Mean Diff` = estimate, t = statistic, `Case Status` = status, `Cohens D`= cohens_d, p = formatted_pvalue) 


tidy_results <- tidy_results %>%
  mutate(variable = case_when(
    variable == "mps_ps" ~ "Personal Standards",
    variable == "mps_cm" ~ "Concern Over Mistakes",
    variable == "mps_da" ~ "Doubts About Actions",
    TRUE ~ as.character(variable)
  ))


tidy_results <- tidy_results |> 
  mutate(`Mean Diff` = as.numeric(`Mean Diff`)*-1) |> 
  mutate(`t` = as.numeric(`t`)*-1)

tidy_results$`FDR p val` <- sprintf("%.2e", p.adjust(tidy_results$p, method = 'fdr'))

MPS_tidy_results <- tidy_results

resave(MPS_tidy_results, file = df_file)

case_status_vars <- c('AN', 'AN Mixed', 'BN', 'BN-BED Mixed', 'BED')
mps_vars <- c('mps_ps', 'mps_cm', 'mps_da')
level_labels <- c("No Compulsive Exercise", "Compulsive Exercise")
mps_labels <- c("Personal Standards", "Concern Over Mistakes", "Doubts About Actions")


graph_df <- EDGI_exercise_cleaned |> 
  select(case_status, all_of(mps_vars), ED100k_ex_compulsive) |>
  filter(!is.na(ED100k_ex_compulsive)) |> 
  rename(`Personal Standards` = mps_ps, 
         `Concern Over Mistakes` = mps_cm, 
         `Doubts About Actions` = mps_da)

graph_df$`case_status` <- factor(graph_df$`case_status`, levels = diagnosis_order)

ggplots <- list()

for (var in mps_labels) {
  
  p_vals <- MPS_tidy_results |> 
    filter (variable == var) 
  p = p_vals$`FDR p val`
  
  annotation_df = data.frame(x = c(0.8,1.8,2.8,3.8,4.8),
                             xend = c(1.2,2.2,3.2,4.2,5.2),
                             y = c(21,21,21,21,21),
                             annotation = p) |> 
    filter (as.numeric(p) < 0.05) 
  
  plot <- 
    ggplot(graph_df, aes(x = case_status, y = !!sym(var), fill = factor(ED100k_ex_compulsive))) +
    geom_boxplot() +
    geom_signif(annotation = annotation_df$annotation,
                xmin = annotation_df$x,
                xmax = annotation_df$xend,
                y_position = annotation_df$y) +
    labs(fill = "ED100k Compulsive Exercise History", title = var) +
    scale_fill_manual(values = c('0' = 'indianred', '1' = 'deepskyblue1'),
                      labels = c("No", "Yes")) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = if (var == mps_labels[3]) element_text(angle = 45, hjust = 1, size = 12) else element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = ifelse(var == mps_labels[3], "bottom", "none")) +
    ylim(4,23)
  
  ggplots[[var]] <- plot
}

MPS_plot <- ggplots[[1]] +
  ggplots[[2]] +
  ggplots[[3]] +
  plot_layout(ncol = 1) +
  plot_annotation(title = "MPS Subscale Scores Across Exercise and Diagnosis Group", theme = theme(plot.title = element_text(hjust = 0.5))) 

MPS_plot

MPS_plot_file <- paste0("validation_paper/figs/MPS_compulsive_", cohort, ".png") 
ggsave(MPS_plot_file)  



