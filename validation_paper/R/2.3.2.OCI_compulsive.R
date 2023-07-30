
case_status_vars <- c('AN', 'AN Mixed', 'BN', 'BN-BED Mixed', 'BED')
exercise_vars <- c('ED100k_ex_maladaptive', 'ED100k_ex_compulsive')
oci_vars <- c('oci12_wash', 'oci12_check', 'oci12_order', 'oci12_obsess', 'oci12_total')
diagnosis_order <- c("AN", "AN Mixed", "BN", "BN-BED Mixed", "BED")


# Define the labels for the levels
level_labels <- c("No Compulsive Exercise", "Compulsive Exercise")

# Define custom labels for the OCI variables
oci_labels <- c(
  oci12_wash = "Washing",
  oci12_check = "Checking",
  oci12_order = "Order",
  oci12_obsess = "Obsessions",
  oci12_total = "Total"
)


# Create an empty list to store the nested data
nested_data <- list()

# Perform filtering, t-tests, and calculate additional statistics in nested loops
for (status in case_status_vars) {
  nested_data[[status]] <- list()
  
  for (var in oci_vars) {
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

OCI_compulsive_data <- nested_data
resave(OCI_compulsive_data, file = df_file)


tidy_results <- data.frame()

# Loop through case_status and variable
for (status in case_status_vars) {
  for (var in oci_vars) {
    # Extract the t-test result
    t_test_result <- OCI_compulsive_data[[status]][[var]]$t_test_result
    
    # Tidy the t-test result and add it to the tidy_results data frame
    tidy_result <- broom::tidy(t_test_result)
    tidy_result$status <- status
    tidy_result$variable <- var
    
    tidy_results <- rbind(tidy_results, tidy_result)
  }
}


# Iterate over case statuses
for (status in case_status_vars) {
  # Iterate over oci variables
  for (var in oci_vars) {
    # Extract sd from oci_compulsive_data
    sd1 <- OCI_compulsive_data[[status]][[var]]$level_stats$`0`$sd
    sd2 <- OCI_compulsive_data[[status]][[var]]$level_stats$`1`$sd
    
    # Find the rows matching the current case status and oci variable
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

tidy_results <- tidy_results |> 
  mutate(`Mean Diff` = as.numeric(`Mean Diff`)*-1) |> 
  mutate(`t` = as.numeric(`t`)*-1)

tidy_results$`FDR p val` <- sprintf("%.2e", p.adjust(tidy_results$p, method = 'fdr'))


tidy_results <- tidy_results %>%
  mutate(variable = case_when(
    variable == "oci12_wash" ~ "Washing",
    variable == "oci12_check" ~ "Checking",
    variable == "oci12_order" ~ "Order",
    variable == "oci12_total" ~ "Total",
    variable == "oci12_obsess" ~ "Obsess",
    TRUE ~ as.character(variable)
  ))


OCI_tidy_results <- tidy_results
resave(OCI_tidy_results, file = df_file)

case_status_vars <- c('AN', 'AN Mixed', 'BN', 'BN-BED Mixed', 'BED')
exercise_vars <- c('ED100k_ex_maladaptive', 'ED100k_ex_compulsive')
oci_vars <- c('oci12_wash', 'oci12_check', 'oci12_order', 'oci12_obsess', 'oci12_total')

# Define the labels for the levels
level_labels <- c("No Compulsive Exercise", "Compulsive Exercise")

# Define custom labels for the OCI variables
oci_labels <- c("Washing", "Checking", "Order", "Obsess", "Total")



graph_df <- EDGI_exercise_cleaned |>
  select(case_status, all_of(oci_vars), ED100k_ex_compulsive) |>
  filter(!is.na(ED100k_ex_compulsive)) |>
  rename(Washing = oci12_wash,
         Checking = oci12_check,
         Order = oci12_order,
         Obsess = oci12_obsess,
         Total = oci12_total)

max_vals <- list()
for (var in oci_labels) {
  max_vals[[var]] <- max(graph_df[[var]], na.rm = TRUE) 
}


graph_df$`case_status` <- factor(graph_df$`case_status`, levels = diagnosis_order)

ggplots <- list()

for (var in oci_labels) {
  p_vals <- OCI_tidy_results |>
    filter(variable == var) 
  
  p <- p_vals$`FDR p val` 
  
  annotation_df <- data.frame(
    x = c(0.8, 1.8, 2.8, 3.8, 4.8),
    xend = c(1.2, 2.2, 3.2, 4.2, 5.2),
    y = rep(max_vals[[var]] + 1),
    annotation = p 
  ) |> 
    filter(as.numeric(p) < 0.05)
  
  plot <- ggplot(graph_df, aes(x = case_status, y = !!sym(var), fill = factor(ED100k_ex_compulsive))) +
    geom_boxplot() +
    labs(fill = "Compulsive Exercise", title = var) +
    scale_fill_manual(
      values = c('0' = 'indianred', '1' = 'deepskyblue1'),
      labels = c("No", "Yes")
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = if (var == oci_labels[5]) element_text(angle = 45, hjust = 1) else element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      legend.position = ifelse(var == oci_labels[5], "bottom", "none")
    ) + 
    ylim(0, max_vals[[var]] + max_vals[[var]]*.25)
  
  
  if (nrow(annotation_df) > 0) {
    plot <- plot +
      geom_signif(
        annotation = annotation_df$annotation,
        xmin = annotation_df$x,
        xmax = annotation_df$xend,
        y_position = annotation_df$y
      )
  }
  
  ggplots[[var]] <- plot
}


OCI_plot <- ggplots[[oci_labels[1]]]

for (var in oci_labels[-1]) {
  if (var %in% names(ggplots)) {
    OCI_plot <- OCI_plot + ggplots[[var]]
  }
}

OCI_plot <- OCI_plot +
  plot_layout(ncol = 1) +
  plot_annotation(title = "OCI Subscale Scores Across Diagnosis Group \n Among those With and Without Compulsive Exercise History", theme = theme(plot.title = element_text(hjust = 0.5))) 

OCI_plot 

OCI_plot_file <- paste0("validation_paper/figs/OCI_compulsive_", cohort, ".png") 
ggsave(OCI_plot_file)  
