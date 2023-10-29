traits_aim2 <- c('ED100k_ex1_Q1_broad',
                 'ED100k_ex2_Q1_narrow', 
                 'ED100k_ex6_excessive', 
                 'ED100k_ex7_compensatory', 
                 'ED100k_ex8_maladaptive_current')

# Below makes the 5 case status diagnosis groups
case_status_vars <- c('AN', 'AN Mixed', 'BN', 'BN-BED Mixed', 'BED')
mps_vars <- c('mps_ps', 'mps_cm', 'mps_da')
diagnosis_order <- c("AN", "AN Mixed", "BN", "BN-BED Mixed", "BED")

# Define the labels for the levels
level_labels <- c("No", "Yes")

# Define custom labels for the MPS variables
mps_labels <- c(
  mps_ps = "Personal Standards",
  mps_cm = "Concern over Mistakes",
  mps_da = "Doubts about Actions")

# Create an empty list to store the nested data
nested_data <- list()

# Perform filtering, t-tests, and calculate additional statistics in nested loops
for (status in case_status_vars) {
  nested_data[[status]] <- list()
  
  for (trait in traits_aim2) {
    for (var in mps_vars) {
      filtered_data <- EDGI_exercise_cleaned %>%
        filter(case_status == status)
      
      t_test <- t.test(filtered_data[[var]] ~ filtered_data[[trait]])
      
      levels <- c(0, 1)
      level_stats <- list()
      
      for (level in levels) {
        level_data <- filtered_data %>%
          filter(.data[[trait]] == level)
        
        level_stats[[as.character(level)]] <- list(
          mean = mean(level_data[[var]], na.rm = TRUE),
          sd = sd(level_data[[var]], na.rm = TRUE),
          n = length(level_data[[var]]),
          se = sd(level_data[[var]], na.rm = TRUE)/sqrt(length(level_data[[var]]))
        )
      }
      
      nested_data[[status]][[paste(trait, var, sep = '.')]] <- list(
        t_test_result = t_test,
        p_value = t_test$p.value,
        level_stats = level_stats
      )
    }
  }
}


MPS_compulsive_data <- nested_data
resave(MPS_compulsive_data, file = df_file)

# Load the necessary library
library(broom)

# Create an empty list to store the tidy results
tidy_results <- list()

# Loop through case_status and variable
for (status in case_status_vars) {
  for (trait_var in names(nested_data[[status]])) {
    # Extract the t-test result
    t_test_result <- nested_data[[status]][[trait_var]]$t_test_result
    
    # Tidy the t-test result
    tidy_result <- broom::tidy(t_test_result)
    
    # Add case_status and variable information to the tidy result
    tidy_result$status <- status
    tidy_result$variable <- trait_var
    
    # Append the tidy result to the list of tidy results
    tidy_results[[length(tidy_results) + 1]] <- tidy_result
  }
}

# Combine the list of tidy results into a single data frame
tidy_results_df <- do.call(rbind, tidy_results)

# Reorder columns
tidy_results_df <- tidy_results_df[, c("status", "variable", "estimate", "estimate1", "estimate2", "statistic", "p.value", "method")]

def_combos <- unique(tidy_results_df$variable)

# Iterate over case statuses
for (status in case_status_vars) {
  # Iterate over mps variables
  for (var in def_combos) {
    # Extract sd from MPS_compulsive_data
    sd1 <- MPS_compulsive_data[[status]][[var]]$level_stats$`0`$sd
    sd2 <- MPS_compulsive_data[[status]][[var]]$level_stats$`1`$sd
    
    # Extract estimates from MPS_compulsive_data
    sd1 <- MPS_compulsive_data[[status]][[var]]$level_stats$`0`$sd
    sd2 <- MPS_compulsive_data[[status]][[var]]$level_stats$`1`$sd
    # Find the rows matching the current case status and mps variable
    rows <- tidy_results_df$status == status & tidy_results_df$variable == var
    
    # Update sd1 and sd2 in the matching rows
    tidy_results_df[rows, c("sd1", "sd2")] <- list(sd1, sd2)
  }
}

tidy_results <- tidy_results_df %>%
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

tidy_results$ex_var <- sapply(strsplit(tidy_results$variable, ".", fixed = TRUE), `[`, 1)

tidy_results$mps_var <- sapply(strsplit(tidy_results$variable, ".", fixed = TRUE), `[`, 2)

tidy_results <- tidy_results %>%
  mutate(mps_var = case_when(
    mps_var == "mps_ps" ~ "Personal Standards",
    mps_var == "mps_cm" ~ "Concern Over Mistakes",
    mps_var == "mps_da" ~ "Doubts About Actions",
    TRUE ~ as.character(mps_var)
  ), 
  ex_var = case_when(
  ex_var == 'ED100k_ex1_Q1_broad' ~ '1. Q1 Broad',
  ex_var == 'ED100k_ex2_Q1_narrow' ~ '2. Q1 Narrow', 
  ex_var == 'ED100k_ex6_excessive' ~ '6. Excessive', 
  ex_var == 'ED100k_ex7_compensatory' ~ '7. Compensate', 
  ex_var == 'ED100k_ex8_maladaptive_current' ~ '8. Current', 
  TRUE ~ as.character(ex_var)))

tidy_results <- tidy_results |> 
  mutate(`Mean Diff` = as.numeric(`Mean Diff`)*-1) |> 
  mutate(`t` = as.numeric(`t`)*-1) |> 
  select(-c(variable))

tidy_results$`FDR p val` <- sprintf("%.2e", p.adjust(tidy_results$p, method = 'fdr'))

MPS_tidy_results <- tidy_results

resave(MPS_tidy_results, file = df_file)

case_status_vars <- c('AN', 'AN Mixed', 'BN', 'BN-BED Mixed', 'BED')
mps_vars <- c('mps_ps', 'mps_cm', 'mps_da')
level_labels <- c("No", "Yes")
mps_labels <- c("Personal Standards", "Concern Over Mistakes", "Doubts About Actions")
trait_labels_aim2 <- c('1. Q1 Broad',
                 '2. Q1 Narrow', 
                 '6. Excessive', 
                 '7. Compensate', 
                 '8. Current')

graph_df_list <- list()

graph_df_base <- EDGI_exercise_cleaned |> 
  rename(`Personal Standards` = mps_ps, 
       `Concern Over Mistakes` = mps_cm, 
       `Doubts About Actions` = mps_da,
       `1. Q1 Broad` = ED100k_ex1_Q1_broad,
       `2. Q1 Narrow` = ED100k_ex2_Q1_narrow,
       `6. Excessive` = ED100k_ex6_excessive,
       `7. Compensate` = ED100k_ex7_compensatory,
       `8. Current` = ED100k_ex8_maladaptive_current)

for (trait in trait_labels_aim2) {
  graph_df_list[[trait]] <- graph_df_base |>
    select(case_status, all_of(mps_labels), trait) |>
    filter(!is.na(.data[[trait]])) |>
    mutate(case_status = factor(case_status, levels = diagnosis_order)) 
}


ggplots <- list()

for (trait in trait_labels_aim2) {
  for (var in mps_labels) {
    p_vals <- MPS_tidy_results |>
      filter(mps_var == var) |>
      filter(ex_var == trait)
    p <- p_vals$`FDR p val`
    
    annotation_df <- data.frame(x = c(0.8, 1.8, 2.8, 3.8, 4.8),
                                xend = c(1.2, 2.2, 3.2, 4.2, 5.2),
                                y = c(21, 21, 21, 21, 21),
                                annotation = p) |> 
      filter(as.numeric(p) < 0.05) 
    
    graph_df_list[[trait]][[trait]] <- factor(graph_df_list[[trait]][[trait]])
    
    plot <- 
      ggplot(graph_df_list[[trait]], aes(x = case_status, y = !!sym(var), fill = .data[[trait]])) +
      geom_boxplot(outlier.shape = NA) +
      labs(fill = "Scoring Criteria Met", title = var) +
      scale_fill_manual(values = c('0' = 'indianred', '1' = 'deepskyblue1'),
                        labels = c("No", "Yes")) +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = if (var == mps_labels[3]) element_text(angle = 45, hjust = 1, size = 12) else element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            legend.position = ifelse(var == mps_labels[3], "bottom", "none")) +
      ylim(4, 23)
    
    if (nrow(annotation_df) > 0) {
      plot <- plot +
        geom_signif(
          annotation = annotation_df$annotation,
          xmin = annotation_df$x,
          xmax = annotation_df$xend,
          y_position = annotation_df$y
        )
    }
    
    ggplots[[trait]][[var]] <- plot
  }
}

MPS_plots <- list()

for (var in names(ggplots)) {
  cohort <- cohort
  MPS_plots[[var]] <- Reduce(`+`, ggplots[[var]]) +
    plot_layout(ncol = 1) +
    plot_annotation(
      title = paste("MPS Subscale Scores Across Scoring Approach", var, "and Diagnosis Group"),
      theme = theme(plot.title = element_text(hjust = 0.5))
    )
  
  MPS_plot_file <- paste0("validation_paper/figs/MPS_", var,"_", cohort, ".png") 
  ggsave(filename = MPS_plot_file, plot = MPS_plots[[var]])
}



graph_df_list_1 <- list()

for (trait in trait_labels_aim2) {
  graph_df_list_1[[trait]] <- graph_df_base |>
    select(case_status, all_of(mps_labels), trait) |>
    filter(!is.na(.data[[trait]])) |>
    mutate(case_status = factor(case_status, levels = diagnosis_order), 
           operationalization = trait) |> 
    rename(scoring_met = trait)
  
}

# Combine all data into a single dataframe

combined_data <- do.call(rbind, graph_df_list_1)
combined_data$operationalization <- as.factor(combined_data$operationalization)

# Create the plot


ggplots_1 <- list()

for (var in mps_labels) {
  p_vals <- MPS_tidy_results |>
    filter(mps_var == var)
  p <- p_vals$`FDR p val`
  
  # First, define 'x' and 'annotation'
  annotation_df <- data.frame(
    x = c(0.64, 0.80, 0.96, 1.12, 1.28,
          1.64, 1.80, 1.96, 2.12, 2.28,
          2.64, 2.80, 2.96, 3.12, 3.28,
          3.64, 3.80, 3.96, 4.12, 4.28,
          4.64, 4.80, 4.96, 5.12, 5.28),
    annotation = p
  )
  
  # Then, add 'xend' and 'y' to 'annotation_df'
  annotation_df$xend <- annotation_df$x + 0.08
  annotation_df$y <- rep(21, times = 25)
  
  # Finally, filter 'annotation_df' based on 'p'
  annotation_df <- annotation_df |> filter(as.numeric(annotation) < 0.05)
  
  plot <- ggplot(combined_data, aes(x = case_status, y = !!sym(var), fill = as.factor(operationalization), alpha = as.factor(scoring_met))) +
    geom_boxplot(position = position_dodge(width = 0.8), 
                 outlier.shape = NA) +
    scale_alpha_manual(values = c("0" = 0.2, "1" = 1)) +
    labs(x = "Case Status", fill = "Operationalization", title = var) +
    guides(alpha = "none") +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = if (var == mps_labels[3]) element_text(size = 14) else element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = ifelse(var == mps_labels[3], "bottom", "none"),
          axis.text.y = element_text(size = 14),
          text = element_text(size = 14)) +
    ylim(4, 23)
  
  if (nrow(annotation_df) > 0) {
    plot <- plot +
      geom_signif(
        annotation = "*",
        xmin = annotation_df$x,
        xmax = annotation_df$xend,
        y_position = annotation_df$y,
        colour = 'red',
        alpha = 1)
  }
  ggplots_1[[var]] <- plot
  
}


# use Reduce to add all the ggplots together
MPS_plots_1 <- Reduce(`+`, ggplots_1)

# add the layout, annotation, and labels
MPS_plots_1 <- MPS_plots_1 +
  plot_layout(ncol = 1) +
  plot_annotation(
    title = paste("MPS Subscale Scores Across Exercise \n Groups by Scoring Approach and Diagnosis Group"),
    theme = theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 18))
  ) 

MPS_plots_1

MPS_plot_file <- paste0("validation_paper/figs/MPS_combined", cohort, ".png")
ggsave(MPS_plot_file, width = 10, height = 7) 

