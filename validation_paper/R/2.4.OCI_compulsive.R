
# Below makes the 5 case status diagnosis groups

#Make case hierarchy and recode restrictive and binge spectrum mixed cases
EDGI_exercise_cleaned <- EDGI_exercise_cleaned |> 
  mutate (case_status = case_when(an_case == 1 & bn_case == 0 & bed_case == 0 ~ 'AN',
                                  an_case == 0 & bn_case == 1 & bed_case == 0 ~ 'BN',
                                  an_case == 0 & bn_case == 0 & bed_case == 1 ~ 'BED', 
                                  an_case == 1 & (bn_case == 1 | bed_case == 1) ~ 'AN Mixed',
                                  an_case == 0 & bn_case ==1 & bed_case ==1 ~ 'BN-BED Mixed' )) |> 
  mutate (case_heirarchy = case_when (an_case == 1 ~ 'AN', 
                                      bn_case == 1 ~ 'BN', 
                                      bed_case == 1 ~ 'BED'))

case_status_vars <- c('AN', 'AN Mixed', 'BN', 'BN-BED Mixed', 'BED')
exercise_vars <- c('ED100k_ex_maladaptive', 'ED100k_ex_compulsive')
oci_vars <- c('oci12_wash', 'oci12_check', 'oci12_order', 'oci12_obsess', 'oci12_total')
mps_vars <- c('mps_ps', 'mps_cm', 'mps_da')

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

oci_compulsive_data <- nested_data


library(ggplot2)
library(dplyr)
library(patchwork)

# Create an empty list to store the ggplot objects
ggplots <- list()

# Iterate over OCI variables
for (var in oci_vars) {
  # Create an empty data frame to store the combined plot data for the current OCI variable
  combined_data <- NULL
  
  # Iterate over case statuses
  for (status in case_status_vars) {
    # Create a data frame for the specific case status
    data <- nested_data[[status]]
    
    # Create a data frame for the specific OCI variable
    var_data <- data[[var]]
    
    # Extract the level statistics
    level_stats <- var_data$level_stats
    
    # Create a data frame for plotting
    plot_data <- data.frame(
      Status = factor(status, levels = case_status_vars),
      Mean = sapply(level_stats, function(x) x$mean),
      SE = sapply(level_stats, function(x) x$se),
      Variable = factor(names(level_stats), levels = c('0', '1'))  # Convert Variable to a factor with desired order
    )
    
    # Combine the plot data for the current case status with the existing combined data
    if (is.null(combined_data)) {
      combined_data <- plot_data
    } else {
      combined_data <- bind_rows(combined_data, plot_data)
    }
  }
  
  # Create the ggplot object using the combined plot data
  p <- ggplot(combined_data, aes(x = Status, y = Mean, fill = Variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2, position = position_dodge(0.9)) +
    labs(title = oci_labels[var], x = NULL, y = "Mean") +
    scale_fill_discrete(labels = level_labels, name = NULL) +
    scale_x_discrete(labels = case_status_vars) +  # Set custom labels for x-axis
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.y = element_blank(),
          legend.position = ifelse(var == oci_vars[4], "bottom", "none"))
  
  # Add the ggplot object to the list
  ggplots[[var]] <- p
}

# Combine the ggplot objects and facet wrap by variable
oci_plot <- ggplots[[1]] +
  ggplots[[2]] +
  ggplots[[3]] +
  ggplots[[4]] +
  ggplots[[5]] +
  plot_layout(ncol = 2) +
  plot_annotation(title = "OCI12 Mean Subscale and \n Total Scores Across Exercise and Diagnosis Group", theme = theme(plot.title = element_text(hjust = 0.5))) 

oci_plot

oci_plot_file <- paste0("validation_paper/figs/OCI_compulsive_", cohort, ".png") 
ggsave(oci_plot_file)  

oci_compulsive_file <- paste0("validation_paper/tabs/OCI_compulsive_df_", cohort, ".RData") 
save(oci_compulsive_data, file = oci_compulsive_file)


tidy_results <- data.frame()

# Loop through case_status and variable
for (status in case_status_vars) {
  for (var in oci_vars) {
    # Extract the t-test result
    t_test_result <- oci_compulsive_data[[status]][[var]]$t_test_result
    
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
    sd1 <- oci_compulsive_data[[status]][[var]]$level_stats$`0`$sd
    sd2 <- oci_compulsive_data[[status]][[var]]$level_stats$`1`$sd
    
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


tidy_results <- tidy_results %>%
  mutate(variable = case_when(
    variable == "oci12_wash" ~ "Washing",
    variable == "oci12_check" ~ "Checking",
    variable == "oci12_order" ~ "Order",
    variable == "oci12_total" ~ "Total",
    variable == "oci12_obsess" ~ "Obsess",
    TRUE ~ as.character(variable)
  ))


oci_tidy_results <- tidy_results
resave(oci_tidy_results, file = oci_compulsive_file)

