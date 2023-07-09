# Filter the compulsive percentages
compulsive_percents <- dx_row_percents %>%
  filter(name == 'Compulsive')

# # Fit the logistic regression model
 Dx_table <- Dx_table |> 
   rename(`Diagnosis Group` = case_status)
my_comparisons <- list(c("AN", "AN Mixed"), c("AN", "BN"), c("AN", "BN-BED Mixed"), c("AN", "BED"))


model <- glm(ED100k_ex_compulsive ~ `Diagnosis Group`, data = Dx_table, family = binomial)

# Extract the coefficient p-values from the model summary
p_values <- summary(model)$coefficients[,"Pr(>|z|)"]

# Create a dataframe with the p-values and labels for annotation
p_values_df <- data.frame(Diagnosis_Group = levels(Dx_table$`Diagnosis Group`),
                          p_value = p_values,
                          label = paste0("p = ", format(p_values, digits = 2, nsmall = 2))) |> 
  filter (Diagnosis_Group != 'AN')

# Update your ggplot code to include p-values and significance levels using geom_text
# Update your ggplot code to include p-values and significance levels using geom_text
ggplot(compulsive_percents, aes(x = `Diagnosis Group`, y = value * 100, fill = `Diagnosis Group`)) +
  geom_col() +
  facet_wrap(~ name) +
  labs(title = 'Percentages by Diagnosis Group \n and Exercise Construct',
       x = 'Diagnosis Group', y = 'Percentage (within Diagnosis Group)') +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  theme(legend.position = 'none') +
  geom_text(data = compulsive_percents, aes(x = `Diagnosis Group`, y = value * 100 - 5,
                                            label = paste0(round(value * 100, 0), '%')),
            size = rel(3)) +
  geom_text(data = p_values_df, aes(x = Diagnosis_Group, y = c(110, 140, 120, 130), label = label),
            size = rel(3), hjust = 1, inherit.aes = FALSE)  
