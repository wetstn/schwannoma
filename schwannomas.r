install.packages("ggpubr")
install.packages("rstatix")
library(googlesheets4)
library(dplyr)
library(finalfit)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(rstatix)

schwannoma_raw_unprocessed <- 
    read_sheet("https://docs.google.com/spreadsheets/d/1mpfQU7jvrCNgVOe1_6mN44bxdjz7BlikbI1zhnAQQtE/edit?usp=sharing", 
        sheet = "schwannoma_report_raw_20231029")

grouped_schwannoma_raw <- 
    schwannoma_raw_unprocessed %>%    
        group_by(record_id) %>% 
            summarize_all(~ paste(na.omit(.), 
                collapse = ""))

clean_schwannoma <- grouped_schwannoma_raw %>%
  mutate(
    true_scwhannoma = NA,
    pathology_report = NA
  ) %>%
  relocate(true_scwhannoma, .after = 1) %>%
  relocate(pathology_report, .after = 2)

write_sheet(clean_schwannoma, ss = "https://docs.google.com/spreadsheets/d/1mpfQU7jvrCNgVOe1_6mN44bxdjz7BlikbI1zhnAQQtE/edit?usp=sharing", sheet = "clean_schwannoma")

# Preop and postop details
medical_hx <- 
    read_sheet("https://docs.google.com/spreadsheets/d/1mpfQU7jvrCNgVOe1_6mN44bxdjz7BlikbI1zhnAQQtE/edit?usp=sharing", 
        sheet = "medical_hx")

preop_details <- 
    read_sheet("https://docs.google.com/spreadsheets/d/1mpfQU7jvrCNgVOe1_6mN44bxdjz7BlikbI1zhnAQQtE/edit?usp=sharing", 
        sheet = "preop_details")

path_details <- 
    read_sheet("https://docs.google.com/spreadsheets/d/1TjkA7KpNMX-mZ-h9dndKRBac2q2SbIy8Xn6a3IVDNkU/edit?usp=sharing", 
        sheet = "path_details")

path_details <- filter(path_details, redcap_repeat_instrument == "index_surgery_tumor_surgery_pathology_details")

postop_details <- 
    read_sheet("https://docs.google.com/spreadsheets/d/1mpfQU7jvrCNgVOe1_6mN44bxdjz7BlikbI1zhnAQQtE/edit?usp=sharing", 
        sheet = "postop_details")

schwannoma_patient_list <- 
    read_sheet("https://docs.google.com/spreadsheets/d/1mpfQU7jvrCNgVOe1_6mN44bxdjz7BlikbI1zhnAQQtE/edit?usp=sharing", 
        sheet = "schwannoma_patient_list")

schwannoma_patient_list <- 
    read_sheet("https://docs.google.com/spreadsheets/d/1mpfQU7jvrCNgVOe1_6mN44bxdjz7BlikbI1zhnAQQtE/edit?usp=sharing", 
        sheet = "schwannoma_patient_list")

# Assuming the common variable is "record_id"
combined_details <- merge(medical_hx, preop_details, by = "record_id")
combined_details <- merge(combined_details, path_details, by = "record_id")
combined_details <- merge(combined_details, postop_details, by = "record_id")


# Filtering based on schwannoma_patient_list
filtered_details <- combined_details[combined_details$record_id %in% schwannoma_patient_list$record_id, ]

# Selecting columns for the subset
selected_columns <- c("record_id", "tumor_surgery_age",
    "radiation_yn", "radiation_yn_post",
    "chemo_yn", "chemo_yn_post", "tumor_recurrence_yn",
    "tumor_fu_cx_spine_nonop___8",
    "tumor_fu_cx_spine_nonop___9",
    "tumor_fu_reop_ind___8", "tumor_fu_reop_ind___9",
    "tumor_preop_sx___1", "tumor_preop_sx___2", "tumor_preop_sx___3",
    "tumor_preop_sx___4", "tumor_preop_sx___5", "tumor_preop_sx___6",
    "tumor_preop_sx___7", "tumor_preop_sx___8", "tumor_preop_sx___9",
    "tumor_preop_sx___10", "tumor_preop_ecog",
    "tumor_ecog_fu", "tumor_ecog_fu_date",
    "tumor_ecog_fu_length", "tumor_fu_cx___0",
    "tumor_fu_cx___1", "tumor_fu_cx___2", "tumor_fu_cx___3",
    "tumor_fu_cx___4", "tumor_fu_cx___5", "tumor_fu_cx___6",
    "tumor_fu_cx___7", "tumor_fu_cx___8", "tumor_fu_cx___9",
    "tumor_fu_cx___10", "tumor_fu_cx___11",
    "tumor_fu_cx_other1", "tumor_fu_date", "tumor_fu_duration")

# Missing column check!
# missing_columns <- setdiff(selected_columns, colnames(filtered_details))

# if (length(missing_columns) > 0) {
#   cat("The following columns are missing in filtered_details:\n")
#   print(missing_columns)
# } else {
#   cat("All columns exist. Proceeding with subset.\n")
#   preop_postop <- subset(filtered_details, select = selected_columns)
# }

preop_postop <- subset(filtered_details, select = selected_columns)

write_sheet(preop_postop, ss = "https://docs.google.com/spreadsheets/d/1mpfQU7jvrCNgVOe1_6mN44bxdjz7BlikbI1zhnAQQtE/edit?usp=sharing", 
sheet = "preop_postop")


# Summary and analysis
schwan_df <- read_sheet("https://docs.google.com/spreadsheets/d/1mpfQU7jvrCNgVOe1_6mN44bxdjz7BlikbI1zhnAQQtE/edit?usp=sharing", sheet = "schwan_df")

# Make the following variables logical
variables_list <- c("region_1", "region_2", "region_3", "region_4", "region_5",
  "no_level", "occiput_level", "c1_level", "c2_level", "c3_level",
  "c4_level", "c5_level", "c6_level", "c7_level", "t1_level", "t2_level",
  "t3_level", "t4_level", "t5_level", "t6_level", "t7_level", "t8_level",
  "t9_level", "t10_level", "t11_level", "t12_level", "l1_level", "l2_level",
  "l3_level", "l4_level", "l5_level", "s1_level", "s2_level", "s3_level",
  "s4_level", "s5_level", "coccyx_level", "radiation_yn", "radiation_yn_post",
  "chemo_yn", "chemo_yn_post", "tumor_recurrence_yn",
  "neck_mech_pain", "tumor_ed_admit_30d",
  "thoracic_mech_pain", "lumbar_mech_pain", "cervical_radicular",
  "lumbar_radicular", "constipation", "bowel_incontinence",
  "urinary_retention", "urinary_incontinence", "parasthesia", 
  "tumor_fu_cx___0",
  "tumor_fu_cx___1",
  "tumor_fu_cx___2",
  "tumor_fu_cx___3",
  "tumor_fu_cx___4",
  "tumor_fu_cx___5",
  "tumor_fu_cx___6",
  "tumor_fu_cx___7",
  "tumor_fu_cx___8",
  "tumor_fu_cx___9",
  "tumor_fu_cx___10",
  "tumor_fu_cx___11"
  )

schwan_df <- schwan_df %>% mutate_at(vars(variables_list), as.logical)

# Specify the variables we want to change to numeric class
numerics_to_change <- c("level", "largest_dimension", "adi_national", "age", "tumor_surgery_age", "tumor_fu_duration", "tumor_ecog_fu_length", 
"record_id", "tumor_readmit_number", "body_mass_index_bmi", "race", "ethnicity")

schwan_df <- schwan_df %>% mutate_at(vars(numerics_to_change), as.numeric)

# Specify the variables we want to change to character class
characters_to_change <- c("final_diagnosis", "region", "pathology_synoptic", "pathology_micro", "pathology_immunohisto")

schwan_df <- schwan_df %>% mutate_at(vars(characters_to_change), as.character)

# Specify the factor variables (having problems, not sure if necessary anyway)
factors_to_change <- c("region", "ki67", "sox10", "s100", "ema", "gender", "race", "ethnicity", "smoking_history", "tumor_preop_ecog", "tumor_ecog_fu")
#factors_to_change <- c("region", "gender", "ki67", "race", "ethnicity", "smoking_history") 

schwan_df <- schwan_df %>% mutate_at(vars(factors_to_change), as.factor)

# Summary factor list
dependent = "region"

explanatory = c("tumor_surgery_age", "gender", "race", "ethnicity","body_mass_index_bmi",
"smoking_history", "radiation_yn", "chemo_yn", "level",
"adi_national", "neck_mech_pain", "thoracic_mech_pain",
"lumbar_mech_pain",
"cervical_radicular", "lumbar_radicular", "constipation",
"bowel_incontinence", "urinary_retention",
"urinary_incontinence",
"parasthesia", "largest_dimension", "rad_for_dimension",
"s100", "ki67", "sox10", "ema", "tumor_ed_admit_30d",
"tumor_readmit_number", "c5_level", "c6_level",
"c7_level", "t1_level", "t2_level", "t3_level",
"t4_level", "t5_level", "t6_level", "t7_level",
"t8_level", "t9_level", "t10_level", "t11_level",
"t12_level", "l1_level", "l2_level", "l3_level",
"l4_level", "l5_level", "s1_level", "s2_level", "s3_level",
"chemo_yn_post", "tumor_recurrence_yn", 
"tumor_preop_ecog",
"tumor_ecog_fu",
"tumor_fu_duration", "tumor_ecog_fu_length", 
"tumor_fu_cx___0",
"tumor_fu_cx___1",
"tumor_fu_cx___2",
"tumor_fu_cx___3",
"tumor_fu_cx___4",
"tumor_fu_cx___5",
"tumor_fu_cx___6",
"tumor_fu_cx___7",
"tumor_fu_cx___8",
"tumor_fu_cx___9",
"tumor_fu_cx___10",
"tumor_fu_cx___11"
)

schwan_summary <- schwan_df %>%
  summary_factorlist(dependent, explanatory,
    na_include = FALSE,
    na_include_dependent = TRUE,
    total_col = TRUE,
    add_col_totals = TRUE,
    col_totals_prefix = "n = ",
    p = FALSE,
    cont = "mean",
    cont_cut = 2,
    orderbytotal = TRUE,
    include_row_totals_percent = TRUE,
    digits = c(1, 1, 3, 1, 0)
  )

write_sheet(schwan_summary, ss = "https://docs.google.com/spreadsheets/d/1mpfQU7jvrCNgVOe1_6mN44bxdjz7BlikbI1zhnAQQtE/edit?usp=sharing", sheet = "schwan_summary")


# Basic stats stuff

test = c("largest_dimension", "adi_national", "age",
"record_id", "tumor_readmit_number", "body_mass_index_bmi", "race", "ethnicity")

test_df <- schwan_df %>%
  summary_factorlist(dependent, test,
    na_include = FALSE,
    na_include_dependent = TRUE,
    total_col = TRUE,
    add_col_totals = TRUE,
    col_totals_prefix = "n = ",
    p = TRUE,
    cont = "mean",
    cont_cut = 2,
    orderbytotal = TRUE,
    include_row_totals_percent = TRUE,
    digits = c(1, 1, 3, 1, 0)
  )


# Contingency table
regions <- c("region_1", "region_2", "region_3", "region_4", "region_5")

variables_of_interest <- c("gender", "race", "ethnicity", "smoking_history", "radiation_yn", "chemo_yn",
                "neck_mech_pain", "thoracic_mech_pain",
                "lumbar_mech_pain",
                "cervical_radicular", "lumbar_radicular", "constipation",
                "bowel_incontinence", "urinary_retention",
                "urinary_incontinence",
                "parasthesia", 
                "s100", "ki67", "sox10", "ema", "tumor_ed_admit_30d",
                "tumor_readmit_number", "c5_level", "c6_level",
                "c7_level", "t1_level", "t2_level", "t3_level",
                "t4_level", "t5_level", "t6_level", "t7_level",
                "t8_level", "t9_level", "t10_level", "t11_level",
                "t12_level", "l1_level", "l2_level", "l3_level",
                "l4_level", "l5_level", "s1_level", "s2_level", "s3_level",
                "chemo_yn_post")

contingency_tables <- list()

# Create contingency tables for each variable and region
for (variable in variables_of_interest) {
  for (region in regions) {
    table_name <- paste(variable, region, sep = "_")
    contingency_tables[[table_name]] <- table(schwan_df[[variable]], schwan_df[[region]])
  }
}

contingency_table_gender_region_1 <- contingency_tables[["gender_region_1"]]

#--------------------------------------------
# Automated chi square
#--------------------------------------------

# Create a list to store p-values
p_values <- list()

# Loop through each variable and perform a chi-square test
for (variable in variables_of_interest) {
  contingency_table <- table(schwan_df[[variable]], schwan_df[[region]])
  
  # Check if any cell in the contingency table is zero
  if (any(contingency_table == 0)) {
    cat("Skipping chi-square test for", variable, "due to empty sets.\n")
    p_values[[variable]] <- NA  # You can choose how to handle this case
  } else {
    chi_squared_test <- chisq.test(contingency_table)
    # Store the p-value in the list
    p_values[[variable]] <- chi_squared_test$p.value
  }
}

# Combine the results into a data frame
result_df <- data.frame(Variable = names(p_values), P_Value = unlist(p_values))

# ANOVA for testing tumor size as a dependent on region
# Fit one-way ANOVA model
anova_model <- aov(largest_dimension ~ region, data = schwan_df)

summary(anova_model)

schwan_df %>% aov(largest_dimension ~ region, data = .) %>% 
    TukeyHSD() %>%
    plot()

# Plot the regions and tumor sizes
ggplot(schwan_df, aes(x = region, y = largest_dimension)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Boxplot of largest_dimension by Region", x = "Region", y = "largest_dimension")

# Density plot with means
ggplot(schwan_df, aes(x = largest_dimension, fill = region)) +
  geom_density(alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Density Plot of largest_dimension by Region", x = "largest_dimension", y = "Density", fill = "Region")

# Plotting the levels with box and whisker and with density plots
# Recode levels of the "region" variable
schwan_df$region <- factor(schwan_df$region,
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("High Cervical", "Low Cervical", "Thoracic", "Lumbar", "Sacral/Coccygeal"))

# Now, create and save the plots with different colors for each region

# Boxplot with means
boxplot_plot <- ggplot(schwan_df, aes(x = largest_dimension, y = region, fill = region)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Boxplot of largest_dimension by Region", x = "Region", y = "largest_dimension") +
  outlier.shape = NA +
  scale_fill_manual(values = c("High Cervical" = "#6d89e4", "Low Cervical" = "#00d0ff", 
                               "Thoracic" = "#65dcac", "Lumbar" = "#dad259", 
                               "Sacral/Coccygeal" = "#eb904a")) +
  theme_minimal() +  # White background
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        panel.grid.major = element_blank(),  # Remove grid lines
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),  # Remove border
        axis.line = element_line(color = "black"),  # Black axis line
        panel.background = element_rect(fill = "white")) +  # White background

# Set the x-axis limit to 12
coord_cartesian(ylim = c(0, 12))

# Print boxplot
print(boxplot_plot)

# Save the boxplot as an image file (e.g., PNG)
ggsave("boxplot_largest_dimension.png", plot = boxplot_plot)

# Density plot with means
density_plot <- ggplot(schwan_df, aes(x = largest_dimension, fill = region)) +
  geom_density(alpha = 0.5) +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red") +
  labs(title = "Density Plot of largest_dimension by Region", x = "largest_dimension", y = "Density", fill = "Region") +
  scale_fill_manual(values = c("high cervical" = "red", "low cervical" = "blue", "thoracic" = "green", "lumbar" = "orange", "sacral/coccygeal" = "purple"))

# Save the density plot as an image file (e.g., PNG)
ggsave("density_plot_largest_dimension.png", plot = density_plot)

box_sch <- ggboxplot(schwan_df, x = "region", y = "largest_dimension")

print(box_sch)

# ------ density plot rn -----

sch_theme <- theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 20),  # Adjust x-axis text size
    axis.text.y = element_text(size = 20),  # Adjust y-axis text size
    axis.title.x = element_text(size = 20),  # Adjust x-axis title size
    axis.title.y = element_text(size = 20),  # Adjust y-axis title size
    plot.title = element_text(size = 20),  # Adjust plot title size
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black"),
    panel.background = element_rect(fill = "white"),
    aspect.ratio = 1
  )

den_sch <- ggdensity(schwan_df, x = "largest_dimension", fill = "region", palette = "ucscgb") +
  labs(title = "Density Plot of Tumor Size by Region", x = "Tumor Size (cm)", y = "Density", fill = "Region") +
  sch_theme +
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 0.5))

den_sch + theme(legend.spacing.x = unit(0.5, "cm")) +  
 theme(legend.position = "top",  # Position the legend at the top
        legend.title = element_text(size = 0),  # Adjust legend title size
        legend.text = element_text(size = 16),  # Adjust legend text size
        legend.key.size = unit(1, "cm"))

plot(den_sch)


# ------- best box plot rn -----

box_sch <- ggboxplot(schwan_df, x = "region", y = "largest_dimension", outlier.shape = 1, outlier.size = 4, fill = "region", palette = "ucscgb") +
  stat_summary(fun = mean, geom = "point", shape = 16, size = 5, color = "#b80404") +
  labs(title = "Boxplots of Tumor Size by Region", x = "Region", y = "Tumor Size (cm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 20),  # Adjust x-axis text size
        axis.text.y = element_text(size = 20),  # Adjust y-axis text size
        axis.title.x = element_text(size = 20),  # Adjust x-axis title size
        axis.title.y = element_text(size = 20),  # Adjust y-axis title size
        plot.title = element_text(size = 20),  # Adjust plot title size
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black"),
        panel.background = element_rect(fill = "white"),
        aspect.ratio = 1) +
        geom_bracket(
        xmin = c("High Cervical", "Lumbar", "Sacral/Coccygeal"), xmax = c("Sacral/Coccygeal", "Sacral/Coccygeal", "Thoracic"),
        y.position = c(12.25, 10.75, 11.5), label = c("5.9e-2", "4.9e-3", "1.2e-2"),
        tip.length = 0.01, label.size = 5) + 
        coord_cartesian(ylim = c(0, 12.5))

box_sch <- box_sch +
  theme(legend.position = "top",  # Position the legend at the top
        legend.title = element_text(size = 0),  # Adjust legend title size
        legend.text = element_text(size = 14),  # Adjust legend text size
        legend.key.size = unit(2, "cm"))  # Adjust legend key size (increase or decrease as needed)

plot(box_sch)

my_comparisons <- list( c("Thoracic", "Lumbar"), c("Lumbar", "Sacral/Coccygeal"), c("Sacral/Coccygeal", "Thoracic") )
p + stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)                   # Add global p-value

pairwise_pvalues <- data.frame(
  comparison = c("High Cervical vs Sacral/Coccygeal", "Thoracic vs Sacral/Coccygeal", "Lumbar vs Sacral/Coccygeal"),
  pvalue = c(0.0593725, 0.0119205, 0.048966)
)

my_comparisons <- list( c("High Cervical", "Sacral/Coccygeal"), c("Thoracic", "Sacral/Coccygeal"), c("Lumbar", "Sacral/Coccygeal") )

box_sch + stat_compare_means(comparisons = my_comparisons)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)

print(box_sch)

# ---- k-means clustering --------
# Perform k-means clustering
k <- 5  # Number of clusters
kmeans_result <- kmeans(schwan_df[, c("level", "largest_dimension")], centers = k)

# Add cluster information to the data frame
schwan_df$cluster <- as.factor(kmeans_result$cluster)

# Plot the data with clusters
k_cluster <- plot(schwan_df$largest_dimension, col = schwan_df$cluster, main = "Tumor Clustering around Spinal Levels", xlab = "Tumor Size", ylab = "Spinal Level")
points(kmeans_result$centers[, 2], col = 1:k, pch = 8, cex = 2)
legend("topright", legend = 1:k, col = 1:k, pch = 8, title = "Cluster")

k_cluster + coord_cartesian(xlim = c(0, 12), ylim = c(0, 0.5))

print(k_cluster)

# Print cluster centers
cat("Cluster Centers:\n")
print(kmeans_result$centers)

# Perform k-means clustering
k <- 5  # Number of clusters
kmeans_result <- kmeans(schwan_df[, c("true_schwannoma", "level")], centers = k)

# Add cluster information to the data frame
schwan_df$cluster <- as.factor(kmeans_result$cluster)

# Plot the data with clusters
k_cluster <- plot(schwan_df$level, col = schwan_df$cluster, main = "Tumor Clustering around Spinal Levels", xlab = "Tumor Size", ylab = "Spinal Level")
points(kmeans_result$centers[, 2], col = 1:k, pch = 8, cex = 2)
legend("topright", legend = 1:k, col = 1:k, pch = 8, title = "Cluster")


# Logistic regression

model <- glm(tumor_ecog_fu == 0 ~ region, data = schwan_df, family = "binomial")

# Print model summary
summary(model)

summary_with_ci <- summary(model)
ci <- confint(model)

# Combine coefficients and confidence intervals
result <- cbind(summary_with_ci$coefficients, ci)

# Print the result
print(result)

# Test the performance of the model with pseudo R-squared

# Obtain the predicted probabilities
predicted_probabilities <- predict(model, type = "response")

# Create a data frame with actual and predicted values
results <- data.frame(Actual = schwan_df$tumor_ecog_fu == 0, Predicted = predicted_probabilities)

# Create a confusion matrix
conf_matrix <- table(results$Actual, results$Predicted > 0.5)

# Calculate Nagelkerke's R-squared
nagelkerke_r2 <- 1 - (-(2 * logLik(model)) / (2 * logLik(null_model <- glm(tumor_ecog_fu == 0 ~ 1, data = schwan_df, family = "binomial"))))^2 / (1 + (-(2 * logLik(null_model)) / nrow(schwan_df)))^(2 / nrow(schwan_df))

# Print the result
cat("Nagelkerke's R-squared:", nagelkerke_r2, "\n")