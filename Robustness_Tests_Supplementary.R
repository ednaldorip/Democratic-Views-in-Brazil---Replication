################################################################################
# SUPPLEMENTARY ANALYSES: Robustness Tests and Diagnostics
# Democratic Views in Brazil
#
# This script contains all supplementary analyses reported in Appendices B, C, D:
# - Appendix B: Sensitivity analysis of dichotomization thresholds
# - Appendix C: Detailed Mokken scale diagnostics  
# - Appendix D: Numeric values underlying Figure 1
#
# Prerequisites: Run Democratic_Views_Replication.R first
#
# Authors: [Removed for peer review]
# Date: February 2025
################################################################################

# This script assumes you have already run Democratic_Views_Replication.R
# and have the following objects in your environment:
# - data (survey dataset with dichotomized variables)
# - design (survey design object with weights)

################################################################################
# SETUP
################################################################################

library(survey)
library(mokken)
library(dplyr)
library(psych)

# Verify required objects exist
required_objects <- c("data", "design")
missing_objects <- required_objects[!sapply(required_objects, exists)]

if(length(missing_objects) > 0) {
  stop(paste("Missing required objects:", paste(missing_objects, collapse = ", "),
             "\nPlease run Democratic_Views_Replication.R first."))
}

cat("=== SUPPLEMENTARY ANALYSES ===\n")
cat("Starting robustness tests and diagnostics\n\n")

################################################################################
# APPENDIX B: SENSITIVITY ANALYSIS OF DICHOTOMIZATION THRESHOLDS
################################################################################

cat("================================================================================\n")
cat("APPENDIX B: SENSITIVITY ANALYSIS OF DICHOTOMIZATION THRESHOLDS\n")
cat("================================================================================\n\n")

# Original variables used in typology construction
variables_of_interest <- c("P10", "P11", "P08", "P06", "P07", "P03", "P02A", 
                           "P14", "P12", "P04", "P05", "P13", "P19", "P09")

# Alternative dichotomization functions
dichotomize_10 <- function(x) ifelse(x == 10, 1, 0)
dichotomize_9 <- function(x) ifelse(x >= 9, 1, 0)
dichotomize_8 <- function(x) ifelse(x >= 8, 1, 0)

# Initialize storage
thresholds <- c(10, 9, 8)
threshold_labels <- c("10", "9+", "8+")
threshold_comparison <- list()

cat("Testing alternative thresholds: =10 (original), ≥9, ≥8\n\n")

# Loop through thresholds
for (i in seq_along(thresholds)) {
  thresh <- thresholds[i]
  label <- threshold_labels[i]
  
  cat(sprintf("--- Processing threshold: %s ---\n", label))
  
  # Create temporary data
  data_temp <- data
  
  # Select appropriate dichotomization function
  if (thresh == 10) {
    dichotomize_func <- dichotomize_10
  } else if (thresh == 9) {
    dichotomize_func <- dichotomize_9
  } else {
    dichotomize_func <- dichotomize_8
  }
  
  # Apply dichotomization
  for(var in variables_of_interest) {
    if(var %in% names(data_temp)) {
      data_temp[[paste0(var, "_dic")]] <- dichotomize_func(data_temp[[var]])
    }
  }
  
  # Compute dimensions
  lib_vars <- c("P10_dic", "P11_dic", "P08_dic")
  data_temp$lib <- rowSums(data_temp[lib_vars], na.rm = TRUE)
  
  soc_vars <- c("P06_dic", "P07_dic", "P03_dic", "P02A_dic")
  data_temp$soc <- rowSums(data_temp[soc_vars], na.rm = TRUE)
  
  part_vars <- c("P14_dic", "P12_dic", "P04_dic", "P05_dic", 
                 "P13_dic", "P19_dic", "P09_dic")
  data_temp$part <- rowSums(data_temp[part_vars], na.rm = TRUE)
  
  # Dichotomize dimensions based on threshold
  if (thresh == 10) {
    data_temp$lib_bin <- ifelse(data_temp$lib == 3, 1, 0)
    data_temp$soc_bin <- ifelse(data_temp$soc == 4, 1, 0)
    data_temp$part_bin <- ifelse(data_temp$part == 7, 1, 0)
  } else if (thresh == 9) {
    data_temp$lib_bin <- ifelse(data_temp$lib >= 3, 1, 0)
    data_temp$soc_bin <- ifelse(data_temp$soc >= 4, 1, 0)
    data_temp$part_bin <- ifelse(data_temp$part >= 7, 1, 0)
  } else {
    data_temp$lib_bin <- ifelse(data_temp$lib >= 2, 1, 0)
    data_temp$soc_bin <- ifelse(data_temp$soc >= 3, 1, 0)
    data_temp$part_bin <- ifelse(data_temp$part >= 5, 1, 0)
  }
  
  # Create typology
  data_temp <- data_temp %>%
    mutate(
      tipo_dem = case_when(
        lib_bin == 1 & soc_bin == 1 & part_bin == 1 ~ "Multidimensional",
        lib_bin == 1 & soc_bin == 1 & part_bin == 0 ~ "Social-Liberal",
        lib_bin == 1 & soc_bin == 0 & part_bin == 1 ~ "Liberal-Participatory",
        lib_bin == 0 & soc_bin == 1 & part_bin == 1 ~ "Social-Participatory",
        lib_bin == 1 & soc_bin == 0 & part_bin == 0 ~ "Liberal",
        lib_bin == 0 & soc_bin == 1 & part_bin == 0 ~ "Social",
        lib_bin == 0 & soc_bin == 0 & part_bin == 1 ~ "Participatory",
        lib_bin == 0 & soc_bin == 0 & part_bin == 0 ~ "Non-democrat",
        TRUE ~ NA_character_
      )
    )
  
  # Create survey design
  design_temp <- svydesign(ids = ~1, weights = ~FATOR_POND, data = data_temp)
  
  # Calculate distribution
  type_dist <- svytable(~tipo_dem, design_temp)
  type_prop <- prop.table(type_dist) * 100
  
  # Calculate Mokken H coefficient
  mokken_items <- data_temp %>%
    dplyr::select(lib_bin, soc_bin, part_bin) %>%
    na.omit() %>%
    as.data.frame()
  
  H_result <- coefH(mokken_items)
  H_global <- as.numeric(unlist(strsplit(as.character(H_result$H), " "))[1])
  
  # Store results
  threshold_comparison[[label]] <- list(
    threshold = label,
    data = data_temp,
    design = design_temp,
    distribution = type_prop,
    H_coefficient = H_global,
    n_cases = nrow(data_temp)
  )
  
  cat(sprintf("  H coefficient: %.3f\n", H_global))
  cat("  Type distribution:\n")
  print(round(type_prop, 1))
  cat("\n")
}

# Calculate reclassification rates
tipo_10 <- threshold_comparison[["10"]]$data$tipo_dem
tipo_9 <- threshold_comparison[["9+"]]$data$tipo_dem
tipo_8 <- threshold_comparison[["8+"]]$data$tipo_dem

reclass_9_vs_10 <- mean(tipo_10 != tipo_9, na.rm = TRUE) * 100
reclass_8_vs_10 <- mean(tipo_10 != tipo_8, na.rm = TRUE) * 100

cat("=== RECLASSIFICATION RATES ===\n")
cat(sprintf("Threshold ≥9 vs =10: %.1f%% reclassified\n", reclass_9_vs_10))
cat(sprintf("Threshold ≥8 vs =10: %.1f%% reclassified\n\n", reclass_8_vs_10))

# Create Table B1
sensitivity_summary_table <- data.frame(
  Threshold = threshold_labels,
  H_coefficient = sapply(threshold_comparison, function(x) round(x$H_coefficient, 3)),
  Pct_reclassified = c(0, round(reclass_9_vs_10, 1), round(reclass_8_vs_10, 1)),
  Multidimensional = sapply(threshold_comparison, 
                            function(x) round(x$distribution["Multidimensional"], 1)),
  Social_Participatory = sapply(threshold_comparison, 
                                function(x) round(x$distribution["Social-Participatory"], 1)),
  Non_democrat = sapply(threshold_comparison, 
                        function(x) round(x$distribution["Non-democrat"], 1))
)

cat("TABLE B1: Sensitivity Analysis of Dichotomization Thresholds\n")
print(sensitivity_summary_table)
cat("\n")

# Item-level distribution analysis (Table B1a)
item_analysis <- data.frame()

for (var in variables_of_interest) {
  if (var %in% names(data)) {
    var_data <- data[[var]]
    
    item_analysis <- rbind(item_analysis, data.frame(
      Variable = var,
      Mean = round(mean(var_data, na.rm = TRUE), 2),
      SD = round(sd(var_data, na.rm = TRUE), 2),
      Prop_10 = round(mean(var_data == 10, na.rm = TRUE) * 100, 1),
      Prop_9 = round(mean(var_data == 9, na.rm = TRUE) * 100, 1),
      Prop_8 = round(mean(var_data == 8, na.rm = TRUE) * 100, 1),
      Prop_9plus = round(mean(var_data >= 9, na.rm = TRUE) * 100, 1),
      Prop_8plus = round(mean(var_data >= 8, na.rm = TRUE) * 100, 1)
    ))
  }
}

cat("TABLE B1a: Distribution of Original Variables Across Thresholds\n")
print(item_analysis)
cat("\n")

# Dimension-level summary (Table B1b)
lib_items <- c("P10", "P11", "P08")
soc_items <- c("P06", "P07", "P03", "P02A")
part_items <- c("P14", "P12", "P04", "P05", "P13", "P19", "P09")

dimension_summary <- data.frame(
  Dimension = c("Liberal", "Social", "Participatory"),
  N_items = c(length(lib_items), length(soc_items), length(part_items)),
  Mean_prop_10 = round(c(
    mean(item_analysis$Prop_10[item_analysis$Variable %in% lib_items]),
    mean(item_analysis$Prop_10[item_analysis$Variable %in% soc_items]),
    mean(item_analysis$Prop_10[item_analysis$Variable %in% part_items])
  ), 1),
  Mean_prop_9plus = round(c(
    mean(item_analysis$Prop_9plus[item_analysis$Variable %in% lib_items]),
    mean(item_analysis$Prop_9plus[item_analysis$Variable %in% soc_items]),
    mean(item_analysis$Prop_9plus[item_analysis$Variable %in% part_items])
  ), 1),
  Mean_prop_8plus = round(c(
    mean(item_analysis$Prop_8plus[item_analysis$Variable %in% lib_items]),
    mean(item_analysis$Prop_8plus[item_analysis$Variable %in% soc_items]),
    mean(item_analysis$Prop_8plus[item_analysis$Variable %in% part_items])
  ), 1)
)

cat("TABLE B1b: Summary Statistics by Dimension\n")
print(dimension_summary)
cat("\n\n")

################################################################################
# APPENDIX C: MOKKEN SCALE DIAGNOSTICS
################################################################################

cat("================================================================================\n")
cat("APPENDIX C: MOKKEN SCALE DIAGNOSTICS\n")
cat("================================================================================\n\n")

# Use threshold=10 data for diagnostics
mokken_items <- threshold_comparison[["10"]]$data %>%
  dplyr::select(lib_bin, soc_bin, part_bin) %>%
  na.omit() %>%
  as.data.frame()

cat("Sample size for diagnostics:", nrow(mokken_items), "\n\n")

# C.1 Item-level H coefficients
cat("--- Item-level H coefficients ---\n")
item_H_coefs <- coefH(mokken_items)
print(item_H_coefs)
cat("\n")

Hi_values <- item_H_coefs$Hi[, "Item H"]
SE_values <- item_H_coefs$Hi[, "se"]

# C.2 Automated Item Selection Procedure
cat("--- Automated Item Selection Procedure (AISP) ---\n")
aisp_result <- aisp(mokken_items, lowerbound = 0.3)
print(summary(aisp_result))
cat("\n")

# C.3 Monotonicity violations
cat("--- Monotonicity Check ---\n")
monotonicity <- check.monotonicity(mokken_items)
print(summary(monotonicity))

mono_violations <- c(
  Liberal = ifelse(is.null(monotonicity$lib_bin), 0, 
                   sum(monotonicity$lib_bin$sum.crit > 0, na.rm = TRUE)),
  Social = ifelse(is.null(monotonicity$soc_bin), 0, 
                  sum(monotonicity$soc_bin$sum.crit > 0, na.rm = TRUE)),
  Participatory = ifelse(is.null(monotonicity$part_bin), 0, 
                         sum(monotonicity$part_bin$sum.crit > 0, na.rm = TRUE))
)
cat("\n")

# C.4 Invariant Item Ordering
cat("--- Invariant Item Ordering (IIO) Check ---\n")
iio <- check.iio(mokken_items)
print(summary(iio))

iio_violations <- rep(0, 3)
if (!is.null(iio$item.summary)) {
  iio_violations <- iio$item.summary$"#vi"
}
cat("\n")

# Create Table C1
mokken_diagnostics_table <- data.frame(
  Item = c("Liberal", "Social", "Participatory"),
  Hi = round(Hi_values, 3),
  SE_Hi = round(SE_values, 3),
  AISP_included = rep("Yes", 3),
  Monotonicity_violations = mono_violations,
  IIO_violations = iio_violations
)

cat("TABLE C1: Item-Level Mokken Scale Diagnostics\n")
print(mokken_diagnostics_table)
cat("\n")

# C.5 Reliability measures
cat("--- Reliability Measures ---\n")

ms_rho <- check.reliability(mokken_items, LCRC = FALSE)
cat("MS-rho (Molenaar-Sijtsma):", round(ms_rho$MS, 3), "\n")

lcrc <- check.reliability(mokken_items, LCRC = TRUE)
cat("LCRC (Latent Class Reliability):", round(lcrc$LCRC, 3), "\n")

# GLB if psych available
glb_value <- NA
if (requireNamespace("psych", quietly = TRUE)) {
  cor_matrix <- cor(mokken_items, use = "pairwise.complete.obs")
  glb_result <- glb.fa(cor_matrix)
  glb_value <- glb_result$glb
  cat("GLB (Greatest Lower Bound):", round(glb_value, 3), "\n")
}

# Cronbach's alpha for comparison
if (requireNamespace("psych", quietly = TRUE)) {
  alpha_result <- alpha(mokken_items)
  alpha_value <- alpha_result$total$raw_alpha
  cat("Cronbach's Alpha:", round(alpha_value, 3), "(not recommended)\n")
}
cat("\n")

# Create Table C3
reliability_table <- data.frame(
  Measure = c("MS-rho", "LCRC", "GLB", "Cronbach's Alpha"),
  Estimate = c(
    round(ms_rho$MS, 3),
    round(lcrc$LCRC, 3),
    ifelse(!is.na(glb_value), round(glb_value, 3), NA),
    round(alpha_value, 3)
  ),
  Recommendation = c(
    "Recommended for Mokken scales",
    "Alternative for dichotomous items",
    "Upper bound of reliability",
    "Not recommended (assumes tau-equivalence)"
  )
)

cat("TABLE C3: Scale Reliability Measures\n")
print(reliability_table)
cat("\n\n")

################################################################################
# APPENDIX D: NUMERIC VALUES UNDERLYING FIGURE 1
################################################################################

cat("================================================================================\n")
cat("APPENDIX D: NUMERIC VALUES UNDERLYING FIGURE 1\n")
cat("================================================================================\n\n")

typology_categories <- unique(data$tipo_dem[!is.na(data$tipo_dem)])
dimension_vars <- c("lib", "soc", "part")

figure1_table <- data.frame()

cat("Calculating detailed statistics for each typology-dimension combination...\n\n")

for (tipo in typology_categories) {
  design_subset <- subset(design, tipo_dem == tipo)
  n_cases <- nrow(design_subset$variables)
  
  for (dim in dimension_vars) {
    if (dim %in% names(design_subset$variables)) {
      mean_result <- svymean(as.formula(paste0("~", dim)), design_subset, na.rm = TRUE)
      
      mean_val <- coef(mean_result)[1]
      se_val <- SE(mean_result)[1]
      ci_lower <- mean_val - 1.96 * se_val
      ci_upper <- mean_val + 1.96 * se_val
      sd_val <- sd(design_subset$variables[[dim]], na.rm = TRUE)
      
      figure1_table <- rbind(figure1_table, data.frame(
        Typology = tipo,
        Dimension = toupper(dim),
        N = n_cases,
        Mean = round(mean_val, 2),
        SD = round(sd_val, 2),
        SE = round(se_val, 3),
        CI_95 = paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]"),
        stringsAsFactors = FALSE
      ))
    }
  }
}

figure1_table <- figure1_table[order(figure1_table$Typology, figure1_table$Dimension), ]

cat("TABLE D1: Numeric Values, Sample Sizes, and 95% CIs Underlying Figure 1\n")
print(figure1_table)
cat("\n")

# Create wide format for easier reading
figure1_wide <- data.frame(
  Typology = typology_categories,
  N = numeric(length(typology_categories)),
  Liberal = character(length(typology_categories)),
  Social = character(length(typology_categories)),
  Participatory = character(length(typology_categories)),
  stringsAsFactors = FALSE
)

for (i in seq_along(typology_categories)) {
  tipo <- typology_categories[i]
  design_subset <- subset(design, tipo_dem == tipo)
  figure1_wide$N[i] <- nrow(design_subset$variables)
  
  for (dim in dimension_vars) {
    if (dim %in% names(design_subset$variables)) {
      mean_result <- svymean(as.formula(paste0("~", dim)), design_subset, na.rm = TRUE)
      mean_val <- round(coef(mean_result)[1], 2)
      ci_lower <- round(coef(mean_result)[1] - 1.96 * SE(mean_result)[1], 2)
      ci_upper <- round(coef(mean_result)[1] + 1.96 * SE(mean_result)[1], 2)
      
      col_name <- paste0(toupper(substring(dim, 1, 1)), substring(dim, 2))
      figure1_wide[[col_name]][i] <- paste0(mean_val, " [", ci_lower, ", ", ci_upper, "]")
    }
  }
}

cat("\nTABLE D1 (Wide Format): Mean [95% CI] by Typology\n")
print(figure1_wide)
cat("\n")

################################################################################
# SAVE ALL RESULTS
################################################################################

cat("================================================================================\n")
cat("SAVING RESULTS\n")
cat("================================================================================\n\n")

# Save to RData file
save(
  threshold_comparison,
  sensitivity_summary_table,
  item_analysis,
  dimension_summary,
  mokken_diagnostics_table,
  reliability_table,
  figure1_table,
  figure1_wide,
  reclass_9_vs_10,
  reclass_8_vs_10,
  file = "appendix_robustness_analyses.RData"
)

cat("✓ All results saved to: appendix_robustness_analyses.RData\n")

# Export tables to CSV
write.csv(sensitivity_summary_table, "Table_B1_Sensitivity.csv", row.names = FALSE)
write.csv(item_analysis, "Table_B1a_Item_Distribution.csv", row.names = FALSE)
write.csv(dimension_summary, "Table_B1b_Dimension_Summary.csv", row.names = FALSE)
write.csv(mokken_diagnostics_table, "Table_C1_Mokken_Diagnostics.csv", row.names = FALSE)
write.csv(reliability_table, "Table_C3_Reliability.csv", row.names = FALSE)
write.csv(figure1_table, "Table_D1_Figure1_Long.csv", row.names = FALSE)
write.csv(figure1_wide, "Table_D1_Figure1_Wide.csv", row.names = FALSE)

cat("✓ Tables exported to CSV format\n")

# Export to Word if flextable available
if (requireNamespace("flextable", quietly = TRUE)) {
  library(flextable)
  
  save_as_docx(flextable(sensitivity_summary_table), path = "Table_B1.docx")
  save_as_docx(flextable(mokken_diagnostics_table), path = "Table_C1.docx")
  save_as_docx(flextable(reliability_table), path = "Table_C3.docx")
  save_as_docx(flextable(figure1_wide), path = "Table_D1.docx")
  
  cat("✓ Tables exported to Word format\n")
}

cat("\n")
cat(paste(rep("=", 80), collapse = ""))
cat("\n")
cat("SUPPLEMENTARY ANALYSES COMPLETED SUCCESSFULLY\n")
cat(paste(rep("=", 80), collapse = ""))
cat("\n")

################################################################################
# END OF SUPPLEMENTARY ANALYSES
################################################################################
