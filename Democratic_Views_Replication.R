################################################################################
# REPLICATION CODE: Democratic Views in Brazil
# Analysis of Citizens' Conceptions of Democracy
#
# Based on: Kriesi, Saris and Moncagatta (2016)
# Data: INCT-ReDem National Survey (February 2025, N=1,504)
#
# This script replicates all analyses reported in the manuscript:
# "Democratic Views in Brazil: A Research Note on Citizens' Conceptions 
# of Democracy"
#
# Required R version: 4.3.0 or higher
# Required packages: tidyverse, haven, ggplot2, reshape2, survey, mokken, dplyr, psych
#
# Estimated execution time: 3-5 minutes
# Memory requirements: <500 MB RAM
#
# Authors: [Removed for peer review]
# Date: February 2025
################################################################################

################################################################################
# E.1 SETUP AND PACKAGE LOADING
################################################################################

# Load required packages
library(tidyverse)
library(haven)
library(ggplot2)
library(reshape2)
library(survey)    # For weighted analysis
library(mokken)    # For Item Response Theory
library(dplyr)     # For data manipulation
library(psych)     # For Cronbach's alpha

# Set working directory (adjust as needed)
# setwd("your/working/directory")

################################################################################
# E.2 DATA IMPORT AND INITIAL SETUP
################################################################################

# Import data
# Note: Data will be publicly available at [repository URL] in February 2026
# For early access for replication purposes, contact [removed for peer review]
data <- readRDS("INCT_ReDem_reduced.rds")

# Verify weighting variable exists
if(!"FATOR_POND" %in% names(data)) {
  stop("FATOR_POND variable not found in database. 
       This variable contains post-stratification weights calculated using 
       Random Iterative Method (RIM Weighting).")
}

# Create survey design object with post-stratification weights
# Weights adjust for: region, sex, age, education, occupation, race/color,
# and 2022 presidential vote
design <- svydesign(
  ids = ~1,                    # No clustering (final-stage weights)
  weights = ~FATOR_POND,       # Post-stratification weight variable
  data = data
)

cat("Data loaded successfully\n")
cat("Sample size:", nrow(data), "\n")
cat("Weighted sample size:", sum(data$FATOR_POND), "\n\n")

################################################################################
# E.3 VARIABLE DICHOTOMIZATION
################################################################################

cat("=== VARIABLE DICHOTOMIZATION ===\n")

# Function to dichotomize variables (10 = 1, all other values = 0)
# This captures only attributes considered absolutely essential by respondents
dichotomize <- function(x) {
  ifelse(x == 10, 1, 0)
}

# Variables used in democratic conception measurement
# Based on Kriesi, Saris and Moncagatta (2016) battery of questions
variables_of_interest <- c(
  "P10",   # Equality before the law
  "P11",   # Free and fair elections
  "P08",   # Government explains decisions
  "P06",   # Government reduces income inequality
  "P07",   # Government ensures social security
  "P03",   # Government protects minority rights
  "P02A",  # Opposition can criticize government
  "P14",   # Citizens have final say on political issues
  "P12",   # Civil society influences government decisions
  "P04",   # Courts treat all equally
  "P05",   # Media is free to criticize government
  "P13",   # Government protects unemployed
  "P19",   # Government reduces income inequality (alternative)
  "P09"    # Citizens participate in referendums
)

# Apply dichotomization to all variables
for(var in variables_of_interest) {
  if(var %in% names(data)) {
    data[[paste0(var, "_dic")]] <- dichotomize(data[[var]])
    cat(sprintf("  %s dichotomized: %.1f%% scored 10\n", 
                var, mean(data[[var]] == 10, na.rm = TRUE) * 100))
  }
}

# Update survey design with dichotomized variables
design <- svydesign(ids = ~1, weights = ~FATOR_POND, data = data)

cat("\nDichotomization completed\n\n")

################################################################################
# E.4 DIMENSION CONSTRUCTION
################################################################################

cat("=== DIMENSION CONSTRUCTION ===\n")

# LIBERAL DEMOCRACY dimension (11 items)
# Captures procedural and institutional aspects
liberal_items <- c("P10", "P11", "P08", "P06", "P07", "P03", "P02A", 
                   "P14", "P12", "P04", "P05")
data$liberal_democracy_scale <- rowMeans(
  data[, paste0(liberal_items, "_dic")], 
  na.rm = TRUE
)
cat("Liberal democracy scale created (11 items)\n")
cat("  Mean:", round(mean(data$liberal_democracy_scale, na.rm = TRUE), 3), "\n")

# SOCIAL JUSTICE dimension (2 items)
# Captures redistributive expectations
social_items <- c("P13", "P19")
data$social_justice_scale <- rowMeans(
  data[, paste0(social_items, "_dic")], 
  na.rm = TRUE
)
cat("Social justice scale created (2 items)\n")
cat("  Mean:", round(mean(data$social_justice_scale, na.rm = TRUE), 3), "\n")

# DIRECT DEMOCRACY dimension (1 item)
# Captures participatory expectations
data$direct_democracy_scale <- data$P09_dic
cat("Direct democracy scale created (1 item)\n")
cat("  Mean:", round(mean(data$direct_democracy_scale, na.rm = TRUE), 3), "\n\n")

# Update survey design again
design <- svydesign(ids = ~1, weights = ~FATOR_POND, data = data)

################################################################################
# E.5 MOKKEN SCALE ANALYSIS (HIERARCHICAL VALIDATION)
################################################################################

cat("=== MOKKEN SCALE ANALYSIS ===\n")
cat("Validating hierarchical structure using Item Response Theory\n\n")

# Prepare variables for Mokken analysis
liberal_vars <- c("P10", "P11", "P08", "P06", "P07", "P03", "P02A", 
                  "P14", "P12", "P04", "P05")
mokken_vars <- paste0(liberal_vars, "_dic")

# Check variable existence
existing_vars <- mokken_vars[mokken_vars %in% names(data)]

if(length(existing_vars) >= 2) {
  
  cat("Variables for analysis:", length(existing_vars), "\n")
  
  # Extract complete cases
  temp_data <- data[complete.cases(data[existing_vars]), ]
  
  # Create numeric matrix for Mokken analysis
  mokken_data <- matrix(nrow = nrow(temp_data), ncol = length(existing_vars))
  colnames(mokken_data) <- existing_vars
  
  for(i in 1:length(existing_vars)) {
    mokken_data[, i] <- as.numeric(temp_data[[existing_vars[i]]])
  }
  
  # Convert to data frame
  mokken_data <- as.data.frame(mokken_data)
  
  cat("Valid cases:", nrow(mokken_data), "\n")
  
  if(nrow(mokken_data) >= 50) {
    
    # Check variability (remove constant variables)
    has_var <- sapply(mokken_data, function(x) var(x, na.rm = TRUE) > 0)
    mokken_data <- mokken_data[, has_var, drop = FALSE]
    
    cat("Variables with variability:", ncol(mokken_data), "\n\n")
    
    if(ncol(mokken_data) >= 2) {
      
      # E.5.1 Automated Item Selection Procedure (AISP)
      cat("--- Automated Item Selection Procedure ---\n")
      aisp_result <- NULL
      try({
        aisp_result <- mokken::aisp(mokken_data, lowerbound = 0.3, verbose = FALSE)
      }, silent = TRUE)
      
      if(!is.null(aisp_result) && length(aisp_result) > 0) {
        cat("Scales identified:", length(aisp_result), "\n")
        for(i in 1:length(aisp_result)) {
          if(length(aisp_result[[i]]) > 1) {
            vars_in_scale <- colnames(mokken_data)[aisp_result[[i]]]
            cat("Scale", i, ":", paste(vars_in_scale, collapse = ", "), "\n")
          }
        }
      } else {
        cat("No strong scale identified (H >= 0.3)\n")
      }
      
      # E.5.2 H Coefficient for complete scale
      cat("\n--- H Coefficient for Complete Scale ---\n")
      h_result <- NULL
      try({
        h_result <- mokken::coefH(mokken_data)
      }, silent = TRUE)
      
      if(!is.null(h_result) && "H" %in% names(h_result)) {
        h_value <- h_result$H
        if(is.numeric(h_value) && length(h_value) == 1) {
          cat("H =", round(h_value, 3), "\n")
          
          # Interpret H coefficient
          if(h_value >= 0.5) {
            cat("Quality: STRONG (H >= 0.5)\n")
          } else if(h_value >= 0.4) {
            cat("Quality: MODERATE (H >= 0.4)\n")
          } else if(h_value >= 0.3) {
            cat("Quality: WEAK (H >= 0.3)\n")
          } else {
            cat("Quality: INADEQUATE (H < 0.3)\n")
          }
        }
        
        # E.5.3 Item-level coefficients
        if("Hi" %in% names(h_result) && is.numeric(h_result$Hi)) {
          cat("\n--- Coefficients by Item ---\n")
          hi_values <- h_result$Hi
          for(j in 1:length(hi_values)) {
            item_name <- names(hi_values)[j]
            if(is.null(item_name)) item_name <- colnames(mokken_data)[j]
            cat(item_name, ":", round(hi_values[j], 3), "\n")
          }
        }
      }
      
      # E.5.4 Monotonicity check
      cat("\n--- Monotonicity Check ---\n")
      mono_result <- NULL
      try({
        mono_result <- mokken::check.monotonicity(mokken_data, minvi = 0.03)
      }, silent = TRUE)
      
      if(!is.null(mono_result) && "InspectedPairs" %in% names(mono_result)) {
        violations <- sum(mono_result$InspectedPairs$Crit > 40, na.rm = TRUE)
        cat("Significant violations:", violations, "\n")
        if(violations == 0) {
          cat("Result: No significant monotonicity violations detected\n")
        }
      }
      
      # E.5.5 Item statistics (unweighted - Mokken doesn't support weights)
      cat("\n--- Item Statistics (unweighted) ---\n")
      item_means <- colMeans(mokken_data, na.rm = TRUE) * 100
      for(k in 1:length(item_means)) {
        cat(names(item_means)[k], ":", round(item_means[k], 1), "%\n")
      }
      
    } else {
      cat("Inadequate data: fewer than 2 variables with variability\n")
    }
  } else {
    cat("Sample too small for Mokken analysis (minimum 50 cases)\n")
  }
} else {
  cat("Insufficient variables for Mokken analysis\n")
}

# E.5.6 Cronbach's Alpha (for comparison)
cat("\n--- Cronbach's Alpha ---\n")
cat("Note: Alpha assumes tau-equivalence and is not the primary reliability\n")
cat("measure for Mokken scales. Reported for comparative purposes only.\n")
alpha_value <- psych::alpha(mokken_data)$total$raw_alpha
cat("Alpha =", round(alpha_value, 3), "\n\n")

################################################################################
# E.6 TYPOLOGY CONSTRUCTION
################################################################################

cat("=== DEMOCRATIC TYPOLOGY CONSTRUCTION ===\n")

# E.6.1 Identify basic democrats
# Basic democrats endorse the two fundamental elements:
# - Equality before the law (P10)
# - Free and fair elections (P02A)
data$basic_democrat <- ifelse(data$P10_dic == 1 & data$P02A_dic == 1, 1, 0)

cat("Basic democrats (P10=10 & P02A=10):", 
    round(mean(data$basic_democrat, na.rm = TRUE) * 100, 1), "%\n")

# E.6.2 Classify by intensity of liberal orientation
# Minimalist: meet basic threshold but < 5/11 items
# Maximalist: meet basic threshold and >= 5/11 items
data$liberal_level <- ifelse(
  data$basic_democrat == 0, 
  "non_democrat",
  ifelse(data$liberal_democracy_scale >= 5/11, "maximalist", "minimalist")
)

# E.6.3 Classify by social justice dimension
data$social_level <- ifelse(
  data$social_justice_scale > 0, 
  "social", 
  "non_social"
)

# E.6.4 Classify by participatory dimension
data$participatory_level <- ifelse(
  data$direct_democracy_scale == 1, 
  "participatory", 
  "non_participatory"
)

# E.6.5 Create complete 9-category typology
data$democracy_typology <- case_when(
  data$liberal_level == "non_democrat" ~ "Non-democrat",
  
  data$liberal_level == "minimalist" & 
    data$social_level == "non_social" & 
    data$participatory_level == "non_participatory" ~ "Minimalist",
  
  data$liberal_level == "minimalist" & 
    data$social_level == "social" & 
    data$participatory_level == "non_participatory" ~ "Social Minimalist",
  
  data$liberal_level == "minimalist" & 
    data$social_level == "non_social" & 
    data$participatory_level == "participatory" ~ "Participatory Minimalist",
  
  data$liberal_level == "minimalist" & 
    data$social_level == "social" & 
    data$participatory_level == "participatory" ~ "Social-Participatory Minimalist",
  
  data$liberal_level == "maximalist" & 
    data$social_level == "non_social" & 
    data$participatory_level == "non_participatory" ~ "Maximalist",
  
  data$liberal_level == "maximalist" & 
    data$social_level == "social" & 
    data$participatory_level == "non_participatory" ~ "Social Maximalist",
  
  data$liberal_level == "maximalist" & 
    data$social_level == "non_social" & 
    data$participatory_level == "participatory" ~ "Participatory Maximalist",
  
  data$liberal_level == "maximalist" & 
    data$social_level == "social" & 
    data$participatory_level == "participatory" ~ "Social-Participatory Maximalist",
  
  TRUE ~ NA_character_
)

# E.6.6 Create consolidated 5-category typology (used in manuscript)
# This consolidation improves analytical robustness by avoiding small cell sizes
data$democracy_typology_consolidated <- case_when(
  data$democracy_typology == "Non-democrat" ~ "Non-democrat",
  
  data$democracy_typology %in% c("Minimalist", "Maximalist") ~ "Basic Liberal",
  
  data$democracy_typology %in% c("Social Minimalist", "Social Maximalist") ~ "Social",
  
  data$democracy_typology %in% c("Participatory Minimalist", 
                                  "Participatory Maximalist") ~ "Participatory",
  
  data$democracy_typology %in% c("Social-Participatory Minimalist", 
                                  "Social-Participatory Maximalist") ~ "Multidimensional",
  
  TRUE ~ NA_character_
)

# Update survey design with typology variables
design <- svydesign(ids = ~1, weights = ~FATOR_POND, data = data)

# Calculate weighted distribution
cat("\n--- Typology Distribution (weighted) ---\n")
tipo_dist <- svytable(~democracy_typology_consolidated, design)
tipo_prop <- prop.table(tipo_dist) * 100
print(round(tipo_prop, 1))

cat("\n")

################################################################################
# E.7 DESCRIPTIVE STATISTICS BY TYPOLOGY
################################################################################

cat("=== DESCRIPTIVE STATISTICS BY TYPOLOGY ===\n\n")

# Create summary dimension scores for analysis
# Liberal dimension (0-3 scale based on binary dimensions)
data$lib <- rowSums(data[, c("P10_dic", "P11_dic", "P08_dic")], na.rm = TRUE)

# Social dimension (0-4 scale)
data$soc <- rowSums(data[, c("P06_dic", "P07_dic", "P03_dic", "P02A_dic")], 
                    na.rm = TRUE)

# Participatory dimension (0-7 scale)
data$part <- rowSums(data[, c("P14_dic", "P12_dic", "P04_dic", "P05_dic", 
                               "P13_dic", "P19_dic", "P09_dic")], 
                     na.rm = TRUE)

# Create binary dimension indicators (for typology)
data$lib_bin <- ifelse(data$lib == 3, 1, 0)   # All 3 liberal items
data$soc_bin <- ifelse(data$soc == 4, 1, 0)   # All 4 social items  
data$part_bin <- ifelse(data$part == 7, 1, 0) # All 7 participatory items

# Create final typology based on binary dimensions
data$tipo_dem <- case_when(
  data$lib_bin == 1 & data$soc_bin == 1 & data$part_bin == 1 ~ "Multidimensional",
  data$lib_bin == 1 & data$soc_bin == 1 & data$part_bin == 0 ~ "Social-Liberal",
  data$lib_bin == 1 & data$soc_bin == 0 & data$part_bin == 1 ~ "Liberal-Participatory",
  data$lib_bin == 0 & data$soc_bin == 1 & data$part_bin == 1 ~ "Social-Participatory",
  data$lib_bin == 1 & data$soc_bin == 0 & data$part_bin == 0 ~ "Liberal",
  data$lib_bin == 0 & data$soc_bin == 1 & data$part_bin == 0 ~ "Social",
  data$lib_bin == 0 & data$soc_bin == 0 & data$part_bin == 1 ~ "Participatory",
  data$lib_bin == 0 & data$soc_bin == 0 & data$part_bin == 0 ~ "Non-democrat",
  TRUE ~ NA_character_
)

# Update survey design
design <- svydesign(ids = ~1, weights = ~FATOR_POND, data = data)

# Calculate mean scores by typology
typology_categories <- unique(data$tipo_dem[!is.na(data$tipo_dem)])
dimension_vars <- c("lib", "soc", "part")

for (tipo in typology_categories) {
  cat(sprintf("\n--- %s ---\n", tipo))
  
  design_subset <- subset(design, tipo_dem == tipo)
  n_cases <- nrow(design_subset$variables)
  cat(sprintf("N = %d (unweighted)\n", n_cases))
  
  for (dim in dimension_vars) {
    if (dim %in% names(design_subset$variables)) {
      mean_result <- svymean(as.formula(paste0("~", dim)), 
                            design_subset, na.rm = TRUE)
      mean_val <- coef(mean_result)[1]
      se_val <- SE(mean_result)[1]
      
      cat(sprintf("  %s: Mean=%.2f (SE=%.3f)\n", 
                  toupper(dim), mean_val, se_val))
    }
  }
}

cat("\n")

################################################################################
# E.8 FIGURE 1 VISUALIZATION
################################################################################

cat("=== GENERATING FIGURE 1 ===\n")

# Prepare data for visualization
plot_data <- data.frame()

for (tipo in typology_categories) {
  design_subset <- subset(design, tipo_dem == tipo)
  
  for (dim in dimension_vars) {
    if (dim %in% names(design_subset$variables)) {
      mean_result <- svymean(as.formula(paste0("~", dim)), 
                            design_subset, na.rm = TRUE)
      
      plot_data <- rbind(plot_data, data.frame(
        Typology = tipo,
        Dimension = toupper(dim),
        Mean = coef(mean_result)[1],
        SE = SE(mean_result)[1],
        CI_lower = coef(mean_result)[1] - 1.96 * SE(mean_result)[1],
        CI_upper = coef(mean_result)[1] + 1.96 * SE(mean_result)[1]
      ))
    }
  }
}

# Create bar plot with error bars
ggplot(plot_data, aes(x = Typology, y = Mean, fill = Dimension)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_errorbar(
    aes(ymin = CI_lower, ymax = CI_upper),
    position = position_dodge(width = 0.9),
    width = 0.25
  ) +
  theme_minimal() +
  labs(
    title = "Democratic Dimension Scores by Typological Category",
    subtitle = "Weighted means with 95% confidence intervals",
    x = "Typological Category",
    y = "Mean Score",
    fill = "Dimension"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray40")
  ) +
  scale_fill_brewer(palette = "Set2")

# Save plot
ggsave("Figure1_Democratic_Dimensions.png", width = 10, height = 6, dpi = 300)
ggsave("Figure1_Democratic_Dimensions.pdf", width = 10, height = 6)

cat("Figure 1 saved as PNG and PDF\n\n")

################################################################################
# E.9 SESSION INFORMATION
################################################################################

cat("=== SESSION INFORMATION ===\n")
print(sessionInfo())

################################################################################
# END OF MAIN ANALYSIS SCRIPT
################################################################################

cat("\n")
cat(paste(rep("=", 80), collapse = ""))
cat("\n")
cat("MAIN ANALYSIS COMPLETED SUCCESSFULLY\n")
cat("For robustness tests and supplementary analyses, see:\n")
cat("  - Robustness_Tests.R (Appendix B sensitivity analyses)\n")
cat("  - Mokken_Diagnostics.R (Appendix C detailed diagnostics)\n")
cat("  - Figure1_Numeric_Values.R (Appendix D Table D1 generation)\n")
cat(paste(rep("=", 80), collapse = ""))
cat("\n")
