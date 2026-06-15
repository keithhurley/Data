# ------------------------------------------------------------------------------
# R Script: Aggregate 2025 Angler Survey with Master Dataset
# Goal: Standardize names, factorize based on codebook, and merge datasets.
# ------------------------------------------------------------------------------

# 1. SETUP
library(tidyverse)
library(readxl)

# Define file paths (using absolute paths or relative to workspace)
master_file <- "d_20180216_2018"
survey_2025_file <- "2025_Anglers_Final.xlsx"
codebook_file <- "Codebook.xlsx"
output_file <- "Angler_Survey_Aggregated_2025_Final.RData"

cat("Loading data files...\n")

# 2. LOAD DATA
# Load historical master (loads object 'd')
load(master_file) 
master_df <- d # rename for clarity in script
rm(d)

# Load 2025 survey
df_2025 <- read_excel(survey_2025_file)

# Load Codebook
cb_full <- read_excel(codebook_file, sheet = 1, col_names = FALSE)

# 3. PARSE CODEBOOK FOR LABEL MAPPINGS
# This function extracts numeric-to-label mappings from the tricky Excel format
extract_mappings <- function(cb_df) {
  mappings <- list()
  current_var <- NULL
  is_labeled_section <- FALSE
  
  for (i in 1:nrow(cb_df)) {
    val1 <- as.character(cb_df[i, 1])
    val2 <- as.character(cb_df[i, 2])
    val3 <- as.character(cb_df[i, 3])
    
    # Detect new variable section
    # Variable names usually appear in column 1 when it's not "Labeled Values" or "Standard Attributes"
    if (!is.na(val1) && !val1 %in% c("Labeled Values", "Standard Attributes", "N", "Central Tendency and Dispersion")) {
      current_var <- val1
      is_labeled_section <- FALSE
    }
    
    # Detect start of labeled values
    if (!is.na(val1) && val1 == "Labeled Values") {
      is_labeled_section <- TRUE
      next
    }
    
    # Extract value and label if in section
    if (is_labeled_section && !is.na(current_var)) {
      if (!is.na(val2) && !is.na(val3) && val2 != "Value" && val2 != "Total") {
        # Check if val2 is numeric-like
        if (grepl("^[0-9]+$", val2)) {
          if (is.null(mappings[[current_var]])) mappings[[current_var]] <- list()
          mappings[[current_var]][[val2]] <- val3
        }
      }
      # Stop section if we hit an empty row or Total
      if (is.na(val2) || val2 == "Total") {
        is_labeled_section <- FALSE
      }
    }
  }
  return(mappings)
}

cat("Extracting factor labels from codebook...\n")
cb_mappings <- extract_mappings(cb_full)

# 4. STANDARDIZE COLUMN NAMES
cat("Standardizing 2025 column names...\n")
# Rule: Remove '_2025' suffix, and handle specific manual remaps if needed
names_2025_orig <- colnames(df_2025)
names_2025_new <- names_2025_orig %>%
  str_remove("_2025$")

colnames(df_2025) <- names_2025_new

# Add SurveyYear column
df_2025$surveyYear <- 2025

# 5. FACTORIZE 2025 DATA
cat("Applying factor labels to 2025 data...\n")

# We want to apply labels from the codebook
for (var_name in names(df_2025)) {
  # Find matching mapping (try with and without _2025 suffix as stored in codebook)
  map_key <- NULL
  if (var_name %in% names(cb_mappings)) {
    map_key <- var_name
  } else if (paste0(var_name, "_2025") %in% names(cb_mappings)) {
    map_key <- paste0(var_name, "_2025")
  }
  
  if (!is.null(map_key)) {
    mapping <- cb_mappings[[map_key]]
    codes <- as.numeric(names(mapping))
    labels <- as.character(unlist(mapping))
    
    # Convert column to factor
    # We use levels=codes and labels=labels to map the numeric input to character output
    df_2025[[var_name]] <- factor(df_2025[[var_name]], levels = codes, labels = labels)
  }
}

# 6. UNIFY FACTORS WITH MASTER DATA
cat("Unifying factor levels between 2025 and Master data...\n")

common_cols <- intersect(names(master_df), names(df_2025))

for (col in common_cols) {
  if (is.factor(master_df[[col]]) || is.factor(df_2025[[col]])) {
    # Combine levels from both
    lev_master <- levels(master_df[[col]])
    lev_2025 <- levels(df_2025[[col]])
    
    all_levels <- unique(c(lev_master, lev_2025))
    
    # Re-factor both with the superset of levels
    master_df[[col]] <- factor(master_df[[col]], levels = all_levels)
    df_2025[[col]] <- factor(df_2025[[col]], levels = all_levels)
  }
}

# 7. AGGREGATE
cat("Merging datasets...\n")
# Ensure columns are compatible
# Some columns might be numeric in one and char in other? bind_rows handles most, but we check.
final_df <- bind_rows(master_df, df_2025)

# 8. SAVE (Non-destructive)
cat("Saving aggregated data to:", output_file, "\n")
save(final_df, file = output_file)

cat("Aggregation complete!\n")
cat("Final dataset contains", nrow(final_df), "records across", ncol(final_df), "variables.\n")
