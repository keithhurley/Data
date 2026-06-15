# ------------------------------------------------------------------------------
# R Script: Aggregate Survey Raw Data and Factor Levels (Year-Specific)
# Goal: 
# 1. Combine raw unfactored data and save as CSV.
# 2. Extract 2025 factor levels, combine with historical, save as CSV.
# 3. Apply factor levels (accounting for year differences) and save RData.
# ------------------------------------------------------------------------------

# 1. SETUP
library(tidyverse)
library(readxl)
library(readr)

# Define file paths
raw_historical_file <- "surveyData_20180117.csv"
survey_2025_file <- "2025_Anglers_Final.xlsx"
factor_historical_file <- "FactorLevels.csv"
codebook_file <- "Codebook.xlsx"

out_raw_csv <- "surveyData_aggregated.csv"
out_factors_csv <- "FactorLevels_aggregated.csv"
out_final_rdata <- "Angler_Survey_Aggregated_2025_Final.RData"

cat("Loading data files...\n")

# 2. LOAD DATA
raw_historical <- read_csv(raw_historical_file, show_col_types = FALSE)
# If raw_historical has a weird first column like `...1`, let's remove it if it's just row numbers
if ("...1" %in% colnames(raw_historical)) {
  raw_historical <- raw_historical %>% select(-`...1`)
}

raw_2025 <- read_excel(survey_2025_file)
factors_historical <- read_csv(factor_historical_file, show_col_types = FALSE)
cb_full <- read_excel(codebook_file, sheet = 1, col_names = FALSE)

# 3. STANDARDIZE 2025 RAW DATA NAMES & ADD YEAR
cat("Standardizing 2025 raw data names...\n")
names_2025_orig <- colnames(raw_2025)
names_2025_new <- names_2025_orig %>% str_remove("_2025$")
colnames(raw_2025) <- names_2025_new

if (!"surveyYear" %in% colnames(raw_2025)) {
  raw_2025$surveyYear <- 2025
}

# 4. AGGREGATE RAW DATA & SAVE
cat("Aggregating raw data...\n")
# Ensure types match for binding (convert everything to character first to avoid bind_rows type conflicts, 
# since we'll refactor anyway based on the codebook)
raw_historical_chr <- raw_historical %>% mutate(across(everything(), as.character))
raw_2025_chr <- raw_2025 %>% mutate(across(everything(), as.character))

raw_combined <- bind_rows(raw_historical_chr, raw_2025_chr)

cat("Saving aggregated raw data to:", out_raw_csv, "\n")
write_csv(raw_combined, out_raw_csv)

# 5. PARSE CODEBOOK FOR 2025 FACTOR LEVELS
cat("Extracting 2025 factor levels from Codebook...\n")
extract_codebook_factors <- function(cb_df) {
  res <- data.frame(Type=character(), Question=character(), Field=character(), Year=numeric(), Value=numeric(), Label=character(), stringsAsFactors=FALSE)
  
  current_var <- NA
  current_question <- NA
  is_labeled_section <- FALSE
  
  for (i in 1:nrow(cb_df)) {
    val1 <- as.character(cb_df[i, 1])
    val2 <- as.character(cb_df[i, 2])
    val3 <- as.character(cb_df[i, 3])
    
    if (!is.na(val1) && !val1 %in% c("Labeled Values", "Standard Attributes", "N", "Central Tendency and Dispersion", "New names:")) {
      current_var <- val1
      current_question <- NA
      is_labeled_section <- FALSE
    }
    
    if (!is.na(val2) && val2 == "Label" && !is.na(val3)) {
      current_question <- val3
    }
    
    if (!is.na(val1) && val1 == "Labeled Values") {
      is_labeled_section <- TRUE
      next
    }
    
    if (is_labeled_section && !is.na(current_var)) {
      if (!is.na(val2) && !is.na(val3) && val2 != "Value" && val2 != "Total" && val2 != "Count" && val2 != "Percent") {
        if (grepl("^-?[0-9.]+$", val2)) {
          clean_field <- sub("_2025$", "", current_var)
          res <- rbind(res, data.frame(
            Type = "SelectOne", 
            Question = ifelse(is.na(current_question), "", current_question),
            Field = clean_field,
            Year = 2025,
            Value = as.numeric(val2),
            Label = val3,
            stringsAsFactors = FALSE
          ))
        }
      }
      if (is.na(val2) || val2 == "Total") {
        is_labeled_section <- FALSE
      }
    }
  }
  return(res)
}

factors_2025 <- extract_codebook_factors(cb_full)

# Combine Factor Levels & SAVE
cat("Combining factor levels...\n")
factors_combined <- bind_rows(factors_historical, factors_2025)
cat("Saving aggregated factor levels to:", out_factors_csv, "\n")
write_csv(factors_combined, out_factors_csv)

# 6. APPLY FACTORS TO RAW DATA (ACCOUNTING FOR YEAR)
cat("Applying factor levels to aggregated raw data (year-specific)...\n")

# Get fields that have mapping
fields_with_mapping <- unique(factors_combined$Field)

# We want surveyYear to be numeric for joining
raw_combined$surveyYear <- as.numeric(raw_combined$surveyYear)

for (col_name in names(raw_combined)) {
  if (col_name %in% fields_with_mapping) {
    # Extract mapping for this column
    col_map <- factors_combined %>% 
      filter(Field == col_name) %>% 
      select(Year, Value, Label) %>% 
      distinct() %>%
      mutate(Value = as.character(Value)) # match raw data character type
    
    if (nrow(col_map) > 0) {
      # Prepare a temporary lookup dataframe
      temp_df <- data.frame(
        RowID = 1:nrow(raw_combined),
        Year = raw_combined$surveyYear,
        Value = raw_combined[[col_name]],
        stringsAsFactors = FALSE
      )
      
      # Join by Year and Value
      temp_df <- temp_df %>%
        left_join(col_map, by = c("Year", "Value"))
      
      # If Label is available, use it; otherwise fall back to original Value
      # For values that are already character labels, they won't match the numeric Value,
      # which is fine as they'll just fall back to themselves.
      new_values <- ifelse(!is.na(temp_df$Label), temp_df$Label, temp_df$Value)
      
      # Replace column and turn into a factor
      raw_combined[[col_name]] <- factor(new_values)
    }
  } else if (col_name != "surveyYear" && col_name != "ID") {
    # For columns without a mapping, we can try to guess numeric vs factor
    # But usually leaving as character or converting to factor is safest.
    # We will convert character fields with fewer than 100 unique values to factors
    num_unique <- length(unique(raw_combined[[col_name]]))
    if (num_unique < 100) {
      raw_combined[[col_name]] <- factor(raw_combined[[col_name]])
    } else {
      # Try converting to numeric if it looks numeric
      if (all(grepl("^-?[0-9.]*$", na.omit(raw_combined[[col_name]])))) {
        raw_combined[[col_name]] <- as.numeric(raw_combined[[col_name]])
      }
    }
  }
}

# Ensure surveyYear is numeric
raw_combined$surveyYear <- as.numeric(raw_combined$surveyYear)

# 7. SAVE FINAL RDATA
cat("Saving final aggregated and factored data to:", out_final_rdata, "\n")
final_df <- raw_combined
save(final_df, file = out_final_rdata)

cat("Aggregation and factoring complete!\n")
cat("Final dataset contains", nrow(final_df), "records across", ncol(final_df), "variables.\n")
