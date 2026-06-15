library(dplyr)
library(tidyr)
library(forcats)
library(readxl)

oldSurveys <- read.csv("surveyData_20180117.csv")
newSurveys <- read_excel("2025_Anglers_Final.xlsx")
factors <- read.csv("FactorLevels_aggregated.csv")
tracking <- read_excel("TrackingForm_Client.xlsx")[, c(1, 5)]

#initially have to adjust names of new survey fields
names(newSurveys) <- sub("_2025", "", names(newSurveys))
newSurveys$surveyYear <- 2025
newSurveys$venue <- as.factor(
  newSurveys %>%
    left_join(tracking, by = c("ID" = "CustomerID")) %>%
    pull("Mode of completion (Mail, web, phone)")
)
oldSurveys$venue <- as.integer(as.factor(oldSurveys$venue))
newSurveys$venue <- as.integer(as.factor(newSurveys$venue))

#write_xlsx(newSurveys,path="2025_Anglers_Final_updated.xlsx")

factor_fields <- factors |> distinct(Field) |> pull(Field)

oldSurveys2 <- oldSurveys

for (field in factor_fields) {
  if (!field %in% names(oldSurveys2)) {
    next
  }

  # All possible labels for this field across all years (union of levels)
  all_labels <- factors |>
    filter(Field == field) |>
    distinct(Label) |>
    pull(Label)

  # Build a lookup key: "Year_Value" -> Label
  lookup <- factors |>
    filter(Field == field) |>
    mutate(key = paste(Year, Value, sep = "_")) |>
    select(key, Label)

  lookup_vec <- setNames(lookup$Label, lookup$key)

  # Replace integer values with labels, using year-specific lookup
  keys <- paste(oldSurveys2$surveyYear, oldSurveys2[[field]], sep = "_")
  labeled <- lookup_vec[keys]

  # Convert to factor with the full union of levels
  oldSurveys2[[field]] <- factor(labeled, levels = all_labels)
}


newSurveys2 <- newSurveys

for (field in factor_fields) {
  if (!field %in% names(newSurveys2)) {
    next
  }

  # All possible labels for this field across all years (union of levels)
  all_labels <- factors |>
    filter(Field == field) |>
    distinct(Label) |>
    pull(Label)

  # Build a lookup key: "Year_Value" -> Label
  lookup <- factors |>
    filter(Field == field) |>
    mutate(key = paste(Year, Value, sep = "_")) |>
    select(key, Label)

  lookup_vec <- setNames(lookup$Label, lookup$key)

  # Replace integer values with labels, using year-specific lookup
  keys <- paste(newSurveys2$surveyYear, newSurveys2[[field]], sep = "_")
  labeled <- lookup_vec[keys]

  # Convert to factor with the full union of levels
  newSurveys2[[field]] <- factor(labeled, levels = all_labels)
}

aggregatedData <- oldSurveys2 %>%
  bind_rows(newSurveys2)

names(aggregatedData)
#remove ids from 2025
aggregatedData <- aggregatedData %>% select(-ID)

save(aggregatedData, file = "AggregatedData_Final.rData")

#add weighting values for 2018 and 2025 surveys
aggregatedData %>%
  left_join(respondents_for_raking)
