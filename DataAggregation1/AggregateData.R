library(foreach)
library(tidyverse)
library(readxl)
library(naniar)

options(stringsAsFactors = FALSE)
source("./Data/DataAggregation1/BaseFunctions.R") #load file of common functions
factors <- read.csv("./Data/DataAggregation1/FactorLevels_aggregated.csv")

#Get data into R ----

##create a dataset of previous surveys ----
oldSurveys <- base.loaddata(
  myYears = c("2002", "2012", "2018"),
  myVenues = c("mail", "email"),
  includeComments = TRUE
)

##create dataset of 2025 survey ----
newSurveys <- read_excel("./Data/DataAggregation1/2025_Anglers_Final.xlsx")
tracking <- read_excel("./Data/DataAggregation1/TrackingForm_Client.xlsx")[, c(
  1,
  5
)]

#initially have to adjust names of new survey fields
names(newSurveys) <- sub("_2025", "", names(newSurveys))
newSurveys$surveyYear <- 2025
# newSurveys$venue <- as.factor(
#   newSurveys %>%
#     left_join(tracking, by = c("ID" = "CustomerID")) %>%
#     pull("Mode of completion (Mail, web, phone)") %>% unique()
# )

newSurveys$venue <- newSurveys %>%
  left_join(tracking, by = c("ID" = "CustomerID")) %>%
  mutate(
    ContactMode = case_when(
      grepl("mail", tolower(`Mode of completion (Mail, web, phone)`)) ~ "2",
      grepl("email", tolower(`Mode of completion (Mail, web, phone)`)) ~ "1",
      grepl("text", tolower(`Mode of completion (Mail, web, phone)`)) ~ "4",
      grepl(
        "web letter 1|web letter 2|postcard",
        tolower(`Mode of completion (Mail, web, phone)`)
      ) ~ "3",
      TRUE ~ NA_character_
    )
  ) %>%
  select(`Mode of completion (Mail, web, phone)`, ContactMode) %>%
  pull(ContactMode)


#standardize all datasets ----
##run corrections on old dataset ----
oldSurveys <- base.data.corrections(
  oldSurveys,
  myYears = c("2002", "2012", "2018")
)


##run corrections on new dataset ----

#replace -1 values with NA
newSurveys <- replace_with_na_all(newSurveys, condition = ~ .x == -1)

###recode A1 (permit types) to match previous surveys and add new factors
newSurveys$A1_recoded <- newSurveys$A1
#res 1-day fish
newSurveys$A1_recoded[newSurveys$A1 == 1] <- -10
#res 3-day fish
newSurveys$A1_recoded[newSurveys$A1 == 2] <- -2
#res 3-year fish
newSurveys$A1_recoded[newSurveys$A1 == 3] <- -17
#res annual fish
newSurveys$A1_recoded[newSurveys$A1 == 4] <- -1
#res 5-year fish
newSurveys$A1_recoded[newSurveys$A1 == 5] <- -18
#res lifetime fish
newSurveys$A1_recoded[newSurveys$A1 == 6] <- -11
#res annual hunt/fish combo
newSurveys$A1_recoded[newSurveys$A1 == 7] <- -3
#res 3-year hunt/fish combo
newSurveys$A1_recoded[newSurveys$A1 == 8] <- -21
#res 5-year hunt/fish combo
newSurveys$A1_recoded[newSurveys$A1 == 9] <- -22
#res lifetime hunt/fish combo
newSurveys$A1_recoded[newSurveys$A1 == 10] <- -23
#nr 1-day fish
newSurveys$A1_recoded[newSurveys$A1 == 11] <- -15
#nr 3-day fish
newSurveys$A1_recoded[newSurveys$A1 == 12] <- -8
#nr annual fish
newSurveys$A1_recoded[newSurveys$A1 == 13] <- -7
#nr  3-year fish
newSurveys$A1_recoded[newSurveys$A1 == 14] <- -19
#nr 5-year fish
newSurveys$A1_recoded[newSurveys$A1 == 15] <- -20
#nr lifetime fish
newSurveys$A1_recoded[newSurveys$A1 == 16] <- -16
#nr annual hunt/fish combo
newSurveys$A1_recoded[newSurveys$A1 == 17] <- -14
#nr 3-year hunt/fish combo
newSurveys$A1_recoded[newSurveys$A1 == 18] <- -24
#nr 5-year hunt/fish combo
newSurveys$A1_recoded[newSurveys$A1 == 19] <- -25
#nr lifetime hunt/fish combo
newSurveys$A1_recoded[newSurveys$A1 == 20] <- -26
#senior hunt/fish combo
newSurveys$A1_recoded[newSurveys$A1 == 21] <- -12
#disabled
newSurveys$A1_recoded[newSurveys$A1 == 22] <- -6
#veteran disabled fee-exempt
newSurveys$A1_recoded[newSurveys$A1 == 23] <- -28
#Veteran annual hunt/fish combo
newSurveys$A1_recoded[newSurveys$A1 == 24] <- -13
#deployed military
newSurveys$A1_recoded[newSurveys$A1 == 25] <- -27
#did not have a permit
newSurveys$A1_recoded[newSurveys$A1 == 26] <- -9
#no answer
newSurveys$A1_recoded[newSurveys$A1 == 999] <- -999
newSurveys$A1 <- newSurveys$A1_recoded
newSurveys <- newSurveys %>% select(-A1_recoded)
newSurveys$A1 <- newSurveys$A1 * -1


###recode A3 (why no fish) to match previous surveys and add new factors
newSurveys$A3_recoded <- newSurveys$A3
#Lack Of time
newSurveys$A3_recoded[newSurveys$A3 == 1] <- -1
#too expensive
newSurveys$A3_recoded[newSurveys$A3 == 2] <- -5
#Dissatisfied with past fishing trips
newSurveys$A3_recoded[newSurveys$A3 == 3] <- -2
#Physically unable to fish
newSurveys$A3_recoded[newSurveys$A3 == 4] <- -6
#No one to fish with
newSurveys$A3_recoded[newSurveys$A3 == 5] <- -3
#No longer enjoy fishing
newSurveys$A3_recoded[newSurveys$A3 == 6] <- -7
#lack of access to fishing areas
newSurveys$A3_recoded[newSurveys$A3 == 7] <- -4
#Complicated regulations
newSurveys$A3_recoded[newSurveys$A3 == 8] <- -9
#Waters are too crowded
newSurveys$A3_recoded[newSurveys$A3 == 9] <- -10
#other
newSurveys$A3_recoded[newSurveys$A3 == 10] <- -8
newSurveys$A1_recoded[newSurveys$A1 == 999] <- -999
newSurveys$A3 <- newSurveys$A3_recoded
newSurveys <- newSurveys %>% select(-A3_recoded)
newSurveys$A3 <- newSurveys$A3 * -1


# #if someone makes did not fish tournament, change number answer to 0 otherwise don't get counted in # that answered
# newSurveys$Q18a[newSurveys$Q18c==1] <- 0
# newSurveys$Q18b[newSurveys$Q18c==1] <- 0
#
#
# #if someone makes did not hire guide, change number answer to 0 otherwise don't get counted in # that answered
# newSurveys$A13[newSurveys$A13a==1] <- 0

###recode B1 (preferred spp) to match previous surveys and add new factors

newSurveys$B1_recoded <- newSurveys$B1
newSurveys$B1_recoded[newSurveys$B1 == 20] <- 19
newSurveys$B1_recoded[newSurveys$B1 == 21] <- 20
newSurveys$B1_recoded[newSurveys$B1 == 23] <- 21
newSurveys$B1_recoded[newSurveys$B1 == 19] <- 23

newSurveys$B1 <- newSurveys$B1_recoded
newSurveys <- newSurveys %>% select(-B1_recoded)


# ###recode C1jan (days fished) to match previous surveys and add new factors
#
# newSurveys$C1jan_recoded <- newSurveys$C1jan + 1
# newSurveys$C1jan_recoded[newSurveys$C1jan == 13] <- 1
#
# newSurveys$C1jan <- newSurveys$C1jan_recoded
# newSurveys <- newSurveys %>% select(-C1jan_recoded)

####data aggregated is raw values...any inversion is in the reporting side
# #invert E1 questions
# newSurveys$E1a <- car::recode(newSurveys$E1a, "1=5; 2=4; 3=3;4=2;5=1")
# newSurveys$E1f <- car::recode(newSurveys$E1f, "1=5; 2=4; 3=3;4=2;5=1")
# newSurveys$E1k <- car::recode(newSurveys$E1k, "1=5; 2=4; 3=3;4=2;5=1")
# newSurveys$E1l <- car::recode(newSurveys$E1l, "1=5; 2=4; 3=3;4=2;5=1")

newSurveys <- newSurveys %>%
  rename(
    Q31a = Q31_1,
    Q31b = Q31_2,
    Q31c = Q31_3,
    Q31d = Q31_4,
    Q31e = Q31_5,
    Q31f = Q31_6,
    Q31g = Q31_7,
    Q31h = Q31_8,
    Q31i = Q31_9,
    Q31j = Q31_10,
    Q31k = Q31_11,
    Q31l = Q31_12
  )

####data aggregated is raw values...any inversion is in the reporting side
# #invert Q31 regulation questions
# newSurveys$Q31a <- car::recode(newSurveys$Q31a, "1=5; 2=4; 3=3;4=2;5=1")
# newSurveys$Q31b <- car::recode(newSurveys$Q31b, "1=5; 2=4; 3=3;4=2;5=1")
# newSurveys$Q31d <- car::recode(newSurveys$Q31d, "1=5; 2=4; 3=3;4=2;5=1")
# newSurveys$Q31f <- car::recode(newSurveys$Q31f, "1=5; 2=4; 3=3;4=2;5=1")
# newSurveys$Q31i <- car::recode(newSurveys$Q31i, "1=5; 2=4; 3=3;4=2;5=1")

#create combined B2 field
newCol <- newSurveys %>%
  select(contains("B2")) %>%
  select(contains("_lrp")) %>%
  names() %>%
  sub("_lrp", "", .)
lrpCol <- newSurveys %>%
  select(contains("B2")) %>%
  select(contains("_lrp")) %>%
  names()
rscCol <- newSurveys %>%
  select(contains("B2")) %>%
  select(contains("_rsc")) %>%
  names()

foreach(i = 1:length(newCol)) %do%
  {
    newSurveys[, newCol[i]] <- ifelse(
      newSurveys[, lrpCol[i]] + newSurveys[, rscCol[i]] > 0,
      1,
      0
    )
  }


#Question 6 to question 5
col.from <- newSurveys %>% select(contains("A6")) %>% names()
col.to <- sub("A6", "A5", col.from)
newSurveys <- newSurveys %>% rename_at(vars(col.from), function(x) col.to)

#Question 7 to question 6
col.from <- newSurveys %>% select(contains("A7")) %>% names()
col.to <- sub("A7", "A6", col.from)
newSurveys <- newSurveys %>% rename_at(vars(col.from), function(x) col.to)


##renumber questions to match previous surveys
newSurveys <- newSurveys %>%
  rename(T10 = A5, T9 = A10, T8 = A9, T7 = A8)

newSurveys <- newSurveys %>%
  rename(A10 = T10, A9 = T9, A8 = T8, A7 = T7) %>%
  rename(A6spr = A6sp) %>%
  rename(A17priv = A14priv) %>%
  rename(A17pub = A14pub) %>%
  rename(D13oga = D4oga) %>%
  rename(D13tr = D4tr) %>%
  rename(D13lrp = D4lrp) %>%
  rename(D13rsc = D4rsc) %>%
  rename(
    C1Jan = C1jan,
    C1Feb = C1feb,
    C1Mar = C1mar,
    C1Apr = C1apr,
    C1May = C1may,
    C1June = C1june,
    C1July = C1july,
    C1Aug = C1aug,
    C1Sept = C1sept,
    C1Oct = C1oct
  )


###correct responses for A17 Pub/Private access if answered no to BOTH questions
newSurveys <- newSurveys %>%
  mutate(
    A17priv = ifelse(
      !is.na(A17priv) & !is.na(A17pub),
      A17priv,
      NA
    ),
    A17pub = ifelse(
      !is.na(A17priv) & !is.na(A17pub),
      A17pub,
      NA
    )
  )

##change 2's to 0's as No was coded to 2 and historically been 0
newSurveys$A17priv[newSurveys$A17priv == 2] <- 0
newSurveys$A17pub[newSurveys$A17pub == 2] <- 0

#in 2025 recode days fished

newSurveys <- newSurveys %>%
  mutate(
    C1Jan = C1Jan + 1,
    C1Feb = C1Feb + 1,
    C1Mar = C1Mar + 1,
    C1Apr = C1Apr + 1,
    C1May = C1May + 1,
    C1June = C1June + 1,
    C1July = C1July + 1,
    C1Aug = C1Aug + 1,
    C1Sept = C1Sept + 1,
    C1Oct = C1Oct + 1
  ) %>%
  mutate(
    C1Jan = if_else(C1Jan == 13, 1, C1Jan),
    C1Feb = if_else(C1Feb == 13, 1, C1Feb),
    C1Mar = if_else(C1Mar == 13, 1, C1Mar),
    C1Apr = if_else(C1Apr == 13, 1, C1Apr),
    C1May = if_else(C1May == 13, 1, C1May),
    C1June = if_else(C1June == 13, 1, C1June),
    C1July = if_else(C1July == 13, 1, C1July),
    C1Aug = if_else(C1Aug == 13, 1, C1Aug),
    C1Sept = if_else(C1Sept == 13, 1, C1Sept),
    C1Oct = if_else(C1Oct == 13, 1, C1Oct)
  )

##flip genders as the codes are inverted in 2025 compared to previous surveys
newSurveys$E2 <- car::recode(newSurveys$E2, "1=2; 2=1")


###add post weights

newSurveys <- read.csv("2025_raking_weights.csv") %>%
  mutate(ID = CustomerID) %>%
  select(CustomerID, rake_weight) %>%
  left_join(newSurveys %>% mutate(CustomerID = ID), by = c("CustomerID")) %>%
  rename(postWeight = rake_weight)

###add ages

newSurveys <- newSurveys %>%
  mutate(
    E3 = cut(
      Age,
      breaks = c(15, 24, 34, 44, 54, 64, Inf),
      labels = 1:6,
      right = TRUE
    ) |>
      as.numeric()
  )

#combine datasets ----
d <- newSurveys %>%
  bind_rows(oldSurveys)


##create aggregate variables ----

#process datasets

d <- base.create.aggregate.variables(d, myYears = c(2002, 2012, 2018, 2025))
d <- base.data.createFactors(d)


## Harmonize reverse-keyed attitude items across survey eras --------------
# The reverse-coded E1 items are stored in OPPOSITE polarity in the 2002/2012
# historical data (already pre-reversed) versus the 2018 BOSR import and the
# 2025 survey (raw orientation). add_scale_scores() applies one uniform
# reversal from Scales.csv, which is correct for 2018/2025 but double-flips
# 2002/2012, inflating attitude_catch / attitude_harvest for those years.
# Flip the 2002/2012 rows of these items onto the 2018/2025 raw orientation so
# the uniform reversal is correct for every year. Verified to reproduce the
# published 2018 trend report subscales (Catch 2.9/2.8/2.8, Harvest 2.6/2.4/2.4).
e1_reverse_items <- c("E1a", "E1k", "E1f", "E1l")
e1_flip_rows <- d$surveyYear %in% c(2002, 2012)
for (it in e1_reverse_items) {
  f <- d[[it]]
  codes <- as.integer(f)
  codes[e1_flip_rows] <- (nlevels(f) + 1L) - codes[e1_flip_rows]
  d[[it]] <- factor(levels(f)[codes], levels = levels(f))
}


save(d, file = "aggregateData_20260624.rData")

# # Tag reverse-coded items in FactorLevels_aggregated.csv so the column
# # survives every reaggregation run.
# reversed_fields <- c(
#   "E1a",
#   "E1f",
#   "E1k",
#   "E1l",
#   "Q31a",
#   "Q31b",
#   "Q31d",
#   "Q31f",
#   "Q31i"
# )
# fl_path <- "./Data/DataAggregation1/FactorLevels_aggregated.csv"
# fl <- read.csv(fl_path) %>%
#   mutate(Reversed = Field %in% reversed_fields)
# write.csv(fl, fl_path, row.names = FALSE)
