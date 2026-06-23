##This script takes a data dump from BOSR and converts it to match the master dataset from 2002 and 2012
options(stringsAsFactors = FALSE)

library(haven)
library(naniar)
library(car)
library(dplyr)
library(foreach)

base.getBosrData <- function() {
  #set location of spss data
  #bosrFile <- "..\\..\\Data\\Anglers_prelim 2-8-19_final.sav"
  #bosrFile <- "..\\..\\Data\\Anglers_all merged_2-22.sav"
  bosrFile <- "./Data/DataAggregation1/Anglers_final.sav"

  #read spss
  bData = read_sav(bosrFile)

  #create data.frame to use with modifications
  new_bData <- data.frame(bData)

  #change NA values to -1
  new_bData[, c(18:98, 101:270)] <- sapply(
    new_bData[, c(18:98, 101:270)],
    function(x) ifelse(is.na(x), -1, x)
  )

  #convert to basic numeric columns
  new_bData[, c(18:98, 101:270)] <- sapply(
    new_bData[, c(18:98, 101:270)],
    function(x) as.numeric(x)
  )

  #label venue
  new_bData <- new_bData %>%
    mutate(venue = ifelse(DistributionChannel == "email", "email", "mail"))

  #label year
  new_bData <- new_bData %>% mutate(surveyYear = 2018)

  #recode field labels to match previous surveys
  #Question 6 to question 5
  col.from <- new_bData %>% select(contains("A6")) %>% names()
  col.to <- sub("A6", "A5", col.from)
  new_bData <- new_bData %>% rename_at(vars(col.from), function(x) col.to)

  #Question 7 to question 6
  col.from <- new_bData %>% select(contains("A7")) %>% names()
  col.to <- sub("A7", "A6", col.from)
  new_bData <- new_bData %>% rename_at(vars(col.from), function(x) col.to)

  #Question D1 to question D12
  col.from <- new_bData %>% select(contains("D1")) %>% names()
  col.to <- sub("D1", "D12", col.from)
  new_bData <- new_bData %>% rename_at(vars(col.from), function(x) col.to)

  new_bData <- new_bData %>%
    rename(A7 = A8) %>%
    rename(A8 = A9) %>%
    rename(A9 = A10) %>%
    rename(A10 = A5) %>%
    rename(A6spr = A6sp) %>%
    rename(A17priv = A14priv) %>%
    rename(A17pub = A14pub) %>%
    rename(D13oga = D4oga) %>%
    rename(D13tr = D4tr) %>%
    rename(D13lrp = D4lrp) %>%
    rename(D13rsc = D4rsc) %>%
    rename(D14a = D12a) %>%
    rename(D14b = D12b) %>%
    rename(D14c = D12c) %>%
    rename(D14d = D12d) %>%
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

  ####data aggregated is raw values...any inversion is in the reporting side
    # #invert E1 questions
  # new_bData$E1a <- car::recode(new_bData$E1a, "1=5; 2=4; 3=3;4=2;5=1")
  # new_bData$E1f <- car::recode(new_bData$E1f, "1=5; 2=4; 3=3;4=2;5=1")
  # new_bData$E1k <- car::recode(new_bData$E1k, "1=5; 2=4; 3=3;4=2;5=1")
  # new_bData$E1l <- car::recode(new_bData$E1l, "1=5; 2=4; 3=3;4=2;5=1")

  #remove unnecessary fields
  new_bData <- new_bData %>%
    select(18:270, venue, surveyYear, licenseUID = ID, OwnerCustomerUID = UID) #UID)

  #create combined B2 field
  newCol <- new_bData %>%
    select(contains("B2")) %>%
    select(contains("_lrp")) %>%
    names() %>%
    sub("_lrp", "", .)
  lrpCol <- new_bData %>%
    select(contains("B2")) %>%
    select(contains("_lrp")) %>%
    names()
  rscCol <- new_bData %>%
    select(contains("B2")) %>%
    select(contains("_rsc")) %>%
    names()

  foreach(i = 1:length(newCol)) %do%
    {
      new_bData[, newCol[i]] <- ifelse(
        new_bData[, lrpCol[i]] + new_bData[, rscCol[i]] > 0,
        1,
        0
      )
    }

  new_bData <- base.AddAgeGroupFor2018(new_bData)

  #return data
  return(new_bData)
}


#uses E3 response first, then grabs permit data for missing answers...needed for raking
base.AddAgeGroupFor2018 <- function(myD) {
  load(file = "./Data/DataAggregation1/AgeDistributions_2018.rdata")

  #fill in unanswered E3 with database derived age group
  #must use unique on myPermits as there are multiples of the same ID

  myPermits2 <- myPermits2 %>%
    group_by(BosrId) %>%
    slice(1)

  d <- myD %>%
    left_join(
      myPermits2[, c("BosrId", "Age_cat")],
      by = c("licenseUID" = "BosrId")
    ) %>%
    mutate(
      age_group = ifelse(
        is.na(E3) | E3 == -1,
        as.numeric(Age_cat),
        as.character(E3)
      )
    )

  return(d)
}


base.getLatLongForSample <- function() {
  #add lat/long of home address
  mySf <- read_sf("../../Data/FullDocGeocode/Full_DocLocations.shp") %>%
    select(BosrId = USER_BosrI, lat = FinalY, long = FinalX) %>%
    st_set_crs(4269)
}
#myD<-base.loaddata(myYears=c(2012,2018), myVenues=c("mail", "email"))
#bind_rows(head(myD[myD$surveyYear==2012,]), head(myD[myD$surveyYear==2018,])) %>% View()
