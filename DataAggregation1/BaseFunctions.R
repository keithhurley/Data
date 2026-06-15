options(stringsAsFactors = FALSE, tigris_use_cache = TRUE)

library(foreign)
library(dplyr)
library(tidyr)
library(car) #to use for recode function
library(naniar) #use to replace na values with replace_with_na function
library(asbio) #median confidance intervals
library(tigris) #urban variable
library(USAboundaries) #urban variable
library(sf)
#library(zipcode)
#library(SDMTools) #weighted means and sd
library(survey)
library(zipcodeR)

source("./Data/DataAggregation1/ImportBosrData.R")

# Create functions for confidence intervals ---------------------------------------
base.ci.CI <- function(x) {
  return(1.96 * (sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))))
}

base.ci.GetMedianUpperCI.old <- function(x) {
  bootmed = apply(
    matrix(sample(x, rep = TRUE, 10^4 * length(x)), nrow = 10^4),
    1,
    median
  )
  ci.upper <- quantile(bootmed, c(.025, 0.975))[1]
  return(ci.upper)
  rm("ci")
}

base.ci.GetMedianLowerCI.old <- function(x) {
  bootmed = apply(
    matrix(sample(x, rep = TRUE, 10^4 * length(x)), nrow = 10^4),
    1,
    median
  )
  ci.lower <- quantile(bootmed, c(.025, 0.975))[2]
  return(ci.lower)
  rm("ci")
}

#' Function copied from **spatstat** package.
#'
#' @param x Vector of values
#' @param w Vector of weights
#' @param probs Vector of probabilities
#' @param na.rm Ignore missing data?
#' @export
weighted.quantile <- function(x, w, probs = seq(0, 1, 0.25), na.rm = TRUE) {
  x <- as.numeric(as.vector(x))
  w <- as.numeric(as.vector(w))
  if (anyNA(x) || anyNA(w)) {
    ok <- !(is.na(x) | is.na(w))
    x <- x[ok]
    w <- w[ok]
  }
  stopifnot(all(w >= 0))
  if (all(w == 0)) {
    stop("All weights are zero", call. = FALSE)
  }
  #'
  oo <- order(x)
  x <- x[oo]
  w <- w[oo]
  Fx <- cumsum(w) / sum(w)
  #'
  result <- numeric(length(probs))
  for (i in seq_along(result)) {
    p <- probs[i]
    lefties <- which(Fx <= p)
    if (length(lefties) == 0) {
      result[i] <- x[1]
    } else {
      left <- max(lefties)
      result[i] <- x[left]
      if (Fx[left] < p && left < length(x)) {
        right <- left + 1
        y <- x[left] +
          (x[right] - x[left]) * (p - Fx[left]) / (Fx[right] - Fx[left])
        if (is.finite(y)) result[i] <- y
      }
    }
  }
  names(result) <- paste0(format(100 * probs, trim = TRUE), "%")
  return(result)
}

# Create Functions To Load Data -------------------------------------------
####venues: 1=2012 mail, 2=2012 web, 3=2002 mail
####reversed scales in E1 questions (attitudes) have already been inverted
base.loaddata <- function(myYears, myVenues, includeComments = FALSE) {
  mydata <- read.csv('./Data/DataAggregation1/surveyData_20180116.csv')

  if (2018 %in% myYears) {
    op <- base.getBosrData()
    mydata <- bind_rows(op, mydata)
  }

  mydata <- mydata %>%
    filter(surveyYear %in% myYears) %>%
    filter(venue %in% myVenues)

  # myAngData <- read.csv(file="Anglers.csv") %>%
  #   select(ID, "wgs84_X"=X, "wgs84_Y"=Y) %>%
  #   right_join(mydata, by=c("ID"="licenseUID"))

  if (includeComments == FALSE) {
    mydata <- mydata %>% dplyr::select(-F2, -F3)
  }

  #mydata<-base.data.corrections(mydata, myYears)
  #mydata<-base.create.aggregate.variables(mydata, myYears)
  #mydata<-base.data.createFactors(mydata)

  load("./Data/DataAggregation1/weights_raked_ageGroup_20190705.rData")

  mydata <- mydata %>%
    left_join(postWeights, by = c("licenseUID")) %>%
    mutate(postWeight = tidyr::replace_na(postWeight, 1))

  mydata
}

#function to rake weight values for survey
#base.summary.rake(d, list(~E3), list(ageDist))
base.summary.rake <- function(myData, myRakeVars, myPopMargins) {
  num <- nrow(myData)

  for (i in 1:length(myPopMargins)) {
    myPopMargins[[i]]$Freq <- myPopMargins[[i]]$Freq * num
  }

  require(survey)
  suppressWarnings(mySurveyObject <- svydesign(ids = ~1, data = myData))

  t_rake <- rake(
    design = mySurveyObject,
    sample.margins = myRakeVars,
    population.margins = myPopMargins
  )

  t_rake <- trimWeights(t_rake, lower = 0.5, upper = 3, strict = TRUE)

  myData <- cbind(myData, postWeight = weights(t_rake))
  return(myData)
}

base.summary.rake.loop <- function(myData, myRakeVars, myPopDists) {
  require(foreach)

  myRakeYears <- unique(myRakeVars$surveyYear)
  myYears <- unique(myData$surveyYear)

  #loop through each year
  #if year in rakeVars...rake the data...otherwise pass it through
  newdata <- foreach(i = myYears, .combine = "rbind") %do%
    {
      tempData <- myData %>%
        filter(surveyYear == i)

      if (i %in% myRakeYears) {
        #get rakeVars for current year
        tempRakeVars <- myRakeVars %>%
          filter(surveyYear == i)

        #create formula for rakevars
        tempRakeVars <- list(noquote(paste(
          "~",
          paste(tempRakeVars$rakeVar, collapse = " + "),
          sep = ""
        )))

        #list(parse(text=tempRakeVars))
        #parse(tempRakeVars)

        #create list of popDistributions
        tmpPopDists <- list()
        tempPopDists <- foreach(
          intX = 1:nrow(myRakeVars),
          .combine = "list"
        ) %do%
          {
            if (myRakeVars$surveyYear == i) {
              if (length(tmpPopDists) < 1) {
                tmpPopDists <- list(data.frame(myPopDists[[intX]]))
              } else {
                tmpPopDists <- list(tmpPopDists, data.frame(myPopDists[[intX]]))
              }
            }
          }

        #return(base.summary.rake(tempData, tempRakeVars, tempPopDists))
        return(base.summary.rake(tempData, list(~age_group), tempPopDists))
      } else {
        tempData$postWeight <- 1
        return(tempData)
      }
    }

  #return full dataset
  return(newdata)
}

base.cancelRake <- function() {
  rakeVars_bkup <<- rakeVars

  rakeVars <<- data.frame(surveyYear = c(0001), rakeVar = c("age_group"))
}


base.restoreRake <- function() {
  rakeVars <<- rakeVars_bkup
}

base.loaddata.factorlevels <- function() {
  factorData <- read.csv(
    file = "./Data/DataAggregation1/FactorLevels_aggregated.csv",
    header = TRUE
  )
  return(factorData)
}

# Create aggreagate variables and data corrections ------------------------
base.data.corrections <- function(mydata, myYears) {
  #E1 ....7 is not an available answer...convert to missing
  mydata$E1a[mydata$E1a == 7] <- -1
  mydata$E1f[mydata$E1f == 7] <- -1
  mydata$E1k[mydata$E1k == 7] <- -1
  mydata$E1l[mydata$E1l == 7] <- -1

  #remove 6's from B3
  tmp <- match("B3stb", names(mydata))
  tmp2 <- tmp + 22
  for (i in tmp:tmp2) {
    if (sum(!is.na(mydata[i])) > 0) {
      mydata[mydata[i] == 6 & !is.na(mydata[i]), i] <- -1
    }
  }

  if (any(myYears %in% c(2018))) {
    #correct responses for A17 Pub/Private access if answered no to BOTH questions
    mydata <- mydata %>%
      #filter out if someone answered both as no or only one answer
      mutate(
        A17priv = ifelse(
          (!is.na(A17priv) & !is.na(A17pub)) & (A17priv == 1 | A17pub == 1),
          A17priv,
          NA
        ),
        A17pub = ifelse(
          (!is.na(A17priv) & !is.na(A17pub)) & (A17priv == 1 | A17pub == 1),
          A17pub,
          NA
        )
      )
  }

  #replace -1 values with NA
  mydata <- replace_with_na_all(mydata, condition = ~ .x == -1)

  #return corrected data
  return(mydata)
}

base.create.aggregate.variables <- function(mydata, myYears) {
  ###################################################
  #Did They Answer Question Variables
  ###################################################
  #these variables indicated answered at least once in the list
  mydata <- mydata %>%
    mutate(
      A4_Answered = ifelse(
        rowSums(
          .[mydata %>% select(contains("A4")) %>% names()],
          na.rm = TRUE
        ) >
          0,
        TRUE,
        FALSE
      )
    )
  mydata <- mydata %>%
    mutate(
      A5_Answered = ifelse(
        rowSums(
          .[mydata %>% select(contains("A5")) %>% names()],
          na.rm = TRUE
        ) >
          0,
        TRUE,
        FALSE
      )
    )
  mydata <- mydata %>%
    mutate(
      A6_Answered = ifelse(
        rowSums(
          .[mydata %>% select(contains("A6")) %>% names()],
          na.rm = TRUE
        ) >
          0,
        TRUE,
        FALSE
      )
    )
  mydata <- mydata %>%
    mutate(
      B2_Answered = ifelse(
        rowSums(
          .[mydata %>% select(contains("B2")) %>% names()],
          na.rm = TRUE
        ) >
          0,
        TRUE,
        FALSE
      )
    )
  mydata <- mydata %>%
    mutate(
      F1_Answered = ifelse(
        rowSums(
          .[mydata %>% select(contains("F1")) %>% names()],
          na.rm = TRUE
        ) >
          0,
        TRUE,
        FALSE
      )
    )
  if (any(myYears %in% c(2018))) {
    mydata <- mydata %>%
      mutate(
        D14_Answered = ifelse(
          rowSums(
            .[mydata %>% select(contains("D14")) %>% names()],
            na.rm = TRUE
          ) >
            0,
          TRUE,
          FALSE
        )
      )
  }
  #these variables indicated answered all in the list
  if (any(myYears %in% c(2018))) {
    mydata <- mydata %>%
      mutate(
        D3_AnsweredAll = ifelse(
          rowSums(
            sapply(
              .[mydata %>% select(contains("D3")) %>% names()],
              function(x) !is.na(x)
            ),
            na.rm = TRUE
          ) ==
            20,
          TRUE,
          FALSE
        )
      )
  }
  #these variables indicated answered all in the list
  if (any(myYears %in% c(2025))) {
    mydata <- mydata %>%
      mutate(
        Q31_AnsweredAll = if_else(
          surveyYear == 2025,
          ifelse(
            rowSums(
              sapply(
                .[mydata %>% select(contains("Q31")) %>% names()],
                function(x) !is.na(x)
              ),
              na.rm = TRUE
            ) ==
              12,
            TRUE,
            FALSE
          ),
          NA
        )
      )
  }
  ###################################################
  ####Residency Variable
  ###################################################
  mydata$Resi <- car::recode(
    mydata$A1,
    "7=2; 8=2; 14=2;15=2;16=2;19=2; 20=2;24=2; 25=2; 26=2; 27=2; -1=-1;NA=NA; else=1"
  )
  mydata$Resi <- factor(
    mydata$Resi,
    levels = c("1", "2"),
    labels = c("Resident", "Non-Resident")
  )

  ###################################################
  ####Attitude scales
  ###################################################
  ########Catch Something
  myFields <- c("E1a", "E1k", "E1g", "E1p")
  mydata <- mydata %>%
    mutate(attitude_catch_AnsweredAll = rowSums(is.na(.[myFields])) == 0) %>%
    mutate(
      attitude_catch = ifelse(
        attitude_catch_AnsweredAll == TRUE,
        rowMeans(.[myFields], na.rm = TRUE),
        NA
      )
    )
  ########Catch Numbers
  myFields <- c("E1e", "E1n", "E1o", "E1c")
  mydata <- mydata %>%
    mutate(attitude_numbers_AnsweredAll = rowSums(is.na(.[myFields])) == 0) %>%
    mutate(
      attitude_numbers = ifelse(
        attitude_numbers_AnsweredAll == TRUE,
        rowMeans(.[myFields], na.rm = TRUE),
        NA
      )
    )
  ########Catching Large Fish
  myFields <- c("E1j", "E1b", "E1h", "E1m")
  mydata <- mydata %>%
    mutate(attitude_size_AnsweredAll = rowSums(is.na(.[myFields])) == 0) %>%
    mutate(
      attitude_size = ifelse(
        attitude_size_AnsweredAll == TRUE,
        rowMeans(.[myFields], na.rm = TRUE),
        NA
      )
    )
  ########Harvesting Fish
  myFields <- c("E1i", "E1l", "E1d", "E1f")
  mydata <- mydata %>%
    mutate(attitude_harvest_AnsweredAll = rowSums(is.na(.[myFields])) == 0) %>%
    mutate(
      attitude_harvest = ifelse(
        attitude_harvest_AnsweredAll == TRUE,
        rowMeans(.[myFields], na.rm = TRUE),
        NA
      )
    )
  ########Entire Scale
  mydata$E1_AnsweredAll <- rowSums(
    mydata %>%
      select(
        "attitude_catch_AnsweredAll",
        "attitude_numbers_AnsweredAll",
        "attitude_size_AnsweredAll",
        "attitude_harvest_AnsweredAll"
      )
  ) ==
    4

  #############################################
  #Motivation Scales
  #############################################
  ########NonCatch
  myFields <- c(
    "C2a",
    "C2c",
    "C2f",
    "C2g",
    "C2h",
    "C2j",
    "C2l",
    "C2m",
    "C2o",
    "C2p"
  )
  mydata <- mydata %>%
    mutate(
      motivation_noncatch_AnsweredAll = rowSums(is.na(.[myFields])) == 0
    ) %>%
    mutate(
      motivation_noncatch = ifelse(
        motivation_noncatch_AnsweredAll == TRUE,
        rowMeans(.[myFields], na.rm = TRUE),
        NA
      )
    )
  ########NonCatch
  myFields <- c("C2b", "C2d", "C2e", "C2i", "C2k", "C2n", "C2q")
  mydata <- mydata %>%
    mutate(motivation_catch_AnsweredAll = rowSums(is.na(.[myFields])) == 0) %>%
    mutate(
      motivation_catch = ifelse(
        motivation_catch_AnsweredAll == TRUE,
        rowMeans(.[myFields], na.rm = TRUE),
        NA
      )
    )
  ########Psychological and Physiological
  myFields <- c("C2e", "C2f", "C2j", "C2m")
  mydata <- mydata %>%
    mutate(motivation_pp_AnsweredAll = rowSums(is.na(.[myFields])) == 0) %>%
    mutate(
      motivation_pp = ifelse(
        motivation_pp_AnsweredAll == TRUE,
        rowMeans(.[myFields], na.rm = TRUE),
        NA
      )
    )
  ########Natural Environment
  myFields <- c("C2a", "C2c", "C2g", "C2l")
  mydata <- mydata %>%
    mutate(
      motivation_natural_AnsweredAll = rowSums(is.na(.[myFields])) == 0
    ) %>%
    mutate(
      motivation_natural = ifelse(
        motivation_natural_AnsweredAll == TRUE,
        rowMeans(.[myFields], na.rm = TRUE),
        NA
      )
    )
  ########Social
  myFields <- c("C2d", "C2h", "C2o", "C2p")
  mydata <- mydata %>%
    mutate(motivation_social_AnsweredAll = rowSums(is.na(.[myFields])) == 0) %>%
    mutate(
      motivation_social = ifelse(
        motivation_social_AnsweredAll == TRUE,
        rowMeans(.[myFields], na.rm = TRUE),
        NA
      )
    )
  ########Fishery Resource
  myFields <- c("C2i", "C2k", "C2n", "C2q", "C2b")
  mydata <- mydata %>%
    mutate(
      motivation_resource_AnsweredAll = rowSums(is.na(.[myFields])) == 0
    ) %>%
    mutate(
      motivation_resource = ifelse(
        motivation_resource_AnsweredAll == TRUE,
        rowMeans(.[myFields], na.rm = TRUE),
        NA
      )
    )
  ########Entire Scale
  mydata$C2_AnsweredAll <- rowSums(
    mydata %>%
      select(
        "motivation_pp_AnsweredAll",
        "motivation_natural_AnsweredAll",
        "motivation_social_AnsweredAll",
        "motivation_resource_AnsweredAll"
      )
  ) ==
    4

  ###################################################
  ####Program scales
  ###################################################
  if (any(myYears %in% c(2002, 2012))) {
    ########Game Fish
    myFields <- c("D1i", "D1l", "D1s")
    mydata <- mydata %>%
      mutate(programs_game_AnsweredAll = rowSums(is.na(.[myFields])) == 0) %>%
      mutate(
        programs_game = ifelse(
          programs_game_AnsweredAll == TRUE,
          rowMeans(.[myFields], na.rm = TRUE),
          NA
        )
      )
    ########Habitat and Access
    myFields <- c("D1b", "D1c", "D1h", "D1k", "D1p", "D1t")
    mydata <- mydata %>%
      mutate(
        programs_habitat_AnsweredAll = rowSums(is.na(.[myFields])) == 0
      ) %>%
      mutate(
        programs_habitat = ifelse(
          programs_habitat_AnsweredAll == TRUE,
          rowMeans(.[myFields], na.rm = TRUE),
          NA
        )
      )
    ########Evironmental Services
    myFields <- c("D1a", "D1d", "D1j", "D1q")
    mydata <- mydata %>%
      mutate(
        programs_environment_AnsweredAll = rowSums(is.na(.[myFields])) == 0
      ) %>%
      mutate(
        programs_environment = ifelse(
          programs_environment_AnsweredAll == TRUE,
          rowMeans(.[myFields], na.rm = TRUE),
          NA
        )
      )
    ########Outreach
    myFields <- c("D1e", "D1f", "D1g", "D1m", "D1n", "D1o", "D1r", "D1u")
    mydata <- mydata %>%
      mutate(
        programs_outreach_AnsweredAll = rowSums(is.na(.[myFields])) == 0
      ) %>%
      mutate(
        programs_outreach = ifelse(
          programs_outreach_AnsweredAll == TRUE,
          rowMeans(.[myFields], na.rm = TRUE),
          NA
        )
      )
    ########Entire Scale
    mydata$D1_AnsweredAll <- rowSums(
      mydata %>%
        select(
          "programs_game_AnsweredAll",
          "programs_habitat_AnsweredAll",
          "programs_environment_AnsweredAll",
          "programs_outreach_AnsweredAll"
        )
    ) ==
      4
  }
  ###################################################
  ####Regulation scales
  ###################################################
  ########Regulation Complexity/Confusion
  myFields <- c("Q31a", "Q31e", "Q31g", "Q31h")
  mydata <- mydata %>%
    mutate(
      regulations_complexity_AnsweredAll = rowSums(is.na(.[myFields])) == 0
    ) %>%
    mutate(
      regulations_complexity = ifelse(
        regulations_complexity_AnsweredAll == TRUE,
        rowMeans(.[myFields], na.rm = TRUE),
        NA
      )
    )
  ########Importance Of Management Objectives
  myFields <- c("Q31f", "Q31i", "Q31k", "Q31l")
  mydata <- mydata %>%
    mutate(
      regulations_objectives_AnsweredAll = rowSums(is.na(.[myFields])) == 0
    ) %>%
    mutate(
      regulations_objectives = ifelse(
        regulations_objectives_AnsweredAll == TRUE,
        rowMeans(.[myFields], na.rm = TRUE),
        NA
      )
    )
  ########Trust/Legitimacy
  myFields <- c("E1b", "E1c", "E1d", "E1j")
  mydata <- mydata %>%
    mutate(regulations_trust_AnsweredAll = rowSums(is.na(.[myFields])) == 0) %>%
    mutate(
      regulations_trust = ifelse(
        regulations_trust_AnsweredAll == TRUE,
        rowMeans(.[myFields], na.rm = TRUE),
        NA
      )
    )
  ########Entire Scale
  mydata$Q31_AnsweredAll <- rowSums(
    mydata %>%
      select(
        "regulations_complexity_AnsweredAll",
        "regulations_objectives_AnsweredAll",
        "regulations_trust_AnsweredAll"
      )
  ) == 3
  
  #############################################
  ####Fishing Frequency
  #############################################
  mydata$C1_AnsweredAll <- mydata$C1Jan != -1 |
    mydata$C1Feb != -1 |
    mydata$C1Mar != -1 |
    mydata$C1Apr != -1 |
    mydata$C1May != -1 |
    mydata$C1June != -1 |
    mydata$C1July != -1 |
    mydata$C1Aug != -1 |
    mydata$C1Sept != -1 |
    mydata$C1Oct != -1
  C1RecodeValues <- "-1=NA; 1 =0; 2=1; 3=2; 4=3; 5=4; 6=5; 7=7; 8=10; 9=13; 10=16; 11=19; 12=21"
  mydata$C1Jan_days <- car::recode(mydata$C1Jan, C1RecodeValues)
  mydata$C1Feb_days <- car::recode(mydata$C1Feb, C1RecodeValues)
  mydata$C1Mar_days <- car::recode(mydata$C1Mar, C1RecodeValues)
  mydata$C1Apr_days <- car::recode(mydata$C1Apr, C1RecodeValues)
  mydata$C1May_days <- car::recode(mydata$C1May, C1RecodeValues)
  mydata$C1June_days <- car::recode(mydata$C1June, C1RecodeValues)
  mydata$C1July_days <- car::recode(mydata$C1July, C1RecodeValues)
  mydata$C1Aug_days <- car::recode(mydata$C1Aug, C1RecodeValues)
  mydata$C1Sept_days <- car::recode(mydata$C1Sept, C1RecodeValues)
  mydata$C1Oct_days <- car::recode(mydata$C1Oct, C1RecodeValues)
  mydata$C1Total_days <- ifelse(
    is.na(mydata$C1Jan_days),
    0,
    mydata$C1Jan_days
  ) +
    ifelse(is.na(mydata$C1Feb_days), 0, mydata$C1Feb_days) +
    ifelse(is.na(mydata$C1Mar_days), 0, mydata$C1Mar_days) +
    ifelse(is.na(mydata$C1Apr_days), 0, mydata$C1Apr_days) +
    ifelse(is.na(mydata$C1May_days), 0, mydata$C1May_days) +
    ifelse(is.na(mydata$C1June_days), 0, mydata$C1June_days) +
    ifelse(is.na(mydata$C1July_days), 0, mydata$C1July_days) +
    ifelse(is.na(mydata$C1Aug_days), 0, mydata$C1Aug_days) +
    ifelse(is.na(mydata$C1Sept_days), 0, mydata$C1Sept_days) +
    ifelse(is.na(mydata$C1Oct_days), 0, mydata$C1Oct_days)

  mydata$C1Total_days[!(mydata$C1_AnsweredAll)] <- NA

  rm("C1RecodeValues")

  A7RecodeValues <- "-1=NA; 1 =5; 2=15.5; 3=30.5; 4=50.5; 5=80.5; 6=175.5; 7=375.5; 8=500"
  mydata$A7_miles <- car::recode(mydata$A7, A7RecodeValues)
  mydata$A8_miles <- car::recode(mydata$A8, A7RecodeValues)
  rm("A7RecodeValues")

  #Generalized Angler Types
  #recoded into Bass=1, Catfish=2, Sunfish=3,YPerch=5 Walleye/Sauger=4/5, Moronides=5/6, Esocids=6/7, Trout=7/8, Uniques=8/9, anything=9/10
  mydata$gtype <- car::recode(
    mydata$B1,
    "c(4,5)=1; c(12,13,14,15)=2; c(6,7,8)=3; 9=4; c(1,2,3)=5; c(10,11)=6; 19=7; c(16,17,18,20, 23)=8; 21=9;else=NA"
  )
  mydata$gtype2 <- car::recode(
    mydata$B1,
    "c(4,5)=1; c(12,13,14,15)=2; c(6,7)=3; 8=4; 9=5; c(1,2,3)=6; c(10,11)=7; 19=8; c(16,17,18,20, 23)=9; 21=10;else=NA"
  )
  mydata$gtype <- factor(
    mydata$gtype,
    levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    labels = c(
      "Bass",
      "Catfish",
      "Sunfish",
      "Walleye-Sauger",
      "Moronides",
      "Esocids",
      "Trout",
      "Uniques",
      "Anything"
    )
  )
  mydata$gtype2 <- factor(
    mydata$gtype2,
    levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    labels = c(
      "Bass",
      "Catfish",
      "Sunfish",
      "Yellow Perch",
      "Walleye-Sauger",
      "Moronides",
      "Esocids",
      "Trout",
      "Uniques",
      "Anything"
    )
  )

  ###################################################
  ####Avidity
  ###################################################
  #groups include:
  # Sporadic=1-2 (per year)
  # Occasional=2-10 (1 per four weeks)
  # Average=11-22 (1 per two weeks)
  # Avid=23-44 (1 per week)
  # Fanatic=44+ (more than 1 per week)

  mydata <- mydata %>%
    mutate(
      avidity = cut(
        C1Total_days,
        breaks = c(0, 2, 10, 22, 43, Inf),
        labels = c("Sporadic", "Occasional", "Average", "Avid", "Fanatic")
      )
    )

  ###################################################
  ####Urban
  ###################################################
  sf_zctas <- zctas(year = 2010, state = "NE") %>%
    st_as_sf() %>%
    st_transform(4326)

  sf_nebr <- us_states(resolution = "high", states = "Nebraska") %>%
    st_transform(4326)
  sf_counties <- us_counties(resolution = "high", states = "Nebraska") %>%
    st_transform(4326)
  sf_urbanZips <- zctas(year = 2010, state = "NE") %>%
    st_as_sf() %>%
    st_transform(4326)

  myUrbanCities = c(
    "Omaha",
    "Lincoln",
    "Bellevue",
    "Grand Island",
    "Kearney",
    "Fremont",
    "Hastings",
    "Norfolk",
    "North Platte",
    "Columbus",
    "Papillion",
    "La Vista",
    "Scottsbluff",
    "South Sioux City",
    "Beatrice",
    "Lexington",
    "Alliance",
    "Offutt A F B",
    "Elkhorn"
  )

  data("zip_code_db")
  myUrbanZips <- zip_code_db %>%
    filter(
      state == "NE",
      major_city %in% myUrbanCities
    ) %>%
    pull(zipcode) %>%
    unique()

  sf_urbanZips <- sf_zctas %>%
    mutate(zc = as.character(ZCTA5CE10)) %>%
    filter(zc %in% myUrbanZips)

  #save(myUrbanZips, sf_urbanZips, file="sf_urbanZips.rData")

  myOmahaCities = c(
    "Omaha",
    "Bellevue",
    "Papillion",
    "La Vista",
    "Offutt A F B",
    "Elkhorn"
  )

  myOmahaZips <- zip_code_db %>%
    filter(
      state == "NE",
      major_city %in% myOmahaCities
    ) %>%
    pull(zipcode) %>%
    unique()

  sf_OmahaZips <- sf_urbanZips %>%
    filter(zc %in% myOmahaZips)

  #save(myOmahaZips, sf_OmahaZips, file="sf_OmahaZips.rData")

  mydata <- mydata %>%
    mutate(
      E8 = as.character(E8),
      urban = E8 %in% sf_urbanZips$zc
    )

  ###################################################
  ####Tournaments
  ###################################################
  if (any(myYears %in% c(2018))) {
    #correct number of tournament answer
    mydata <- mydata %>%
      mutate(A13_corrected = ifelse(is.na(A13a), A13, 0))
    #create boolean for tournament participation
    mydata <- mydata %>%
      mutate(fishedTourney = ifelse(A13_corrected > 0, TRUE, NA)) %>%
      mutate(fishedTourney = ifelse(A13_corrected == 0, FALSE, fishedTourney))
  }

  ###################################################
  ####Setlines - MO
  ###################################################
  if (any(myYears %in% c(2018))) {
    #correct number of tournament answer
    mydata <- mydata %>%
      mutate(A16_corrected = ifelse(is.na(A16a), A16, 0))
    #create boolean for tournament participation
    mydata <- mydata %>%
      mutate(fishedSetlinesMo = ifelse(A16_corrected > 0, TRUE, NA)) %>%
      mutate(
        fishedSetlinesMo = ifelse(A16_corrected == 0, FALSE, fishedSetlinesMo)
      )
  }

  ###################################################
  ####Setlines - rsc
  ###################################################
  if (any(myYears %in% c(2018))) {
    #correct number of tournament answer
    mydata <- mydata %>%
      mutate(A15_corrected = ifelse(is.na(A15a), A15, 0))
    #create boolean for tournament participation
    mydata <- mydata %>%
      mutate(fishedSetlinesOther = ifelse(A15_corrected > 0, TRUE, NA)) %>%
      mutate(
        fishedSetlinesOther = ifelse(
          A15_corrected == 0,
          FALSE,
          fishedSetlinesOther
        )
      )
  }

  ###################################################
  ####Days Fished - MO River
  ###################################################
  if (any(myYears %in% c(2018))) {
    #correct number of tournament answer
    mydata <- mydata %>%
      mutate(C1_mo_corrected = ifelse(is.na(C1_moa), C1_mo, 0))
    #create boolean for tournament participation
    mydata <- mydata %>%
      mutate(daysFishedMo = ifelse(C1_mo_corrected > 0, TRUE, NA)) %>%
      mutate(daysFishedMo = ifelse(C1_mo_corrected == 0, FALSE, daysFishedMo))
  }

  ###################################################
  ####Days Fished - RSC
  ###################################################
  if (any(myYears %in% c(2018))) {
    #correct number of tournament answer
    mydata <- mydata %>%
      mutate(C1_rsc_corrected = ifelse(is.na(C1_rsca), C1_rsc, 0))
    #create boolean for tournament participation
    mydata <- mydata %>%
      mutate(daysFishedRsc = ifelse(C1_rsc_corrected > 0, TRUE, NA)) %>%
      mutate(
        daysFishedRsc = ifelse(C1_rsc_corrected == 0, FALSE, daysFishedRsc)
      )
  }

  ###################################################
  ####create variable for sole pub or private access
  ###################################################

  if (any(myYears %in% c(2018, 2025))) {
    #create new variables
    mydata$A17both = ifelse(
      (!is.na(mydata$A17priv) & !is.na(mydata$A17pub)) &
        (mydata$A17priv == 1 | mydata$A17pub == 1) &
        (mydata$A17priv == 1 & mydata$A17pub == 1),
      1,
      0
    )
    mydata$A17justPriv = ifelse(
      (!is.na(mydata$A17priv) & !is.na(mydata$A17pub)) &
        (mydata$A17priv == 1 | mydata$A17pub == 1) &
        (mydata$A17priv == 1 & mydata$A17pub == 0),
      1,
      0
    )
    mydata$A17justPub = ifelse(
      (!is.na(mydata$A17priv) & !is.na(mydata$A17pub)) &
        (mydata$A17priv == 1 | mydata$A17pub == 1) &
        (mydata$A17priv == 0 & mydata$A17pub == 1),
      1,
      0
    )
    mydata$A17Priv_corrected = ifelse(
      (!is.na(mydata$A17priv) & !is.na(mydata$A17pub)) &
        (mydata$A17priv == 1 | mydata$A17pub == 1) &
        (mydata$A17priv == 1),
      1,
      0
    )
    mydata$A17Pub_corrected = ifelse(
      (!is.na(mydata$A17priv) & !is.na(mydata$A17pub)) &
        (mydata$A17priv == 1 | mydata$A17pub == 1) &
        (mydata$A17pub == 1),
      1,
      0
    )
    mydata$A17justPriv[is.na(mydata$A17pub) | is.na(mydata$A17priv)] <- NA
    mydata$A17justPub[is.na(mydata$A17pub) | is.na(mydata$A17priv)] <- NA
    mydata$A17both[is.na(mydata$A17pub) | is.na(mydata$A17priv)] <- NA
    mydata$A17Pub_corrected[is.na(mydata$A17pub) | is.na(mydata$A17priv)] <- NA
    mydata$A17Priv_corrected[is.na(mydata$A17pub) | is.na(mydata$A17priv)] <- NA

    mydata <- mydata %>%
      mutate(
        A17both = factor(A17both, levels = c(0, 1), labels = c("No", "Yes")),
        A17justPriv = factor(
          A17justPriv,
          levels = c(0, 1),
          labels = c("No", "Yes")
        ),
        A17justPub = factor(
          A17justPub,
          levels = c(0, 1),
          labels = c("No", "Yes")
        ),
        A17Priv_corrected = factor(
          A17Priv_corrected,
          levels = c(0, 1),
          labels = c("No", "Yes")
        ),
        A17Pub_corrected = factor(
          A17Pub_corrected,
          levels = c(0, 1),
          labels = c("No", "Yes")
        )
      )
  }

  ###################################################
  ####create variables for barriers to fishing scales
  ###################################################

  if (any(myYears %in% c(2018))) {
    ########Access
    myFields <- c("D3f", "D3h", "D3l", "D3t")
    mydata <- mydata %>%
      mutate(barriers_access_AnsweredAll = rowSums(is.na(.[myFields])) == 0) %>%
      mutate(
        barriers_access = ifelse(
          barriers_access_AnsweredAll == TRUE,
          rowMeans(.[myFields], na.rm = TRUE),
          NA
        )
      )
    ########Time
    myFields <- c("D3b", "D3c", "D3k", "D3p")
    mydata <- mydata %>%
      mutate(barriers_time_AnsweredAll = rowSums(is.na(.[myFields])) == 0) %>%
      mutate(
        barriers_time = ifelse(
          barriers_time_AnsweredAll == TRUE,
          rowMeans(.[myFields], na.rm = TRUE),
          NA
        )
      )
    ########Social
    myFields <- c("D3a", "D3e", "D3j", "D3m")
    mydata <- mydata %>%
      mutate(barriers_social_AnsweredAll = rowSums(is.na(.[myFields])) == 0) %>%
      mutate(
        barriers_social = ifelse(
          barriers_social_AnsweredAll == TRUE,
          rowMeans(.[myFields], na.rm = TRUE),
          NA
        )
      )
    ########Knowledge
    myFields <- c("D3d", "D3i", "D3q", "D3s")
    mydata <- mydata %>%
      mutate(
        barriers_knowledge_AnsweredAll = rowSums(is.na(.[myFields])) == 0
      ) %>%
      mutate(
        barriers_knowledge = ifelse(
          barriers_knowledge_AnsweredAll == TRUE,
          rowMeans(.[myFields], na.rm = TRUE),
          NA
        )
      )
    ########Cost
    myFields <- c("D3g", "D3n", "D3o", "D3r")
    mydata <- mydata %>%
      mutate(barriers_cost_AnsweredAll = rowSums(is.na(.[myFields])) == 0) %>%
      mutate(
        barriers_cost = ifelse(
          barriers_cost_AnsweredAll == TRUE,
          rowMeans(.[myFields], na.rm = TRUE),
          NA
        )
      )
    ########Entire Scale
    mydata$D3_AnsweredAll <- rowSums(
      mydata %>%
        select(
          "barriers_access_AnsweredAll",
          "barriers_time_AnsweredAll",
          "barriers_social_AnsweredAll",
          "barriers_knowledge_AnsweredAll",
          "barriers_cost_AnsweredAll"
        )
    ) ==
      4
  }

  return(mydata)
}

base.data.createFactors <- function(myDataframe) {
  myFactors <- base.loaddata.factorlevels() %>%
    filter(Value < 900)
  # When a Field/Value pair carries different labels across survey years (for
  # example a response that was renamed over time, like "Asian Carp" becoming
  # "Invasive Carp"), the old approach `unique(...)` kept BOTH rows. Because the
  # factor() call below maps each Value to the FIRST matching Label in row
  # order, the oldest year's wording silently won. Here we instead sort by Year
  # descending and keep only the first (i.e. most recent) Label per Field/Value,
  # so the newest survey wording is the one applied to the factor.
  f <- myFactors %>%
    arrange(Field, Value, desc(Year)) %>%
    distinct(Field, Value, .keep_all = TRUE) %>%
    select(Field, Value, Label)
  myTypes <- unique(myFactors[, c("Field", "Type")])
  # f<-unique(myFactors[myFactors$Year!=2018,c("Field", "Value", "Label")])
  # myTypes<-unique(myFactors[myFactors$Year!=2018,c("Field", "Type")])
  myFields <- unique(f$Field)

  #create factor levels for fields
  for (i in 1:length(myFields)) {
    #CHECK to see if field exists in current data
    if (myFields[i] %in% names(myDataframe)) {
      #check if field is a data type that should be factors
      if (
        any(
          myTypes[myTypes$Field == myFields[i], c("Type")] %in%
            c("SelectOne", "SelectAll", "SelectOneDual")
        )
      ) {
        #check factor levels
        myDataframe[, myFields[i]] <- factor(
          myDataframe %>% pull(myFields[i]),
          levels = f$Value[f$Field == myFields[i]],
          labels = f$Label[f$Field == myFields[i]]
        )
      }
    }
  }

  myDataframe$age_group <- factor(
    as.numeric(myDataframe$age_group),
    levels = c(1, 2, 3, 4, 5, 6),
    labels = levels(myDataframe$E3)
  )

  return(myDataframe)
}

# create functions for summary analysis ----------------------------------
base.summary.percent.selectOne <- function(
  mydata,
  myQuestion,
  myGroupVar = NA
) {
  #enquo arguments
  myQuestion <- enquo(myQuestion)
  myGroupVar <- enquo(myGroupVar)

  #get initial data...include grouping variable if passed.
  if (!is.na(quo_name(myGroupVar))) {
    qData <- mydata %>%
      mutate(group = !!myGroupVar) %>%
      select(surveyYear, !!myQuestion, group, age_group)
  } else {
    qData <- mydata %>%
      select(surveyYear, !!myQuestion, age_group) %>%
      mutate(group = "Overall")
  }

  #filter for NA's
  qData <- qData %>%
    filter(!is.na(!!myQuestion)) %>%
    filter(!is.na(group))

  #perform raking if needed
  if (is.numeric(nrow(rakeVars))) {
    #add rake weights if called for
    qData <- base.summary.rake.loop(qData, rakeVars, rakePopDists)
    #qData$postWeight<-1
    #sum(qData$postWeight)
  } else {
    #fill in any missing weights with 1 to create unweighted estimates
    qData$postWeight <- 1
  }

  #calculate total responses and summarise by group
  qData <- qData %>%
    select(surveyYear, group, response = !!myQuestion, postWeight) %>%
    group_by(surveyYear, group, response) %>%
    count(response, wt = postWeight, name = "num") %>%
    ungroup() %>%
    group_by(surveyYear, group) %>%
    mutate(totNum = sum(num, na.rm = TRUE)) %>%
    mutate(perc = num / totNum * 100) %>%
    mutate(
      ci = round(
        (1.96 * (sqrt((perc / 100) * (1 - (perc / 100)) / totNum))) * 100,
        4
      ),
      num = ceiling(num)
    ) %>%
    dplyr::select(
      Year = surveyYear,
      Group = group,
      Response = response,
      Value = perc,
      CI = ci,
      Number = num
    )

  return(qData %>% ungroup())
}

#base.summary.percent.selectOne(d, E2, myGroupVar = E2)

base.summary.percent.selectAll <- function(
  mydata,
  myQuestions,
  myAnsweredVar,
  myGroupVar = NA
) {
  #enquo arguments
  myQuestion <- enquo(myQuestions)
  myAnsweredVar <- enquo(myAnsweredVar)
  myGroupVar <- enquo(myGroupVar)

  #get initial data...include grouping variable if passed.
  if (!is.na(quo_name(myGroupVar))) {
    qData <- mydata %>%
      filter(!!myAnsweredVar == TRUE) %>%
      select(
        surveyYear,
        group = !!quo_name(myGroupVar),
        !!myQuestion,
        age_group
      )
  } else {
    qData <- mydata %>%
      filter(!!myAnsweredVar == TRUE) %>%
      mutate(group = "Overall") %>%
      select(surveyYear, group, !!myQuestion, age_group)
  }

  #filter for NA's, calculate total responses and summarise by group
  qData <- qData %>%
    filter(!is.na(group))

  #perform raking if needed
  if (is.numeric(nrow(rakeVars))) {
    #add rake weights if called for
    qData <- base.summary.rake.loop(qData, rakeVars, rakePopDists)
    #qData$postWeight<-1
    #sum(qData$postWeight)
  } else {
    #fill in any missing weights with 1 to create unweighted estimates
    qData$postWeight <- 1
  }

  #old unweighted code
  # #filter for NA's, calculate total responses and summarise by group
  # qData<-qData %>%
  #  filter(!is.na(group)) %>%
  #  group_by(surveyYear, group) %>%
  #  mutate(totNum=n()) %>%
  #  gather(Response, Answer, !!myQuestion) %>%
  #  filter(trimws(Answer)!="Unchecked") %>%
  #  ungroup() %>%
  #  group_by(surveyYear, group, Answer, totNum) %>%
  #  summarise(num=n()) %>%
  #  mutate(perc=num/totNum*100) %>%
  #  mutate(ci=round((1.96 * (sqrt((perc/100)*(1-(perc/100))/totNum)))*100,4)) %>%
  #  select(Year=surveyYear, Group=group, Response=Answer, Value=perc, CI=ci, Number=num)

  #calculate total responses and summarise by group
  qData <- qData %>%
    select(surveyYear, group, !!myQuestion, postWeight) %>%
    gather(response, Answer, !!myQuestion) %>%
    group_by(surveyYear, group, response, Answer) %>%
    count(response, wt = postWeight, name = "num") %>%
    ungroup() %>%
    group_by(surveyYear, group, response) %>%
    mutate(totNum = sum(num, na.rm = TRUE)) %>%
    filter(trimws(Answer) != "Unchecked") %>%
    ungroup() %>%
    mutate(perc = num / totNum * 100) %>%
    mutate(
      ci = round(
        (1.96 * (sqrt((perc / 100) * (1 - (perc / 100)) / totNum))) * 100,
        4
      ),
      num = ceiling(num)
    ) %>%
    select(
      Year = surveyYear,
      Group = group,
      Response = Answer,
      Value = perc,
      CI = ci,
      Number = num
    )

  return(qData %>% ungroup())
}

#base.summary.percent.selectAll(d, c("A4lr", "A4mo"), myAnsweredVar = A4_Answered)
#base.summary.percent.selectAll(d, myFields, myAnsweredVar = A6_Answered)

base.summary.means <- function(mydata, myQuestion, myGroupVar = NA) {
  #enquo arguments
  myQuestion <- enquo(myQuestion)
  myGroupVar <- enquo(myGroupVar)

  #get initial data...include grouping variable if passed.
  if (!is.na(quo_name(myGroupVar))) {
    qData <- mydata %>%
      select(
        surveyYear,
        group = !!quo_name(myGroupVar),
        !!myQuestion,
        age_group
      )
  } else {
    qData <- mydata %>%
      mutate(group = "Overall") %>%
      select(surveyYear, group, !!myQuestion, age_group)
  }

  #filter for NA's
  qData <- qData %>%
    filter(!is.na(group)) %>%
    filter(!is.na(!!myQuestion))

  #perform raking if needed
  if (is.numeric(nrow(rakeVars))) {
    #add rake weights if called for
    qData <- base.summary.rake.loop(qData, rakeVars, rakePopDists)
    #qData$postWeight<-1
    #sum(qData$postWeight)
  } else {
    #fill in any missing weights with 1 to create unweighted estimates
    qData$postWeight <- 1
  }

  #calculate total responses and summarise by group
  qData <- qData %>%
    mutate(!!quo_name(myQuestion) := as.numeric(!!myQuestion)) %>%
    gather(Response, Answer, !!myQuestion) %>%
    group_by(surveyYear, group) %>%
    summarize(
      meanValue = wt.mean(Answer, postWeight),
      sdValue = wt.sd(Answer, postWeight),
      numValue = sum(!is.na(Answer))
    ) %>%
    mutate(CI = 1.96 * (sdValue / sqrt(numValue))) %>%
    mutate(Response = quo_name(myQuestion)) %>%
    select(
      Year = surveyYear,
      Group = group,
      Response,
      Value = meanValue,
      CI = CI,
      Number = numValue
    )

  return(qData %>% ungroup())
}

#base.summary.means(d, A9)

base.summary.medians <- function(mydata, myQuestion, myGroupVar = NA) {
  #enquo arguments
  myQuestion <- enquo(myQuestion)
  myGroupVar <- enquo(myGroupVar)

  #get initial data...include grouping variable if passed.
  if (!is.na(quo_name(myGroupVar))) {
    qData <- mydata %>%
      select(
        surveyYear,
        group = !!quo_name(myGroupVar),
        !!myQuestion,
        age_group
      )
  } else {
    qData <- mydata %>%
      mutate(group = "Overall") %>%
      select(surveyYear, group, !!myQuestion, age_group)
  }

  #filter for NA's
  #filter for NA's
  qData <- qData %>%
    filter(!is.na(group)) %>%
    filter(!is.na(!!myQuestion))

  #perform raking if needed
  if (is.numeric(nrow(rakeVars))) {
    #add rake weights if called for
    qData <- base.summary.rake.loop(qData, rakeVars, rakePopDists)
    #qData$postWeight<-1
    #sum(qData$postWeight)
  } else {
    #fill in any missing weights with 1 to create unweighted estimates
    qData$postWeight <- 1
  }

  #unweighted values - old
  #calculate total responses and summarise by group
  # qData<-qData %>%
  #   mutate(!!quo_name(myQuestion):=as.numeric(!!myQuestion)) %>%
  #   gather(Response, Answer, !!myQuestion) %>%
  #   group_by(surveyYear, group)  %>%
  #   summarize(medianValue=ci.median(Answer, conf=0.95)$ci[1],
  #              numValue=sum(!is.na(Answer)),
  #              CIupper=ci.median(Answer, conf=0.95)$ci[3],
  #              CIlower=ci.median(Answer, conf=0.95)$ci[2]) %>%
  #    mutate(Response=quo_name(myQuestion)) %>%
  #    select(Year=surveyYear, Group=group, Response, Value=medianValue, CIupper, CIlower, Number=numValue)

  #calculate total responses and summarise by group
  qData <- qData %>%
    mutate(!!quo_name(myQuestion) := as.numeric(!!myQuestion)) %>%
    gather(Response, Answer, !!myQuestion) %>%
    group_by(surveyYear, group) %>%
    summarize(
      medianValue = weighted.quantile(Answer, postWeight, probs = (0.5)),
      numValue = sum(!is.na(Answer)),
      CIupper = weighted.quantile(Answer, postWeight, probs = (0.95)),
      CIlower = weighted.quantile(Answer, postWeight, probs = (0.5))
    ) %>%
    mutate(Response = quo_name(myQuestion)) %>%
    select(
      Year = surveyYear,
      Group = group,
      Response,
      Value = medianValue,
      CIupper,
      CIlower,
      Number = numValue
    )

  return(qData %>% ungroup())
}

#base.summary.medians(d, C1Total_days, myGroupVar = "Resi")

# Create Function to get question text ------------------------------------
GetQuestion <- function(myQuestionFactors, myField, myYear) {
  myField <- enquo(myField)

  if (rlang::quo_text(myField) %in% c("C2", "E1", "D14", "D3", "D4")) {
    switch(
      rlang::quo_text(myField),
      C2 = {
        op <- "Indicate the importance for each item as a reason why you fish."
      },
      E1 = {
        op <- "Indicate how much you agree with each of the following statements about fishing."
      },
      D14 = {
        op <- "Thinking about your fishing in Nebraska during 2018, how often are each of the following statements true?"
      },
      D3 = {
        op <- "Please complete the following statements."
      },
      D4 = {
        op <- "Thinking about the one type of fish that you prefer to fish for, how much do you agree or disagree with the following about your fishing in Nebraska during 2018?."
      }
    )
  } else {
    op <- myQuestionFactors %>%
      filter(str_detect(.$Field, rlang::quo_text(myField)) & Year == myYear) %>%
      pull(Question) %>%
      unique() %>%
      as.character()
  }

  return(op[1])
}
