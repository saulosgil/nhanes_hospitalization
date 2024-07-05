#' Example R code to replicate NCHS Data Brief No.303, Figures 1
#' Prevalence of Depression Among Adults Aged 20 and Over: United States, 2013-2016

#' Brody DJ, Pratt LA, Hughes JP. Prevalence of Depression Among Adults Aged 20 and Over: United
#' States, 2013-2016. NCHS Data Brief. No 303. Hyattsville, MD: National Center for Health Statistics. 2018.

#' Available at: https://www.cdc.gov/nchs/products/databriefs/db303.htm

# Project - Association between PA and hospitalization ----------------------------------------

#' ------------------------------------------------------------------------------------------------------------

# Load survey and dplyr packages
#+ message = FALSE, warning=FALSE
library(tidyverse)
library(survey)
#'
options(survey.lonely.psu='adjust')

# Display Version Information
cat("R package versions:\n")
for (p in c("base", "survey","dplyr")) {
  cat(p, ": ", as.character(packageVersion(p)), "\n")
}

#' # Analysis
# reading dataset --------------------------------------------------------------------------------
df <- read.csv2(file = "df.csv")

# DataPrep ------------------------------------------------------------------------------------
One <-
  df |>
  # adjusting physical functioning (disability) parameters
  dplyr::mutate(WALKING_ROOMS = PFQ061H) |>
  dplyr::mutate(STANDINGUP = PFQ061I) |>
  dplyr::mutate(EATING = PFQ061K) |>
  dplyr::mutate(DRESSING = PFQ061L) |>
  # To create the variable INCAPAZ - PRIMARY OUTCOME
  mutate(WALKING_ROOMS_NOVO = case_when(WALKING_ROOMS == 1 ~ 0,
                                        WALKING_ROOMS >=2 & WALKING_ROOMS <=4 ~ 1),
         STANDINGUP_NOVO = case_when(STANDINGUP == 1 ~ 0,
                                     STANDINGUP >=2 & WALKING_ROOMS <=4 ~ 1),
         EATING_NOVO = case_when(EATING == 1 ~ 0,
                                 EATING >= 2 & EATING <=4 ~ 1),
         DRESSING_NOVO = case_when(DRESSING == 1 ~ 0,
                                   DRESSING >= 2 & WALKING_ROOMS <=4 ~ 1),
         INCAPAZ = WALKING_ROOMS_NOVO + STANDINGUP_NOVO + EATING_NOVO + DRESSING_NOVO,
         INCAPAZ_CLASSE = case_when(INCAPAZ < 1 ~ 0, # no disability
                                    INCAPAZ >= 1 & INCAPAZ <= 16 ~ 1)) |>
  # To create the variable INCAPAZ - PRIMARY OUTCOME
  mutate(
    BMXHT = BMXHT / 100,
    BMI = BMXWT / (BMXHT^2),
    OBESITY = case_when(BMI >= 30 ~ "OBESO",
                        BMI < 30 ~ "NORMAL"),
    # create AGE CLASS
    AGE_CLASS = case_when(RIDAGEYR < 80 ~ "A_<80",
                          RIDAGEYR >= 80 ~ "B_>=80"),
    # POVERT INDEX
    POVERT_INDEX = case_when(INDFMPIR > 1 ~ ">1",
                             INDFMPIR <= 1 ~ "<1"),
    # create mutimorbidity
    SUM_COMORB = DIQ010 + MCQ160F + MCQ160B + MCQ160E + MCQ220 + KIQ022 + MCQ160L,
    MULT_COMORB = case_when(RXDCOUNT < 2 ~ "A_NAO_MULT_COMORB",
                            RXDCOUNT >= 2 ~ "B_MULT_COMORB"),
    # create polypharmacy
    POLYPHARM = case_when(RXDCOUNT < 3 ~ "A_NO_POLYPHARM",
                          RXDCOUNT >= 3 ~ "B_POLYPHARM"),
    internação_ano = case_when(HUQ071 == 1 ~ 1,
                               HUQ071 == 2 ~ 0),
    internação_frequencia = case_when(HUD080 <= 3 ~ 0,
                                      HUD080 > 3 ~ 1),
    # weighted
    WTMEC10YR = WTMEC2YR * 1/5,
    inAnalysis = (
      RIDAGEYR >= 65 &
      internação_ano == 1 &
      !is.na(INCAPAZ_CLASSE) &
      !is.na(AGE_CLASS) &
      !is.na(RIDRETH1) &
      !is.na(POLYPHARM) &
      !is.na(MULT_COMORB)
    )
  )

#' ## Define survey design
# Define survey design for overall dataset
NHANES_all <- svydesign(data=One, id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR, nest=TRUE)

# Create a survey design object for the subset of interest: adults aged 20 and over with a valid depression score
# Subsetting the original survey design object ensures we keep the design information about the number of clusters and strata
NHANES <- subset(NHANES_all, inAnalysis)

# to verify number of Lines and Cols
nrow(NHANES$variables)
ncol(NHANES$variables)

# Exploratory analysis ------------------------------------------------------------------------
# General Descriptive and distribution analysis
glimpse(NHANES$variables)
skimr::skim_without_charts(NHANES$variables)
DataExplorer::plot_missing(NHANES$variables)

# Count inactive vs actives
knitr::kable(
  NHANES$variables |>
    count(INCAPAZ_CLASSE)
  )

# Count hospital admission in active and inactive
knitr::kable(
  NHANES$variables |>
    count(INCAPAZ_CLASSE,
          as.factor(internação_frequencia))
  )

# Creating two-way table from data frame
knitr::kable(
  addmargins(
    table(
      "INCAPAZ_CLASSE" = NHANES$variables$INCAPAZ_CLASSE,
      "internação" = NHANES$variables$internação_frequencia
    )
  )
)

# DISABILITY AND FREQUENCIA DE INTERNAÇÃO -----------------------------------------------------
# <= 3 ou > 3 -----------------------------------------------------
## crude logistic regression
crude_svy <-
  survey::svyglm(
    formula = as.factor(internação_frequencia) ~ as.factor(INCAPAZ_CLASSE),
    design = NHANES,
    family = binomial(link = "logit")
  )

# Summary
summary(crude_svy)
cbind(odds = exp(crude_svy$coefficients), exp(confint(crude_svy)))
sjPlot::tab_model(crude_svy)

## Adjusted logistic regression
### Adjusts:
# - age [<80 or ≥80 years],
# - race/ethnicity [Mexican American, other Hispanic, non-Hispanic white, non-Hispanic Black, and others],
# - POLYPHARMACY [<3 AND >3],
# - MULTIMORBIDITY [<3 AND >=5]
# - POVERT_INDEX [<=1 AND >1]
adjusted_svy <-
  survey::svyglm(
    formula = as.factor(internação_frequencia) ~ as.factor(INCAPAZ_CLASSE) + as.factor(AGE_CLASS) + as.factor(RIDRETH1) + as.factor(POLYPHARM) + as.factor(MULT_COMORB) + as.factor(POVERT_INDEX),
    design = NHANES,
    family = binomial(link = "logit")
  )

# Summary
summary(adjusted_svy)
cbind(odds = exp(adjusted_svy$coefficients), exp(confint(adjusted_svy)))
sjPlot::tab_model(adjusted_svy)
