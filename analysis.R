# Load survey and dplyr packages
#+ message = FALSE, warning=FALSE
library(tidyverse)
library(survey)
library(sjPlot)

#'
options(survey.lonely.psu='adjust')

# Display Version Information
cat("R package versions:\n")
for (p in c("base", "survey","dplyr")) {
  cat(p, ": ", as.character(packageVersion(p)), "\n")
}

# load dataset
df <- read.csv2(file = "df.csv")

# DataPrep ------------------------------------------------------------------------------------
One <-
  df |>
  # remove duplicates
  dplyr::distinct(SEQN, .keep_all = TRUE) |>
  # adjusting physical activity - where is NA to change to zero
  dplyr::mutate(
    PAQ610 = tidyr::replace_na(PAQ605, 0),
    PAQ610 = tidyr::replace_na(PAQ610, 0),
    PAD615 = tidyr::replace_na(PAD615, 0),
    PAQ620 = tidyr::replace_na(PAQ620, 0),
    PAQ625 = tidyr::replace_na(PAQ625, 0),
    PAD630 = tidyr::replace_na(PAD630, 0),
    PAD630 = tidyr::replace_na(PAQ635, 0),
    PAQ640 = tidyr::replace_na(PAQ640, 0),
    PAD645 = tidyr::replace_na(PAD645, 0),
    PAD645 = tidyr::replace_na(PAQ650, 0),
    PAQ655 = tidyr::replace_na(PAQ655, 0),
    PAD660 = tidyr::replace_na(PAD660, 0),
    PAD660 = tidyr::replace_na(PAQ665, 0),
    PAQ670 = tidyr::replace_na(PAQ670, 0),
    PAD675 = tidyr::replace_na(PAD675, 0),
    PAD675 = tidyr::replace_na(PAD680, 0)
  ) |>
  # created physical activity outcomes
  dplyr::mutate(
    # PA work
    PAW = PAQ610 * PAD615 + PAQ625 * PAD630,
    # PA transport
    PAT = PAQ640 * PAD645,
    # PA leisure
    PAL = PAQ655 * PAD660 + PAQ670 * PAD675,
    PATOTAL = PAW + PAT + PAL,
    PA_CLASS = case_when(PATOTAL >= 150 ~ "ATIVO",
                         PATOTAL < 150 ~ "INATIVO")
  ) |>
  # To create the variable INCAPAZ - PRIMARY OUTCOME
  mutate(
    BMXHT = BMXHT /100,
    BMI = BMXWT / (BMXHT^2),
    OBESITY = case_when(BMI >= 30 ~ "OBESO",
                        BMI < 30 ~ "NORMAL"),
    ENERGY = DR1TKCAL,
    PTN = DR1TPROT,
    PTNKG = PTN/BMXWT,
    CHO = DR1TCARB,
    FAT = DR1TTFAT,
    ENERGY_KG = ENERGY/BMXWT,
    ENERGY_PT_MODEL = ENERGY - PTN * 4,
    ENERGY_STATUS = case_when(RIAGENDR == 1 & ENERGY < 800 ~ "UNLIKELY",
                              RIAGENDR == 1 & ENERGY > 4000 ~ "UNLIKELY",
                              RIAGENDR == 1 & ENERGY >= 800 & ENERGY <=4000 ~ "LIKELY",
                              RIAGENDR == 2 & ENERGY < 500 ~ "UNLIKELY",
                              RIAGENDR == 2 & ENERGY > 3500 ~ "UNLIKELY",
                              RIAGENDR == 2 & ENERGY >= 500 & ENERGY <=3500 ~ "LIKELY"),
    # create protein consumptoin status RDA
    PTN_RDA = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                        PTNKG >=0.8 ~ "B_ADEQUADO"),
    internação_ano = case_when(HUQ071 == 1 ~ 1,
                               HUQ071 == 2 ~ 0),
    internação_frequencia = HUD080,
    inAnalysis = (
      RIDAGEYR >= 65 &
        internação_ano < 3 &
        !is.na(PA_CLASS) &
        # ENERGY_STATUS == 'LIKELY' & # veriricar se iremos incluir consumo alimentar no projeto
        # !is.na(ENERGY_PT_MODEL) &
        DIQ010 < 3 & # Diabetes (1 = yes; 2 = no)
        MCQ160F < 3 & # AVC (1 = yes; 2 = no)
        MCQ160B < 3 & # ICC (1 = yes; 2 = no)
        MCQ160E < 3 & # IAM (1 = yes; 2 = no)
        MCQ220 < 3 & # cancer (1 = yes; 2 = no)
        KIQ022 < 3 & # renal (1 = yes; 2 = no)
        MCQ160O < 3 & # DPOC (1 = yes; 2 = no)
        MCQ160L < 3  # hepatico (1 = yes; 2 = no)
    )
  )

#' ## Define survey design
# Define survey design for overall dataset
NHANES_all <- svydesign(data=One, id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC2YR, nest=TRUE)

# Create a survey design object for the subset of interest: adults aged 20 and over with a valid depression score
# Subsetting the original survey design object ensures we keep the design information about the number of clusters and strata
NHANES <- subset(NHANES_all, inAnalysis)

# to verify number of protein < 0.80
nrow(NHANES$variables)

# Exploratory analysis ------------------------------------------------------------------------
# General Descriptive and distribution analysis
glimpse(NHANES$variables)
skimr::skim_without_charts(NHANES$variables)
DataExplorer::plot_missing(NHANES$variables)

# Count
knitr::kable(
  NHANES$variables |>
    count(PA_CLASS,
          as.factor(internação_ano))
  )

# Creating two-way table from data frame
knitr::kable(
  addmargins(
    table(
      "PA_CLASS" = NHANES$variables$PA_CLASS,
      "internação" = NHANES$variables$internação_frequencia
    )
  )
)

# analysis ------------------------------------------------------------------------------------
## crude logistic regression
crude_svy <-
  survey::svyglm(
    formula = as.factor(internação_ano) ~ as.factor(PA_CLASS),
    design = NHANES,
    family = binomial(link = "logit")
  )

# Summary
summary(crude_svy)
cbind(odds = exp(crude_svy$coefficients), exp(confint(crude_svy)))
sjPlot::tab_model(crude_svy)

## Adjusted logistic regression
adjusted_svy <-
  survey::svyglm(
    formula = as.factor(internação_ano) ~ as.factor(PA_CLASS) + as.factor(RIAGENDR)+ as.factor(RIDRETH1) + OBESITY,
    design = NHANES,
    family = binomial(link = "logit")
  )
NHANES$variables$SMQ020
# Summary
summary(adjusted_svy)
cbind(odds = exp(adjusted_svy$coefficients), exp(confint(adjusted_svy)))
sjPlot::tab_model(adjusted_svy)

