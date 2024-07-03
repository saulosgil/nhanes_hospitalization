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

#' # Data preparation
# Download & Read Transport Files

# Demographic ---------------------------------------------------------------------------------
# 09-10
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DEMO_F.XPT", tf <- tempfile(), mode="wb")
DEMO_10 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "RIDRETH1",
                                       "DMDEDUC2",
                                       "DMDMARTL",
                                       "INDFMIN2",
                                       "INDFMPIR",
                                       "WTINT2YR",
                                       "WTMEC2YR",
                                       "SDMVSTRA",
                                       "SDMVPSU")]
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT", tf <- tempfile(), mode="wb")
DEMO_12 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "RIDRETH1",
                                       "DMDEDUC2",
                                       "DMDMARTL",
                                       "INDFMIN2",
                                       "INDFMPIR",
                                       "WTINT2YR",
                                       "WTMEC2YR",
                                       "SDMVSTRA",
                                       "SDMVPSU")]
# 13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT", tf <- tempfile(), mode="wb")
DEMO_14 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "RIDRETH1",
                                       "DMDEDUC2",
                                       "DMDMARTL",
                                       "INDFMIN2",
                                       "INDFMPIR",
                                       "WTINT2YR",
                                       "WTMEC2YR",
                                       "SDMVSTRA",
                                       "SDMVPSU")]
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT", tf <- tempfile(), mode="wb")
DEMO_16 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "RIDRETH1",
                                       "DMDEDUC2",
                                       "DMDMARTL",
                                       "INDFMIN2",
                                       "INDFMPIR",
                                       "WTINT2YR",
                                       "WTMEC2YR",
                                       "SDMVSTRA",
                                       "SDMVPSU")]
# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT", tf <- tempfile(), mode="wb")
DEMO_18 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "RIDRETH1",
                                       "DMDEDUC2",
                                       "DMDMARTL",
                                       "INDFMIN2",
                                       "INDFMPIR",
                                       "WTINT2YR",
                                       "WTMEC2YR",
                                       "SDMVSTRA",
                                       "SDMVPSU")]

# Hospital Utilization and access to care-------------------------------------------------------
# 09-10
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/HUQ_F.XPT", tf <- tempfile(), mode="wb")
HOSPITAL_10 <- foreign::read.xport(tf)[, c("SEQN",
                                           "HUQ010",
                                           "HUQ071",
                                           "HUD080")]
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/HUQ_G.XPT", tf <- tempfile(), mode="wb")
HOSPITAL_12 <- foreign::read.xport(tf)[, c("SEQN",
                                           "HUQ010",
                                           "HUQ071",
                                           "HUD080")]
# 13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/HUQ_H.XPT", tf <- tempfile(), mode="wb")
HOSPITAL_14 <- foreign::read.xport(tf)[, c("SEQN",
                                           "HUQ010",
                                           "HUQ071",
                                           "HUD080")]
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HUQ_I.XPT", tf <- tempfile(), mode="wb")
HOSPITAL_16 <- foreign::read.xport(tf)[, c("SEQN",
                                           "HUQ010",
                                           "HUQ071",
                                           "HUD080")]
# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/HUQ_J.XPT", tf <- tempfile(), mode="wb")
HOSPITAL_18 <- foreign::read.xport(tf)[, c("SEQN",
                                           "HUQ010",
                                           "HUQ071",
                                           "HUD080")]

# Physical function ---------------------------------------------------------------------------
# 09-10
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/PFQ_F.XPT", tf <- tempfile(), mode="wb")
PHYSICAL_FUCTION_10 <- foreign::read.xport(tf)[, c("SEQN",
                                                   "PFQ061H",
                                                   "PFQ061I",
                                                   "PFQ061K",
                                                   "PFQ061L")]# 15-16
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PFQ_G.XPT", tf <- tempfile(), mode="wb")
PHYSICAL_FUCTION_12 <- foreign::read.xport(tf)[, c("SEQN",
                                                   "PFQ061H",
                                                   "PFQ061I",
                                                   "PFQ061K",
                                                   "PFQ061L")]
# 13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/PFQ_H.XPT", tf <- tempfile(), mode="wb")
PHYSICAL_FUCTION_14 <- foreign::read.xport(tf)[, c("SEQN",
                                                   "PFQ061H",
                                                   "PFQ061I",
                                                   "PFQ061K",
                                                   "PFQ061L")]
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PFQ_I.XPT", tf <- tempfile(), mode="wb")
PHYSICAL_FUCTION_16 <- foreign::read.xport(tf)[, c("SEQN",
                                                   "PFQ061H",
                                                   "PFQ061I",
                                                   "PFQ061K",
                                                   "PFQ061L")]
# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/PFQ_J.XPT", tf <- tempfile(), mode="wb")
PHYSICAL_FUCTION_18 <- foreign::read.xport(tf)[, c("SEQN",
                                                   "PFQ061H",
                                                   "PFQ061I",
                                                   "PFQ061K",
                                                   "PFQ061L")]













# Append Files ---------------------------------------------------------------------------------
# Append Files
DEMO <- dplyr::bind_rows(DEMO_15,
                         DEMO_17)

DEMO |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

HOSPITAL <- dplyr::bind_rows(HOSPITAL_15,
                             HOSPITAL_17)

HOSPITAL |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

PA <- dplyr::bind_rows(PA_15,
                       PA_17)

PA |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

CVD <- dplyr::bind_rows(CVD_15,
                        CVD_17)

CVD |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

DM <- dplyr::bind_rows(DM_15,
                       DM_17)

DM |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

DRUGS <- dplyr::bind_rows(DRUGS_15,
                          DRUGS_17)

DRUGS |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

SMOKING <- dplyr::bind_rows(SMOKING_15,
                            SMOKING_17)

SMOKING |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

MEDCOND <- dplyr::bind_rows(MEDCOND_15,
                            MEDCOND_17)

MEDCOND |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

KIDNEY <- dplyr::bind_rows(KIDNEY_15,
                           KIDNEY_17)

KIDNEY |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "YES"

DIETD1 <- dplyr::bind_rows(DIETD1_15,
                           DIETD1_17)

DIETD1 |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "YES"

DIETD2 <- dplyr::bind_rows(DIETD2_15,
                           DIETD2_17)

DIETD2 |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

BODY <- dplyr::bind_rows(BODY_15,
                         BODY_17)

BODY |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

BLOODPRESS <- dplyr::bind_rows(BLOODPRESS_15,
                               BLOODPRESS_17)

BLOODPRESS |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

PHYSICAL_FUCTION <- dplyr::bind_rows(PHYSICAL_FUCTION_15,
                                     PHYSICAL_FUCTION_17)

PHYSICAL_FUCTION |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

# Merge HOSPITAL and DEMO files

HOSPITAL_DEMO <-
  dplyr::left_join(HOSPITAL, DEMO, by="SEQN")

# Merge HOSPITAL_DEMO and PA

HOSPITAL_DEMO_PA <-
  dplyr::left_join(HOSPITAL_DEMO, PA, by="SEQN")

# Merge HOSPITAL_DEMO_PA and CVD

HOSPITAL_DEMO_PA_CVD <-
  dplyr::left_join(HOSPITAL_DEMO_PA, CVD, by="SEQN")

# Merge HOSPITAL_DEMO_PA_CVD and DM

HOSPITAL_DEMO_PA_CVD_DM <-
  dplyr::left_join(HOSPITAL_DEMO_PA_CVD, DM, by="SEQN")

# Merge HOSPITAL_DEMO_PA_CVD_DM and DRUGS

HOSPITAL_DEMO_PA_CVD_DM_DRUGS <-
  dplyr::left_join(HOSPITAL_DEMO_PA_CVD_DM, DRUGS, by="SEQN")

# Merge HOSPITAL_DEMO_PA_CVD_DM_DRUGS and SMOKING

HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING <-
  dplyr::left_join(HOSPITAL_DEMO_PA_CVD_DM_DRUGS, SMOKING, by="SEQN")

# Merge HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING and MEDCOND

HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND <-
  dplyr::left_join(HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING, MEDCOND, by="SEQN")

# Merge HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING and KIDNEY

HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY <-
  dplyr::left_join(HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND, KIDNEY, by="SEQN")

# Merge HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY and DIETD1

HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1 <-
  dplyr::left_join(HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY, DIETD1, by="SEQN")

# Merge HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1 and DIETD1

HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1_DIETD2 <-
  dplyr::left_join(HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1, DIETD2, by="SEQN")

# Merge HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1_DIETD2 and BODY

HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1_DIETD2_BODY <-
  dplyr::left_join(HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1_DIETD2, BODY, by="SEQN")

# Merge HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1_DIETD2_BODY and BLOODPRESS

HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1_DIETD2_BODY_BLOODPRESS <-
  dplyr::left_join(HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1_DIETD2_BODY, BLOODPRESS, by="SEQN")

# Merge HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1_DIETD2_BODY_BLOODPRESS and PHYSICAL FUNCTION

HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1_DIETD2_BODY_BLOODPRESS_PHYSICAL_FUNCTION <-
  dplyr::left_join(HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1_DIETD2_BODY_BLOODPRESS, PHYSICAL_FUCTION, by="SEQN")

df_bruto <- HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1_DIETD2_BODY_BLOODPRESS_PHYSICAL_FUNCTION

df <- df_bruto |>
  dplyr::distinct(SEQN, .keep_all = TRUE) # removing duplicate rows

# Created new variables -----------------------------------------------------------------------

###### salvando data.frame para explorar

# readr::write_csv2(x = df, file = "df.csv") # deixar comentado para salvar

# reading dataset --------------------------------------------------------------------------------

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
                                 EATING >=2 & EATING <=4 ~ 1),
         DRESSING_NOVO = case_when(DRESSING == 1 ~ 0,
                                   DRESSING >=2 & WALKING_ROOMS <=4 ~ 1),
         INCAPAZ = WALKING_ROOMS_NOVO + STANDINGUP_NOVO + EATING_NOVO + DRESSING_NOVO,
         INCAPAZ_CLASSE = case_when(INCAPAZ < 1 ~ 0, # no disability
                                    INCAPAZ >= 1 & INCAPAZ <= 16 ~ 1)) |>
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
    # create AGE CLASS
    AGE_CLASS = case_when(RIDAGEYR < 80 ~ "A_<80",
                          RIDAGEYR >= 80 ~ "B_>=80"),
    # create mutimorbidity
    SUM_COMORB = DIQ010 + MCQ160F + MCQ160B + MCQ160E + MCQ220 + KIQ022 + MCQ160O + MCQ160L,
    MULT_COMORB = case_when(RXDCOUNT < 2 ~ "A_NAO_MULT_COMORB",
                            RXDCOUNT >= 2 ~ "B_MULT_COMORB"),
    # create polypharmacy
    POLYPHARM = case_when(RXDCOUNT < 3 ~ "A_NO_POLYPHARM",
                          RXDCOUNT >= 3 ~ "B_POLYPHARM"),
    # create protein consumptoin status RDA
    PTN_RDA = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                        PTNKG >=0.8 ~ "B_ADEQUADO"),
    internação_ano = case_when(HUQ071 == 1 ~ 1,
                               HUQ071 == 2 ~ 0),
    internação_frequencia = HUD080,
    inAnalysis = (
      RIDAGEYR >= 65 &
        internação_ano < 3 &
        !is.na(INCAPAZ_CLASSE) &
        !is.na(AGE_CLASS) &
        !is.na(RIDRETH1) &
        !is.na(POLYPHARM) &
        !is.na(MULT_COMORB)
        # PAQ610 < 8 &
        # PAD615 < 841 &
        # PAQ625 < 8 &
        # PAD630 < 841 &
        # PAQ640 < 8 &
        # PAD645 < 661 &
        # PAQ655 < 7 &
        # PAD660 < 481 &
        # PAQ670 < 8 &
        # PAD675 < 540
        # ENERGY_STATUS == 'LIKELY' & # veriricar se iremos incluir consumo alimentar no projeto
        # !is.na(ENERGY_PT_MODEL) &
        # DIQ010 < 3 & # Diabetes (1 = yes; 2 = no)
        # MCQ160F < 3 & # AVC (1 = yes; 2 = no)
        # MCQ160B < 3 & # ICC (1 = yes; 2 = no)
        # MCQ160E < 3 & # IAM (1 = yes; 2 = no)
        # MCQ220 < 3 & # cancer (1 = yes; 2 = no)
        # KIQ022 < 3 & # renal (1 = yes; 2 = no)
        # MCQ160O < 3 & # DPOC (1 = yes; 2 = no)
        # MCQ160L < 3  # hepatico (1 = yes; 2 = no)
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
