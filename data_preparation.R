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
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT", tf <- tempfile(), mode="wb")
DEMO_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "RIDRETH1",
                                       "DMDEDUC2",
                                       "DMDMARTL",
                                       "INDFMIN2",
                                       "WTINT2YR",
                                       "WTMEC2YR",
                                       "SDMVSTRA",
                                       "SDMVPSU")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT", tf <- tempfile(), mode="wb")
DEMO_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "RIDRETH1",
                                       "DMDEDUC2",
                                       "DMDMARTL",
                                       "INDFMIN2",
                                       "WTINT2YR",
                                       "WTMEC2YR",
                                       "SDMVSTRA",
                                       "SDMVPSU")]

# Hospital Utilization and access to care-------------------------------------------------------
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/HUQ_I.XPT", tf <- tempfile(), mode="wb")
HOSPITAL_15 <- foreign::read.xport(tf)[, c("SEQN",
                                           "HUQ010",
                                           "HUQ071",
                                           "HUD080")]


# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/HUQ_J.XPT", tf <- tempfile(), mode="wb")
HOSPITAL_17 <- foreign::read.xport(tf)[, c("SEQN",
                                           "HUQ010",
                                           "HUQ071",
                                           "HUD080")]

# Physical activity ----------------------------------------------------------------------------
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PAQ_I.XPT", tf <- tempfile(), mode="wb")
PA_15 <- foreign::read.xport(tf)[, c("SEQN",
                                     "PAQ605",
                                     "PAQ610",
                                     "PAD615",
                                     "PAQ620",
                                     "PAQ625",
                                     "PAD630",
                                     "PAQ635",
                                     "PAQ640",
                                     "PAD645",
                                     "PAQ650",
                                     "PAQ655",
                                     "PAD660",
                                     "PAQ665",
                                     "PAQ670",
                                     "PAD675",
                                     "PAD680")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/PAQ_J.XPT", tf <- tempfile(), mode="wb")
PA_17 <- foreign::read.xport(tf)[, c("SEQN",
                                     "PAQ605",
                                     "PAQ610",
                                     "PAD615",
                                     "PAQ620",
                                     "PAQ625",
                                     "PAD630",
                                     "PAQ635",
                                     "PAQ640",
                                     "PAD645",
                                     "PAQ650",
                                     "PAQ655",
                                     "PAD660",
                                     "PAQ665",
                                     "PAQ670",
                                     "PAD675",
                                     "PAD680")]

# cardiovascular health -------------------------------------------------------------------------
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/CDQ_I.XPT", tf <- tempfile(), mode="wb")
CVD_15 <- foreign::read.xport(tf)[, c("SEQN",
                                      "CDQ010",
                                      "CDQ001")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/CDQ_J.XPT", tf <- tempfile(), mode="wb")
CVD_17 <- foreign::read.xport(tf)[, c("SEQN",
                                      "CDQ010",
                                      "CDQ001")]

# Diabetes -------------------------------------------------------------------------
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DIQ_I.XPT", tf <- tempfile(), mode="wb")
DM_15 <- foreign::read.xport(tf)[, c("SEQN",
                                     "DIQ010",
                                     "DIQ050")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DIQ_J.XPT", tf <- tempfile(), mode="wb")
DM_17 <- foreign::read.xport(tf)[, c("SEQN",
                                     "DIQ010",
                                     "DIQ050")]

# Prescription Medications --------------------------------------------------------------------
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/RXQ_RX_I.XPT", tf <- tempfile(), mode="wb")
DRUGS_15 <- foreign::read.xport(tf)[, c("SEQN",
                                        "RXDUSE",
                                        "RXDCOUNT")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/RXQ_RX_J.XPT", tf <- tempfile(), mode="wb")
DRUGS_17 <- foreign::read.xport(tf)[, c("SEQN",
                                        "RXDUSE",
                                        "RXDCOUNT")]

# Smoking - Cigarret use ---------------------------------------------------------------------
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/SMQ_I.XPT", tf <- tempfile(), mode="wb")
SMOKING_15 <- foreign::read.xport(tf)[, c("SEQN",
                                          "SMQ020",
                                          "SMD030",
                                          "SMQ040",
                                          "SMQ050Q",
                                          "SMD641",
                                          "SMD650")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/SMQ_J.XPT", tf <- tempfile(), mode="wb")
SMOKING_17 <- foreign::read.xport(tf)[, c("SEQN",
                                          "SMQ020",
                                          "SMD030",
                                          "SMQ040",
                                          "SMQ050Q",
                                          "SMD641",
                                          "SMD650")]

# Medical Conditions -------------------------------------------------------------------------
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/MCQ_I.XPT", tf <- tempfile(), mode="wb")
MEDCOND_15 <- foreign::read.xport(tf)[, c("SEQN",
                                          "MCQ160B",
                                          "MCQ160C",
                                          "MCQ160D",
                                          "MCQ160E",
                                          "MCQ160F",
                                          "MCQ160O",
                                          "MCQ080",
                                          "MCQ160L",
                                          "MCQ220")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/MCQ_J.XPT", tf <- tempfile(), mode="wb")
MEDCOND_17 <- foreign::read.xport(tf)[, c("SEQN",
                                          "MCQ160B",
                                          "MCQ160C",
                                          "MCQ160D",
                                          "MCQ160E",
                                          "MCQ160F",
                                          "MCQ160O",
                                          "MCQ080",
                                          "MCQ160L",
                                          "MCQ220")]
# Kidney Conditions  -------------------------------------------------------------------------
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/KIQ_U_I.XPT", tf <- tempfile(), mode="wb")
KIDNEY_15 <- foreign::read.xport(tf)[, c("SEQN",
                                         "KIQ022")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/KIQ_U_J.XPT", tf <- tempfile(), mode="wb")
KIDNEY_17 <- foreign::read.xport(tf)[, c("SEQN",
                                         "KIQ022")]


# Dietary Interview Day1 -------------------------------------------------------------------------
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1TOT_I.XPT", tf <- tempfile(), mode="wb")
DIETD1_15 <- foreign::read.xport(tf)[, c("SEQN",
                                         "WTDRD1",
                                         "DR1TKCAL",
                                         "DR1TPROT",
                                         "DR1TCARB",
                                         "DR1TSUGR",
                                         "DR1TFIBE",
                                         "DR1TTFAT",
                                         "DR1TSFAT",
                                         "DR1TMFAT",
                                         "DR1TPFAT",
                                         "DR1TCHOL")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1TOT_J.XPT", tf <- tempfile(), mode="wb")
DIETD1_17 <- foreign::read.xport(tf)[, c("SEQN",
                                         "WTDRD1",
                                         "DR1TKCAL",
                                         "DR1TPROT",
                                         "DR1TCARB",
                                         "DR1TSUGR",
                                         "DR1TFIBE",
                                         "DR1TTFAT",
                                         "DR1TSFAT",
                                         "DR1TMFAT",
                                         "DR1TPFAT",
                                         "DR1TCHOL")]


# Dietary Interview Day2 -------------------------------------------------------------------------
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR2TOT_I.XPT", tf <- tempfile(), mode="wb")
DIETD2_15 <- foreign::read.xport(tf)[, c("SEQN",
                                         "WTDR2D",
                                         "DR2TKCAL",
                                         "DR2TPROT",
                                         "DR2TCARB",
                                         "DR2TSUGR",
                                         "DR2TFIBE",
                                         "DR2TTFAT",
                                         "DR2TSFAT",
                                         "DR2TMFAT",
                                         "DR2TPFAT",
                                         "DR2TCHOL")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR2TOT_J.XPT", tf <- tempfile(), mode="wb")
DIETD2_17 <- foreign::read.xport(tf)[, c("SEQN",
                                         "WTDR2D",
                                         "DR2TKCAL",
                                         "DR2TPROT",
                                         "DR2TCARB",
                                         "DR2TSUGR",
                                         "DR2TFIBE",
                                         "DR2TTFAT",
                                         "DR2TSFAT",
                                         "DR2TMFAT",
                                         "DR2TPFAT",
                                         "DR2TCHOL")]

# Body measurements -------------------------------------------------------------------------
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT", tf <- tempfile(), mode="wb")
BODY_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT", tf <- tempfile(), mode="wb")
BODY_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT")]

# Blood Pressure  -------------------------------------------------------------------------
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BPX_I.XPT", tf <- tempfile(), mode="wb")
BLOODPRESS_15 <- foreign::read.xport(tf)[, c("SEQN",
                                             "BPXSY1",
                                             "BPXDI1",
                                             "BPXSY2",
                                             "BPXDI2",
                                             "BPXSY3",
                                             "BPXDI3")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BPX_J.XPT", tf <- tempfile(), mode="wb")
BLOODPRESS_17 <- foreign::read.xport(tf)[, c("SEQN",
                                             "BPXSY1",
                                             "BPXDI1",
                                             "BPXSY2",
                                             "BPXDI2",
                                             "BPXSY3",
                                             "BPXDI3")]

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

df_bruto <- HOSPITAL_DEMO_PA_CVD_DM_DRUGS_SMOKING_MEDCOND_KIDNEY_DIETD1_DIETD2_BODY_BLOODPRESS

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
        !is.na(PA_CLASS) &
        !is.na(AGE_CLASS) &
        !is.na(RIDRETH1) &
        !is.na(POLYPHARM) &
        !is.na(MULT_COMORB)&
        PAQ610 < 8 &
        PAD615 < 841 &
        PAQ625 < 8 &
        PAD630 < 841 &
        PAQ640 < 8 &
        PAD645 < 661 &
        PAQ655 < 7 &
        PAD660 < 481 &
        PAQ670 < 8 &
        PAD675 < 540
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
