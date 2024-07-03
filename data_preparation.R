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
                                                   "PFQ061L")]
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

# Diabetes ---------------------------------------------------------------------------
# 09-10
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DIQ_F.XPT", tf <- tempfile(), mode="wb")
DIABETES_10 <- foreign::read.xport(tf)[, c("SEQN",
                                           "DIQ010",
                                           "DIQ050")]
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DIQ_G.XPT", tf <- tempfile(), mode="wb")
DIABETES_12 <- foreign::read.xport(tf)[, c("SEQN",
                                           "DIQ010",
                                           "DIQ050")]
# 13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DIQ_H.XPT", tf <- tempfile(), mode="wb")
DIABETES_14 <- foreign::read.xport(tf)[, c("SEQN",
                                           "DIQ010",
                                           "DIQ050")]
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DIQ_I.XPT", tf <- tempfile(), mode="wb")
DIABETES_16 <- foreign::read.xport(tf)[, c("SEQN",
                                           "DIQ010",
                                           "DIQ050")]
# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DIQ_J.XPT", tf <- tempfile(), mode="wb")
DIABETES_18 <- foreign::read.xport(tf)[, c("SEQN",
                                           "DIQ010",
                                           "DIQ050")]

# Prescription Medications  ---------------------------------------------------------------------------
# 09-10
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/RXQ_RX_F.XPT", tf <- tempfile(), mode="wb")
MEDICATIONS_10 <- foreign::read.xport(tf)[, c("SEQN",
                                              "RXDUSE",
                                              "RXDCOUNT")]
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/RXQ_RX_G.XPT", tf <- tempfile(), mode="wb")
MEDICATIONS_12 <- foreign::read.xport(tf)[, c("SEQN",
                                              "RXDUSE",
                                              "RXDCOUNT")]
# 13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/RXQ_RX_H.XPT", tf <- tempfile(), mode="wb")
MEDICATIONS_14 <- foreign::read.xport(tf)[, c("SEQN",
                                              "RXDUSE",
                                              "RXDCOUNT")]
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/RXQ_RX_I.XPT", tf <- tempfile(), mode="wb")
MEDICATIONS_16 <- foreign::read.xport(tf)[, c("SEQN",
                                              "RXDUSE",
                                              "RXDCOUNT")]
# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/RXQ_RX_J.XPT", tf <- tempfile(), mode="wb")
MEDICATIONS_18 <- foreign::read.xport(tf)[, c("SEQN",
                                              "RXDUSE",
                                              "RXDCOUNT")]

# Smoking – Cigarret use   ---------------------------------------------------------------------------
# 09-10
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/SMQ_F.XPT", tf <- tempfile(), mode="wb")
SMOKING_10 <- foreign::read.xport(tf)[, c("SEQN",
                                          "SMQ020",
                                          "SMD030",
                                          "SMQ020",
                                          "SMQ040",
                                          "SMQ050Q",
                                          "SMD641",
                                          "SMD650")]
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/SMQ_G.XPT", tf <- tempfile(), mode="wb")
SMOKING_12 <- foreign::read.xport(tf)[, c("SEQN",
                                          "SMQ020",
                                          "SMD030",
                                          "SMQ020",
                                          "SMQ040",
                                          "SMQ050Q",
                                          "SMD641",
                                          "SMD650")]
# 13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/SMQ_H.XPT", tf <- tempfile(), mode="wb")
SMOKING_14 <- foreign::read.xport(tf)[, c("SEQN",
                                          "SMQ020",
                                          "SMD030",
                                          "SMQ020",
                                          "SMQ040",
                                          "SMQ050Q",
                                          "SMD641",
                                          "SMD650")]
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/SMQ_I.XPT", tf <- tempfile(), mode="wb")
SMOKING_16 <- foreign::read.xport(tf)[, c("SEQN",
                                          "SMQ020",
                                          "SMD030",
                                          "SMQ020",
                                          "SMQ040",
                                          "SMQ050Q",
                                          "SMD641",
                                          "SMD650")]
# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/SMQ_J.XPT", tf <- tempfile(), mode="wb")
SMOKING_18 <- foreign::read.xport(tf)[, c("SEQN",
                                          "SMQ020",
                                          "SMD030",
                                          "SMQ020",
                                          "SMQ040",
                                          "SMQ050Q",
                                          "SMD641",
                                          "SMD650")]

# Medical Conditions  ---------------------------------------------------------------------------
# 09-10
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/MCQ_F.XPT", tf <- tempfile(), mode="wb")
MED_CONDITIONS_10 <- foreign::read.xport(tf)[, c("SEQN",
                                                 "MCQ160B",
                                                 "MCQ160C",
                                                 "MCQ160D",
                                                 "MCQ160E",
                                                 "MCQ160F",
                                                 "MCQ080",
                                                 "MCQ160L",
                                                 "MCQ220")]
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/MCQ_G.XPT", tf <- tempfile(), mode="wb")
MED_CONDITIONS_12 <- foreign::read.xport(tf)[, c("SEQN",
                                                 "MCQ160B",
                                                 "MCQ160C",
                                                 "MCQ160D",
                                                 "MCQ160E",
                                                 "MCQ160F",
                                                 "MCQ080",
                                                 "MCQ160L",
                                                 "MCQ220")]
# 13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/MCQ_H.XPT", tf <- tempfile(), mode="wb")
MED_CONDITIONS_14 <- foreign::read.xport(tf)[, c("SEQN",
                                                 "MCQ160B",
                                                 "MCQ160C",
                                                 "MCQ160D",
                                                 "MCQ160E",
                                                 "MCQ160F",
                                                 "MCQ080",
                                                 "MCQ160L",
                                                 "MCQ220")]
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/MCQ_I.XPT", tf <- tempfile(), mode="wb")
MED_CONDITIONS_16 <- foreign::read.xport(tf)[, c("SEQN",
                                                 "MCQ160B",
                                                 "MCQ160C",
                                                 "MCQ160D",
                                                 "MCQ160E",
                                                 "MCQ160F",
                                                 "MCQ080",
                                                 "MCQ160L",
                                                 "MCQ220")]
# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/MCQ_J.XPT", tf <- tempfile(), mode="wb")
MED_CONDITIONS_18 <- foreign::read.xport(tf)[, c("SEQN",
                                                 "MCQ160B",
                                                 "MCQ160C",
                                                 "MCQ160D",
                                                 "MCQ160E",
                                                 "MCQ160F",
                                                 "MCQ080",
                                                 "MCQ160L",
                                                 "MCQ220")]

# Kidney Conditions   ---------------------------------------------------------------------------
# 09-10
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/KIQ_U_F.XPT", tf <- tempfile(), mode="wb")
KIDNEY_10 <- foreign::read.xport(tf)[, c("SEQN",
                                         "KIQ022")]
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/KIQ_U_G.XPT", tf <- tempfile(), mode="wb")
KIDNEY_12 <- foreign::read.xport(tf)[, c("SEQN",
                                         "KIQ022")]
# 13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/KIQ_U_H.XPT", tf <- tempfile(), mode="wb")
KIDNEY_14 <- foreign::read.xport(tf)[, c("SEQN",
                                         "KIQ022")]
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/KIQ_U_I.XPT", tf <- tempfile(), mode="wb")
KIDNEY_16 <- foreign::read.xport(tf)[, c("SEQN",
                                         "KIQ022")]
# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/KIQ_U_J.XPT", tf <- tempfile(), mode="wb")
KIDNEY_18 <- foreign::read.xport(tf)[, c("SEQN",
                                         "KIQ022")]

# Body measurements   ---------------------------------------------------------------------------
# 09-10
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BMX_F.XPT", tf <- tempfile(), mode="wb")
BODY_MEASURES_10 <- foreign::read.xport(tf)[, c("SEQN",
                                                "BMXWT",
                                                "BMXHT",
                                                "BMXBMI",
                                                "BMXWAIST")]
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BMX_G.XPT", tf <- tempfile(), mode="wb")
BODY_MEASURES_12 <- foreign::read.xport(tf)[, c("SEQN",
                                                "BMXWT",
                                                "BMXHT",
                                                "BMXBMI",
                                                "BMXWAIST")]
# 13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BMX_H.XPT", tf <- tempfile(), mode="wb")
BODY_MEASURES_14 <- foreign::read.xport(tf)[, c("SEQN",
                                                "BMXWT",
                                                "BMXHT",
                                                "BMXBMI",
                                                "BMXWAIST")]
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT", tf <- tempfile(), mode="wb")
BODY_MEASURES_16 <- foreign::read.xport(tf)[, c("SEQN",
                                                "BMXWT",
                                                "BMXHT",
                                                "BMXBMI",
                                                "BMXWAIST")]
# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT", tf <- tempfile(), mode="wb")
BODY_MEASURES_18 <- foreign::read.xport(tf)[, c("SEQN",
                                                "BMXWT",
                                                "BMXHT",
                                                "BMXBMI",
                                                "BMXWAIST")]

# Blood Pressure ---------------------------------------------------------------------------
# 09-10
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/BPX_F.XPT", tf <- tempfile(), mode="wb")
BLOOD_PRESSURE_10 <- foreign::read.xport(tf)[, c("SEQN",
                                                 "BPXSY1",
                                                 "BPXDI1",
                                                 "BPXSY2",
                                                 "BPXDI2",
                                                 "BPXSY3",
                                                 "BPXDI3")]
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BPX_G.XPT", tf <- tempfile(), mode="wb")
BLOOD_PRESSURE_12 <- foreign::read.xport(tf)[, c("SEQN",
                                                 "BPXSY1",
                                                 "BPXDI1",
                                                 "BPXSY2",
                                                 "BPXDI2",
                                                 "BPXSY3",
                                                 "BPXDI3")]
# 13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BPX_H.XPT", tf <- tempfile(), mode="wb")
BLOOD_PRESSURE_14 <- foreign::read.xport(tf)[, c("SEQN",
                                                 "BPXSY1",
                                                 "BPXDI1",
                                                 "BPXSY2",
                                                 "BPXDI2",
                                                 "BPXSY3",
                                                 "BPXDI3")]
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BPX_I.XPT", tf <- tempfile(), mode="wb")
BLOOD_PRESSURE_16 <- foreign::read.xport(tf)[, c("SEQN",
                                                 "BPXSY1",
                                                 "BPXDI1",
                                                 "BPXSY2",
                                                 "BPXDI2",
                                                 "BPXSY3",
                                                 "BPXDI3")]
# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BPX_J.XPT", tf <- tempfile(), mode="wb")
BLOOD_PRESSURE_18 <- foreign::read.xport(tf)[, c("SEQN",
                                                 "BPXSY1",
                                                 "BPXDI1",
                                                 "BPXSY2",
                                                 "BPXDI2",
                                                 "BPXSY3",
                                                 "BPXDI3")]

# Append Files ---------------------------------------------------------------------------------
# Append Files
DEMO <- dplyr::bind_rows(DEMO_10,
                         DEMO_12,
                         DEMO_14,
                         DEMO_16,
                         DEMO_18)
DEMO |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

HOSPITAL <- dplyr::bind_rows(HOSPITAL_10,
                             HOSPITAL_12,
                             HOSPITAL_14,
                             HOSPITAL_16,
                             HOSPITAL_18)

HOSPITAL |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

PHYSICAL_FUCTION <- dplyr::bind_rows(PHYSICAL_FUCTION_10,
                                     PHYSICAL_FUCTION_12,
                                     PHYSICAL_FUCTION_14,
                                     PHYSICAL_FUCTION_16,
                                     PHYSICAL_FUCTION_18)

PHYSICAL_FUCTION |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

DIABETES <- dplyr::bind_rows(DIABETES_10,
                             DIABETES_12,
                             DIABETES_14,
                             DIABETES_16,
                             DIABETES_18)

DIABETES |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

MEDICATIONS <- dplyr::bind_rows(MEDICATIONS_10,
                                MEDICATIONS_12,
                                MEDICATIONS_14,
                                MEDICATIONS_16,
                                MEDICATIONS_18)

MEDICATIONS |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

SMOKING <- dplyr::bind_rows(SMOKING_10,
                            SMOKING_12,
                            SMOKING_14,
                            SMOKING_16,
                            SMOKING_18)

SMOKING |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

MED_CONDITIONS <- dplyr::bind_rows(MED_CONDITIONS_10,
                                   MED_CONDITIONS_12,
                                   MED_CONDITIONS_14,
                                   MED_CONDITIONS_16,
                                   MED_CONDITIONS_18)

MED_CONDITIONS |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

KIDNEY <- dplyr::bind_rows(KIDNEY_10,
                           KIDNEY_12,
                           KIDNEY_14,
                           KIDNEY_16,
                           KIDNEY_18)

KIDNEY |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

BODY_MEASURES <- dplyr::bind_rows(BODY_MEASURES_10,
                                  BODY_MEASURES_12,
                                  BODY_MEASURES_14,
                                  BODY_MEASURES_16,
                                  BODY_MEASURES_18)

BODY_MEASURES |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

BLOOD_PRESSURE <- dplyr::bind_rows(BLOOD_PRESSURE_10,
                                   BLOOD_PRESSURE_12,
                                   BLOOD_PRESSURE_14,
                                   BLOOD_PRESSURE_16,
                                   BLOOD_PRESSURE_18)

BLOOD_PRESSURE |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

# Merge HOSPITAL and DEMO files
HOSPITAL_DEMO <-
  dplyr::left_join(DEMO, HOSPITAL, by="SEQN")

# Merge HOSPITAL_DEMO and PHYSICAL_FUCTION
HOSPITAL_DEMO_PHYSICAL_FUCTION <-
  dplyr::left_join(HOSPITAL_DEMO, PHYSICAL_FUCTION, by="SEQN")

# Merge HOSPITAL_DEMO_PHYSICAL_FUCTION and DIABETES
HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES <-
  dplyr::left_join(HOSPITAL_DEMO_PHYSICAL_FUCTION, DIABETES, by="SEQN")

# Merge HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES and MEDICATIONS
HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS <-
  dplyr::left_join(HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES, MEDICATIONS, by="SEQN")

# Merge HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS and SMOKING
HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING <-
  dplyr::left_join(HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS, SMOKING, by="SEQN")

# Merge HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING and MED_CONDITIONS
HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING_MED_CONDITIONS <-
  dplyr::left_join(HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING, MED_CONDITIONS, by="SEQN")

# Merge HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING_MED_CONDITIONS and KIDNEY
HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING_MED_CONDITIONS_KIDNEY <-
  dplyr::left_join(HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING_MED_CONDITIONS, KIDNEY, by="SEQN")

# Merge HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING_MED_CONDITIONS_KIDNEY and BODY_MEASURES
HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING_MED_CONDITIONS_KIDNEY_BODY_MEASURES <-
  dplyr::left_join(HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING_MED_CONDITIONS_KIDNEY, BODY_MEASURES, by="SEQN")

# Merge HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING_MED_CONDITIONS_KIDNEY_BODY_MEASURES and BLOOD_PRESSURE
HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING_MED_CONDITIONS_KIDNEY_BODY_MEASURES_BLOOD_PRESSURE <-
  dplyr::left_join(HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING_MED_CONDITIONS_KIDNEY_BODY_MEASURES, BLOOD_PRESSURE, by="SEQN")

# MERGED DATASET  
df_bruto <- HOSPITAL_DEMO_PHYSICAL_FUCTION_DIABETES_MEDICATIONS_SMOKING_MED_CONDITIONS_KIDNEY_BODY_MEASURES_BLOOD_PRESSURE

df <- df_bruto |>
  dplyr::distinct(SEQN, .keep_all = TRUE) # removing duplicate rows

# Salve data.frame to analyse
# readr::write_csv2(x = df, file = "df.csv") # deixar comentado para salvar

# Reading dataset --------------------------------------------------------------------------------
df <- read.csv2(file = "df.csv")