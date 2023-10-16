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
library(GGally)
library(sjPlot)
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
                                       "WTMEC2YR"
                                       )]

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
                                       "WTMEC2YR")]

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
                                      "CDQ020")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/CDQ_J.XPT", tf <- tempfile(), mode="wb")
CVD_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# Alcohol use -------------------------------------------------------------------------
# 15-16
download.file("", tf <- tempfile(), mode="wb")
DEMO_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# 17-18
download.file("", tf <- tempfile(), mode="wb")
DEMO_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# Diabetes -------------------------------------------------------------------------
# 15-16
download.file("", tf <- tempfile(), mode="wb")
DEMO_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# 17-18
download.file("", tf <- tempfile(), mode="wb")
DEMO_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# Prescription Medications --------------------------------------------------------------------
# 15-16
download.file("", tf <- tempfile(), mode="wb")
DEMO_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# 17-18
download.file("", tf <- tempfile(), mode="wb")
DEMO_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# Smoking - Cigarret use ---------------------------------------------------------------------
# 15-16
download.file("", tf <- tempfile(), mode="wb")
DEMO_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# 17-18
download.file("", tf <- tempfile(), mode="wb")
DEMO_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# Medical Conditions -------------------------------------------------------------------------
# 15-16
download.file("", tf <- tempfile(), mode="wb")
DEMO_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# 17-18
download.file("", tf <- tempfile(), mode="wb")
DEMO_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# Kidney Conditions  -------------------------------------------------------------------------
# 15-16
download.file("", tf <- tempfile(), mode="wb")
DEMO_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# 17-18
download.file("", tf <- tempfile(), mode="wb")
DEMO_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]


# Dietary Interview Day1 -------------------------------------------------------------------------
# 15-16
download.file("", tf <- tempfile(), mode="wb")
DEMO_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# 17-18
download.file("", tf <- tempfile(), mode="wb")
DEMO_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# Dietary Interview Day2 -------------------------------------------------------------------------
# 15-16
download.file("", tf <- tempfile(), mode="wb")
DEMO_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# 17-18
download.file("", tf <- tempfile(), mode="wb")
DEMO_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# Body measurements -------------------------------------------------------------------------
# 15-16
download.file("", tf <- tempfile(), mode="wb")
DEMO_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# 17-18
download.file("", tf <- tempfile(), mode="wb")
DEMO_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# Blood Pressure  -------------------------------------------------------------------------
# 15-16
download.file("", tf <- tempfile(), mode="wb")
DEMO_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# 17-18
download.file("", tf <- tempfile(), mode="wb")
DEMO_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "",
                                       "WTINT2YR",
                                       "WTMEC2YR")]

# Append Files ---------------------------------------------------------------------------------
# Append Files
DEMO <- dplyr::bind_rows(DEMO_99,
                         DEMO_01,
                         DEMO_03,
                         DEMO_05)

DEMO |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"


DIET <-  dplyr::bind_rows(DIET_99,
                          DIET_01,
                          DIET_03,
                          DIET_05)

DIET |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

BODY <-  dplyr::bind_rows(BODY_99,
                          BODY_01,
                          BODY_03,
                          BODY_05)

BODY |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

QUEST <- dplyr::bind_rows(Q_99,
                          Q_01,
                          Q_03,
                          Q_05)

QUEST |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

ATV <- dplyr::bind_rows(ATV_99,
                        ATV_01,
                        ATV_03,
                        ATV_05)

ATV |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

DM <- dplyr::bind_rows(DM_99,
                       DM_01,
                       DM_03,
                       DM_05)

DM |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

HAS <- dplyr::bind_rows(HAS_99,
                        HAS_01,
                        HAS_03,
                        HAS_05)

HAS |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

CVD_CANCER <- dplyr::bind_rows(CVD_CANCER_99,
                               CVD_CANCER_01,
                               CVD_CANCER_03,
                               CVD_CANCER_05)

CVD_CANCER |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

# Merge DEMO and DIET files

DEMO_DIET <-
  dplyr::left_join(DEMO, DIET, by="SEQN")

# Merge DEMO_DIET and BODY

DEMO_DIET_BODY <-
  dplyr::left_join(DEMO_DIET, BODY, by="SEQN")

# Merge DEMO_DIET_BODY and QUEST

DEMO_DIET_BODY_QUEST <-
  dplyr::left_join(DEMO_DIET_BODY, QUEST, by="SEQN")

# Merge DEMO_DIET_BODY_QUEST and ATV

DEMO_DIET_BODY_QUEST_ATV <-
  dplyr::left_join(DEMO_DIET_BODY_QUEST, ATV, by="SEQN")

# Merge DEMO_DIET_BODY_QUEST_ATV and DM

DEMO_DIET_BODY_QUEST_ATV_DM <-
  dplyr::left_join(DEMO_DIET_BODY_QUEST_ATV, DM, by="SEQN")

# Merge DEMO_DIET_BODY_QUEST_ATV_DM and HAS

DEMO_DIET_BODY_QUEST_ATV_DM_HAS <-
  dplyr::left_join(DEMO_DIET_BODY_QUEST_ATV_DM, HAS, by="SEQN")

# Merge DEMO_DIET_BODY_QUEST_ATV_DM_HAS and CVD_CANCER

DEMO_DIET_BODY_QUEST_ATV_DM_HAS_CVD_CANCER <-
  dplyr::left_join(DEMO_DIET_BODY_QUEST_ATV_DM_HAS, CVD_CANCER, by="SEQN")


df_bruto <- DEMO_DIET_BODY_QUEST_ATV_DM_HAS_CVD_CANCER

df <- df_bruto |>
  dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "YES" NEED TREATMENT!

# Created new variables -----------------------------------------------------------------------

###### salvando data.frame para explorar

readr::write_csv2(x = df, file = "df.csv")

# Lendo a base --------------------------------------------------------------------------------

df <- read.csv2(file = "df.csv")

# DataPrep ------------------------------------------------------------------------------------

One_1 <-
  df |>
  # input 0 in NA due to different variable names
  dplyr::mutate(DRXTPROT = tidyr::replace_na(DRXTPROT, 0),
                DR1TPROT = tidyr::replace_na(DR1TPROT, 0),
                DR1TCARB = tidyr::replace_na(DR1TCARB, 0),
                DRXTCARB = tidyr::replace_na(DRXTCARB, 0),
                DRXTKCAL = tidyr::replace_na(DRXTKCAL, 0),
                DR1TKCAL = tidyr::replace_na(DR1TKCAL, 0),
                DRXTTFAT = tidyr::replace_na(DRXTTFAT, 0),
                DR1TTFAT = tidyr::replace_na(DR1TTFAT, 0),
                DRXTCALC = tidyr::replace_na(DRXTCALC, 0),
                DR1TCALC = tidyr::replace_na(DR1TCALC, 0),
                PFQ060A = tidyr::replace_na(PFQ060A, 0),
                PFQ061A = tidyr::replace_na(PFQ061A, 0),
                PFQ060B = tidyr::replace_na(PFQ060B, 0),
                PFQ061B = tidyr::replace_na(PFQ061B, 0),
                PFQ060C = tidyr::replace_na(PFQ060C, 0),
                PFQ061C = tidyr::replace_na(PFQ061C, 0),
                PFQ060D = tidyr::replace_na(PFQ060D, 0),
                PFQ061D = tidyr::replace_na(PFQ061D, 0),
                PFQ060E = tidyr::replace_na(PFQ060E, 0),
                PFQ061E = tidyr::replace_na(PFQ061E, 0),
                PFQ060F = tidyr::replace_na(PFQ060F, 0),
                PFQ061F = tidyr::replace_na(PFQ061F, 0),
                PFQ060G = tidyr::replace_na(PFQ060G, 0),
                PFQ061G = tidyr::replace_na(PFQ061G, 0),
                PFQ060H = tidyr::replace_na(PFQ060H, 0),
                PFQ061H = tidyr::replace_na(PFQ061H, 0),
                PFQ060I = tidyr::replace_na(PFQ060I, 0),
                PFQ061I = tidyr::replace_na(PFQ061I, 0),
                PFQ060J = tidyr::replace_na(PFQ060J, 0),
                PFQ061J = tidyr::replace_na(PFQ061J, 0),
                PFQ060K = tidyr::replace_na(PFQ060K, 0),
                PFQ061K = tidyr::replace_na(PFQ061K, 0),
                PFQ060L = tidyr::replace_na(PFQ060L, 0),
                PFQ061L = tidyr::replace_na(PFQ061L, 0),
                PADACTIV = tidyr::replace_na(PADACTIV, 0),
                PADLEVEL = tidyr::replace_na(PADLEVEL, 0),
                PADTIMES = tidyr::replace_na(PADTIMES, 0),
                PADDURAT = tidyr::replace_na(PADDURAT, 0),
                PADMETS = tidyr::replace_na(PADMETS, 0),
                DIQ010 = tidyr::replace_na(DIQ010, 0),
                BPQ020 = tidyr::replace_na(BPQ020, 0),
                MCQ160B = tidyr::replace_na(MCQ160B, 0),
                MCQ160C = tidyr::replace_na(MCQ160C, 0),
                MCQ160E = tidyr::replace_na(MCQ160E, 0),
                MCQ220 = tidyr::replace_na(MCQ220, 0)
  )

# adjusting physical function parameters

One_2 <-
  One_1 |>
  dplyr::mutate(MONEY_FUNCTION = PFQ060A + PFQ061A) |>
  dplyr::mutate(WALKING_MILE = PFQ060B + PFQ061B) |>
  dplyr::mutate(WALKING_STEPS = PFQ060C + PFQ061C) |>
  dplyr::mutate(STOOPING = PFQ060D + PFQ061D) |>
  dplyr::mutate(LIFTING = PFQ060E + PFQ061E) |>
  dplyr::mutate(HOUSE_CHORE = PFQ060F + PFQ061F) |>
  dplyr::mutate(PREP_MEALS = PFQ060G + PFQ061G) |>
  dplyr::mutate(WALKING_ROOMS = PFQ060H + PFQ061H) |>
  dplyr::mutate(STANDINGUP = PFQ060I + PFQ061I) |>
  dplyr::mutate(BED_DIFFICULT = PFQ060J + PFQ061J) |>
  dplyr::mutate(EATING = PFQ060K + PFQ061K) |>
  dplyr::mutate(DRESSING = PFQ060L + PFQ061L)

# To create the variable INCAPAZ - PRIMARY OUTCOME
One_3 <-
  One_2 |>
  mutate(WALKING_ROOMS_NOVO = case_when(WALKING_ROOMS == 0 ~ 1000,
                                        WALKING_ROOMS == 1 ~ 0,
                                        WALKING_ROOMS >=2 & WALKING_ROOMS <=4 ~ 1,
                                        WALKING_ROOMS >= 5 & WALKING_ROOMS < 10 ~ 100),
         STANDINGUP_NOVO = case_when(STANDINGUP == 0 ~ 1000,
                                     STANDINGUP == 1 ~ 0,
                                     STANDINGUP >=2 & WALKING_ROOMS <=4 ~ 1,
                                     STANDINGUP >= 5 & STANDINGUP < 10 ~ 100),
         EATING_NOVO = case_when(EATING == 0 ~ 1000,
                                 EATING == 1 ~ 0,
                                 EATING >=2 & EATING <=4 ~ 1,
                                 EATING >= 5 & EATING < 10 ~ 100),
         DRESSING_NOVO = case_when(DRESSING == 0 ~ 1000,
                                   DRESSING == 1 ~ 0,
                                   DRESSING >=2 & WALKING_ROOMS <=4 ~ 1,
                                   DRESSING >= 5 & DRESSING < 10 ~ 100),
         INCAPAZ = WALKING_ROOMS_NOVO + STANDINGUP_NOVO + EATING_NOVO + DRESSING_NOVO,
         INCAPAZ_CLASSE = case_when(INCAPAZ <= 1 ~ 0, # no disability
                                    INCAPAZ > 1 & INCAPAZ <= 16 ~ 1, # disability
                                    INCAPAZ > 12 & INCAPAZ <=300 ~ 3))

# To create the variable METPA

One_4 <-
  One_3 |>
  # calculate HOMA-IR
  dplyr::mutate(METPA = PADTIMES * PADDURAT * PADMETS) |>
  dplyr::mutate(pad_sem =  METPA * 0.2333) |>
  dplyr::mutate(pad_class = case_when(pad_sem >= 450 ~ "ativo",
                                      pad_sem < 450 ~ "inativo"))
# Adjusting sex, age and ethnicity

One_5 <-
  One_4 |>
  mutate(GENDER = case_when(RIAGENDR == 1 ~ "male",
                            RIAGENDR == 2 ~ "female"),
         AGE = case_when(RIDAGEYR >= 65 & RIDAGEYR < 80 ~ "< 80 years",
                         RIDAGEYR >= 80 ~ ">= 80 years"),
         RIDRETH1 = as_factor(RIDRETH1))

# Creating apendicular lean mass, osteoporose and protein uptake variables

One_6 <-
  One_5 |>
  # create apendicular lean mass
  dplyr::mutate(# create obesity class
    OBESITY = case_when(BMXBMI >= 30 ~ "OBESO",
                        BMXBMI < 30 ~ "NORMAL"),
    # create protein consumption variable of distinct assessments
    PTN = DR1TPROT + DRXTPROT,
    # create relative protein consumption variable
    PTNKG = PTN/BMXWT,
    # create CHO consumption variable
    CHO = DR1TCARB + DRXTCARB,
    # create FAT consumption variable
    FAT = DRXTTFAT + DR1TTFAT,
    # create CALCIUM consuption variable
    CAL = DRXTCALC + DR1TCALC,
    # create ENERGY consumption variable
    ENERGY = DRXTKCAL + DR1TKCAL,
    # create RELATIVE ENERGY - ENERGY/KG
    ENERGY_KG = ENERGY/BMXWT,
    # create ENERGY_PT_MODEL
    ENERGY_PT_MODEL = ENERGY - PTN * 4,
    # create energy class for unlikely data
    ENERGY_STATUS = case_when(RIAGENDR == 1 & ENERGY < 800 ~ "UNLIKELY",
                              RIAGENDR == 1 & ENERGY > 4000 ~ "UNLIKELY",
                              RIAGENDR == 1 & ENERGY >= 800 & ENERGY <=4000 ~ "LIKELY",
                              RIAGENDR == 2 & ENERGY < 500 ~ "UNLIKELY",
                              RIAGENDR == 2 & ENERGY > 3500 ~ "UNLIKELY",
                              RIAGENDR == 2 & ENERGY >= 500 & ENERGY <=3500 ~ "LIKELY"),
    # create protein consumption status
    PTN_STATUS = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                           PTNKG >= 0.8 & PTNKG < 1.2 ~ "B_ADEQUADO",
                           PTNKG >= 1.2  & PTNKG < 1.6 ~ "C_MODERADO",
                           PTNKG >= 1.6 ~ "D_ELEVADO"),
    # create pritein consumptoin status RDA
    PTN_RDA = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                        PTNKG >=0.8 ~ "B_ADEQUADO"),
    # Saulo/Hamilton protein consumption status
    PTN_STATUS_ROSCHEL = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                                   PTNKG >= 0.8 & PTNKG < 1.2 ~ "B_ADEQUADO",
                                   PTNKG >= 1.2 ~ "D_ELEVADO"),
    # Peso de 8 anos
    MEC8YR = case_when(SDDSRVYR <= 2 ~ 2/4 * WTMEC4YR,
                       (SDDSRVYR > 2 ~ 1/4 * WTMEC2YR)),
    inAnalysis = (RIDAGEYR >= 65 &
                    !is.na(INCAPAZ_CLASSE) &
                    !is.na(PTN_STATUS) &
                    INCAPAZ_CLASSE != 3 &
                    ENERGY_STATUS == "LIKELY" &
                    DIQ010 < 3 & # Diabetes
                    BPQ020 < 3 & BPQ020 >= 1 & # HAS
                    MCQ160B < 3 & # ICC
                    MCQ160E < 3 # heart attack
    )
  )


# Final database

One <- One_6

#' ## Define survey design
# Define survey design for overall dataset
NHANES_all <- svydesign(data=One, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)

# Create a survey design object for the subset of interest: adults aged 20 and over with a valid depression score
# Subsetting the original survey design object ensures we keep the design information about the number of clusters and strata
NHANES <- subset(NHANES_all, inAnalysis)
