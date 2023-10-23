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
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1IFF_I.XPT", tf <- tempfile(), mode="wb")
DIETD1_15 <- foreign::read.xport(tf)[, c("SEQN",
                                         "WTDRD1",
                                         "DR1IGRMS",
                                         "DR1IKCAL",
                                         "DR1IPROT",
                                         "DR1ICARB",
                                         "DR1ISUGR",
                                         "DR1IFIBE",
                                         "DR1ITFAT",
                                         "DR1ISFAT",
                                         "DR1IMFAT",
                                         "DR1IPFAT",
                                         "DR1ICHOL")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1IFF_J.XPT", tf <- tempfile(), mode="wb")
DIETD1_17 <- foreign::read.xport(tf)[, c("SEQN",
                                         "WTDRD1",
                                         "DR1IGRMS",
                                         "DR1IKCAL",
                                         "DR1IPROT",
                                         "DR1ICARB",
                                         "DR1ISUGR",
                                         "DR1IFIBE",
                                         "DR1ITFAT",
                                         "DR1ISFAT",
                                         "DR1IMFAT",
                                         "DR1IPFAT",
                                         "DR1ICHOL")]


# Dietary Interview Day2 -------------------------------------------------------------------------
# 15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR2IFF_I.XPT", tf <- tempfile(), mode="wb")
DIETD2_15 <- foreign::read.xport(tf)[, c("SEQN",
                                         "WTDR2D",
                                         "DR2IGRMS",
                                         "DR2IKCAL",
                                         "DR2IPROT",
                                         "DR2ICARB",
                                         "DR2ISUGR",
                                         "DR2IFIBE",
                                         "DR2ITFAT",
                                         "DR2ISFAT",
                                         "DR2IMFAT",
                                         "DR2IPFAT",
                                         "DR2ICHOL")]

# 17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR2IFF_J.XPT", tf <- tempfile(), mode="wb")
DIETD2_17 <- foreign::read.xport(tf)[, c("SEQN",
                                         "WTDR2D",
                                         "DR2IGRMS",
                                         "DR2IKCAL",
                                         "DR2IPROT",
                                         "DR2ICARB",
                                         "DR2ISUGR",
                                         "DR2IFIBE",
                                         "DR2ITFAT",
                                         "DR2ISFAT",
                                         "DR2IMFAT",
                                         "DR2IPFAT",
                                         "DR2ICHOL")]

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

KIDNEY |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

DIETD1 <- dplyr::bind_rows(DIETD1_15,
                           DIETD1_17)

DIETD1 |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

DIETD2 <- dplyr::bind_rows(DIETD2_15,
                           DIETD2_17)

DIETD2 |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

BODY <- dplyr::bind_rows(BODY_15,
                         BODY_17)

BODY |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

BLOODPRESS <- dplyr::bind_rows(BLOODPRESS_15,
                               BLOODPRESS_17)

BLOODPRESS |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"


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
