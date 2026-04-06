# 🏥 Association between Physical Disability and Hospitalization in Older Adults

📊 Repository with **R** scripts for data preparation and statistical analyses examining the association between **physical disability** and **hospitalization** in older adults using **NHANES 2009–2018** data.

---

## 📖 About the project

This repository contains the analytical workflow for the study:

**Porto G, Gil S, Ferriolli E, Gualano B, Roschel H.**  
*Physical disability is associated with increased odds for hospitalization in older adults: analysis from NHANES 2009–2018.*  
**Brazilian Journal of Physical Therapy.** 2025;29:101254.

The main aim was to investigate whether **physical disability** is associated with:

- higher odds of **hospitalization** in the last 12 months;
- higher odds of **frequent hospitalizations** among older adults.

---

## 🎯 Study objective

To assess whether physical disability predicts:

1. **hospitalization** (yes/no);
2. **frequency of hospitalizations** in the previous year.

---

## 🧾 Study design

This is a **cross-sectional** study based on the **National Health and Nutrition Examination Survey (NHANES)**, including the cycles:

- 2009–2010
- 2011–2012
- 2013–2014
- 2015–2016
- 2017–2018

Participants were adults aged **65 years or older**.

### 🧍 Exposure
**Physical disability**, assessed using a 4-item physical functioning questionnaire covering difficulty in:

- walking from one room to another on the same level;
- rising from an armless chair;
- eating;
- dressing.

Participants were classified as having physical disability if they reported **any difficulty in at least one of these activities**.

### 🏥 Outcomes
- **Hospitalization** in the previous 12 months (yes/no)
- **Frequency of hospitalizations** in the previous 12 months (≤3 vs. ≥4)

### ⚙️ Covariates
Adjusted models included:

- age;
- sex;
- race/ethnicity;
- number of medications;
- number of comorbidities;
- poverty index.

---

## 📈 Main findings

A total of **4,346 participants** were included in the study. The main findings showed that older adults with physical disability had:

- **higher odds of hospitalization**  
  **adjusted OR = 2.13**; 95% CI: 1.74–2.62

- **higher odds of 4 or more hospitalizations within one year**  
  **adjusted OR = 5.81**; 95% CI: 2.40–14.05

These findings reinforce the relevance of physical disability as an important marker of clinical vulnerability in older adults.

---

## 🗂️ Repository structure

```text
nhanes_hospitalization/
├── README.md
├── analysis_hosp.R
├── analysis_num_hosp.R
├── data_preparation.R
├── df.csv
└── nhanes_hospitalization.Rproj
```

### 📌 File description

- `data_preparation.R`  
  Script for data cleaning, preparation, and construction of analytical variables.

- `analysis_hosp.R`  
  Main analysis script for hospitalization (yes/no).

- `analysis_num_hosp.R`  
  Secondary analysis script for hospitalization frequency.

- `df.csv`  
  Analytical dataset used in the project.

- `nhanes_hospitalization.Rproj`  
  Project file for use in RStudio.

---

## 💻 Technologies used

- ![R](https://img.shields.io/badge/R-gray?style=flat&logo=r&logoColor=white)

- Packages for data manipulation, survey analysis, and regression modeling.

---

## 🧬 Dataset

This project uses data from the **National Health and Nutrition Examination Survey (NHANES)**, a nationally representative survey conducted by the **National Center for Health Statistics (NCHS/CDC)** to assess the health and nutritional status of the United States population.

---

## ▶️ How to use

### 1. Clone the repository

```bash
git clone https://github.com/saulosgil/nhanes_hospitalization.git
```

### 2. Open the project in RStudio

Open:

```r
nhanes_hospitalization.Rproj
```

### 3. Run the scripts

A recommended order is:

```r
source("data_preparation.R")
source("analysis_hosp.R")
source("analysis_num_hosp.R")
```

---

## 🔎 Reproducibility

To reproduce the analyses:

- use **RStudio** with the `.Rproj` file;
- make sure all required packages are installed;
- run the scripts following the analytical workflow;
- confirm that `df.csv` is available in the project directory.

---

## 📚 Reference

If you use this repository, please cite:

```bibtex
@article{porto2025physical,
  title={Physical disability is associated with increased odds for hospitalization in older adults: analysis from NHANES 2009--2018},
  author={Porto, Guilherme and Gil, Saulo and Ferriolli, Eduardo and Gualano, Bruno and Roschel, Hamilton},
  journal={Brazilian Journal of Physical Therapy},
  volume={29},
  pages={101254},
  year={2025},
  doi={10.1016/j.bjpt.2025.101254}
}
```

---

## 🔗 Useful links

- 📄 Repository: `saulosgil/nhanes_hospitalization`
- 👨‍💻 GitHub: [@saulosgil](https://github.com/saulosgil)

---

## 🌟 Final note

This repository was developed to promote **transparency**, **reproducibility**, and **open science** in epidemiological research using population-based data, contributing to a better understanding of the impact of physical disability on hospitalization in older adults.

# 

👨‍💻 Made by Saulo Gil.