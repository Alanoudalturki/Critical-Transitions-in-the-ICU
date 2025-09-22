# ICU Outcomes Prediction: Prolonged Stay, Readmission, and Mortality  
Retrospective Cohort Analysis using MIMIC-IV (2025)  
Analysis and code conducted by: Alanoud Alturki  
Language & stack: Python 3.11 (pandas, numpy, scikit-learn, matplotlib, seaborn) + R 4.3 (survival, survminer, nnet, ggplot2, pROC)

---

## Project Overview
Critically ill patients often face complications such as prolonged ICU stays, hospital readmissions, and high short-term mortality. Identifying risk factors for these outcomes can guide resource allocation, improve patient management, and inform predictive decision-support tools.  

This project uses the MIMIC-IV v3.1 database to examine patient-level predictors of:  
- Prolonged ICU length of stay (>7 days)  
- 30-day hospital readmission  
- 30-day all-cause mortality  

---

## Objectives
**Primary Objective:**  
- Identify independent predictors of prolonged ICU stay using logistic regression models.  

**Secondary Objectives:**  
- Examine risk factors for 30-day readmission and mortality using multinomial logistic regression.  
- Estimate time-to-death using Cox proportional hazards models.  
- Assess subgroup outcomes for polypharmacy, immunosuppression, and multimorbidity using Kaplan–Meier survival curves.  
- Evaluate model calibration, discrimination, and residual diagnostics.  

---

## Methods
**Study Design:** Retrospective cohort study (N = 65,366 adult ICU patients).  
**Data Source:** MIMIC-IV v3.1, 2008–2019.  

**Inclusion Criteria:**  
- Adults ≥18 years  
- ICU admission recorded in MIMIC-IV  

**Exclusion Criteria:**  
- Missing core demographics (age, sex) or labs (creatinine, sodium, albumin)  
- Duplicate admissions  

**Outcomes Measured:**  
- Prolonged ICU stay (>7 days)  
- 30-day readmission  
- 30-day all-cause mortality  

---

## Statistical Analyses
### Descriptive Statistics  
- Continuous: mean ± SD, median [IQR]  
- Categorical: counts (n) and percentages  

### Group Comparisons  
- Kruskal–Wallis for continuous variables  
- χ² / Fisher’s exact test for categorical variables  

### Survival Analysis  
- Kaplan–Meier curves (comorbidity, immunosuppression, polypharmacy)  
- Log-rank tests  
- Cox proportional hazards models  

### Multivariable Models  
- Logistic regression for prolonged ICU stay  
- Multinomial logistic regression for 30-day outcomes (Alive, Readmitted, Dead)  
- Cox regression for time-to-death  

### Sensitivity Analyses  
- Model diagnostics (residual plots, Q–Q plots)  
- ROC curve analysis for discrimination (AUC)  
- Subgroup validation  

---

## Repository Contents
- `python_scripts/` — preprocessing, feature engineering, and cohort selection  
- `r_scripts/` — survival analysis, logistic regression, multinomial regression  
- `results/tables/` — publication-ready tables (CSV, LaTeX, Excel)  
- `results/figures/` — high-resolution figures (PNG, PDF)  
- `results/reports/` — compiled HTML/PDF analysis reports  
- `README.md` — project documentation  

---

## Tables
- **Table 1.** Overall Cohort Characteristics  
- **Table 2.** Cohort Characteristics by 30-Day Outcome  
- **Table 3.** Cohort Characteristics by ICU Length of Stay (≤7 vs >7 days)  
- **Table 4.** Univariable Logistic Regression – Prolonged ICU LOS  
- **Table 5.** Multivariable Logistic Regression – Prolonged ICU LOS  
- **Table 6.** Multinomial Logistic Regression – 30-Day Outcomes  
- **Table 7.** Cox Proportional Hazards – Mortality  

---

## Figures
- **Figure 1.** Frequency of 30-Day Outcomes  
- **Figure 2.** Age Distribution by 30-Day Outcome  
- **Figure 3.** Prevalence of Comorbidities  
- **Figure 4.** ICU Length of Stay Distribution  
- **Figure 5.** Hospital Length of Stay Distribution  
- **Figure 6a.** Normal Q–Q Plot of Deviance Residuals  
- **Figure 6b.** Deviance Residuals vs. Fitted Values  
- **Figure 7a.** Predicted Readmission Probability (Density)  
- **Figure 7b.** Histogram of Predicted 30-Day Readmission Probability  
- **Figure 8.** ROC Curve for Prolonged ICU Stay (AUC = 0.77)  
- **Figure 9.** Kaplan–Meier: Immunosuppression vs No Immunosuppression  
- **Figure 10.** Kaplan–Meier: Polypharmacy vs No Polypharmacy  
- **Figure 11.** Outcomes by Polypharmacy and Immunosuppression  
- **Figure 12.** Kaplan–Meier: Comorbidity Burden  

---

## Software & Dependencies
**Python (3.11):** pandas, numpy, scikit-learn, matplotlib, seaborn  
**R (4.3):** survival, survminer, nnet, ggplot2, pROC  

---

## Skills Demonstrated
- Clinical cohort extraction and feature engineering (MIMIC-IV)  
- Multivariable logistic regression and survival analysis  
- Causal inference with subgroup/sensitivity analyses  
- Advanced visualization (Kaplan–Meier, ROC, diagnostic plots)  
- Reproducible research pipelines in Python + R  
- Publication-ready reporting (tables, figures, LaTeX/PDF export)  

---

##Author
**Alanoud Alturki**  
Health Data Analyst | Health Informatics Specialist | Pharmacist  
MS in Health Informatics · MS in Health Data Analysis · PhD Student  
[LinkedIn](https://www.linkedin.com/in/alanoud-alturki-5601b2b5)  

---

