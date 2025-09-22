# ======================================================
# Project: Critical Transitions – ICU Outcomes
# Author: Alanoud Alturki
# ======================================================

# -----------------------------
# Project Objectives
# -----------------------------
# Primary Objectives:
# 1. Identify demographic, clinical, laboratory, treatment, and comorbidity
#    factors associated with prolonged ICU stay (>7 days) among critically ill adults.
# 2. Determine predictors of 30-day post-discharge outcomes (readmission or mortality)
#    among ICU survivors, compared to patients who remain alive without readmission.

# Secondary / Exploratory Objectives:
# 3. Assess whether polypharmacy, immunosuppression, and comorbidity burden
#    modify the risks of prolonged ICU stay and adverse 30-day outcomes.
# 4. Evaluate predictive model performance (ROC curves, calibration,
#    decision curve analysis) and develop clinically interpretable
#    visual tools (nomograms, survival curves, probability plots).

# Overall Goal:
# To generate evidence-based insights into ICU patient trajectories,
# quantify risk factors for prolonged stay and adverse post-discharge outcomes,
# and identify modifiable predictors to inform early intervention
# and improve continuity of critical care.

# -----------------------------
# Note on Outcome Definition
# -----------------------------
# The outcome variable is a three-level factor:
# - Alive: survived without readmission in 30 days
# - Readmitted: discharged alive but readmitted within 30 days
# - Dead: died within 30 days (in-hospital or post-discharge)

# ======================================================
# Step 1. Load Libraries
# -----------------------------
library(dplyr)
library(readr)
library(gtsummary)
library(gt)
library(flextable)
library(officer)
library(ggplot2)
library(tidyr)
library(pROC)
library(survival)
library(survminer)
library(nnet)
library(pscl)
library(ResourceSelection)

# Set a custom font for all plots and tables
ggplot2::theme_set(ggplot2::theme_bw(base_size = 12, base_family = "Times New Roman") +
                     ggplot2::theme(
                       plot.title = ggplot2::element_text(face="bold", hjust=0.5, size=14, family = "Times New Roman"),
                       axis.title = ggplot2::element_text(face="bold", family = "Times New Roman"),
                       axis.text = ggplot2::element_text(family = "Times New Roman"),
                       legend.text = ggplot2::element_text(family = "Times New Roman"),
                       legend.title = ggplot2::element_text(family = "Times New Roman"),
                       strip.text = ggplot2::element_text(family = "Times New Roman")
                     ))

# Manual Font Setting for Flextable
flextable::set_flextable_defaults(font.family = "Times New Roman", font.size = 10)

# -----------------------------
# Step 2. Load Dataset
# -----------------------------
out_dir <- "/Users/alanoudalturki/Desktop/ICU_Figures"
if(!dir.exists(out_dir)) dir.create(out_dir)

df <- read_csv("/Users/alanoudalturki/Desktop/mimic_full_icu_enhanced.csv") %>%
  filter(icu_los >= 0)

# -----------------------------
# Step 3. Define Outcomes
# -----------------------------
df <- df %>%
  mutate(
    outcome_30d = case_when(
      death_30d == 1 ~ "Dead",
      readmit_30d == 1 ~ "Readmitted",
      TRUE ~ "Alive"
    ),
    outcome_30d = factor(outcome_30d, levels = c("Alive", "Readmitted", "Dead")),
    prolonged_icu_los = factor(prolonged_icu_los, levels = c(0, 1),
                               labels = c("≤7 days", ">7 days")),
    prolonged_icu_los_num = as.numeric(prolonged_icu_los) - 1
  )

# -----------------------------
# Step 4. Variable Groups
# -----------------------------
demographics   <- c("age", "gender", "insurance")
labs           <- c("wbc", "hemoglobin", "platelets", "creatinine", "bun",
                    "sodium", "potassium", "lactate", "albumin")
treatments     <- c("mech_vent", "dialysis", "vasopressor", "polypharmacy")
comorbidities  <- c("hypertension", "diabetes", "chf", "ckd", "copd", "cancer",
                    "liver_disease", "immunosuppression")
vital_signs    <- c("heart_rate", "map")
outcomes       <- c("icu_los", "hosp_los", "prolonged_icu_los", "outcome_30d")

vars_for_table <- c(demographics, labs, treatments, comorbidities, outcomes)

# -----------------------------
# Step 5. Professional Labels
# -----------------------------
var_labels <- list(
  age              ~ "Age (years)",
  gender           ~ "Gender",
  insurance        ~ "Insurance Type",
  wbc              ~ "White Blood Cell Count (×10⁹/L)",
  hemoglobin       ~ "Hemoglobin (g/dL)",
  platelets        ~ "Platelet Count (×10⁹/L)",
  creatinine       ~ "Creatinine (mg/dL)",
  bun              ~ "Blood Urea Nitrogen (mg/dL)",
  sodium           ~ "Sodium (mmol/L)",
  potassium        ~ "Potassium (mmol/L)",
  lactate          ~ "Lactate (mmol/L)",
  albumin          ~ "Albumin (g/dL)",
  mech_vent        ~ "Mechanical Ventilation",
  dialysis         ~ "Renal Replacement Therapy",
  vasopressor      ~ "Vasopressor Use",
  polypharmacy     ~ "Polypharmacy (≥5 medications)",
  hypertension     ~ "Hypertension",
  diabetes         ~ "Diabetes Mellitus",
  chf              ~ "Congestive Heart Failure",
  ckd              ~ "Chronic Kidney Disease",
  copd             ~ "Chronic Obstructive Pulmonary Disease",
  cancer           ~ "Cancer (Malignancy)",
  liver_disease    ~ "Liver Disease",
  immunosuppression~ "Immunosuppression",
  icu_los          ~ "ICU Length of Stay (days)",
  hosp_los         ~ "Hospital Length of Stay (days)"
)

# -----------------------------
# Step 6. Statistical Tests
# -----------------------------
exclude_vars <- c("insurance", "mech_vent", "dialysis", "vasopressor")

test_mapping_outcome <- list(
  all_continuous() ~ "kruskal.test",
  all_categorical() ~ "chisq.test.no.correct"
)

test_mapping_los <- list(
  all_continuous() ~ "t.test",
  all_categorical() ~ "chisq.test.no.correct"
)

# -----------------------------
# Step 7. Table 1 – Overall
# -----------------------------
table_overall <- df %>%
  select(all_of(vars_for_table)) %>%
  tbl_summary(
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing = "ifany",
    label = var_labels
  ) %>%
  modify_header(all_stat_cols() ~ "**Overall**", label ~ "**Characteristic**") %>%
  modify_caption("**Table 1. Overall Characteristics of the ICU Cohort**") %>%
  bold_labels()

# -----------------------------
# Step 8. Table 2 – By 30-Day Outcome
# -----------------------------
table_outcomes <- df %>%
  select(all_of(vars_for_table) %>% setdiff("outcome_30d")) %>%
  tbl_summary(
    by = outcome_30d,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing = "ifany",
    label = var_labels
  ) %>%
  add_overall() %>%
  add_p(
    test = test_mapping_outcome,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    include = everything() %>% setdiff(exclude_vars)
  ) %>%
  bold_p(t = 0.05) %>%
  modify_header(all_stat_cols() ~ "**{level}**", label ~ "**Characteristic**") %>%
  modify_caption("**Table 2. ICU Cohort Characteristics Stratified by 30-Day Outcome**") %>%
  bold_labels()

# -----------------------------
# Step 9. Table 3 – By ICU LOS
# -----------------------------
table_icu <- df %>%
  select(all_of(vars_for_table) %>% setdiff("prolonged_icu_los")) %>%
  tbl_summary(
    by = prolonged_icu_los,
    statistic = list(
      all_continuous() ~ "{mean} ± {sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 2,
    missing = "ifany",
    label = var_labels
  ) %>%
  add_overall() %>%
  add_p(
    test = test_mapping_los,
    pvalue_fun = ~style_pvalue(.x, digits = 3),
    include = everything() %>% setdiff(exclude_vars)
  ) %>%
  bold_p(t = 0.05) %>%
  modify_header(all_stat_cols() ~ "**{level}**", label ~ "**Characteristic**") %>%
  modify_caption("**Table 3. ICU Cohort Characteristics Stratified by ICU Length of Stay (≤7 vs >7 days)**") %>%
  bold_labels()

# -----------------------------
# Step 10. Save Tables
# -----------------------------
# Save as HTML (gt objects)
gtsave(as_gt(table_overall), file.path(out_dir,"Table1_Overall.html"))
gtsave(as_gt(table_outcomes), file.path(out_dir,"Table2_ByOutcome.html"))
gtsave(as_gt(table_icu), file.path(out_dir,"Table3_ByICU_LOS.html"))

# Save as DOCX (flextable objects with manual font setting)
save_as_docx(
  "Table 1. Overall Characteristics of the ICU Cohort" = flextable::font(as_flex_table(table_overall), fontname = "Times New Roman"),
  path = file.path(out_dir,"Table1_Overall.docx")
)

save_as_docx(
  "Table 2. ICU Cohort Characteristics Stratified by 30-Day Outcome" = flextable::font(as_flex_table(table_outcomes), fontname = "Times New Roman"),
  path = file.path(out_dir,"Table2_ByOutcome.docx")
)

save_as_docx(
  "Table 3. ICU Cohort Characteristics Stratified by ICU Length of Stay" = flextable::font(as_flex_table(table_icu), fontname = "Times New Roman"),
  path = file.path(out_dir,"Table3_ByICU_LOS.docx")
)

# -----------------------------
# Step 11. Descriptive Plots
# -----------------------------
colors_30d <- c("Alive" = "steelblue", "Readmitted" = "royalblue", "Dead" = "firebrick")
color_main <- "steelblue"
color_contrast <- "firebrick"

p_age_density <- ggplot(df, aes(x=age, fill=outcome_30d)) +
  geom_density(alpha=0.6) +
  scale_fill_manual(values = colors_30d) +
  labs(title="Age Distribution by 30-Day Outcome", x="Age (years)", y="Density", fill="Outcome")

p_age_box <- ggplot(df, aes(x=outcome_30d, y=age, fill=outcome_30d)) +
  geom_boxplot(alpha=0.8) +
  scale_fill_manual(values = colors_30d) +
  labs(title="Age by 30-Day Outcome", x="Outcome", y="Age (years)", fill="Outcome")

ggsave(file.path(out_dir,"Fig_Age_Distribution.png"),
       cowplot::plot_grid(p_age_density, p_age_box, ncol=2),
       width=12, height=6)

p_icu_los <- ggplot(df, aes(x=icu_los)) +
  geom_histogram(binwidth=1, fill=color_main, color="white", alpha=0.8) +
  labs(title="Distribution of ICU Length of Stay", x="ICU LOS (days)", y="Number of Patients")
ggsave(file.path(out_dir,"Fig_ICU_LOS.png"), p_icu_los, width=9, height=6)

p_hosp_los <- ggplot(df, aes(x=hosp_los)) +
  geom_histogram(binwidth=1, fill=color_contrast, color="white", alpha=0.8) +
  labs(title="Distribution of Hospital Length of Stay", x="Hospital LOS (days)", y="Number of Patients")
ggsave(file.path(out_dir,"Fig_Hosp_LOS.png"), p_hosp_los, width=9, height=6)

p_outcomes <- ggplot(df, aes(x=outcome_30d, fill=outcome_30d)) +
  geom_bar(width=0.7) +
  scale_fill_manual(values=colors_30d) +
  labs(title="Frequency of 30-Day Outcomes", x="Outcome", y="Number of Patients") +
  guides(fill = "none")
ggsave(file.path(out_dir,"Fig_30Day_Outcomes.png"), p_outcomes, width=7, height=5)

comorb_vars <- c("hypertension", "diabetes", "chf", "ckd", "copd", "cancer", "liver_disease")
comorb_data <- df %>%
  summarise(across(all_of(comorb_vars), ~sum(. == 1, na.rm = TRUE))) %>%
  tidyr::pivot_longer(everything(), names_to = "Comorbidity", values_to = "Count")

comorb_labels <- c("hypertension" = "Hypertension", "diabetes" = "Diabetes", "chf" = "CHF",
                   "ckd" = "CKD", "copd" = "COPD", "cancer" = "Cancer", "liver_disease" = "Liver Disease")

p_comorb <- ggplot(comorb_data, aes(x = reorder(Comorbidity, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = color_main) +
  scale_x_discrete(labels = comorb_labels) +
  labs(title = "Prevalence of Comorbidities", x = "Comorbidity", y = "Number of Patients") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(out_dir,"Fig_Comorbidity.png"), p_comorb, width=9, height=6)

p_poly <- ggplot(df, aes(x = factor(polypharmacy), fill = outcome_30d)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = colors_30d) +
  scale_x_discrete(labels = c("0" = "No Polypharmacy", "1" = "Polypharmacy")) +
  labs(title = "30-Day Outcomes by Polypharmacy Status", x = NULL, y = "Proportion", fill = "Outcome")

p_immu <- ggplot(df, aes(x = factor(immunosuppression), fill = outcome_30d)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = colors_30d) +
  scale_x_discrete(labels = c("0" = "Not Immunosuppressed", "1" = "Immunosuppressed")) +
  labs(title = "30-Day Outcomes by Immunosuppression Status", x = NULL, y = "Proportion", fill = "Outcome")

ggsave(file.path(out_dir,"Fig_Outcomes_Poly_Immu.png"),
       cowplot::plot_grid(p_poly, p_immu, ncol=2, common.legend = TRUE),
       width=10, height=5)

sink(file.path(out_dir, "SessionInfo.txt"))
sessionInfo()
sink()

# --------------------------------------------------------------------------
# Step 12. Univariable Logistic Regression
# --------------------------------------------------------------------------
# Define the set of variables to use for the univariable analysis.
univ_vars <- c("age", "gender", "wbc", "hemoglobin", "platelets",
               "creatinine", "bun", "sodium", "potassium", "lactate", "albumin",
               "heart_rate", "map", "chf", "ckd", "copd", "cancer",
               "liver_disease", "immunosuppression", "polypharmacy")

df_univ <- df %>%
  select(all_of(univ_vars), prolonged_icu_los_num)

univ_tbl <- tbl_uvregression(
  df_univ,
  method = glm,
  y = prolonged_icu_los_num,
  method.args = list(family = binomial),
  exponentiate = TRUE,
  label = var_labels[names(var_labels) %in% univ_vars]
) %>%
  modify_caption("**Table 4. Univariable Logistic Regression – Prolonged ICU LOS**") %>%
  bold_labels()

gtsave(as_gt(univ_tbl), file.path(out_dir, "Table4_Univariable.html"))
save_as_docx(
  "Table 4. Univariable Logistic Regression – Prolonged ICU LOS" = flextable::font(as_flex_table(univ_tbl), fontname = "Times New Roman"),
  path = file.path(out_dir, "Table4_Univariable.docx")
)

# --------------------------------------------------------------------------
# Step 13. Multivariable Logistic Regression + ROC
# --------------------------------------------------------------------------
model_vars <- c("age", "gender", "wbc", "hemoglobin", "platelets",
                "creatinine", "bun", "sodium", "potassium", "lactate", "albumin",
                "heart_rate", "map", "chf", "ckd", "copd", "cancer",
                "liver_disease", "immunosuppression", "polypharmacy")

model_var_labels <- var_labels[names(var_labels) %in% model_vars]

log_model <- glm(prolonged_icu_los_num ~ age + gender + wbc + hemoglobin + platelets +
                   creatinine + bun + sodium + potassium + lactate + albumin +
                   heart_rate + map + chf + ckd + copd + cancer +
                   liver_disease + immunosuppression + polypharmacy,
                 data=df, family=binomial)

log_table <- tbl_regression(log_model, exponentiate=TRUE, label = model_var_labels) %>%
  modify_caption("**Table 5. Multivariable Logistic Regression – Prolonged ICU LOS**") %>%
  bold_labels()

gtsave(as_gt(log_table), file.path(out_dir, "Table5_Multivariable.html"))
save_as_docx(
  "Table 5. Multivariable Logistic Regression – Prolonged ICU LOS" = flextable::font(as_flex_table(log_table), fontname = "Times New Roman"),
  path = file.path(out_dir, "Table5_Multivariable.docx")
)

roc_obj <- roc(df$prolonged_icu_los_num, fitted(log_model))
png(file.path(out_dir, "Fig_ROC_Logistic.png"), width=800, height=600)
plot(roc_obj, col="steelblue", lwd=2,
     main=paste0("ROC: Prolonged ICU Stay (AUC = ", round(auc(roc_obj), 3), ")"),
     xlab="1 - Specificity", ylab="Sensitivity")
abline(a=0, b=1, lty=2, col="firebrick")
dev.off()

log_pscl <- pR2(log_model)
hoslem <- hoslem.test(df$prolonged_icu_los_num, fitted(log_model))

# --------------------------------------------------------------------------
# Step 14. Multinomial Logistic Regression (30-Day Outcomes)
# --------------------------------------------------------------------------
df_survivors <- df %>% filter(outcome_30d %in% c("Alive", "Readmitted", "Dead"))

multi_model_vars <- c("age", "gender", "creatinine", "sodium", "albumin",
                      "chf", "ckd", "copd", "cancer", "immunosuppression", "polypharmacy")

multi_var_labels <- var_labels[names(var_labels) %in% multi_model_vars]

multi_model <- multinom(outcome_30d ~ age + gender + creatinine + sodium + albumin +
                          chf + ckd + copd + cancer + immunosuppression + polypharmacy,
                        data=df_survivors)

multi_table <- tbl_regression(multi_model, exponentiate=TRUE, label = multi_var_labels) %>%
  modify_caption("**Table 6. Multinomial Logistic Regression – 30-Day Outcomes**") %>%
  bold_labels()

gtsave(as_gt(multi_table), file.path(out_dir, "Table6_Multinomial.html"))
save_as_docx(
  "Table 6. Multinomial Logistic Regression – 30-Day Outcomes" = flextable::font(as_flex_table(multi_table), fontname = "Times New Roman"),
  path = file.path(out_dir, "Table6_Multinomial.docx")
)

# --------------------------------------------------------------------------
# Step 15. Cox Regression for Mortality
# --------------------------------------------------------------------------
cox_model_vars <- c("age", "gender", "creatinine", "sodium", "albumin",
                    "chf", "ckd", "cancer", "immunosuppression", "polypharmacy")

cox_var_labels <- var_labels[names(var_labels) %in% cox_model_vars]

cox_model <- coxph(Surv(icu_los, death_30d) ~ age + gender + creatinine + sodium +
                     albumin + chf + ckd + cancer + immunosuppression + polypharmacy,
                   data=df)

cox_table <- tbl_regression(cox_model, exponentiate=TRUE, label = cox_var_labels) %>%
  modify_caption("**Table 7. Cox Proportional Hazards – Mortality**") %>%
  bold_labels()

gtsave(as_gt(cox_table), file.path(out_dir, "Table7_Cox.html"))
save_as_docx(
  "Table 7. Cox Proportional Hazards – Mortality" = flextable::font(as_flex_table(cox_table), fontname = "Times New Roman"),
  path = file.path(out_dir, "Table7_Cox.docx")
)

# --------------------------------------------------------------------------
# Step 16. KM Survival Curves (Polypharmacy, Immuno, Comorbidity)
# --------------------------------------------------------------------------
df$poly_group <- factor(ifelse(df$polypharmacy == 1, "Polypharmacy", "No Polypharmacy"))
df$immu_group <- factor(ifelse(df$immunosuppression == 1, "Immunosuppression", "No Immunosuppression"))
df$comorb_count <- df$chf + df$ckd + df$copd + df$cancer + df$liver_disease
df$comorb_group <- factor(ifelse(df$comorb_count >= 2, "≥2 comorbidities", "<2 comorbidities"),
                          levels = c("<2 comorbidities", "≥2 comorbidities"))

plots <- list(
  Poly = survfit(Surv(icu_los, death_30d) ~ poly_group, data=df),
  Immu = survfit(Surv(icu_los, death_30d) ~ immu_group, data=df),
  Comorb = survfit(Surv(icu_los, death_30d) ~ comorb_group, data=df)
)

km_plots <- ggsurvplot_list(
  plots,
  data = df,
  risk.table = TRUE,
  palette = c("steelblue", "firebrick"),
  title = c("Survival by Polypharmacy Status", "Survival by Immunosuppression Status", "Survival by Comorbidity Burden")
)

purrr::iwalk(km_plots, ~ggsave(file.path(out_dir, paste0("Fig_KM_", .y, ".png")),
                               plot = .x$plot,
                               width = 8, height = 6))

# --------------------------------------------------------------------------
# Step 17. Predicted Probability of Readmission
# --------------------------------------------------------------------------
pred_probs <- as.data.frame(predict(multi_model, type = "probs"))
prob_df <- cbind(df_survivors[, c("subject_id", "age", "gender")], pred_probs)
write.csv(prob_df, file.path(out_dir, "Predicted_Probabilities.csv"), row.names = FALSE)

# Histogram for distribution (original)
p_prob_hist <- ggplot(prob_df, aes(x = Readmitted)) +
  geom_histogram(fill = color_main, bins = 30, color="white", alpha = 0.8) +
  labs(title = "Predicted Probability of 30-Day Readmission", x = "Probability", y = "Count")
ggsave(file.path(out_dir, "Fig_Prob_Readmission_Hist.png"), p_prob_hist, width=7, height=5)

# Improved: Density plot stratified by actual outcome
p_prob_density <- ggplot(prob_df, aes(x = Readmitted, fill = df_survivors$outcome_30d)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Alive" = "steelblue", "Readmitted" = "firebrick", "Dead" = "grey")) +
  labs(title = "Distribution of Predicted Readmission Probability by Outcome",
       x = "Predicted Probability of Readmission",
       y = "Density",
       fill = "Actual Outcome") +
  theme(legend.position = "right")
ggsave(file.path(out_dir, "Fig_Prob_Readmission_Density.png"), p_prob_density, width=9, height=6)

# --------------------------------------------------------------------------
# Step 18. Residual Diagnostics
# --------------------------------------------------------------------------
png(file.path(out_dir, "Fig_Logistic_Residuals_Fitted.png"), width=800, height=600)
plot(fitted(log_model), residuals(log_model, type="deviance"),
     main = "Deviance Residuals vs. Fitted Values",
     xlab = "Fitted Values (log-odds)",
     ylab = "Deviance Residuals",
     col = color_main, pch = 16)
abline(h = 0, lty = 2, col = color_contrast)
dev.off()

png(file.path(out_dir, "Fig_Logistic_QQ.png"), width=800, height=600)
qqnorm(residuals(log_model, type="deviance"),
       main = "Normal Q-Q Plot of Deviance Residuals",
       col = color_main)
qqline(residuals(log_model, type="deviance"), col = color_contrast)
dev.off()