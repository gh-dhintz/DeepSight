library(dplyr)
library(survival)
library(lubridate)

# Example dataset structure
set.seed(123)
n <- 1000

# Simulate patient data
patient_data <- data.frame(
  patient_id = 1:n,
  enrollment_date = as.Date("2020-01-01") + sample(0:365, n, replace = TRUE),
  treatment_start = as.Date("2020-01-01") + sample(30:395, n, replace = TRUE),
  event_date = as.Date("2020-01-01") + sample(60:730, n, replace = TRUE),
  event_occurred = rbinom(n, 1, 0.3),  # 30% event rate
  age = rnorm(n, 65, 10),
  baseline_biomarker = rnorm(n, 100, 20)
)

cat("=== PATIENT-SPECIFIC vs COHORT-WIDE BASELINE PERIODS ===\n\n")

# APPROACH A: PATIENT-SPECIFIC BASELINE PERIOD
# Each patient has their own baseline period starting from their individual enrollment/treatment date
cat("APPROACH A: PATIENT-SPECIFIC BASELINE PERIOD\n")
cat("- Each patient's baseline starts from their individual enrollment/treatment date\n")
cat("- Common in clinical trials and treatment effectiveness studies\n\n")

# Set event_date to NA for those without events
patient_data$event_date[patient_data$event_occurred == 0] <- NA

# Define baseline period length (e.g., 30 days)
baseline_period_days <- 30

# PATIENT-SPECIFIC BASELINE PERIOD
patient_specific <- patient_data %>%
  mutate(
    # Each patient's baseline starts from their treatment start date
    patient_baseline_start = treatment_start,
    patient_baseline_end = treatment_start + days(baseline_period_days),
    
    # Identify events during patient's individual baseline
    event_in_patient_baseline = ifelse(
      event_occurred == 1 & !is.na(event_date),
      event_date <= patient_baseline_end,
      FALSE
    ),
    
    # Analysis start time (after patient's baseline period)
    analysis_start = patient_baseline_end,
    
    # Survival time from end of patient's baseline period
    survival_time = case_when(
      event_in_patient_baseline ~ NA_real_,
      event_occurred == 1 ~ as.numeric(event_date - analysis_start),
      TRUE ~ as.numeric(as.Date("2022-12-31") - analysis_start)
    ),
    
    event_final = ifelse(event_in_patient_baseline, NA, event_occurred)
  ) %>%
  filter(!event_in_patient_baseline, survival_time >= 0)

cat("Patient-specific results:\n")
cat("- Patients included:", nrow(patient_specific), "\n")
cat("- Range of analysis start dates:", 
    as.character(range(patient_specific$analysis_start)), "\n\n")

# APPROACH B: COHORT-WIDE BASELINE PERIOD
cat("APPROACH B: COHORT-WIDE BASELINE PERIOD\n")
cat("- All patients share the same baseline period calendar dates\n")
cat("- Common in population studies and policy evaluations\n\n")

# Define cohort-wide baseline period (same calendar dates for everyone)
cohort_baseline_start <- as.Date("2020-06-01")  # Example policy implementation
cohort_baseline_end <- cohort_baseline_start + days(baseline_period_days)

cohort_wide <- patient_data %>%
  mutate(
    # Same baseline period for entire cohort
    cohort_baseline_start = cohort_baseline_start,
    cohort_baseline_end = cohort_baseline_end,
    
    # Only include patients enrolled before baseline ends
    eligible_for_cohort = enrollment_date <= cohort_baseline_end,
    
    # Events during cohort baseline period
    event_in_cohort_baseline = ifelse(
      event_occurred == 1 & !is.na(event_date),
      event_date >= cohort_baseline_start & event_date <= cohort_baseline_end,
      FALSE
    ),
    
    # Analysis starts at same time for everyone
    analysis_start = cohort_baseline_end,
    
    # Survival time from cohort baseline end
    survival_time = case_when(
      !eligible_for_cohort ~ NA_real_,
      event_in_cohort_baseline ~ NA_real_,
      event_occurred == 1 & event_date > cohort_baseline_end ~ 
        as.numeric(event_date - analysis_start),
      event_occurred == 0 ~ as.numeric(as.Date("2022-12-31") - analysis_start),
      TRUE ~ NA_real_
    ),
    
    event_final = case_when(
      !eligible_for_cohort ~ NA_real_,
      event_in_cohort_baseline ~ NA_real_,
      event_occurred == 1 & event_date > cohort_baseline_end ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(eligible_for_cohort, !event_in_cohort_baseline, survival_time >= 0)

cat("Cohort-wide results:\n")
cat("- Patients included:", nrow(cohort_wide), "\n")
cat("- Analysis start date for all patients:", as.character(cohort_baseline_end), "\n\n")

# WHEN TO USE EACH APPROACH:

cat("=== WHEN TO USE EACH APPROACH ===\n\n")

cat("USE PATIENT-SPECIFIC BASELINE when:\n")
cat("✓ Studying treatment effects (each patient starts treatment at different times)\n")
cat("✓ Clinical trials with staggered enrollment\n") 
cat("✓ Drug washout periods (individualized based on prior treatments)\n")
cat("✓ Disease progression studies (baseline = diagnosis date)\n")
cat("✓ Time-to-event from individual medical procedures\n\n")

cat("USE COHORT-WIDE BASELINE when:\n")
cat("✓ Policy implementation studies (policy starts same date for everyone)\n")
cat("✓ Seasonal effects analysis (e.g., flu season impact)\n")
cat("✓ Natural disasters or external shocks\n")
cat("✓ Market changes affecting entire population simultaneously\n")
cat("✓ Calendar time-based exposures (e.g., regulatory changes)\n\n")

# Example survival analyses for both approaches
cat("=== SURVIVAL ANALYSIS EXAMPLES ===\n\n")

# Patient-specific survival analysis
surv_patient <- Surv(time = patient_specific$survival_time, 
                     event = patient_specific$event_final)

cox_patient <- coxph(surv_patient ~ age + baseline_biomarker, 
                     data = patient_specific)

cat("Patient-Specific Baseline Analysis:\n")
print(summary(cox_patient))

# Cohort-wide survival analysis  
surv_cohort <- Surv(time = cohort_wide$survival_time,
                    event = cohort_wide$event_final)

cox_cohort <- coxph(surv_cohort ~ age + baseline_biomarker, 
                    data = cohort_wide)

cat("\nCohort-Wide Baseline Analysis:\n")
print(summary(cox_cohort))

# Visual comparison of the two approaches
cat("\n=== KEY DIFFERENCES ===\n")
cat("Patient-Specific:\n")
cat("- Analysis start dates range from", 
    as.character(min(patient_specific$analysis_start)), "to", 
    as.character(max(patient_specific$analysis_start)), "\n")
cat("- Sample size:", nrow(patient_specific), "\n")
cat("- Events:", sum(patient_specific$event_final, na.rm = TRUE), "\n\n")

cat("Cohort-Wide:\n") 
cat("- Single analysis start date:", as.character(cohort_baseline_end), "\n")
cat("- Sample size:", nrow(cohort_wide), "\n")
cat("- Events:", sum(cohort_wide$event_final, na.rm = TRUE), "\n")