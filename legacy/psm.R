# Propensity Score Matching with Nearest Neighbors for ATT Implementation
# Complete implementation from scratch in R

# Load required libraries
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(42)

# 1. GENERATE DUMMY DATASET
# Create a realistic dataset with confounding variables
generate_dummy_data <- function(n = 1000) {
  # Generate confounding variables
  age <- rnorm(n, mean = 45, sd = 12)
  income <- exp(rnorm(n, mean = 10, sd = 0.5))  # Log-normal distribution
  education <- sample(1:5, n, replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.3, 0.1))
  gender <- rbinom(n, 1, 0.5)
  
  # Generate treatment assignment (with confounding)
  # Higher income, education, and age increase treatment probability
  treatment_logit <- -2 + 0.02 * (age - 45) + 0.00001 * (income - 50000) + 
                     0.3 * education + 0.2 * gender + rnorm(n, 0, 0.5)
  treatment_prob <- 1 / (1 + exp(-treatment_logit))
  treatment <- rbinom(n, 1, treatment_prob)
  
  # Generate outcome with true treatment effect of 5
  # Confounders also affect outcome
  true_treatment_effect <- 5
  outcome_base <- 20 + 0.1 * age + 0.0001 * income + 2 * education + 
                  1 * gender + rnorm(n, 0, 3)
  outcome <- outcome_base + true_treatment_effect * treatment
  
  data.frame(
    id = 1:n,
    age = age,
    income = income,
    education = education,
    gender = gender,
    treatment = treatment,
    outcome = outcome,
    true_effect = true_treatment_effect
  )
}

# 2. PROPENSITY SCORE ESTIMATION
estimate_propensity_scores <- function(data) {
  # Fit logistic regression model
  ps_model <- glm(treatment ~ age + income + education + gender, 
                  data = data, family = binomial(link = "logit"))
  
  # Get propensity scores
  data$propensity_score <- predict(ps_model, type = "response")
  
  # Print model summary
  cat("Propensity Score Model Summary:\n")
  print(summary(ps_model))
  
  return(data)
}

# 3. NEAREST NEIGHBOR MATCHING FUNCTION
nearest_neighbor_matching <- function(data, caliper = 0.1) {
  treated <- data[data$treatment == 1, ]
  control <- data[data$treatment == 0, ]
  
  matches <- data.frame(
    treated_id = integer(),
    control_id = integer(),
    distance = numeric()
  )
  
  # For each treated unit, find closest control unit
  for (i in 1:nrow(treated)) {
    treated_ps <- treated$propensity_score[i]
    
    # Calculate distances to all control units
    distances <- abs(control$propensity_score - treated_ps)
    
    # Find closest match within caliper
    min_distance <- min(distances)
    if (min_distance <= caliper) {
      closest_idx <- which.min(distances)
      
      matches <- rbind(matches, data.frame(
        treated_id = treated$id[i],
        control_id = control$id[closest_idx],
        distance = min_distance
      ))
      
      # Remove matched control unit to ensure 1:1 matching
      control <- control[-closest_idx, ]
    }
  }
  
  return(matches)
}

# 4. CALCULATE ATT (Average Treatment Effect on the Treated)
calculate_att <- function(data, matches) {
  # Get matched treated and control outcomes
  treated_outcomes <- data$outcome[data$id %in% matches$treated_id]
  control_outcomes <- data$outcome[data$id %in% matches$control_id]
  
  # Calculate ATT
  att <- mean(treated_outcomes) - mean(control_outcomes)
  
  # Calculate standard error (simple approach)
  n_matches <- length(treated_outcomes)
  treated_var <- var(treated_outcomes)
  control_var <- var(control_outcomes)
  se_att <- sqrt((treated_var + control_var) / n_matches)
  
  # Calculate confidence interval
  ci_lower <- att - 1.96 * se_att
  ci_upper <- att + 1.96 * se_att
  
  return(list(
    att = att,
    se = se_att,
    ci_lower = ci_lower,
    ci_upper = ci_upper,
    n_matches = n_matches
  ))
}

# 5. BALANCE CHECKING FUNCTIONS
check_balance <- function(data, matches) {
  # Create matched dataset
  matched_treated <- data[data$id %in% matches$treated_id, ]
  matched_control <- data[data$id %in% matches$control_id, ]
  
  # Function to calculate standardized mean difference
  smd <- function(x_treated, x_control) {
    mean_diff <- mean(x_treated) - mean(x_control)
    pooled_sd <- sqrt((var(x_treated) + var(x_control)) / 2)
    return(mean_diff / pooled_sd)
  }
  
  # Calculate balance statistics
  balance_stats <- data.frame(
    variable = c("age", "income", "education", "gender", "propensity_score"),
    smd_before = c(
      smd(data$age[data$treatment == 1], data$age[data$treatment == 0]),
      smd(data$income[data$treatment == 1], data$income[data$treatment == 0]),
      smd(data$education[data$treatment == 1], data$education[data$treatment == 0]),
      smd(data$gender[data$treatment == 1], data$gender[data$treatment == 0]),
      smd(data$propensity_score[data$treatment == 1], data$propensity_score[data$treatment == 0])
    ),
    smd_after = c(
      smd(matched_treated$age, matched_control$age),
      smd(matched_treated$income, matched_control$income),
      smd(matched_treated$education, matched_control$education),
      smd(matched_treated$gender, matched_control$gender),
      smd(matched_treated$propensity_score, matched_control$propensity_score)
    )
  )
  
  return(balance_stats)
}

# 6. VISUALIZATION FUNCTIONS
plot_propensity_scores <- function(data) {
  p1 <- ggplot(data, aes(x = propensity_score, fill = factor(treatment))) +
    geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
    labs(title = "Distribution of Propensity Scores",
         x = "Propensity Score", y = "Frequency",
         fill = "Treatment") +
    theme_minimal()
  
  return(p1)
}

plot_balance <- function(balance_stats) {
  # Manual reshape to avoid naming issues
  balance_long <- data.frame(
    variable = rep(balance_stats$variable, 2),
    value = c(balance_stats$smd_before, balance_stats$smd_after),
    timing = rep(c("Before", "After"), each = nrow(balance_stats))
  )
  
  p2 <- ggplot(balance_long, aes(x = variable, y = value, fill = timing)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = c(-0.1, 0.1), linetype = "dashed", color = "red") +
    labs(title = "Standardized Mean Differences Before/After Matching",
         x = "Variables", y = "Standardized Mean Difference",
         fill = "Timing") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(p2)
}

# 7. MAIN EXECUTION FUNCTION
run_psm_analysis <- function(n = 1000, caliper = 0.1) {
  cat("=== PROPENSITY SCORE MATCHING ANALYSIS ===\n\n")
  
  # Step 1: Generate data
  cat("1. Generating dummy dataset...\n")
  data <- generate_dummy_data(n)
  cat(sprintf("   Generated %d observations\n", nrow(data)))
  cat(sprintf("   Treatment group: %d, Control group: %d\n\n", 
              sum(data$treatment), sum(1 - data$treatment)))
  
  # Step 2: Estimate propensity scores
  cat("2. Estimating propensity scores...\n")
  data <- estimate_propensity_scores(data)
  cat(sprintf("   Propensity scores range: %.3f to %.3f\n\n", 
              min(data$propensity_score), max(data$propensity_score)))
  
  # Step 3: Perform matching
  cat("3. Performing nearest neighbor matching...\n")
  matches <- nearest_neighbor_matching(data, caliper)
  cat(sprintf("   Successfully matched %d treated units\n", nrow(matches)))
  cat(sprintf("   Matching rate: %.1f%%\n\n", 100 * nrow(matches) / sum(data$treatment)))
  
  # Step 4: Calculate ATT
  cat("4. Calculating Average Treatment Effect on the Treated (ATT)...\n")
  att_results <- calculate_att(data, matches)
  cat(sprintf("   ATT Estimate: %.3f\n", att_results$att))
  cat(sprintf("   Standard Error: %.3f\n", att_results$se))
  cat(sprintf("   95%% CI: [%.3f, %.3f]\n", att_results$ci_lower, att_results$ci_upper))
  cat(sprintf("   True Effect: %.3f\n\n", data$true_effect[1]))
  
  # Step 5: Check balance
  cat("5. Checking covariate balance...\n")
  balance_stats <- check_balance(data, matches)
  print(balance_stats)
  cat("\n")
  
  # Step 6: Create visualizations
  cat("6. Creating visualizations...\n")
  p1 <- plot_propensity_scores(data)
  print(p1)
  
  p2 <- plot_balance(balance_stats)
  print(p2)
  
  # Return results
  return(list(
    data = data,
    matches = matches,
    att_results = att_results,
    balance_stats = balance_stats
  ))
}

# 8. RUN THE ANALYSIS
cat("Starting Propensity Score Matching Analysis...\n\n")
results <- run_psm_analysis(n = 1000, caliper = 0.1)

# 9. ADDITIONAL DIAGNOSTICS
cat("=== ADDITIONAL DIAGNOSTICS ===\n")
cat("Distribution of matching distances:\n")
print(summary(results$matches$distance))

cat("\nPropensity score overlap:\n")
treated_ps <- results$data$propensity_score[results$data$treatment == 1]
control_ps <- results$data$propensity_score[results$data$treatment == 0]
cat(sprintf("Treated PS range: [%.3f, %.3f]\n", min(treated_ps), max(treated_ps)))
cat(sprintf("Control PS range: [%.3f, %.3f]\n", min(control_ps), max(control_ps)))

cat("\n=== ANALYSIS COMPLETE ===\n")