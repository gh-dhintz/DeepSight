# test_plot_c.R
# Isolated test for Plot C survival analysis rendering

# Load required libraries
library(ggplot2)
library(survival)
library(survminer)
library(patchwork)

# Set up colors (matching your app)
color_bg <- "#F8F9FA"
color_panel_bg <- "#FFFFFF"
color_fg <- "#212529"
color_primary <- "#cc4c02"
color_secondary <- "#636363"

# Function to create survival data (exact copy from your app)
create_survival_data <- function(n = 100, seed = 123) {
  set.seed(seed)
  
  # Create survival data for two treatment groups
  treatment <- sample(c("Treatment A", "Treatment B"), n, replace = TRUE)
  
  # Generate survival times (exponential distribution with different hazards)
  time <- ifelse(treatment == "Treatment A", 
                 rexp(n, rate = 0.05), 
                 rexp(n, rate = 0.08))
  
  # Generate censoring (some patients don't experience event)
  censoring_prob <- 0.3
  event <- rbinom(n, 1, 1 - censoring_prob)
  
  # For censored observations, truncate time at random point
  time <- ifelse(event == 0, 
                 pmin(time, runif(n, 5, 30)), 
                 time)
  
  data.frame(
    time = time,
    event = event,
    treatment = treatment,
    age = rnorm(n, 60, 10),
    stage = sample(c("Early", "Advanced"), n, replace = TRUE, prob = c(0.6, 0.4))
  )
}

# Create base theme
create_base_plot_theme <- function() {
  theme_minimal(base_family = "sans") +  # Use sans instead of HelveticaNeue-Light for testing
    theme(
      plot.background = element_rect(fill = color_panel_bg, color = NA),
      plot.caption = element_text(size = 11, margin = margin(t = 2, b = 2, unit = "pt")),
      panel.background = element_rect(fill = color_panel_bg, color = NA),
      text = element_text(color = color_fg),
      axis.text = element_text(color = color_fg),
      axis.title = element_text(color = color_fg),
      plot.title = element_text(face = "bold", color = color_primary)
    )
}

# Create border theme
create_border_theme_C <- function() {
  theme_void() + 
    theme(
      plot.background = element_rect(fill = NA, colour = '#f4dbcc', size = 1),
      plot.title = element_text(
        size = 18, hjust = 0.5, face = "bold", family = "sans",
        margin = margin(t = 15, b = 10, unit = "pt")
      ),
      plot.subtitle = element_text(
        size = 16, hjust = 0.5, family = "sans",
        margin = margin(t = 0, b = 85, unit = "pt")
      ),
      plot.caption = element_text(
        size = 17, colour = "#C0C0C0", hjust = 0.9,
        margin = margin(t = 10, b = 55, unit = "pt")
      ),
      plot.tag = element_text(
        size = 18, face = "bold", color = "#cc4c02"
      ),
      plot.margin = margin(t = 15, r = 5, b = 15, l = 5, unit = "pt")
    )
}

# Test 1: Basic survival data creation
cat("Test 1: Creating survival data...\n")
surv_data <- create_survival_data(n = 150, seed = 124)
print(head(surv_data))
print(paste("Events observed:", sum(surv_data$event)))

# Test 2: Create survival objects
cat("\nTest 2: Creating survival objects...\n")
surv_obj_treatment <- survfit(Surv(time, event) ~ treatment, data = surv_data)
surv_obj_stage <- survfit(Surv(time, event) ~ stage, data = surv_data)
print("Survival objects created successfully")

# Test 3: Create individual ggsurvplot objects
cat("\nTest 3: Creating ggsurvplot objects...\n")
base_theme <- create_base_plot_theme()

p1_survplot <- ggsurvplot(
  surv_obj_treatment,
  data = surv_data,
  conf.int = TRUE,
  pval = TRUE,
  risk.table = FALSE,
  palette = c(color_primary, color_secondary),
  ggtheme = base_theme,
  title = "Survival by Treatment (Test)",
  xlab = "Time (months)",
  ylab = "Survival Probability",
  legend.title = "Treatment",
  legend.labs = c("Treatment A", "Treatment B")
)

p2_survplot <- ggsurvplot(
  surv_obj_stage,
  data = surv_data,
  conf.int = TRUE,
  pval = TRUE,
  risk.table = FALSE,
  palette = c("#2E8B57", "#CD853F"),
  ggtheme = base_theme,
  title = "Survival by Disease Stage (Test)",
  xlab = "Time (months)",
  ylab = "Survival Probability",
  legend.title = "Stage",
  legend.labs = c("Advanced", "Early")
)

print("ggsurvplot objects created successfully")

# Test 4: Extract ggplot objects
cat("\nTest 4: Extracting ggplot objects...\n")
p1 <- p1_survplot$plot +
  labs(caption = "Panel C.I.1") +
  theme(
    plot.margin = unit(c(0.8, 0.1, 0.1, 1), "cm"),
    plot.caption = element_text(size = 11, margin = margin(t = 2, b = 2, unit = "pt"))
  )

p2 <- p2_survplot$plot +
  labs(caption = "Panel C.I.2") +
  theme(
    plot.margin = unit(c(2.2, 0.1, 0.05, 0.1), "cm"),
    plot.caption = element_text(size = 11, margin = margin(t = 2, b = 2, unit = "pt"))
  )

print("ggplot objects extracted successfully")
print(paste("p1 class:", class(p1)))
print(paste("p2 class:", class(p2)))

# Test 5: Test individual plots
cat("\nTest 5: Testing individual plots...\n")
ggsave("test_p1.png", p1, width = 7, height = 4, dpi = 150)
ggsave("test_p2.png", p2, width = 7, height = 4, dpi = 150)
print("Individual plots saved successfully")

# Test 6: Test patchwork combination
cat("\nTest 6: Testing patchwork combination...\n")
theme_border <- create_border_theme_C()

combined_plot <- p1 / p2 + 
  plot_annotation(
    title = "Plot C.I",
    subtitle = "Figure I Analysis",
    caption = "Figure C.I",
    tag_levels = "1",
    tag_suffix = ")",
    theme = theme_border
  ) +
  plot_layout(
    heights = c(1, 1),  # Equal heights for both survival plots
    guides = "collect"
  )

print("Patchwork combination successful")
print(paste("Combined plot class:", class(combined_plot)))

# Test 7: Save combined plot
cat("\nTest 7: Saving combined plot...\n")
ggsave("test_combined_survival.png", combined_plot, width = 7, height = 8, dpi = 150)
print("Combined plot saved successfully")

# Test 8: Alternative approach - simpler combination
cat("\nTest 8: Testing simpler combination...\n")
simple_combined <- p1 / p2
ggsave("test_simple_combined.png", simple_combined, width = 7, height = 8, dpi = 150)
print("Simple combined plot saved successfully")

# Test 9: Print survival summaries for debugging
cat("\nTest 9: Survival analysis summaries...\n")
print("Treatment survival summary:")
print(summary(surv_obj_treatment))
print("\nStage survival summary:")
print(summary(surv_obj_stage))

cat("\n=== All tests completed successfully! ===\n")
cat("Check the generated PNG files:\n")
cat("- test_p1.png (individual treatment plot)\n")
cat("- test_p2.png (individual stage plot)\n")
cat("- test_combined_survival.png (full combined plot)\n")
cat("- test_simple_combined.png (simple combination)\n")
