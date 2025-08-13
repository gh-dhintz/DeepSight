# app41.R
# testing
# ──────────────────────────────────────────────────────────────────
#### Load Libraries ####
# ────────────────────────────────────────────────────────────────
pacman::p_load(dplyr, ggplot2, tidyr, shiny, DT, plotly, quarto, shinythemes, yaml, crew, patchwork, gt, survival, survminer, networkD3, htmlwidgets, stringr)

# ──────────────────────────────────────────────────────────────────
#### Create Fake Breast Cancer Dataset ####
# ──────────────────────────────────────────────────────────────────

create_breast_cancer_data <- function(n = 500, seed = 123) {
  set.seed(seed)
  
  # Define cohorts
  cohorts <- c("HR+/HER2", "HR+/HER2_wESR1", "HR+/HER2_wco_PIK3CA_PTEN_AKT", "ESR1 alone")
  
  # Generate patient data
  data <- data.frame(
    patient_id = paste0("PT", sprintf("%04d", 1:n)),
    cohort = sample(cohorts, n, replace = TRUE, prob = c(0.3, 0.25, 0.25, 0.2)),
    stringsAsFactors = FALSE
  )
  
  # Add clinical characteristics based on cohort
  data <- data %>%
    mutate(
      # ESR1 expression levels (higher in ESR1+ cohorts)
      esr1_expression = case_when(
        cohort == "HR+/HER2" ~ rnorm(n(), mean = 2, sd = 0.5),
        cohort == "HR+/HER2_wESR1" ~ rnorm(n(), mean = 5, sd = 1),
        cohort == "HR+/HER2_wco_PIK3CA_PTEN_AKT" ~ rnorm(n(), mean = 4.5, sd = 0.8),
        cohort == "ESR1 alone" ~ rnorm(n(), mean = 6, sd = 1.2)
      ),
      
      # PIK3CA mutation frequency (higher in co-mutation cohort)
      pik3ca_mutation = case_when(
        cohort == "HR+/HER2" ~ rbinom(n(), 1, 0.15),
        cohort == "HR+/HER2_wESR1" ~ rbinom(n(), 1, 0.20),
        cohort == "HR+/HER2_wco_PIK3CA_PTEN_AKT" ~ rbinom(n(), 1, 0.85),
        cohort == "ESR1 alone" ~ rbinom(n(), 1, 0.10)
      ),
      
      # Treatment response score (0-100)
      treatment_response = case_when(
        cohort == "HR+/HER2" ~ rnorm(n(), mean = 70, sd = 15),
        cohort == "HR+/HER2_wESR1" ~ rnorm(n(), mean = 55, sd = 18),
        cohort == "HR+/HER2_wco_PIK3CA_PTEN_AKT" ~ rnorm(n(), mean = 45, sd = 20),
        cohort == "ESR1 alone" ~ rnorm(n(), mean = 50, sd = 17)
      ),
      
      # Time to treatment discontinuation (days)
      time_to_ttd = case_when(
        cohort == "HR+/HER2" ~ rexp(n(), rate = 1/180),
        cohort == "HR+/HER2_wESR1" ~ rexp(n(), rate = 1/120),
        cohort == "HR+/HER2_wco_PIK3CA_PTEN_AKT" ~ rexp(n(), rate = 1/90),
        cohort == "ESR1 alone" ~ rexp(n(), rate = 1/100)
      ),
      
      # Time to next treatment (days)
      time_to_ttnt = time_to_ttd + rexp(n, rate = 1/30),
      
      # Treatment line (1-5)
      treatment_line = sample(1:5, n, replace = TRUE, prob = c(0.35, 0.30, 0.20, 0.10, 0.05)),
      
      # Therapy type
      therapy_type = sample(c("Endocrine", "Chemotherapy", "Targeted Agent", "Immunotherapy"), 
                           n, replace = TRUE, prob = c(0.40, 0.30, 0.25, 0.05)),
      
      # ESR1 mutation count
      esr1_mutations = case_when(
        cohort == "HR+/HER2" ~ rpois(n(), lambda = 0.5),
        cohort == "HR+/HER2_wESR1" ~ rpois(n(), lambda = 2.5),
        cohort == "HR+/HER2_wco_PIK3CA_PTEN_AKT" ~ rpois(n(), lambda = 2.0),
        cohort == "ESR1 alone" ~ rpois(n(), lambda = 3.0)
      ),
      
      # Diagnostic accuracy score
      diagnostic_accuracy = rnorm(n, mean = 85, sd = 10),
      
      # Age at diagnosis
      age = rnorm(n, mean = 60, sd = 12),
      
      # Stage
      stage = sample(c("Early", "Advanced"), n, replace = TRUE, prob = c(0.4, 0.6)),
      
      # Previous therapies count
      previous_therapies = rpois(n, lambda = 2),
      
      # Biomarker positivity rate
      biomarker_positivity = runif(n, min = 0, max = 1),
      
      # Days from ESR1 diagnosis to treatment
      days_to_tx = rexp(n, rate = 1/21)
    )
  
  # Ensure positive values
  data <- data %>%
    mutate(
      esr1_expression = pmax(esr1_expression, 0),
      treatment_response = pmax(pmin(treatment_response, 100), 0),
      time_to_ttd = pmax(time_to_ttd, 1),
      time_to_ttnt = pmax(time_to_ttnt, time_to_ttd + 1),
      diagnostic_accuracy = pmax(pmin(diagnostic_accuracy, 100), 0),
      age = pmax(age, 18),
      days_to_tx = pmax(days_to_tx, 0)
    )
  
  return(data)
}

# Create the global dataset that replaces mtcars
bc_data <- create_breast_cancer_data(n = 500, seed = 123)

# ──────────────────────────────────────────────────────────────────
#### Survival Analysis Functions ####
# ──────────────────────────────────────────────────────────────────


create_survival_data <- function(n = 100, seed = 123, cohort_filter = NULL) {
  set.seed(seed)
  
  # If cohort_filter is provided, create cohort-specific survival data
  if (!is.null(cohort_filter)) {
    # Create survival data that varies by cohort
    treatment <- sample(c("Endocrine + CDK4/6", "Chemotherapy"), n, replace = TRUE)
    
    # Base rates that vary by cohort
    base_rates <- switch(cohort_filter,
      "HR+/HER2" = list(treatment1 = 0.04, treatment2 = 0.07),
      "HR+/HER2_wESR1" = list(treatment1 = 0.05, treatment2 = 0.09),
      "HR+/HER2_wco_PIK3CA_PTEN_AKT" = list(treatment1 = 0.06, treatment2 = 0.10),
      "ESR1 alone" = list(treatment1 = 0.055, treatment2 = 0.085),
      # Default
      list(treatment1 = 0.05, treatment2 = 0.08)
    )
    
    # Generate survival times with cohort-specific hazards
    time <- ifelse(treatment == "Endocrine + CDK4/6", 
                   rexp(n, rate = base_rates$treatment1), 
                   rexp(n, rate = base_rates$treatment2))
    
    # ESR1 status probability varies by cohort
    esr1_prob <- switch(cohort_filter,
      "HR+/HER2" = 0.2,
      "HR+/HER2_wESR1" = 0.8,
      "HR+/HER2_wco_PIK3CA_PTEN_AKT" = 0.7,
      "ESR1 alone" = 0.95,
      0.5  # Default
    )
    
    esr1_status <- sample(c("ESR1+", "ESR1-"), n, replace = TRUE, 
                         prob = c(esr1_prob, 1 - esr1_prob))
    
  } else {
    # Original logic for when no cohort filter is provided
    treatment <- sample(c("Endocrine + CDK4/6", "Chemotherapy"), n, replace = TRUE)
    
    time <- ifelse(treatment == "Endocrine + CDK4/6", 
                   rexp(n, rate = 0.05), 
                   rexp(n, rate = 0.08))
    
    esr1_status <- sample(c("ESR1+", "ESR1-"), n, replace = TRUE, prob = c(0.4, 0.6))
  }
  
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
    stage = sample(c("Early", "Advanced"), n, replace = TRUE, prob = c(0.4, 0.6)),
    esr1_status = esr1_status,
    cohort = if(!is.null(cohort_filter)) cohort_filter else "All"
  )
}

create_sankey_data <- function(cohort_filter = NULL) {
  # Define nodes for the treatment pathway
  nodes <- data.frame(
    name = c(
      # 0: Initial diagnosis
      "ESR1+ Diagnosis",
      # 1-3: First-line treatments
      "Endocrine + CDK4/6", "Chemotherapy", "Targeted Therapy",
      # 4-6: First outcomes
      "Complete Response", "Partial Response", "Progressive Disease",
      # 7-9: Second-line treatments
      "Elacestrant", "Clinical Trial", "Palliative Care",
      # 10-12: Final outcomes
      "Stable Disease", "Disease Progression", "Long-term Response"
    ),
    stringsAsFactors = FALSE
  )
  
  # Define cohort-specific flow values
  if (!is.null(cohort_filter)) {
    link_values <- switch(cohort_filter,
      "HR+/HER2" = list(
        first_line = c(50, 25, 25),          # To first-line treatments
        endo_outcomes = c(25, 20, 5),        # Endocrine outcomes
        chemo_outcomes = c(8, 12, 5),        # Chemo outcomes
        targeted_outcomes = c(18, 5, 2),     # Targeted outcomes
        second_line_pd = c(4, 3, 5),         # PD to second-line
        second_line_pr = c(2, 1, 2),         # PR to second-line
        elacestrant_final = c(2, 1, 1),     # Elacestrant final
        trial_final = c(1, 1, 1),            # Trial final
        palliative_final = c(3, 2, 2)       # Palliative final
      ),
      
      "HR+/HER2_wESR1" = list(
        first_line = c(40, 35, 25),
        endo_outcomes = c(15, 20, 5),
        chemo_outcomes = c(12, 18, 5),
        targeted_outcomes = c(12, 10, 3),
        second_line_pd = c(5, 4, 6),
        second_line_pr = c(3, 2, 3),
        elacestrant_final = c(3, 2, 2),
        trial_final = c(2, 1, 1),
        palliative_final = c(4, 3, 3)
      ),
      
      "HR+/HER2_wco_PIK3CA_PTEN_AKT" = list(
        first_line = c(35, 40, 25),
        endo_outcomes = c(10, 20, 5),
        chemo_outcomes = c(15, 20, 5),
        targeted_outcomes = c(10, 12, 3),
        second_line_pd = c(6, 5, 7),
        second_line_pr = c(4, 3, 3),
        elacestrant_final = c(4, 3, 3),
        trial_final = c(3, 2, 2),
        palliative_final = c(5, 4, 4)
      ),
      
      "ESR1 alone" = list(
        first_line = c(45, 30, 25),
        endo_outcomes = c(20, 20, 5),
        chemo_outcomes = c(10, 15, 5),
        targeted_outcomes = c(15, 8, 2),
        second_line_pd = c(4, 3, 4),
        second_line_pr = c(2, 2, 2),
        elacestrant_final = c(2, 1, 1),
        trial_final = c(1, 1, 1),
        palliative_final = c(2, 2, 2)
      ),
      
      # Default values
      list(
        first_line = c(45, 30, 25),
        endo_outcomes = c(20, 20, 5),
        chemo_outcomes = c(10, 15, 5),
        targeted_outcomes = c(15, 8, 2),
        second_line_pd = c(4, 3, 4),
        second_line_pr = c(2, 2, 2),
        elacestrant_final = c(2, 1, 1),
        trial_final = c(1, 1, 1),
        palliative_final = c(2, 2, 2)
      )
    )
  } else {
    # Default values when no cohort specified
    link_values <- list(
      first_line = c(45, 30, 25),
      endo_outcomes = c(20, 20, 5),
      chemo_outcomes = c(10, 15, 5),
      targeted_outcomes = c(15, 8, 2),
      second_line_pd = c(4, 3, 4),
      second_line_pr = c(2, 2, 2),
      elacestrant_final = c(2, 1, 1),
      trial_final = c(1, 1, 1),
      palliative_final = c(2, 2, 2)
    )
  }
  
  # Build links from the cohort-specific values
  links <- data.frame(
    source = c(
      # From diagnosis to first-line treatments
      0, 0, 0,
      # From Endocrine to outcomes
      1, 1, 1,
      # From Chemo to outcomes
      2, 2, 2,
      # From Targeted to outcomes
      3, 3, 3,
      # From Progressive Disease to second-line
      6, 6, 6,
      # From Partial Response to second-line
      5, 5, 5,
      # From Elacestrant to final outcomes
      7, 7, 7,
      # From Clinical Trial to final outcomes
      8, 8, 8,
      # From Palliative Care to final outcomes
      9, 9, 9
    ),
    target = c(
      # To first-line treatments
      1, 2, 3,
      # To first outcomes
      4, 5, 6,
      4, 5, 6,
      4, 5, 6,
      # To second-line treatments
      7, 8, 9,
      7, 8, 9,
      # To final outcomes
      10, 11, 12,
      10, 11, 12,
      10, 11, 12
    ),
    value = c(
      link_values$first_line,
      link_values$endo_outcomes,
      link_values$chemo_outcomes,
      link_values$targeted_outcomes,
      link_values$second_line_pd,
      link_values$second_line_pr,
      link_values$elacestrant_final,
      link_values$trial_final,
      link_values$palliative_final
    ),
    stringsAsFactors = FALSE
  )
  
  # Add labels to links
  links$label <- paste0(links$value, " patients")
  
  # Define colors for nodes
  node_colors <- c(
    "#8B0000",     # Diagnosis - Dark red
    "#4682B4",     # Endocrine - Steel blue
    "#FF6347",     # Chemo - Tomato
    "#32CD32",     # Targeted - Lime green
    "#228B22",     # Complete Response - Forest green
    "#FFA500",     # Partial Response - Orange
    "#DC143C",     # Progressive Disease - Crimson
    "#9370DB",     # Elacestrant - Medium purple
    "#20B2AA",     # Clinical Trial - Light sea green
    "#708090",     # Palliative - Slate gray
    "#3CB371",     # Stable Disease - Medium sea green
    "#FF4500",     # Disease Progression - Orange red
    "#00CED1"      # Long-term Response - Dark turquoise
  )
  
  return(list(
    nodes = nodes,
    links = links,
    node_colors = node_colors,
    cohort = cohort_filter
  ))
}

# ────────────────────────────────────────────────────────────────
#### Central Configuration Object (MOVE THIS TO THE TOP) ####
# ────────────────────────────────────────────────────────────────

PLOT_CONFIG <- list(
  # SWAPPED: Plot B config is now first (as plot_A)
  plot_A = list(
    label = "ESR1 Dx Landscape (A)",  # Changed label from B to A
    max_plots = 6,
    height = "800px",  # Keep Plot B's original height
    has_annotations = TRUE,
    num_annotations = 8,  # Keep Plot B's 8 annotations
    annotation_configs = list(
      list(suffix = "1", label = "Reference Label:", placeholder = "fig-A-1, etc"),  # Changed from B to A
      list(suffix = "2", label = "Title:", placeholder = "My Title"),
      list(suffix = "3", label = "Subtitle:", placeholder = "My Subtitle"),
      list(suffix = "4", label = "Panel 1 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "5", label = "Panel 2 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "6", label = "Panel 3 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "7", label = "Panel 4 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "8", label = "Overall Figure Caption:", placeholder = "My Figure Caption")
    ),
    display_labels = c("Reference Label", "Title", "Subtitle", "Panel 1", "Panel 2", "Panel 3", "Panel 4", "Overall Figure")
  ),
  
  # SWAPPED: Plot A config is now second (as plot_B)
  plot_B = list(
    label = "Treatment Selection (B)",  # Changed label from A to B
    max_plots = 6,
    height = "655px",  # Keep Plot A's original height
    has_annotations = TRUE,
    num_annotations = 7,  # Keep Plot A's 7 annotations
    annotation_configs = list(
      list(suffix = "1", label = "Reference Label (for referencing figures):", placeholder = "Must start with 'fig-' , ie fig-B-1, etc"),  # Changed from A to B
      list(suffix = "2", label = "Title:", placeholder = "My Title"),
      list(suffix = "3", label = "Subtitle:", placeholder = "My Subtitle"),
      list(suffix = "4", label = "Panel 1 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "5", label = "Panel 2 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "6", label = "Panel 3 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "7", label = "Overall Figure Caption:", placeholder = "My Figure Caption")
    ),
    display_labels = c("Reference Label", "Title", "Subtitle", "Panel 1", "Panel 2", "Panel 3", "Overall Figure")
  ),
  
  # Keep plot_C and plot_D unchanged
  plot_C = list(
    label = "Survival Analysis (C)",
    max_plots = 6,
    height = "655px",
    has_annotations = TRUE,
    num_annotations = 6,
    annotation_configs = list(
      list(suffix = "1", label = "Reference Label (for referencing figures):", 
          placeholder = "Must start with 'fig-' , ie fig-C-1, etc"),
      list(suffix = "2", label = "Title:", placeholder = "My Title"),
      list(suffix = "3", label = "Subtitle:", placeholder = "My Subtitle"),
      list(suffix = "4", label = "Panel 1 Caption:", 
          placeholder = "Treatment Comparison Caption"),
      list(suffix = "5", label = "Panel 2 Caption:", 
          placeholder = "Stage Comparison Caption"),
      list(suffix = "6", label = "Overall Figure Caption:", placeholder = "My Figure Caption")
    ),
    display_labels = c("Reference Label", "Title", "Subtitle", 
                      "Panel 1", "Panel 2", "Overall Figure")
  ),
  plot_D = list(
    label = "Treatment Pathways (D)",
    max_plots = 6,
    height = "655px",
    has_annotations = TRUE,
    num_annotations = 4,
    annotation_configs = list(
      list(suffix = "1", label = "Reference Label (for referencing figures):", 
          placeholder = "Must start with 'fig-' , ie fig-D-1, etc"),
      list(suffix = "2", label = "Title:", placeholder = "My Title"),
      list(suffix = "3", label = "Subtitle:", placeholder = "My Subtitle"),
      list(suffix = "4", label = "Overall Figure Caption:", placeholder = "My Figure Caption")
    ),
    display_labels = c("Reference Label", "Title", "Subtitle", "Overall Figure")
  )
)


REPORT_LOG_FILE <- "config/report_log.tsv"  # Tab-separated file for tracking reports

# addResourcePath("css", "www")
addResourcePath("svg", "assets/images")

# ────────────────────────────────────────────────────────────────
#### UI Support Functions for Server Integration ####
# ────────────────────────────────────────────────────────────────

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

sanitize_annotation_value <- function(value) {
  if (is.null(value) || value == "~") {
    return("")
  }
  return(value)
}

# ────────────────────────────────────────────────────────────────
#### Dynamic UI Generation Functions ####
# ────────────────────────────────────────────────────────────────

# Create plot UI based on configuration
create_plot_ui_config <- function(plot_type, plot_count, input_values) {
  config <- PLOT_CONFIG[[plot_type]]
  
  style_tag <- tags$style(HTML("/* Using styles.css for now */"))
  
  # Generate individual plot inputs
  plot_inputs <- lapply(1:plot_count, function(i) {
    create_single_plot_input_config(plot_type, i, input_values, config)
  })
  
  # Create shared inputs
  shared_inputs <- create_shared_inputs_config(plot_type, input_values, config)
  
  return(div(style_tag, plot_inputs, shared_inputs))
}

# Create single plot input based on configuration
create_single_plot_input_config <- function(plot_type, i, input_values, config) {
  x_var_name <- paste0(plot_type, "_x", i)
  Roman_numerals <- as.roman(1:10)
  
  # Get current values or defaults
  current_x <- input_values[[x_var_name]]
  
  # Updated cohort choices for breast cancer data
  cohort_choices <- unique(bc_data$cohort)
  default_x <- cohort_choices[min(i, length(cohort_choices))]
  
  # Updated label to use Plot.Figure hierarchy
  plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
  
  base_inputs <- list(
    if (i > 1) br(),
    selectInput(x_var_name, 
               paste0("Cohort for Plot ", plot_letter, ".", Roman_numerals[i], ":"), 
               choices = cohort_choices,
               selected = current_x %||% default_x)
  )
  
  # Add annotation inputs if configured
  if (config$has_annotations) {
    annotation_section <- create_annotation_section_config(plot_type, i, input_values, config)
    base_inputs <- c(base_inputs, list(annotation_section))
  }
  
  return(div(base_inputs))
}

# Create annotation section based on configuration
create_annotation_section_config <- function(plot_type, i, input_values, config) {
  Roman_numerals <- as.roman(1:10)
  plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
  
  div(
    h6(
      a(
        href = paste0("#collapse_", plot_type, "_", i),
        `data-toggle` = "collapse",
        `aria-expanded` = "false",
        `aria-controls` = paste0("collapse_", plot_type, "_", i),
        style = "text-decoration: none; color: inherit;",
        tags$i(class = "fa fa-caret-right"),
        paste0(" Figure Annotations for Plot ", plot_letter, ".", Roman_numerals[i]), 
      ),
      class = "text-secondary", style = "font-weight: bold; cursor: pointer; font-size: 16px;"
    ),
    div(
      class = "collapse",
      id = paste0("collapse_", plot_type, "_", i),
      generate_annotation_inputs_config(plot_type, i, input_values, config$annotation_configs)
    )
  )
}

# Generate annotation inputs based on configuration
generate_annotation_inputs_config <- function(plot_type, i, input_values, annotation_configs) {
  inputs <- list()
  
  for (j in seq_along(annotation_configs)) {
    config <- annotation_configs[[j]]
    input_name <- paste0(plot_type, "_annotation_", i, "_", j)
    current_value <- input_values[[input_name]]
    
    inputs[[length(inputs) + 1]] <- textInput(
      input_name, 
      div(paste0(config$label), style = "font-size: 12px; font-weight: bold;"),
      value = current_value %||% "",
      placeholder = config$placeholder
    )
  }
  
  return(inputs)
}

# Create shared inputs section
create_shared_inputs_config <- function(plot_type, input_values, config) {
  Roman_numerals <- as.roman(1:10)
  notes_name <- paste0(plot_type, "_Notes_shared")
  current_notes <- input_values[[notes_name]]
  
  plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
  
  div(
    br(),
    textAreaInput(notes_name, 
                 paste0("Figure Notes"), 
                 value = current_notes %||% "",
                 placeholder = paste0("Add Notes here. Use default or chosen figure reference under annotations, e.g., fig-", plot_letter, "-1, but add the '@' suffix. For example, See @fig-", plot_letter, "-1 becomes See Fig. A.1"), 
                 height = "100px", width = "100%")
  )
}


# ────────────────────────────────────────────────────────────────
#### Server-side UI Render Functions ####
# ────────────────────────────────────────────────────────────────

# Generate UI render functions for all plot types
generate_ui_renders <- function(input, output, session) {
  for (plot_type in names(PLOT_CONFIG)) {
    local({
      my_plot_type <- plot_type
      config <- PLOT_CONFIG[[my_plot_type]]
      
      # Create input UI render function
      output[[paste0(my_plot_type, "_inputs_ui")]] <- renderUI({
        req(input[[paste0(my_plot_type, "_n")]])
        create_plot_ui_config(my_plot_type, input[[paste0(my_plot_type, "_n")]], isolate(reactiveValuesToList(input)))
      })
      
      # Create output UI render function with conditional logic for Plot D
      output[[paste0(my_plot_type, "_outputs_ui")]] <- renderUI({
        req(input[[paste0(my_plot_type, "_n")]])
        
        lapply(1:input[[paste0(my_plot_type, "_n")]], function(i) {
          if (my_plot_type == "plot_D") {
            # Use plotlyOutput for Plot D
            plotlyOutput(paste0("Plot_", toupper(substring(my_plot_type, 6, 6)), "_Obj", i), 
                        height = config$height, width = "100%")
          } else {
            # Use plotOutput for other plots
            plotOutput(paste0("Plot_", toupper(substring(my_plot_type, 6, 6)), "_Obj", i), 
                      height = config$height, width = "100%")
          }
        })
      })
    })
  }
}

# ────────────────────────────────────────────────────────────────
#### Utility Functions for Column Generation ####
# ────────────────────────────────────────────────────────────────

# Generate plot column names dynamically
generate_plot_columns_config <- function(plot_type) {
  config <- PLOT_CONFIG[[plot_type]]
  columns <- c()
  
  for (i in 1:config$max_plots) {
    # Base columns
    columns <- c(columns, 
                paste0(plot_type, "_x", i))
    
    # Add annotation captions if configured
    if (config$has_annotations) {
      for (j in 1:config$num_annotations) {
        columns <- c(columns, paste0(plot_type, "_annotation_", i, "_", j))  # Changed: i=figure, j=panel
      }
    }
  }
  
  return(columns)
}


# ────────────────────────────────────────────────────────────────
#### Validation Functions ####
# ────────────────────────────────────────────────────────────────

# Check if a plot type has been visited/configured
check_plot_visited <- function(plot_type, input) {
  n_plots <- input[[paste0(plot_type, "_n")]]
  
  if (!is.null(n_plots) && n_plots > 0) {
    for (i in 1:n_plots) {
      if (!is.null(input[[paste0(plot_type, "_x", i)]])) {
        return(TRUE)
      }
    }
  }
  
  return(FALSE)
}


# Generate reactive expressions for plot validation
generate_plot_validation_reactives <- function() {
  validation_reactives <- list()
  
  for (plot_type in names(PLOT_CONFIG)) {
    local({
      my_plot_type <- plot_type
      reactive_name <- paste0(my_plot_type, "_visited")
      
      validation_reactives[[reactive_name]] <- reactive({
        check_plot_visited(my_plot_type, input)
      })
    })
  }
  
  return(validation_reactives)
}


# ────────────────────────────────────────────────────────────────
#### Configuration Integration Functions ####
# ────────────────────────────────────────────────────────────────

# Update the log file initialization to use configuration
initialize_log_file_config <- function() {
  cat("DEBUG: Initializing log file at:", REPORT_LOG_FILE, "\n")
  
  # Create config directory if it doesn't exist
  if (!dir.exists(dirname(REPORT_LOG_FILE))) {
    cat("DEBUG: Creating directory:", dirname(REPORT_LOG_FILE), "\n")
    dir.create(dirname(REPORT_LOG_FILE), recursive = TRUE)
  }
  
  if (!file.exists(REPORT_LOG_FILE)) {
    cat("DEBUG: Creating new log file\n")
    
    # Base header columns
    header_cols <- c(
      "timestamp", 
      "format", 
      "title", 
      "subtitle", 
      "name",
      "plot_A_n", 
      "plot_B_n", 
      "plot_C_n", 
      "plot_D_n",
      "plot_A_Notes_shared", 
      "plot_B_Notes_shared", 
      "plot_C_Notes_shared", 
      "plot_D_Notes_shared",
      "bookmark_url"
    )
    
    # Add dynamic plot columns using configuration
    for (plot_type in names(PLOT_CONFIG)) {
      plot_cols <- generate_plot_columns_config(plot_type)
      header_cols <- c(header_cols, plot_cols)
    }
    
    # Write header with explicit tab separation
    header_line <- paste(header_cols, collapse = "\t")
    writeLines(header_line, REPORT_LOG_FILE)
    
    # Debug: verify the header was written correctly
    cat("DEBUG: Log file created with", length(header_cols), "columns\n")
    cat("DEBUG: First few columns:", paste(header_cols[1:5], collapse = ", "), "\n")
    cat("DEBUG: Verifying header line has correct number of tabs:", 
        str_count(header_line, "\t"), "tabs for", length(header_cols), "columns\n")
  } else {
    cat("DEBUG: Log file already exists\n")
  }
}

# Generate checklist data using configuration
generate_checklist_data <- function(input) {
  checklist_data <- data.frame(
    Section = character(),
    Status = character(),
    Description = character(),
    stringsAsFactors = FALSE
  )
  
  for (plot_type in names(PLOT_CONFIG)) {
    config <- PLOT_CONFIG[[plot_type]]
    is_visited <- check_plot_visited(plot_type, input)
    
    checklist_data <- rbind(checklist_data, data.frame(
      Section = config$label,
      Status = if(is_visited) "✅ Completed" else "❌ Not Visited",
      Description = if(is_visited) {
        paste(config$label, "section has been configured")
      } else {
        paste("Please visit", config$label, "tab to configure plots")
      },
      stringsAsFactors = FALSE
    ))
  }
  
  return(checklist_data)
}

# Create shared inputs section
create_shared_inputs_config <- function(plot_type, input_values, config) {
  Roman_numerals <- as.roman(1:10)
  notes_name <- paste0(plot_type, "_Notes_shared")
  current_notes <- input_values[[notes_name]]
  
  # Get plot letter for hierarchy
  plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
  
  div(
    br(),
    textAreaInput(notes_name, 
                 paste0("Figure Notes"), 
                 value = current_notes %||% "",
                 # ← CHANGED: Updated placeholder to use new hierarchy
                 placeholder = paste0("Add Notes here. Use default or chosen figure reference under annotations, e.g., fig-", plot_letter, "-1, but add the '@' suffix. For example, See @fig-", plot_letter, "-1 becomes See Fig. A.1"), 
                 height = "100px", width = "100%")
  )
}



# ────────────────────────────────────────────────────────────────
#### Input Collection Functions ####
# ────────────────────────────────────────────────────────────────

# Collect plot inputs for a specific plot type (UPDATED)
collect_plot_inputs <- function(input, plot_type, n_plots) {
  if (is.null(n_plots) || n_plots <= 0) return(list())
  
  inputs <- list()
  Roman_numerals <- as.roman(1:10)
  numerals <- 1:10
  
  for (i in 1:n_plots) {
    # Base inputs
    x_var_name <- paste0(plot_type, "_x", i)
    
    if (!is.null(input[[x_var_name]])) {
      inputs[[x_var_name]] <- input[[x_var_name]]
    }
    
    # Add annotation captions if this plot type has them
    config <- PLOT_CONFIG[[plot_type]]
    if (config$has_annotations) {
      for (j in 1:config$num_annotations) {
        ann_name <- paste0(plot_type, "_annotation_", i, "_", j)
        ann_value <- input[[ann_name]]
        
        # Get the label for this annotation position
        label <- config$display_labels[j]
        
        # Check if it's the ~ character - preserve it as-is
        if (!is.null(ann_value) && ann_value == "~") {
          inputs[[ann_name]] <- "~"  # Preserve the ~ marker
          next  # Skip default processing
        }
        
        # For Reference Label
        if (label == "Reference Label") {
          if (is.null(ann_value)) {
            plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
            default_ref <- paste0("fig-", plot_letter, "-", numerals[i])
            inputs[[ann_name]] <- default_ref
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else if (label == "Title") {
          if (is.null(ann_value) || ann_value == "") {
            label_clean <- gsub("[()]", "", config$label)
            plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
            default_title <- paste0("Plot ", plot_letter, ".", Roman_numerals[i])
            inputs[[ann_name]] <- default_title
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else if (label == "Subtitle") {
          if (is.null(ann_value) || ann_value == "") {
            default_subtitle <- paste0("Figure ", Roman_numerals[i], " Analysis")
            inputs[[ann_name]] <- default_subtitle
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else if (grepl("Panel", label)) {
          if (is.null(ann_value) || ann_value == "") {
            panel_num <- gsub(".*Panel ([0-9]+).*", "\\1", label)
            plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
            default_panel <- paste0("Panel ", plot_letter, ".", Roman_numerals[i], ".", panel_num)
            inputs[[ann_name]] <- default_panel
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else if (label == "Overall Figure") {
          if (is.null(ann_value) || ann_value == "") {
            plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
            default_figure <- paste0("Figure ", plot_letter, ".", Roman_numerals[i])
            inputs[[ann_name]] <- default_figure
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else {
          # For any other annotations
          if (!is.null(ann_value)) {
            inputs[[ann_name]] <- ann_value
          }
        }
      }
    }
  }
  
  # Add shared notes
  notes_name <- paste0(plot_type, "_Notes_shared")
  if (!is.null(input[[notes_name]])) {
    # Preserve ~ for notes as well
    inputs[[notes_name]] <- input[[notes_name]]
  }
  
  return(inputs)
}

# Collect all active inputs for download
collect_all_active_inputs <- function(input) {
  all_inputs <- reactiveValuesToList(input)
  
  # Remove ALL plot-specific inputs first
  plot_patterns <- c(
  "^plot_A_x", "^plot_B_x", "^plot_C_x", "^plot_D_x",
  "^plot_A_annotation", "^plot_B_annotation", "^plot_C_annotation", "^plot_D_annotation",
  "^plot_A_Notes_shared", "^plot_B_Notes_shared", "^plot_C_Notes_shared", "^plot_D_Notes_shared"
)
  for (pattern in plot_patterns) {
    to_remove <- names(all_inputs)[grepl(pattern, names(all_inputs))]
    all_inputs[to_remove] <- NULL
  }
  
  # Add back only active inputs using configuration
  for (plot_type in names(PLOT_CONFIG)) {
    n_plots <- input[[paste0(plot_type, "_n")]]
    active_inputs <- collect_plot_inputs(input, plot_type, n_plots)
    all_inputs <- c(all_inputs, active_inputs)
  }
  
  # Ensure QMD compatibility variables exist (only if not already present)
   if (!"plot_A_caption_shared" %in% names(all_inputs)) {
    all_inputs$plot_A_caption_shared <- ""
  }
  if (!"plot_B_caption_shared" %in% names(all_inputs)) {
    all_inputs$plot_B_caption_shared <- ""
  }
  # ADD THESE TWO LINES FOR PLOT C AND D:
  if (!"plot_C_caption_shared" %in% names(all_inputs)) {
    all_inputs$plot_C_caption_shared <- ""
  }
  if (!"plot_D_caption_shared" %in% names(all_inputs)) {
    all_inputs$plot_D_caption_shared <- ""
  }

  return(all_inputs)
}

# ────────────────────────────────────────────────────────────────
#### Parameter Extraction Functions ####
# ────────────────────────────────────────────────────────────────

# Configuration for parameter patterns
PARAM_PATTERNS <- list(
  metadata = c("title", "subtitle", "name"),
  base_plot = c("plot_A_n", "plot_B_n", "plot_C_n", "plot_D_n",  # ADD plot_C_n and plot_D_n
                "plot_A_Notes_shared", "plot_B_Notes_shared", "plot_C_Notes_shared", "plot_D_Notes_shared"),  # ADD Notes for C and D
  dynamic_plot = c(
    "^plot_A_x", "^plot_B_x", "^plot_C_x", "^plot_D_x",  # ADD x variable patterns for C and D
  "^plot_A_annotation", "^plot_B_annotation", "^plot_C_annotation", "^plot_D_annotation"
)  # ADD annotation patterns for C and D
)

# Extract relevant parameters from URL parsing
extractReportParams <- function(param_list) {
  all_param_names <- names(param_list)
  
  # Get all relevant parameters using pattern matching
  relevant_params <- c()
  
  # Add metadata and base plot params
  relevant_params <- c(relevant_params, PARAM_PATTERNS$metadata, PARAM_PATTERNS$base_plot)
  
  # Add dynamic plot params using pattern matching
  for (pattern in PARAM_PATTERNS$dynamic_plot) {
    matching_params <- all_param_names[grepl(pattern, all_param_names)]
    relevant_params <- c(relevant_params, matching_params)
  }
  
  # Extract parameters
  result <- setNames(rep(list(NA), length(relevant_params)), relevant_params)
  
  for (param in names(result)) {
    if (param %in% names(param_list)) {
      result[[param]] <- param_list[[param]]
    }
  }
  
  return(result)
}

# ────────────────────────────────────────────────────────────────
#### Readable Parameter Name Functions ####
# ────────────────────────────────────────────────────────────────

# Configuration for readable parameter names
PARAM_LABELS <- list(
  metadata = list(
    title = "📄 Report Title",
    subtitle = "📄 Report Subtitle", 
    name = "📄 Author Name"
  ),
  plot_base = list(
    plot_A_n = paste0("📊 ", PLOT_CONFIG$plot_A$label, " - Number of Plots"),
    plot_B_n = paste0("📊 ", PLOT_CONFIG$plot_B$label, " - Number of Plots"),
    # ADD THESE TWO LINES:
    plot_C_n = paste0("📊 ", PLOT_CONFIG$plot_C$label, " - Number of Plots"),
    plot_D_n = paste0("📊 ", PLOT_CONFIG$plot_D$label, " - Number of Plots"),
    plot_A_Notes_shared = paste0("📊 ", PLOT_CONFIG$plot_A$label, " - Shared Notes"),
    plot_B_Notes_shared = paste0("📊 ", PLOT_CONFIG$plot_B$label, " - Shared Notes"),
    # ADD THESE TWO LINES:
    plot_C_Notes_shared = paste0("📊 ", PLOT_CONFIG$plot_C$label, " - Shared Notes"),
    plot_D_Notes_shared = paste0("📊 ", PLOT_CONFIG$plot_D$label, " - Shared Notes")
  )
)

# Create readable parameter names
create_readable_param_name <- function(param) {
  # Check metadata parameters
  if (param %in% names(PARAM_LABELS$metadata)) {
    return(PARAM_LABELS$metadata[[param]])
  }
  
  # Check base plot parameters
  if (param %in% names(PARAM_LABELS$plot_base)) {
    return(PARAM_LABELS$plot_base[[param]])
  }
  
  # Handle dynamic parameters with pattern matching
  for (plot_type in names(PLOT_CONFIG)) {
    config <- PLOT_CONFIG[[plot_type]]
    
    # X variable pattern
    x_pattern <- paste0("^", plot_type, "_x(\\d+)$")
    matches <- regmatches(param, regexec(x_pattern, param))[[1]]
    if (length(matches) > 1) {
      return(paste0("📊 ", config$label, " ", matches[2], " - X Variable"))
    }
    
    # annotation caption pattern (only for plot types that have annotations)
    if (config$has_annotations) {
      ann_pattern <- paste0("^", plot_type, "_annotation_(\\d+)_(\\d+)$")
      matches <- regmatches(param, regexec(ann_pattern, param))[[1]]
      if (length(matches) > 2) {
        return(paste0("📊 ", config$label, " ", matches[2], " - Annotation ", matches[3]))  # Changed: matches[2]=figure, matches[3]=panel
      }
    }
  }
  
  # Default case
  return(param)
}

# ────────────────────────────────────────────────────────────────
#### Log Entry Generation ####
# ────────────────────────────────────────────────────────────────

# Generate log entry with configuration
generate_log_entry <- function(all_inputs, format, bookmark_url) {
  log_entry <- list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    format = format,
    title = all_inputs$title %||% "",
    subtitle = all_inputs$subtitle %||% "",
    name = all_inputs$name %||% "",
    plot_A_n = all_inputs$plot_A_n %||% "",
    plot_B_n = all_inputs$plot_B_n %||% "",
    plot_C_n = all_inputs$plot_C_n %||% "",  # ADD THIS LINE
    plot_D_n = all_inputs$plot_D_n %||% "",  # ADD THIS LINE
    plot_A_Notes_shared = all_inputs$plot_A_Notes_shared %||% "",
    plot_B_Notes_shared = all_inputs$plot_B_Notes_shared %||% "",
    plot_C_Notes_shared = all_inputs$plot_C_Notes_shared %||% "",  # ADD THIS LINE
    plot_D_Notes_shared = all_inputs$plot_D_Notes_shared %||% "",  # ADD THIS LINE
    bookmark_url = bookmark_url %||% ""
  )
  
  # Add dynamic plot variables using configuration
  for (plot_type in names(PLOT_CONFIG)) {
    columns <- generate_plot_columns_config(plot_type)
    
    for (col in columns) {
      log_entry[[col]] <- all_inputs[[col]] %||% ""
    }
  }
  
  return(log_entry)
}

# ────────────────────────────────────────────────────────────────
#### Input Preview Generation ####
# ────────────────────────────────────────────────────────────────

# Generate input preview table data
# Replace the get_plot_values function with this version that properly handles ~

get_plot_values <- function(plot_type, i, input) {
  Roman_numerals <- as.roman(1:10)
  x_var <- input[[paste0(plot_type, "_x", i)]]
  
  # Helper to get annotation input or default - PROPERLY HANDLES ~
  get_ann <- function(panel_pos, default = "") {
    val <- input[[paste0(plot_type, "_annotation_", i, "_", panel_pos)]]
    # If it's ~, return empty string without applying default
    if (!is.null(val) && val == "~") return("")
    # Otherwise, if null or empty, use default
    if (is.null(val) || val == "") default else val
  }
  
  # Special helper for reference label
  get_ref_label <- function(panel_pos) {
    val <- input[[paste0(plot_type, "_annotation_", i, "_", panel_pos)]]
    # If it's ~, return empty string
    if (!is.null(val) && val == "~") return("")
    if (is.null(val)) "" else val
  }
  
  #### Defualt Labels ####
  plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
  title_suffix <- if(plot_letter == "A"){"ESR1 DX Landscape"} else if(plot_letter == "B"){"Treatment Selection"} else if(plot_letter == "C"){"Real-world Patient Outcomes to First Tx Following ESR1 Dx"} else if(plot_letter == "D"){"Sankey plot of tx sequencing following ESR1 Dx"}
  default_title <- paste0(title_suffix, " ", plot_letter, ".", Roman_numerals[i])
  default_figure <- paste0("Figure ", plot_letter, ".", Roman_numerals[i])
  default_ref <- paste0("@fig-", plot_letter, "-", i)
  panel_suffix <- paste0("Panel ", plot_letter, ".", Roman_numerals[i])
  Blank_Panel <- TRUE

  # SWAPPED: Plot A now returns Plot B's structure (8 annotations, 4 plots)
  if (plot_type == "plot_A") {
    list(
      x_var = x_var,
      figRef = if (get_ref_label(1) != "") paste0("@", gsub("[^a-zA-Z0-9-]", "", get_ref_label(1))) else default_ref,
      title = get_ann(2, default_title),
      subtitle = get_ann(3),
      plot1Cap = get_ann(4, if(Blank_Panel){""} else paste0(panel_suffix, ".1")),
      plot2Cap = get_ann(5, if(Blank_Panel){""} else paste0(panel_suffix, ".2")),
      plot3Cap = get_ann(6, if(Blank_Panel){""} else paste0(panel_suffix, ".3")),
      plot4Cap = get_ann(7, if(Blank_Panel){""} else paste0(panel_suffix, ".4")),
      figOvCap = get_ann(8, default_figure)
    )
  } 
  # SWAPPED: Plot B now returns Plot A's structure (7 annotations, table + 2 plots)
  else if (plot_type == "plot_B") {
    list(
      x_var = x_var,
      figRef = if (get_ref_label(1) != "") paste0("@", gsub("[^a-zA-Z0-9-]", "", get_ref_label(1))) else default_ref,
      title = get_ann(2, default_title),
      subtitle = get_ann(3),
      tbl1Cap = get_ann(4, if(Blank_Panel){""} else paste0(panel_suffix, ".1")),
      plot1Cap = get_ann(5, if(Blank_Panel){""} else paste0(panel_suffix, ".2")),
      plot2Cap = get_ann(6, if(Blank_Panel){""} else paste0(panel_suffix, ".3")),
      figOvCap = get_ann(7, default_figure)
    )
  } else if (plot_type == "plot_C") {
    list(
      x_var = x_var,
      figRef = if (get_ref_label(1) != "") paste0("@", gsub("[^a-zA-Z0-9-]", "", get_ref_label(1))) else default_ref,
      title = get_ann(2, default_title),
      subtitle = get_ann(3),
      plot1Cap = get_ann(4, if(Blank_Panel){""} else paste0(panel_suffix, ".1")),
      plot2Cap = get_ann(5, if(Blank_Panel){""} else paste0(panel_suffix, ".2")),
      figOvCap = get_ann(6, default_figure) 
    )
  } else if (plot_type == "plot_D") {
    list(
      x_var = x_var,
      figRef = if (get_ref_label(1) != "") paste0("@", gsub("[^a-zA-Z0-9-]", "", get_ref_label(1))) else default_ref,
      title = get_ann(2, default_title),
      subtitle = get_ann(3),
      figOvCap = get_ann(4, default_figure) 
    )
  }
}

# Update the collect_plot_inputs function to preserve ~ in the stored data
collect_plot_inputs <- function(input, plot_type, n_plots) {
  if (is.null(n_plots) || n_plots <= 0) return(list())
  
  inputs <- list()
  Roman_numerals <- as.roman(1:10)
  numerals <- 1:10
  
  for (i in 1:n_plots) {
    # Base inputs
    x_var_name <- paste0(plot_type, "_x", i)
    
    if (!is.null(input[[x_var_name]])) {
      inputs[[x_var_name]] <- input[[x_var_name]]
    }
    
    # Add annotation captions if this plot type has them
    config <- PLOT_CONFIG[[plot_type]]
    if (config$has_annotations) {
      for (j in 1:config$num_annotations) {
        ann_name <- paste0(plot_type, "_annotation_", i, "_", j)
        ann_value <- input[[ann_name]]
        
        # Get the label for this annotation position
        label <- config$display_labels[j]
        
        # Check if it's the ~ character - preserve it as-is
        if (!is.null(ann_value) && ann_value == "~") {
          inputs[[ann_name]] <- "~"  # Preserve the ~ marker
          next  # Skip default processing
        }
        
        # For Reference Label
        if (label == "Reference Label") {
          if (is.null(ann_value)) {
            plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
            default_ref <- paste0("fig-", plot_letter, "-", numerals[i])
            inputs[[ann_name]] <- default_ref
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else if (label == "Title") {
          if (is.null(ann_value) || ann_value == "") {
            label_clean <- gsub("[()]", "", config$label)
            plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
            default_title <- paste0("Plot ", plot_letter, ".", Roman_numerals[i])
            inputs[[ann_name]] <- default_title
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else if (label == "Subtitle") {
          if (is.null(ann_value) || ann_value == "") {
            default_subtitle <- paste0("Figure ", Roman_numerals[i], " Analysis")
            inputs[[ann_name]] <- default_subtitle
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else if (grepl("Panel", label)) {
          if (is.null(ann_value) || ann_value == "") {
            panel_num <- gsub(".*Panel ([0-9]+).*", "\\1", label)
            plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
            default_panel <- paste0("Panel ", plot_letter, ".", Roman_numerals[i], ".", panel_num)
            inputs[[ann_name]] <- default_panel
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else if (label == "Overall Figure") {
          if (is.null(ann_value) || ann_value == "") {
            plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
            default_figure <- paste0("Figure ", plot_letter, ".", Roman_numerals[i])
            inputs[[ann_name]] <- default_figure
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else {
          # For any other annotations
          if (!is.null(ann_value)) {
            inputs[[ann_name]] <- ann_value
          }
        }
      }
    }
  }
  
  # Add shared notes
  notes_name <- paste0(plot_type, "_Notes_shared")
  if (!is.null(input[[notes_name]])) {
    # Preserve ~ for notes as well
    inputs[[notes_name]] <- input[[notes_name]]
  }
  
  return(inputs)
}

# Update generate_input_preview_data to show ~ as blank in preview
generate_input_preview_data <- function(input) {
  # Create summary data - only show if inputs actually exist
  summary_data <- data.frame(
    Setting = character(),
    Value = character(),
    stringsAsFactors = FALSE
  )
  
  # Add report metadata first
  metadata_items <- list(
    list(label = "Report Title", value = input$title),
    list(label = "Report Subtitle", value = input$subtitle),
    list(label = "Author Name", value = input$name)
  )
  
  for (item in metadata_items) {
    summary_data <- rbind(summary_data, data.frame(
      Setting = item$label,
      Value = item$value %||% "Not set",
      stringsAsFactors = FALSE
    ))
  }
  
  # Add plot summaries and details using configuration
  plot_details <- data.frame(
    Setting = character(),
    Value = character(),
    stringsAsFactors = FALSE
  )
  
  for (plot_type in names(PLOT_CONFIG)) {
    config <- PLOT_CONFIG[[plot_type]]
    section_visited <- check_plot_visited(plot_type, input)
    plot_label_clean <- gsub("[()]", "", config$label)
    
    # Add summary only if tabs have been visited
    if (section_visited) {
      summary_data <- rbind(summary_data, data.frame(
        Setting = paste(config$label, "- Number of Plots"),
        Value = as.character(input[[paste0(plot_type, "_n")]]),
        stringsAsFactors = FALSE
      ))
      
      # Add individual plot details
      if (!is.null(input[[paste0(plot_type, "_n")]]) && input[[paste0(plot_type, "_n")]] > 0) {
        n_plots <- input[[paste0(plot_type, "_n")]]
        
        for (i in 1:n_plots) {
          x_var <- input[[paste0(plot_type, "_x", i)]]
          
          # Add shared notes
          shared_notes_var <- input[[paste0(plot_type, "_Notes_shared")]]
          # Display ~ as blank in preview
          if (!is.null(shared_notes_var) && shared_notes_var == "~") {
            shared_notes_var <- "(blank)"
          }
          if (!is.null(shared_notes_var) && shared_notes_var != "") {
            plot_details <- rbind(plot_details, data.frame(
              Setting = paste0(config$label, " - Notes"),
              Value = if(nchar(shared_notes_var) > 50) paste0(substr(shared_notes_var, 1, 50), "...") else shared_notes_var,
              stringsAsFactors = FALSE
            ))
          }
          
          if (!is.null(x_var)) {
            plot_details <- rbind(plot_details, data.frame(
              Setting = paste0(plot_label_clean, ".", i, " - X Variable"),
              Value = x_var,
              stringsAsFactors = FALSE
            ))
            
            # Add annotation captions if this plot type has them
            if (config$has_annotations) {
              for (j in 1:config$num_annotations) {
                ann_var <- input[[paste0(plot_type, "_annotation_", i, "_", j)]]
                
                # Display ~ as (blank) in preview
                display_value <- ann_var
                if (!is.null(ann_var) && ann_var == "~") {
                  display_value <- "(blank)"
                }
                
                # Use plot-specific labels from configuration
                annotation_labels <- config$display_labels
                
                if (!is.null(ann_var) && ann_var != "") {
                  plot_details <- rbind(plot_details, data.frame(
                    Setting = paste0(plot_label_clean, ".", i, " - ", annotation_labels[j]),
                    Value = if(nchar(display_value) > 50) paste0(substr(display_value, 1, 50), "...") else display_value,
                    stringsAsFactors = FALSE
                  ))
                }
              }
            }
          }
        }
      }
    }
  }
  
  # Combine summary and details
  final_data <- rbind(summary_data, plot_details)
  
  # If no plots have been configured/visited, still show metadata
  if (nrow(plot_details) == 0) {
    final_data <- rbind(final_data, data.frame(
      Setting = "Plots Status",
      Value = "Visit plot tabs to configure plots for the report",
      stringsAsFactors = FALSE
    ))
  }
  
  return(final_data)
}


# ────────────────────────────────────────────────────────────────
#### Plot Generation Functions for Breast Cancer Data ####
# ────────────────────────────────────────────────────────────────

# Generate Plot A content with breast cancer data
generate_plot_A_content <- function(i, config, input) {
  # Get current values
  values <- get_plot_values("plot_A", i, input)
  
  # Filter data by selected cohort
  dat <- bc_data %>% filter(cohort == values$x_var)
  
  # Use Plot B's component creation (4 plots, no table)
  plots <- create_plot_A_components(dat, i, values)
  
  # Use Plot B's combination method
  combine_plot_A_components(plots, values)
}

# Generate Plot B content with breast cancer data

generate_plot_B_content <- function(i, config, input) {
  # Get current values using helper function
  values <- get_plot_values("plot_B", i, input)
  
  # Filter data by selected cohort
  dat <- bc_data %>% filter(cohort == values$x_var)
  
  # Use Plot A's component creation (2 plots + table)
  plots <- create_plot_B_components(dat, i, values, input)
  
  # Pass plot_type to table creation
  plot_type <- "plot_B"  # Set this so the table function knows which annotations to use
  table_component <- create_table_component_A(dat, i, input)
  
  # Use Plot A's combination method
  combine_plot_B_components(plots, table_component, values$figOvCap,
    values$title, values$subtitle)
}
# Generate Plot C content with survival analysis
generate_plot_C_content <- function(i, config, input) {
  # Get current values using helper function
  values <- get_plot_values("plot_C", i, input)
  
  # Create survival data with cohort filter (different seed for each plot)
  surv_data <- create_survival_data(n = 150, seed = 123 + i, cohort_filter = values$x_var)
  
  plots <- create_plot_C_components(surv_data, i, values)
  
  # Combine with patchwork (no table for survival plots)
  combine_plot_C_components(plots, values$figOvCap, values$title, values$subtitle)
}

# Generate Plot D content with Sankey diagram
generate_plot_D_content <- function(i, config, input) {
  # Get current values using helper function
  values <- get_plot_values("plot_D", i, input)
  
  # Create Sankey data with cohort filter
  sankey_data <- create_sankey_data(cohort_filter = values$x_var)
  
  # Create and return the Sankey plot
  create_plot_D_components(sankey_data, i, values)
}

# Create plot components for Plot A - Treatment Selection
create_plot_B_components <- function(dat, i, values, input) {
  base_theme <- create_base_plot_theme()
  values <- get_plot_values("plot_A", i, input)
  
  # Panel 1: Time to Treatment Initiation
  p1 <- ggplot(dat, aes(x = days_to_tx)) +
    geom_histogram(fill = color_primary, color = color_secondary, alpha = 0.7, bins = 15) +
    base_theme +
    labs(
      title = paste0("Days to Treatment Post-ESR1 Dx (", i, ")"), 
      x = "Days from ESR1 Diagnosis", 
      y = "Number of Patients",
      caption = values$plot1Cap 
    ) +
    theme(plot.margin = unit(c(0.8, 0.1, 0.1, 1), "cm"))
  
  # Panel 2: Treatment Duration by Line
  avg_duration <- dat %>%
    group_by(treatment_line, therapy_type) %>%
    summarise(avg_months = mean(time_to_ttd / 30.44, na.rm = TRUE), .groups = "drop")
  
  p2 <- ggplot(avg_duration, aes(x = factor(treatment_line), y = avg_months, fill = therapy_type)) +
    geom_col(position = "dodge", color = "gray20", alpha = 0.8) +
    scale_fill_manual(values = c(
      "Chemotherapy" = "#BFD7EA",
      "Endocrine" = "#9CBFD9",
      "Targeted Agent" = "#7FA6C9",
      "Immunotherapy" = "#5E8FBF"
    )) +
    base_theme +
    labs(
      title = paste0("Average Treatment Duration by Line (", i, ")"), 
      x = "Treatment Line", 
      y = "Average Duration (Months)",
      fill = "Therapy Type",
      caption = values$plot2Cap 
    ) +
    theme(plot.margin = unit(c(2.2, 0.1, 0.05, 0.1), "cm"))
  
  list(p1 = p1, p2 = p2)
}

# Create plot components for Plot B - ESR1 Landscape
create_plot_A_components <- function(dat, i, values) {
  base_theme <- create_base_plot_theme()
  
  # Plot 1 - ESR1 Expression Distribution
  p1 <- ggplot(dat, aes(x = esr1_expression)) +
    geom_histogram(fill = color_primary, color = color_secondary, alpha = 0.7, bins = 20) +
    base_theme +
    labs(
      title = paste0("ESR1 Expression Distribution (", i, ")"), 
      x = "ESR1 Expression Level", 
      y = "Frequency",
      caption = values$plot1Cap
    ) +
    theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))
  
  # Plot 2 - ESR1 Mutation Count
  p2 <- ggplot(dat, aes(x = factor(esr1_mutations))) +
    geom_bar(fill = color_secondary, color = color_primary, alpha = 0.7) +
    base_theme +
    labs(
      title = paste0("ESR1 Mutation Count (", i, ")"), 
      x = "Number of ESR1 Mutations", 
      y = "Number of Patients",
      caption = values$plot2Cap
    ) +
    theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))
  
  # Plot 3 - Treatment Response by Line
  p3 <- ggplot(dat, aes(x = factor(treatment_line), y = treatment_response)) +
    geom_boxplot(fill = color_primary, alpha = 0.5) +
    base_theme +
    labs(
      title = paste0("Treatment Response by Line (", i, ")"), 
      x = "Treatment Line", 
      y = "Response Score",
      caption = values$plot3Cap  
    ) +
    theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))
  
  # Plot 4 - Biomarker Positivity Rate
  p4 <- ggplot(dat, aes(x = biomarker_positivity)) +
    geom_histogram(fill = color_secondary, color = color_primary, alpha = 0.7, bins = 15) +
    scale_x_continuous(labels = scales::percent_format()) +
    base_theme +
    labs(
      title = paste0("Biomarker Positivity Rate (", i, ")"), 
      x = "Positivity Rate", 
      y = "Frequency",
      caption = values$plot4Cap  
    ) +
    theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))

  list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
}

# Create plot components for Plot C (Survival Analysis)
create_plot_C_components <- function(surv_data, i, values) {
  base_theme <- create_base_plot_theme()
  
  # Add cohort info to title if available
  cohort_label <- if(!is.null(unique(surv_data$cohort)) && unique(surv_data$cohort) != "All") {
    paste0(" - ", unique(surv_data$cohort))
  } else {
    ""
  }
  
  # Create survival objects
  surv_obj_treatment <- survfit(Surv(time, event) ~ treatment, data = surv_data)
  surv_obj_esr1 <- survfit(Surv(time, event) ~ esr1_status, data = surv_data)
  
  # Plot 1 - Survival by Treatment
  p1 <- ggsurvplot(
    surv_obj_treatment,
    data = surv_data,
    conf.int = TRUE,
    pval = TRUE,
    risk.table = FALSE,
    palette = c(color_primary, color_secondary),
    ggtheme = base_theme,
    title = paste0("PFS by Treatment Type (", i, ")", cohort_label),
    xlab = "Time (months)",
    ylab = "Progression-Free Survival",
    legend.title = "Treatment",
    legend.labs = c("Chemotherapy", "Endocrine + CDK4/6")
  )$plot +
    labs(caption = values$plot1Cap) +
    theme(
      plot.margin = unit(c(0.8, 0.1, 0.1, 1), "cm"),
      plot.caption = element_text(size = 15, margin = margin(t = 2, b = 2, unit = "pt"))
    )
  
  # Plot 2 - Survival by ESR1 Status
  p2 <- ggsurvplot(
    surv_obj_esr1,
    data = surv_data,
    conf.int = TRUE,
    pval = TRUE,
    risk.table = FALSE,
    palette = c("#2E8B57", "#CD853F"),
    ggtheme = base_theme,
    title = paste0("PFS by ESR1 Status (", i, ")", cohort_label),
    xlab = "Time (months)",
    ylab = "Progression-Free Survival",
    legend.title = "ESR1 Status",
    legend.labs = c("ESR1-", "ESR1+")
  )$plot +
    labs(caption = values$plot2Cap) +
    theme(
      plot.margin = unit(c(2.2, 0.1, 0.05, 0.1), "cm"),
      plot.caption = element_text(size = 15, margin = margin(t = 2, b = 2, unit = "pt"))
    )
  
  list(p1 = p1, p2 = p2)
}

# Create plot components for Plot D (Treatment Pathways)
create_plot_D_components <- function(sankey_data, i, values) {
  # Add cohort info to title if available
  cohort_label <- if(!is.null(sankey_data$cohort)) {
    paste0(" - ", sankey_data$cohort)
  } else {
    ""
  }
  
  # Create the Sankey plot using plotly
  p <- plot_ly(
    type = "sankey",
    orientation = "h",
    valueformat = ".0f",
    valuesuffix = " patients",
    
    node = list(
      label = sankey_data$nodes$name,
      color = sankey_data$node_colors,
      pad = 15,
      thickness = 20,
      line = list(
        color = "black",
        width = 0.5
      )
    ),
    
    link = list(
      source = sankey_data$links$source,
      target = sankey_data$links$target,
      value = sankey_data$links$value,
      label = sankey_data$links$label,
      color = 'rgba(0,0,0,0.2)'  # Semi-transparent links
    )
  )
  
  # Apply layout with consistent styling
  p <- p %>% layout(
    title = list(
      text = paste0("<b>", values$title, "</b><br>",
                    "<span style='font-size:14px'>", values$subtitle, "</span><br>",
                    "<span style='font-size:16px'>ESR1+ Treatment Pathway (", i, ")", cohort_label, "</span>"),
      font = list(size = 18, color = color_primary),
      x = 0.5,
      xanchor = 'center'
    ),
    font = list(
      size = 12,
      color = color_fg,
      family = "Helvetica Neue"
    ),
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    plot_bgcolor = color_panel_bg,
    paper_bgcolor = color_panel_bg,
    margin = list(l = 20, r = 20, t = 100, b = 80),
    annotations = list(
      list(
        text = values$figOvCap,
        showarrow = FALSE,
        xref = 'paper',
        yref = 'paper',
        x = 0.9,
        y = -0.1,
        xanchor = 'right',
        yanchor = 'top',
        font = list(size = 17, color = "#C0C0C0")
      )
    )
  )
  
  return(p)
}

# Create base plot theme
create_base_plot_theme <- function() {
  theme_minimal(base_family = "Helvetica Neue") +
    theme(
      plot.background = element_rect(fill = color_panel_bg, color = NA),
      plot.caption = element_text(size = 15, margin = margin(t = 2, b = 2, unit = "pt")),
      panel.background = element_rect(fill = color_panel_bg, color = NA),
      text = element_text(color = color_fg),
      axis.text = element_text(color = color_fg),
      axis.title = element_text(color = color_fg),
      plot.title = element_text(face = "bold",size = 18, color = color_primary)
    )
}

# Create table component for breast cancer data
create_table_component_A <- function(dat, i, input) {
  # MODIFIED: Determine which plot type is actually calling this
  # Check if we're being called from plot_A or plot_B
  calling_plot_type <- if (exists("plot_type", parent.frame())) {
    get("plot_type", parent.frame())
  } else {
    # Try to infer from the input structure
    if (!is.null(input$plot_B_annotation_1_4) && grepl("Panel", input$plot_B_annotation_1_4)) {
      "plot_B"
    } else {
      "plot_A"
    }
  }
  
  # Get current values using the appropriate plot type
  values <- get_plot_values(calling_plot_type, i, input)
  
  # Create summary table with time to treatment categories
  bins <- c(-Inf, 7, 14, 21, 28, 35, 42, Inf)
  labels <- c("≤7 days", "8-14 days", "15-21 days", "22-28 days", 
              "29-35 days", "36-42 days", "≥43 days")
  
  table_data <- dat %>%
    mutate(period = cut(days_to_tx, breaks = bins, labels = labels, right = TRUE)) %>%
    group_by(period) %>%
    summarise(
      n = n(),
      pct_in_period = round(100 * n() / nrow(dat), 1)
    ) %>%
    arrange(factor(period, levels = labels)) %>%
    mutate(cumulative_pct = round(cumsum(pct_in_period), 1))
  
  gt_tab <- create_styled_gt_table_B(table_data, i)
  
  wrap_table(gt_tab, panel = "full", space = "fixed") +
    labs(caption = values$tbl1Cap) + 
    theme_minimal() +
    theme(
      plot.margin = unit(c(0.8, 0.8, 0.1, 0.1), "cm"),
      plot.caption = element_text(
        size = 15, hjust = 1, family = "Helvetica Neue",  
        margin = margin(t = 2, b = 2, unit = "pt")
      ),
      text = element_text(family = "Helvetica Neue")
    )
}

create_styled_gt_table_B <- function(table_data, i) {
  table_data %>%
    gt() %>%
    tab_header(title = paste0("Time to Treatment Post-ESR1 Dx (B.", i, ")")) %>%
    cols_label(
      period = "Time Period",
      n = "Patients",
      pct_in_period = "% in Period",
      cumulative_pct = "Cumulative %"
    ) %>%
    tab_options(
      table.width = pct(100),
      table.layout = "fixed",
      data_row.padding = px(8),        
      column_labels.padding = px(8),   
      table.border.left.width = px(0),
      table.border.right.width = px(0)
    ) %>%
    tab_style(
      style = list(
        cell_text(size = px(18), weight = "normal"),
        cell_borders(sides = c("left", "right"), color = "transparent", weight = px(15))
      ),
      locations = cells_body()
    ) %>%
    tab_style(
      style = list(
        cell_text(size = px(19), weight = "bold"),
        cell_borders(sides = c("left", "right"), color = "transparent", weight = px(15))
      ),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(size = px(22), weight = "bold", color = "#cc4c02"),
      locations = cells_title()
    ) %>%
    cols_width(
      period ~ px(180),      
      n ~ px(120),
      pct_in_period ~ px(150),
      cumulative_pct ~ px(150)
    )
}

# Combine plot components (keeping same structure as original)
combine_plot_C_components <- function(plots, overall_caption, title, subtitle) {
  theme_border <- create_border_theme_C()
  
  plots$p1 / plots$p2 + 
    plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = overall_caption,
      tag_levels = '1',
      tag_suffix = ")",
      theme = theme_border
    ) +
    plot_layout(
      heights = c(1, 1),
      guides = "collect"
    )
}

combine_plot_B_components <- function(plots, table_component, overall_caption,
    title, subtitle) {
  theme_border <- create_border_theme_B()
  
  (table_component + plots$p1) / plots$p2 + 
    plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = overall_caption,
      tag_levels = '1',
      tag_suffix = ")",
      theme = theme_border
    ) +
    plot_layout(
      heights = c(0.4, 1),
      guides = "collect"
    )
}

combine_plot_A_components <- function(plots, values) {
  theme_border <- create_border_theme_A()
  
  plots$p1 / (plots$p2 + (plots$p3 / plots$p4)) + 
    plot_annotation(
      title = values$title,
      subtitle = values$subtitle,
      caption = values$figOvCap,
      tag_levels = '1',
      tag_suffix = ")",
      theme = theme_border
    ) +
    plot_layout(
      heights = c(0.4, 1),
      guides = "collect"
    )
}

# Border themes (keeping same as original)
create_border_theme_A <- function() {
  theme_void() +
    theme(
      plot.background = element_rect(fill = NA, colour = '#f4dbcc', size = 1),
      plot.title = element_text(
        size = 18, hjust = 0.5, face = "bold", family = "Helvetica Neue",
        margin = margin(t = 15, b = 0, unit = "pt")
      ),
      plot.subtitle = element_text(
        size = 16, hjust = 0.5, family = "Helvetica Neue",
        margin = margin(t = 0, b = 0, unit = "pt") 
      ),
      plot.caption = element_text(
        size = 17, colour = "#C0C0C0", hjust = 0.9,
        margin = margin(t = 10, b = 55, unit = "pt")
      ),
      plot.tag = element_text(
        size = 18, face = "bold", color = "#cc4c02"
      ),
      plot.margin = margin(t = 10, r = 5, b = 15, l = 5, unit = "pt")  # Changed from t = 15 to t = 10
    )
}


create_border_theme_B <- function() {
  theme_void() + 
    theme(
      plot.background = element_rect(fill = NA, colour = '#f4dbcc', size = 1),
      plot.title = element_text(
        size = 18, hjust = 0.5, face = "bold", family = "Helvetica Neue",
        margin = margin(t = 15, b = 10, unit = "pt")
      ),
      plot.subtitle = element_text(
        size = 16, hjust = 0.5, family = "Helvetica Neue",
        margin = margin(t = 0, b = 85, unit = "pt")  # Changed from b = 10 to b = 85 (Plot A's value)
      ),
      plot.caption = element_text(
        size = 17, colour = "#C0C0C0", hjust = 0.9,
        margin = margin(t = 10, b = 55, unit = "pt")
      ),
      plot.tag = element_text(
        size = 18, face = "bold", color = "#cc4c02"
      ),
      plot.margin = margin(t = 15, r = 5, b = 15, l = 5, unit = "pt")  # Changed from t = 10 to t = 15
    )
}


create_border_theme_C <- function() {
  theme_void() + 
    theme(
      plot.background = element_rect(fill = NA, colour = '#f4dbcc', size = 1),
      plot.title = element_text(
        size = 18, hjust = 0.5, face = "bold", family = "Helvetica Neue",
        margin = margin(t = 15, b = 10, unit = "pt")
      ),
      plot.subtitle = element_text(
        size = 16, hjust = 0.5, family = "Helvetica Neue",
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

create_border_theme_D <- function() {
  theme_void() + 
    theme(
      plot.background = element_rect(fill = NA, colour = '#f4dbcc', size = 1),
      plot.title = element_text(
        size = 18, hjust = 0.5, face = "bold", family = "Helvetica Neue",
        margin = margin(t = 15, b = 10, unit = "pt")
      ),
      plot.subtitle = element_text(
        size = 16, hjust = 0.5, family = "Helvetica Neue",
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

# ────────────────────────────────────────────────────────────────
#### Color and Style Constants ####
# ────────────────────────────────────────────────────────────────

color_bg <- "#F8F9FA"
color_panel_bg <- "#FFFFFF"
color_outer_bg <- "#ECECEC"
color_fg <- "#212529"
color_primary <- "#cc4c02"
color_secondary <- "#636363"

# ────────────────────────────────────────────────────────────────
#### Updated UI with Configuration-Driven Approach ####
# ────────────────────────────────────────────────────────────────



# Helper functions for UI generation
get_max_plots <- function(plot_type) PLOT_CONFIG[[plot_type]]$max_plots
get_plot_label <- function(plot_type) PLOT_CONFIG[[plot_type]]$label

# Create tab panels dynamically for plot types
create_plot_tabs <- function() {
  tabs <- list()
  
  for (plot_type in names(PLOT_CONFIG)) {
    config <- PLOT_CONFIG[[plot_type]]
    
    tabs[[length(tabs) + 1]] <- tabPanel(
      config$label,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4(paste("Inputs for", config$label), class = "text-primary"),
          numericInput(
            paste0(plot_type, "_n"), 
            paste0("Number of Figures per Plot (up to ", config$max_plots, "):"), 
            value = 1, 
            min = 1, 
            max = config$max_plots
          ),
          # Dynamic inputs generated by server
          uiOutput(paste0(plot_type, "_inputs_ui"))
        ),
        mainPanel(
          # Dynamic plot outputs generated by server
          uiOutput(paste0(plot_type, "_outputs_ui"))
        )
      )
    )
  }
  
  return(tabs)
}

# Create navigation items dynamically
create_nav_items <- function() {
  nav_items <- list(
    # Static tabs
    tabPanel("User Guide",
      h4("Instructions for Use", class = "text-primary"),
      tableOutput("guideTable"),
      
      br(),
      h4("Tips", class = "text-primary"),
      tags$ul(
        tags$li("Figures will be arranged in the report as they appear, top to bottom in the dashboard"),
        tags$li("You can save your configuration with a bookmark url by using the 'Update & Copy Bookmark URL' button in the section 'Report Export'")
      ),
      
      br(),
      h4("Report Metadata", class = "text-primary"),
      p("Enter the information that will appear in your report header:"),
      
      fluidRow(
        column(4,
          textInput("title", "Report Title:", 
                   value = "My Report Title", 
                   placeholder = "Enter the main title for your report")
        ),
        column(4,
          textInput("subtitle", "Report Subtitle:", 
                   value = "Analysis Report", 
                   placeholder = "Enter a subtitle (optional)")
        ),
        column(4,
          textInput("name", "Your Name:", 
                   value = "John Doe", 
                   placeholder = "Enter your name")
        )
      ),
      
      br(),
      div(
        class = "alert alert-info",
        style = "margin-top: 10px; padding: 15px;",
        h6("📝 Note:", class = "text-info", style = "margin-bottom: 5px;"),
        p("This information will be automatically included in your generated report. You can modify these fields at any time before downloading.", 
          style = "margin-bottom: 0; font-size: 14px;")
      )
    ),
    
    tabPanel("Preload Reports",
      h4("Quick Start with Predefined Reports", class = "text-primary"),
      p("Load pre-configured report templates to get started quickly. Click any button below to load the report configuration."),
      
      br(),
      
      create_preload_sections()
    ),
    
    # Dynamic plots tab with sub-tabs
    tabPanel("Plots",
      do.call(tabsetPanel, c(
        list(id = "plots_subtabs"),
        create_plot_tabs()
      ))
    ),
    
    # Report Export tab
    create_report_export_tab(),
    
    # Compare Reports tab
    create_compare_reports_tab(),
    
    # Report Log tab
    create_report_log_tab()
  )
  
  return(nav_items)
}

# Create preload sections
create_preload_sections <- function() {
  preload_configs <- list(
    list(
      title = "Basic Report",
      description = "A sample report Prepared for by Guardant Health Real World Evidence team",
      features = c(
        "Item 1: Esse labore esse fugiat nisi enim consequat ut sunt exercitation.",
        "Item 2: Aliqua quis esse non sint reprehenderit culpa Lorem incididunt ad."
      ),
      button_id = "load_basic_report",
      button_text = "Load Basic Report"
    ),
    list(
      title = "Basic Report Two",
      description = "The same sample report Prepared for by Guardant Health Real World Evidence team",
      features = c(
         "Item 1: Esse labore esse fugiat nisi enim consequat ut sunt exercitation.",
        "Item 2: Aliqua quis esse non sint reprehenderit culpa Lorem incididunt ad."
      ),
      button_id = "load_basic_report_two",
      button_text = "Load Basic Report Two"
    )
  )
  
  sections <- list()
  
  for (config in preload_configs) {
    sections[[length(sections) + 1]] <- fluidRow(
      column(8,
        div(
          class = "panel panel-default",
          style = "padding: 15px; margin-bottom: 15px; border: 1px solid #ddd; border-radius: 5px;",
          h5(config$title, class = "text-info", style = "margin-top: 0;"),
          p(config$description, style = "margin-bottom: 10px; color: #666;"),
          tags$ul(lapply(config$features, tags$li))
        )
      ),
      column(4,
        div(style = "padding-top: 30px;",
          actionButton(config$button_id, config$button_text, 
                      class = "btn btn-primary btn-lg", 
                      style = "width: 100%;")
        )
      )
    )
  }
  
  sections[[length(sections) + 1]] <- div(
    class = "alert alert-info",
    style = "margin-top: 20px; padding: 15px;",
    h6("💡 How it works:", class = "text-info", style = "margin-bottom: 5px;"),
    p("After hitting these preload buttons, all fields and metadata will refresh based on predefined templates as you navigate onto each respective section. Thus, after clicking to preload a report, its recommended to navigate through all sections and tabs to load the preset fields. Note, you can modify any of the fields before choosing to export your report.", 
      style = "margin-bottom: 0; font-size: 14px;")
  )
  
  return(sections)
}

# Create Report Export tab
create_report_export_tab <- function() {
  tabPanel("Report Export",
    h4("Export Report", class = "text-primary"),
    
    # Report format selection
    selectInput("format", "Report Format:", choices = c("PDF", "HTML", "Word")),
    
    actionButton("update_and_copy_url", "Update & Copy Bookmark URL", 
          class = "btn btn-info btn-sm"),
    
    # Prominent bookmark URL display
    div(
      style = "margin: 15px 0; padding: 10px; background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 4px;",
        h6("💡 Tip:", style = "color: #6c757d; margin-bottom: 5px;"),
        p("Click 'Update & Copy Bookmark URL' after typing to save your captions and notes.", 
      style = "margin-bottom: 0; font-size: 13px; color: #6c757d;")
        ),
    
    ###### Checklist Section ######
    h4("Checklist Before Exporting", class = "text-primary"),
    p("Please ensure all required sections are completed before exporting:"),
    tableOutput("checklistTable"),
    
    ###### Preview Section ######
    br(),
    h4("Report Preview", class = "text-primary"),
    p("The following inputs will be included in your report:"),
    
    # Input values table
    tableOutput("inputPreviewTable"),
    
    br(),
    # Conditional download button - only show when all conditions are met
    conditionalPanel(
      condition = "output.allConditionsMet == true",
      actionButton("download_trigger", "Download Report", class = "btn btn-primary btn-lg")
    ),
    conditionalPanel(
      condition = "output.allConditionsMet == false",
      div(
        class = "alert alert-warning",
        style = "margin-top: 10px; padding: 15px;",
        h5("⚠ Cannot Download Yet", class = "text-warning"),
        p("Please complete the checklist above before downloading your report.", style = "margin-bottom: 0;")
      )
    ),
    downloadLink("download_report_link", "Download Link", style = "display:none;")
  )
}

# Create Compare Reports tab
create_compare_reports_tab <- function() {
  tabPanel("Compare Reports",
    h4("Compare Report Parameters", class = "text-primary"),
    p("Enter bookmark URLs from previously generated reports to compare their parameters side-by-side."),
    
    fluidRow(
      column(6,
        h5("Report 1", class = "text-info"),
        textAreaInput("bookmark_url_1", "Bookmark URL 1:", 
                     placeholder = "Paste bookmark URL here...", 
                     height = "100px", width = "100%"),
        actionButton("parse_url_1", "Parse URL 1", class = "btn btn-secondary btn-sm")
      ),
      column(6,
        h5("Report 2", class = "text-info"),
        textAreaInput("bookmark_url_2", "Bookmark URL 2:", 
                     placeholder = "Paste bookmark URL here...", 
                     height = "100px", width = "100%"),
        actionButton("parse_url_2", "Parse URL 2", class = "btn btn-secondary btn-sm")
      )
    ),
    
    br(),
    
    fluidRow(
      column(12,
        h5("Parameter Comparison", class = "text-primary"),
        tableOutput("comparisonTable")
      )
    ),
    
    br(),
    
    fluidRow(
      column(12,
        h5("Quick Actions", class = "text-info"),
        actionButton("load_report_1", "Load Report 1 Parameters", class = "btn btn-primary btn-sm"),
        actionButton("load_report_2", "Load Report 2 Parameters", class = "btn btn-primary btn-sm"),
        actionButton("clear_comparison", "Clear Comparison", class = "btn btn-warning btn-sm")
      )
    )
  )
}

# Create Report Log tab
create_report_log_tab <- function() {
  tabPanel("Report Log",
    h4("Report Generation History", class = "text-primary"),
    p("This table shows all reports that have been generated in this application, including their parameters and timestamps."),
    
    fluidRow(
      column(12,
        h5("Generation History", class = "text-info"),
        DT::dataTableOutput("reportLogTable")
      )
    ),
    
    br(),
    
    fluidRow(
      column(12,
        h5("Log Management", class = "text-info"),
        actionButton("refresh_log", "Refresh Log", class = "btn btn-secondary btn-sm"),
        actionButton("clear_log", "Clear Log", class = "btn btn-warning btn-sm", 
                    onclick = "return confirm('Are you sure you want to clear the entire report log?');")
      )
    ),
    
    br(),
    
    div(
      class = "alert alert-info",
      style = "margin-top: 20px; padding: 15px;",
      h6("📝 About the Report Log:", class = "text-info", style = "margin-bottom: 5px;"),
      p("Each row represents a generated report. The log includes the timestamp, report metadata, and all plot configurations used. This helps track different versions and configurations of your reports over time.", 
        style = "margin-bottom: 0; font-size: 14px;")
    )
  )
}

# Main UI function
ui <- function(request) {
  fluidPage(
    includeCSS("www/styles.css"),
    # Add this line to ensure plotly JavaScript is loaded:
    plotlyOutput("dummy_plotly_to_load_js", height = "0px"),

    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
    ),
    # Add JavaScript for clipboard functionality
    tags$head(
      tags$link(rel = "icon", href = "svg/Blood_Drop_Full_Color.svg", type = "image/svg+xml"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      # Add collapsible carrot for Plot Annotations & CLIPBOARD 
      tags$script(src = "app.js")
    ),
    
    # Navigation bar with dynamic content
    do.call(navbarPage, c(
      list(
        theme = shinytheme("flatly"),
        collapsible = TRUE,
        id = "nav",
        title = div(
          tags$img(src = "svg/Blood_Drop_Full_Color.svg", height = "40px", style = "margin-right:10px;"),
          "DeepSight™"
        )
      ),
      
      # Add the head styles
      list(
        tags$head(
          tags$style(HTML("
            /* Your existing styles here... */
          "))
        )
      ),
      
      # Add all navigation items
      create_nav_items()
    ))
  )
}

# ────────────────────────────────────────────────────────────────
#### Server Code Part 1: Setup and Initialization ####
# ────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # DEBUG FUNCTION (keeping your original)
  safe_showNotification <- function(message, type = "default", duration = 5, ...) {
    cat("DEBUG: showNotification called with message:", message, "\n")
    cat("DEBUG: showNotification called with type:", type, "\n")
    cat("DEBUG: Valid types are: default, message, warning, error\n")
    
    valid_types <- c("default", "message", "warning", "error")
    if (!type %in% valid_types) {
      cat("ERROR: Invalid type '", type, "' provided to showNotification\n")
      cat("This call came from:\n")
      print(sys.calls())
      type <- "default"
    }
    
    showNotification(message, type = type, duration = duration, ...)
  }

  # Close button observer (keeping your original)
  observeEvent(input$close_download_modal, {
    removeModal()
  })
  
  # ────────────────────────────────────────────────────────────────
  #### Initialize Log File with Configuration ####
  # ────────────────────────────────────────────────────────────────
  
  # Use the configuration-based log initialization
  initialize_log_file_config()
  
  # ────────────────────────────────────────────────────────────────
  #### Generate Dynamic UI Renders for All Plot Types ####
  # ────────────────────────────────────────────────────────────────
  
  # Generate input and output UI renders for all configured plot types
  generate_ui_renders(input, output, session)
# Force all plot UIs to render even when hidden
for (plot_type in names(PLOT_CONFIG)) {
  outputOptions(output, paste0(plot_type, "_inputs_ui"), suspendWhenHidden = FALSE)
  outputOptions(output, paste0(plot_type, "_outputs_ui"), suspendWhenHidden = FALSE)
}

  
  # ────────────────────────────────────────────────────────────────
  #### Crew Controller Setup (keeping your original) ####
  # ────────────────────────────────────────────────────────────────
  
  controller <- crew_controller_local(
    workers = 3,
    seconds_idle = 30,
    seconds_timeout = 60
  )
  controller$start()
  
  session$onSessionEnded(function() {
    controller$terminate()
  })
  
  # Reactive values (keeping your originals)
  current_bookmark_url <- reactiveVal(NULL)
  download_requested <- reactiveVal(FALSE)
  copy_after_bookmark <- reactiveVal(FALSE)
  plot_A_ui_generation_in_progress <- reactiveVal(FALSE)
  plot_B_ui_generation_in_progress <- reactiveVal(FALSE)
  bookmark_in_progress <- reactiveVal(FALSE)
  
  # ────────────────────────────────────────────────────────────────
  #### Generate Plot Validation Reactives for All Plot Types ####
  # ────────────────────────────────────────────────────────────────
  
  # Create validation reactives for all configured plot types
  plot_validation_reactives <- list()
  
  for (plot_type in names(PLOT_CONFIG)) {
    local({
      my_plot_type <- plot_type
      
      plot_validation_reactives[[paste0(my_plot_type, "_visited")]] <<- reactive({
        check_plot_visited(my_plot_type, input)
      })
    })
  }
  
  # Create combined validation reactive
  all_plots_visited <- reactive({
    all(sapply(names(PLOT_CONFIG), function(plot_type) {
      check_plot_visited(plot_type, input)
    }))
  })

  # ────────────────────────────────────────────────────────────────
  #### Bookmark Processing (keeping your original logic) ####
  # ────────────────────────────────────────────────────────────────
  
  observe({
    invalidateLater(300, session)
    
    if (controller$nonempty()) {
      result <- controller$pop()
      if (!is.null(result) && !is.null(result$result) && grepl("^bookmark_", result$name)) {
        session$doBookmark()
        bookmark_in_progress(FALSE)
      }
    }
  })
  
  # ────────────────────────────────────────────────────────────────
  #### Simplified Input Tracking ####
  # ────────────────────────────────────────────────────────────────
  
  # Configuration for input tracking
  INPUT_TRACKING_CONFIG <- list(
    stable_inputs = c("nav", "plots_subtabs", "format", "title", "subtitle", "name", 
                     paste0(names(PLOT_CONFIG), "_n")),
    debounce_stable = 1000,
    debounce_text = 5000
  )
  
  # Track stable inputs
  inputs_to_track_stable <- reactive({
    all_inputs <- list()
    
    for (input_name in INPUT_TRACKING_CONFIG$stable_inputs) {
      all_inputs[[input_name]] <- input[[input_name]]
    }
    
    return(all_inputs)
  })
  
  # Track text inputs
  text_and_dropdown_inputs <- reactive({
  all_vals <- list()
  
  # Collect text inputs for all configured plot types
  for (plot_type in names(PLOT_CONFIG)) {
    n_plots <- input[[paste0(plot_type, "_n")]]
    
    if (!is.null(n_plots) && n_plots > 0) {
      config <- PLOT_CONFIG[[plot_type]]
      
      for (i in 1:n_plots) {
        x_var_name <- paste0(plot_type, "_x", i)
        all_vals[[x_var_name]] <- input[[x_var_name]]
        # Add annotation captions if this plot type has them
        if (config$has_annotations) {
          for (j in 1:config$num_annotations) {
            ann_name <- paste0(plot_type, "_annotation_", i, "_", j)  # FIXED: i=figure, j=panel
            all_vals[[ann_name]] <- input[[ann_name]]
          }
        }
      }
      
      # Add shared notes
      notes_name <- paste0(plot_type, "_Notes_shared")
      all_vals[[notes_name]] <- input[[notes_name]]
    }
  }
  
  return(all_vals)
})
  
  # Debounced versions
  inputs_stable_debounced <- debounce(inputs_to_track_stable, INPUT_TRACKING_CONFIG$debounce_stable)
  text_debounced <- debounce(text_and_dropdown_inputs, INPUT_TRACKING_CONFIG$debounce_text)
  
  # Auto-bookmark observers (keeping your original logic)
  observeEvent(inputs_stable_debounced(), {
    if (!bookmark_in_progress()) {
      bookmark_in_progress(TRUE)
      
      controller$push(
        command = {
          Sys.sleep(0.1)
          "bookmark_ready"
        },
        name = paste0("bookmark_", Sys.time())
      )
    }
  }, ignoreInit = TRUE)
  
  observeEvent(text_debounced(), {
    if (!bookmark_in_progress()) {
      bookmark_in_progress(TRUE)
      
      controller$push(
        command = {
          Sys.sleep(0.1)
          "bookmark_ready"
        },
        name = paste0("bookmark_text_", Sys.time())
      )
    }
  }, ignoreInit = TRUE)

  # ────────────────────────────────────────────────────────────────
  #### Preload Reports with Configuration ####
  # ────────────────────────────────────────────────────────────────
  
# Update the parseBookmarkURL function to better handle empty strings
parseBookmarkURL <- function(url) {
  if (is.null(url) || url == "" || !grepl("\\?", url)) {
    return(list())
  }
  
  query_string <- sub(".*\\?", "", url)
  params <- strsplit(query_string, "&")[[1]]
  param_list <- list()
  
  for (param in params) {
    if (grepl("=", param)) {
      parts <- strsplit(param, "=")[[1]]
      if (length(parts) == 2) {
        key <- URLdecode(parts[1])
        value <- URLdecode(parts[2])
        value <- gsub('^"|"$', '', value)
        
        # Special handling for empty strings - they come as %22%22 in URLs
        if (value == "" || value == '""') {
          param_list[[key]] <- ""
        } else if (grepl("^[0-9]+$", value)) {
          value <- as.numeric(value)
          param_list[[key]] <- value
        } else {
          param_list[[key]] <- value
        }
      }
    }
  }
  
  return(param_list)
}
 
# Also update your loadPresetReport function to work with this approach
# Also update your loadPresetReport function to work with this approach
loadPresetReport <- function(preset_url) {
  parsed <- parseBookmarkURL(preset_url)
  extracted <- extractReportParams(parsed)
  
  if (length(extracted) > 0) {
    # Clear any existing preset data first
    pending_preset_data(NULL)
    preset_loading_stage("idle")
    preset_retry_count(0)  # Reset retry counter
    
    # Update metadata inputs first (these don't need to wait)
    metadata_updates <- list(
      list(name = "title", value = extracted$title),
      list(name = "subtitle", value = extracted$subtitle),
      list(name = "name", value = extracted$name)
    )
    
    for (update in metadata_updates) {
      if (!is.na(update$value)) {
        updateTextInput(session, update$name, value = update$value)
      }
    }
    
    # Update plot numbers for all configured plot types
    for (plot_type in names(PLOT_CONFIG)) {
      n_param <- paste0(plot_type, "_n")
      if (!is.na(extracted[[n_param]])) {
        updateNumericInput(session, n_param, value = extracted[[n_param]])
      }
    }
    
    # Store the extracted values for the observer to process
    pending_preset_data(extracted)
  }
}
  
# Add these reactive values at the top with your other reactive values
pending_preset_data <- reactiveVal(NULL)  
preset_loading_stage <- reactiveVal("idle")
preset_retry_count <- reactiveVal(0)
max_retries <- 50

# Main observer for preset loading
observe({
  preset_data <- pending_preset_data()
  stage <- preset_loading_stage()

  # Add debug logging
  cat("Observer running - Stage:", stage, "Data exists:", !is.null(preset_data), "\n")
  
  if (is.null(preset_data)) {
    preset_retry_count(0)
    return()
  }
  
  if (preset_retry_count() > max_retries) {
    # Too many retries, give up
    showNotification("Failed to load preset data - please navigate to the Plots tab and try again", type = "error")
    pending_preset_data(NULL)
    preset_loading_stage("idle")
    preset_retry_count(0)
    return()
  }
  
  if (stage == "idle") {
    cat("Starting preset loading...\n")
    preset_loading_stage("loading")
    preset_retry_count(preset_retry_count() + 1)
    
    # Update plot numbers
    for (plot_type in names(PLOT_CONFIG)) {
      n_param <- paste0(plot_type, "_n")
      if (n_param %in% names(preset_data) && !is.na(preset_data[[n_param]])) {
        cat("Updating", n_param, "to", preset_data[[n_param]], "\n")
        updateNumericInput(session, n_param, value = preset_data[[n_param]])
      }
    }
    
    preset_loading_stage("numbers_updated")
    invalidateLater(200, session)
    
  } else if (stage == "numbers_updated") {
    cat("Checking if UI is ready... Retry count:", preset_retry_count(), "\n")
    
    # Check if all UI elements are ready
    all_ready <- TRUE
    ui_missing <- c()
    
    for (plot_type in names(PLOT_CONFIG)) {
      n_plots <- preset_data[[paste0(plot_type, "_n")]]
      if (!is.null(n_plots) && n_plots > 0) {
        for (i in 1:n_plots) {
          x_input_name <- paste0(plot_type, "_x", i)
          if (is.null(input[[x_input_name]])) {
            cat("UI not ready - missing:", x_input_name, "\n")
            all_ready <- FALSE
            ui_missing <- c(ui_missing, x_input_name)
            break
          }
        }
      }
      if (!all_ready) break
    }
    
    if (all_ready) {
      cat("UI is ready, applying preset values...\n")
      
      # CRITICAL: Apply all preset values here
      for (plot_type in names(PLOT_CONFIG)) {
        config <- PLOT_CONFIG[[plot_type]]
        n_plots <- preset_data[[paste0(plot_type, "_n")]]
        
        if (!is.null(n_plots) && n_plots > 0) {
          for (i in 1:n_plots) {
            # Update X variables
            x_param <- paste0(plot_type, "_x", i)
            if (x_param %in% names(preset_data) && !is.na(preset_data[[x_param]])) {
              cat("Updating", x_param, "to", preset_data[[x_param]], "\n")
              updateSelectInput(session, x_param, selected = preset_data[[x_param]])
            }
            
            # Update annotation inputs if this plot type has them
            if (config$has_annotations) {
              for (j in 1:config$num_annotations) {
                ann_param <- paste0(plot_type, "_annotation_", i, "_", j)
                
                if (ann_param %in% names(preset_data) && !is.na(preset_data[[ann_param]])) {
                  cat("Updating", ann_param, "\n")
                  updateTextInput(session, ann_param, value = preset_data[[ann_param]])
                }
              }
            }
          }
          
          # Update shared notes
          notes_param <- paste0(plot_type, "_Notes_shared")
          if (notes_param %in% names(preset_data) && !is.na(preset_data[[notes_param]])) {
            cat("Updating", notes_param, "\n")
            updateTextAreaInput(session, notes_param, value = preset_data[[notes_param]])
          }
        }
      }
      
      # Clear everything after successful loading
      cat("Preset loading complete!\n")
      pending_preset_data(NULL)
      preset_loading_stage("idle")
      preset_retry_count(0)
      
      # Show success notification
      showNotification("Preset data loaded successfully!", type = "message", duration = 3)
      
    } else {
      # Not ready yet
      if (preset_retry_count() == 10 && length(ui_missing) > 0) {
        # Show a helpful message after a few retries
        showNotification(
          "Loading preset data... Please navigate to the Plots tab to complete loading.",
          type = "message",
          duration = 10
        )
      }
      
      # Increment retry and check again
      cat("UI not ready, will retry...\n")
      preset_retry_count(preset_retry_count() + 1)
      invalidateLater(200, session)
    }
  }
})
  
# Alternative approach: Add an observer that reacts when tab changes
observeEvent(input$nav, {
  # When user navigates to Plots tab, it will trigger UI creation naturally
  if (input$nav == "Plots" && !is.null(pending_preset_data())) {
    cat("User navigated to Plots tab, UI should be created soon...\n")
  }
})
  
# Updated preset URLs that include data for all plot types
observeEvent(input$load_basic_report, {
  # This URL now includes Plot C and D data
  preset_url <- "http://127.0.0.1:6639/?_inputs_&nav=%22Report%20Export%22&plots_subtabs=%22Treatment%20Pathways%20(D)%22&load_basic_report=1&load_basic_report_two=0&update_and_copy_url=2&download_trigger=0&parse_url_1=0&parse_url_2=0&load_report_1=0&load_report_2=0&clear_comparison=0&refresh_log=0&clear_log=0&format=%22PDF%22&plot_A_n=1&plot_B_n=1&plot_C_n=1&plot_D_n=1&bookmark_url_1=%22%22&bookmark_url_2=%22%22&title=%22My%20Report%20Title%22&subtitle=%22Analysis%20Report%22&name=%22John%20Doe%22&plot_A_x1=%22HR%2B%2FHER2%22&plot_A_Notes_shared=%22%22&plot_A_annotation_1_1=%22fig-A-1%22&plot_A_annotation_1_2=%22ESR1%20DX%20Landscape%20A.I%22&plot_A_annotation_1_3=%22~%22&plot_A_annotation_1_4=%22~%22&plot_A_annotation_1_5=%22~%22&plot_A_annotation_1_6=%22~%22&plot_A_annotation_1_7=%22~%22&plot_A_annotation_1_8=%22%22&plot_B_x1=%22HR%2B%2FHER2%22&plot_B_Notes_shared=%22%22&plot_B_annotation_1_1=%22fig-B-1%22&plot_B_annotation_1_2=%22Treatment%20Selection%20B.I%22&plot_B_annotation_1_3=%22Treatment%20Selection%20and%20Therapeutic%20Outcomes%22&plot_B_annotation_1_4=%22~%22&plot_B_annotation_1_5=%22~%22&plot_B_annotation_1_6=%22~%22&plot_B_annotation_1_7=%22%22&plot_C_x1=%22HR%2B%2FHER2%22&plot_C_Notes_shared=%22%22&plot_C_annotation_1_1=%22fig-C-1%22&plot_C_annotation_1_2=%22Real-world%20Patient%20Outcomes%20to%20First%20Tx%20Following%20ESR1%20Dx%22&plot_C_annotation_1_3=%22~%22&plot_C_annotation_1_4=%22~%22&plot_C_annotation_1_5=%22~%22&plot_C_annotation_1_6=%22%22&plot_D_x1=%22HR%2B%2FHER2%22&plot_D_Notes_shared=%22%22&plot_D_annotation_1_1=%22fig-D-1%22&plot_D_annotation_1_2=%22Sankey%20Plot%20of%20Tx%20Sequencing%20following%20ESR1%20Dx%22&plot_D_annotation_1_3=%22~%22&plot_D_annotation_1_4=%22%22&.clientValue-default-plotlyCrosstalkOpts=%7B%22on%22%3A%22plotly_click%22%2C%22persistent%22%3Afalse%2C%22dynamic%22%3Afalse%2C%22selectize%22%3Afalse%2C%22opacityDim%22%3A0.2%2C%22selected%22%3A%7B%22opacity%22%3A1%7D%2C%22debounce%22%3A0%2C%22color%22%3A%5B%5D%7D&plotly_afterplot-A=%22%5C%22Plot_D_Obj1%5C%22%22&plotly_hover-A=null&plotly_relayout-A=%22%7B%5C%22width%5C%22%3A1052.65625%2C%5C%22height%5C%22%3A655%7D%22"
  
  loadPresetReport(preset_url)
  showNotification("Loading Basic Report configuration...", type = "message")
})

observeEvent(input$load_basic_report_two, {
  # This URL now includes Plot C and D data with different values
  preset_url <- "http://127.0.0.1:6639/?_inputs_&nav=%22Report%20Export%22&plots_subtabs=%22Treatment%20Pathways%20(D)%22&load_basic_report=1&load_basic_report_two=0&update_and_copy_url=2&download_trigger=0&parse_url_1=0&parse_url_2=0&load_report_1=0&load_report_2=0&clear_comparison=0&refresh_log=0&clear_log=0&format=%22PDF%22&plot_A_n=1&plot_B_n=1&plot_C_n=1&plot_D_n=1&bookmark_url_1=%22%22&bookmark_url_2=%22%22&title=%22My%20Report%20Title%22&subtitle=%22Analysis%20Report%22&name=%22John%20Doe%22&plot_A_x1=%22HR%2B%2FHER2%22&plot_A_Notes_shared=%22%22&plot_A_annotation_1_1=%22fig-A-1%22&plot_A_annotation_1_2=%22ESR1%20DX%20Landscape%20A.I%22&plot_A_annotation_1_3=%22~%22&plot_A_annotation_1_4=%22~%22&plot_A_annotation_1_5=%22~%22&plot_A_annotation_1_6=%22~%22&plot_A_annotation_1_7=%22~%22&plot_A_annotation_1_8=%22%22&plot_B_x1=%22HR%2B%2FHER2%22&plot_B_Notes_shared=%22%22&plot_B_annotation_1_1=%22fig-B-1%22&plot_B_annotation_1_2=%22Treatment%20Selection%20B.I%22&plot_B_annotation_1_3=%22Treatment%20Selection%20and%20Therapeutic%20Outcomes%22&plot_B_annotation_1_4=%22~%22&plot_B_annotation_1_5=%22~%22&plot_B_annotation_1_6=%22~%22&plot_B_annotation_1_7=%22%22&plot_C_x1=%22HR%2B%2FHER2%22&plot_C_Notes_shared=%22%22&plot_C_annotation_1_1=%22fig-C-1%22&plot_C_annotation_1_2=%22Real-world%20Patient%20Outcomes%20to%20First%20Tx%20Following%20ESR1%20Dx%22&plot_C_annotation_1_3=%22~%22&plot_C_annotation_1_4=%22~%22&plot_C_annotation_1_5=%22~%22&plot_C_annotation_1_6=%22%22&plot_D_x1=%22HR%2B%2FHER2%22&plot_D_Notes_shared=%22%22&plot_D_annotation_1_1=%22fig-D-1%22&plot_D_annotation_1_2=%22Sankey%20Plot%20of%20Tx%20Sequencing%20following%20ESR1%20Dx%22&plot_D_annotation_1_3=%22~%22&plot_D_annotation_1_4=%22%22&.clientValue-default-plotlyCrosstalkOpts=%7B%22on%22%3A%22plotly_click%22%2C%22persistent%22%3Afalse%2C%22dynamic%22%3Afalse%2C%22selectize%22%3Afalse%2C%22opacityDim%22%3A0.2%2C%22selected%22%3A%7B%22opacity%22%3A1%7D%2C%22debounce%22%3A0%2C%22color%22%3A%5B%5D%7D&plotly_afterplot-A=%22%5C%22Plot_D_Obj1%5C%22%22&plotly_hover-A=null&plotly_relayout-A=%22%7B%5C%22width%5C%22%3A1052.65625%2C%5C%22height%5C%22%3A655%7D%22"
  
  loadPresetReport(preset_url)
  showNotification("Loading Basic Report Two configuration...", type = "message") 
})

  # ────────────────────────────────────────────────────────────────
  #### Download Handler with Configuration ####
  # ────────────────────────────────────────────────────────────────
  
  # Simplified download trigger using configuration
  observeEvent(input$download_trigger, {
    if (all_plots_visited()) {
      download_requested(TRUE)
      session$doBookmark()
    } else {
      # Show error modal with missing conditions
      missing_sections <- c()
      for (plot_type in names(PLOT_CONFIG)) {
        if (!check_plot_visited(plot_type, input)) {
          missing_sections <- c(missing_sections, paste("Configure", PLOT_CONFIG[[plot_type]]$label, "plots"))
        }
      }
      
      showModal(modalDialog(
        title = "Cannot Download Report",
        p("Please complete the checklist before downloading your report:"),
        tags$ul(lapply(missing_sections, tags$li)),
        easyClose = TRUE,
        footer = actionButton("close_modal", "OK", class = "btn btn-primary", 
                            `data-dismiss` = "modal")
      ))
    }
  })
  
  # Combined button handler for bookmark URL
  observeEvent(input$update_and_copy_url, {
    if (!bookmark_in_progress()) {
      bookmark_in_progress(TRUE)
      copy_after_bookmark(TRUE)
      
      safe_showNotification("Updating bookmark URL...", type = "message", duration = 2)
      
      controller$push(
        command = {
          Sys.sleep(0.1)
          "bookmark_ready"
        },
        name = paste0("bookmark_manual_", Sys.time())
      )
    }
  })
  
  # Handle bookmark completion
  onBookmarked(function(url) {
    current_bookmark_url(url)
    
    if (copy_after_bookmark()) {
      copy_after_bookmark(FALSE)
      session$sendCustomMessage("copyToClipboard", url)
      safe_showNotification("Bookmark URL updated and copied to clipboard!", type = "message", duration = 3)
    }
    
    if (download_requested()) {
      download_requested(FALSE)
      
      # Use the simplified input collection
      all_inputs <- collect_all_active_inputs(input)
      
      # Separate metadata (params) from plot configurations (vars)
      metadata_params <- list(
        title = input$title,
        subtitle = input$subtitle,
        name = input$name
      )
      
      # Get all plot-related variables (everything except metadata)
      vars_list <- all_inputs[!names(all_inputs) %in% c("title", "subtitle", "name")]
      
      # FIX: Only add QMD compatibility variables if they don't already exist
      if (is.null(vars_list$plot_A_caption_shared)) {
        vars_list$plot_A_caption_shared <- ""
      }
      if (is.null(vars_list$plot_B_caption_shared)) {
        vars_list$plot_B_caption_shared <- ""
      }

      # ADD THESE TWO LINES FOR PLOT C AND D:
      if (is.null(vars_list$plot_C_caption_shared)) {
        vars_list$plot_C_caption_shared <- ""
      }
      if (is.null(vars_list$plot_D_caption_shared)) {
        vars_list$plot_D_caption_shared <- ""
      }
      
      # Add additional vars metadata
      vars_list$bookmark_url <- url
      vars_list$generated_at <- Sys.time()
      
      # Write to YAML with separated structure
      yaml_content <- yaml::as.yaml(list(
        params = metadata_params,
        vars = vars_list
      ))
      writeLines(yaml_content, "config/report_vars.yaml")
      
      isolate({
        fmt <- switch(input$format,
          PDF = list(input = "templates/pdf_15.qmd", format = "pdf", ext = "pdf"),
          HTML = list(input = "templates/html.qmd", format = "html", ext = "html"),
          Word = list(input = "templates/word.qmd", format = "docx", ext = "docx")
        )

        out_file <- paste0("report.", fmt$ext)
        
        tryCatch({
          withProgress(message = "Rendering report", value = 0.3, {
            Sys.sleep(0.5)
            quarto::quarto_render(
              input = fmt$input,
              output_file = out_file,
              output_format = fmt$format,
              execute_params = list(title = input$title, subtitle = input$subtitle, name = input$name),
              execute_dir = getwd()
            )
            incProgress(0.6, detail = "Finalizing report...")
            Sys.sleep(0.3)
          })

          # Log the report generation using configuration
          log_entry <- generate_log_entry(all_inputs, input$format, url)
          log_line <- paste(unlist(log_entry), collapse = "\t")
          write(log_line, file = REPORT_LOG_FILE, append = TRUE)
          
          # Trigger log table refresh
          log_refresh_trigger(log_refresh_trigger() + 1)

          showModal(modalDialog(
            title = "📄 Report Ready for Download",
            div(
              style = "text-align: center; padding: 20px;",
              h4("✅ Your report has been generated successfully!", style = "color: #28a745; margin-bottom: 20px;"),
              p(paste("Format:", input$format), style = "font-weight: bold; margin-bottom: 15px;"),
              p(paste("Title:", input$title), style = "margin-bottom: 20px;"),
              
              div(
                style = "margin: 20px 0;",
                downloadLink(
                  "download_report_link", 
                  label = div(
                    style = "display: inline-block; padding: 12px 24px; background-color: #28a745; color: white; border-radius: 6px; text-decoration: none; font-weight: bold; font-size: 16px;",
                    "📥 Download Report"
                  ),
                  class = "btn btn-success btn-lg",
                  style = "text-decoration: none;"
                )
              )
            ),
            easyClose = TRUE,
            footer = div(
              style = "text-align: center;",
              actionButton("close_download_modal", "Close", class = "btn btn-secondary")
            ),
            size = "m"
          ))
        }, error = function(e) {
          showModal(modalDialog(
            title = "Error",
            p("Report generation failed."),
            p(e$message),
            easyClose = TRUE
          ))
        })
      })
    }
  })

  # ────────────────────────────────────────────────────────────────
  #### Generate Plot Outputs for All Plot Types ####
  # ────────────────────────────────────────────────────────────────
  
  # Generate plot outputs for all configured plot types
  for (plot_type in names(PLOT_CONFIG)) {
  config <- PLOT_CONFIG[[plot_type]]
  
  for (i in 1:config$max_plots) {
    local({
      my_i <- i
      my_plot_type <- plot_type
      my_config <- config
      
      output_name <- paste0("Plot_", toupper(substring(my_plot_type, 6, 6)), "_Obj", my_i)
      
      # Use conditional rendering based on plot type
      if (my_plot_type == "plot_D") {
        # Use renderPlotly for Plot D (Sankey diagram)
        output[[output_name]] <- renderPlotly({
          req(input[[paste0(my_plot_type, "_x", my_i)]])
          generate_plot_D_content(my_i, my_config, input)
        })
      } else {
        # Use renderPlot for all other plots (A, B, C)
        output[[output_name]] <- renderPlot({
          req(input[[paste0(my_plot_type, "_x", my_i)]])
          
          if (my_plot_type == "plot_A") {
            generate_plot_A_content(my_i, my_config, input)
          } else if (my_plot_type == "plot_B") {
            generate_plot_B_content(my_i, my_config, input)
          } else if (my_plot_type == "plot_C") {
            generate_plot_C_content(my_i, my_config, input)
          }
        })
      }
    })
  }
}
  
  # ────────────────────────────────────────────────────────────────
  #### Display Functions Using Configuration ####
  # ────────────────────────────────────────────────────────────────
  
  # User Guide Table (keeping your original)
  output$guideTable <- renderTable({
  steps <- list()
  
  # Add metadata step
  steps[[length(steps) + 1]] <- list(
    Step = "Enter report metadata",
    Description = "Fill in report title, subtitle, and your name"
  )
  
  # Add steps for each configured plot type
  for (plot_type in names(PLOT_CONFIG)) {
    config <- PLOT_CONFIG[[plot_type]]
    steps[[length(steps) + 1]] <- list(
      Step = paste("Configure", config$label),
      Description = paste("Choose number of figures per plot and X variables for", config$label)
    )
  }
  
  # Add final steps
  steps[[length(steps) + 1]] <- list(
    Step = "Review plots",
    Description = "Review the generated Figures"
  )
  steps[[length(steps) + 1]] <- list(
    Step = "Choose export format",
    Description = "Pick a format (PDF, HTML, Word)"
  )
  steps[[length(steps) + 1]] <- list(
    Step = "Download report",
    Description = "Download the generated report"
  )
  
  # Convert to data frame and add step numbers
  guide_df <- do.call(rbind, lapply(steps, function(x) data.frame(Step = x$Step, Description = x$Description)))
  
  # Add step numbers to the Step column
  guide_df$Step <- paste0(seq_len(nrow(guide_df)), ") ", guide_df$Step)
  
  return(guide_df)
})
  
  # Checklist Table using configuration
  output$checklistTable <- renderTable({
    generate_checklist_data(input)
  }, striped = TRUE, hover = TRUE)
  
  # Check if all conditions are met for download
  output$allConditionsMet <- reactive({
    all_plots_visited()
  })
  outputOptions(output, "allConditionsMet", suspendWhenHidden = FALSE)
  
  # Input Preview Table using configuration
  output$inputPreviewTable <- renderTable({
    generate_input_preview_data(input)
  }, striped = TRUE, hover = TRUE)

  # ────────────────────────────────────────────────────────────────
  #### Report Log Functions ####
  # ────────────────────────────────────────────────────────────────
  
  # Reactive value to trigger log table refresh
  log_refresh_trigger <- reactiveVal(0)
  
  # Function to read log file (keeping your original)
  read_log_file <- function() {
    cat("DEBUG: Looking for log file at:", REPORT_LOG_FILE, "\n")
    cat("DEBUG: Current working directory:", getwd(), "\n")
    cat("DEBUG: File exists?", file.exists(REPORT_LOG_FILE), "\n") 

    if (!file.exists(REPORT_LOG_FILE)) {
      cat("DEBUG: File does not exist, initializing...\n")
      return(data.frame(Message = "No reports generated yet"))
    }
    
    tryCatch({
      cat("DEBUG: Attempting to read file...\n")
      log_data <- read.delim(REPORT_LOG_FILE, sep = "\t", stringsAsFactors = FALSE)
      cat("DEBUG: Successfully read", nrow(log_data), "rows\n")
      
      if (nrow(log_data) == 0) {
        return(data.frame(Message = "No reports generated yet"))
      }
      
      # Clean up the data for display
      log_data <- log_data[, !apply(log_data, 2, function(x) all(x == "" | is.na(x)))]
      
      # Reorder columns for better display
      priority_cols <- c("timestamp", "format", "title", "subtitle", "name", 
                        paste0(names(PLOT_CONFIG), "_n"), "bookmark_url")
      
      existing_priority_cols <- priority_cols[priority_cols %in% names(log_data)]
      other_cols <- setdiff(names(log_data), existing_priority_cols)
      
      log_data <- log_data[, c(existing_priority_cols, other_cols)]
      
      return(log_data)
    }, error = function(e) {
      cat("DEBUG: Error reading file:", e$message, "\n")
      return(data.frame(Error = paste("Failed to read log file:", e$message)))
    })
  }
  
  # Render the report log table
  output$reportLogTable <- DT::renderDataTable({
    log_refresh_trigger()
    
    log_data <- read_log_file()

    if ("bookmark_url" %in% names(log_data) && nrow(log_data) > 0) {
      log_data$bookmark_url <- sapply(log_data$bookmark_url, function(url) {
        if (is.na(url) || url == "" || url == "Not set") {
          return(url)
        } else {
          display_text <- if (nchar(url) > 50) {
            paste0(substr(url, 1, 50), "...")
          } else {
            url
          }
          return(paste0('<a href="', url, '" target="_blank" title="', url, '">', display_text, '</a>'))
        }
      })
    }
    
    DT::datatable(
      log_data,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(width = "150px", targets = 0),
          list(width = "80px", targets = 1),
          list(width = "200px", targets = 2)
        )
      ),
      filter = "top",
      rownames = FALSE,
      escape = FALSE
    )
  })
  
  # Log management observers
  observeEvent(input$refresh_log, {
    log_refresh_trigger(log_refresh_trigger() + 1)
    safe_showNotification("Report log refreshed", type = "message", duration = 2)
  })
  
  observeEvent(input$clear_log, {
    if (file.exists(REPORT_LOG_FILE)) {
      file.remove(REPORT_LOG_FILE)
      log_refresh_trigger(log_refresh_trigger() + 1)
      safe_showNotification("Report log cleared", type = "warning", duration = 3)
    }
  })
  
  # ────────────────────────────────────────────────────────────────
  #### Compare Reports Functionality ####
  # ────────────────────────────────────────────────────────────────
  
  # Reactive values to store parsed parameters
  report1_params <- reactiveVal(list())
  report2_params <- reactiveVal(list())

  # Parse URL observers
  observeEvent(input$parse_url_1, {
    parsed <- parseBookmarkURL(input$bookmark_url_1)
    extracted <- extractReportParams(parsed)
    report1_params(extracted)
  })

  observeEvent(input$parse_url_2, {
    parsed <- parseBookmarkURL(input$bookmark_url_2)
    extracted <- extractReportParams(parsed)
    report2_params(extracted)
  })

  # Clear comparison
  observeEvent(input$clear_comparison, {
    report1_params(list())
    report2_params(list())
    updateTextAreaInput(session, "bookmark_url_1", value = "")
    updateTextAreaInput(session, "bookmark_url_2", value = "")
  })

  # Load report parameters
  observeEvent(input$load_report_1, {
    loadPresetReport_fromParams(report1_params())
    safe_showNotification("Report 1 parameters loaded successfully!", type = "default")
  })

  observeEvent(input$load_report_2, {
    loadPresetReport_fromParams(report2_params())
    safe_showNotification("Report 2 parameters loaded successfully!", type = "default")
  })
  
  # Fixed function to load parameters from comparison
loadPresetReport_fromParams <- function(params) {
  if (length(params) > 0) {
    # Update metadata inputs
    if (!is.na(params$title)) updateTextInput(session, "title", value = params$title)
    if (!is.na(params$subtitle)) updateTextInput(session, "subtitle", value = params$subtitle)
    if (!is.na(params$name)) updateTextInput(session, "name", value = params$name)
    
    # Update plot numbers for all configured plot types
    for (plot_type in names(PLOT_CONFIG)) {
      n_param <- paste0(plot_type, "_n")
      if (!is.na(params[[n_param]])) {
        updateNumericInput(session, n_param, value = params[[n_param]])
      }
    }
    
    # Store params for later application when UI is ready
    pending_preset_data(params)
  }
}
  # Generate comparison table
  output$comparisonTable <- renderTable({
    params1 <- report1_params()
    params2 <- report2_params()
    
    if (length(params1) == 0 && length(params2) == 0) {
      return(data.frame(
        Parameter = "No reports to compare",
        "Report 1" = "Enter a bookmark URL above",
        "Report 2" = "Enter a bookmark URL above",
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    # Create comparison data frame
    all_params <- unique(c(names(params1), names(params2)))
    comparison_data <- data.frame(
      Parameter = character(),
      "Report 1" = character(),
      "Report 2" = character(),
      "Match" = character(),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    for (param in all_params) {
      val1 <- if (param %in% names(params1)) {
        if (is.na(params1[[param]])) "Not set" else as.character(params1[[param]])
      } else {
        "Not available"
      }
      
      val2 <- if (param %in% names(params2)) {
        if (is.na(params2[[param]])) "Not set" else as.character(params2[[param]])
      } else {
        "Not available"
      }
      
      match_status <- if (val1 == val2) "✅" else "❌"
      readable_param <- create_readable_param_name(param)
      
      comparison_data <- rbind(comparison_data, data.frame(
        Parameter = readable_param,
        "Report 1" = val1,
        "Report 2" = val2,
        "Match" = match_status,
        check.names = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    
    return(comparison_data)
  }, striped = TRUE, hover = TRUE)
  
  # ────────────────────────────────────────────────────────────────
  #### Download Handler ####
  # ────────────────────────────────────────────────────────────────
  
  output$download_report_link <- downloadHandler(
    filename = function() {
      paste0("report.", switch(input$format,
        PDF = "pdf", HTML = "html", Word = "docx"))
    },
    content = function(file) {
      file.copy(paste0("report.", switch(input$format,
        PDF = "pdf", HTML = "html", Word = "docx")), file)
    }
  )
  
  # ────────────────────────────────────────────────────────────────
  #### Current URL Display ####
  # ────────────────────────────────────────────────────────────────
  
  output$current_url <- renderText({
    url <- current_bookmark_url()
    if (is.null(url)) {
      "No bookmark URL yet - change an input to generate"
    } else {
    }
  })
  
# END OF SERVER FUNCTION
}

# ────────────────────────────────────────────────────────────────
#### Run App ####
# ────────────────────────────────────────────────────────────────
enableBookmarking(store = "url")
shinyApp(ui, server)