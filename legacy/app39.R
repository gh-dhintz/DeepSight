# app39.R
# ──────────────────────────────────────────────────────────────
#### Load Libraries ####
# ────────────────────────────────────────────────────────────────
library(shiny)
library(dplyr)
library(ggplot2)
library(quarto)
library(shinythemes)
library(yaml)
library(crew)  # For asynchronous processing
library(DT)   # Add this for DataTable functionality
library(patchwork)
library(gt)

# ────────────────────────────────────────────────────────────────
#### Central Configuration Object (MOVE THIS TO THE TOP) ####
# ────────────────────────────────────────────────────────────────
PLOT_CONFIG <- list(
  plot_A = list(
    label = "Treatment Selection (A)",
    max_plots = 6,
    height = "655px",
    has_annotations = TRUE,
    num_annotations = 7,
    annotation_configs = list(
      list(suffix = "1", label = "Reference Label (for referencing figures):", placeholder = "Must start with 'fig-' , ie fig-A-1, etc"),
      list(suffix = "2", label = "Title:", placeholder = "My Title"),
      list(suffix = "3", label = "Subtitle:", placeholder = "My Subtitle"),
      list(suffix = "4", label = "Panel 1 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "5", label = "Panel 2 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "6", label = "Panel 3 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "7", label = "Overall Figure Caption:", placeholder = "My Figure Caption")
    ),
    # ADD: Display labels for preview table
    display_labels = c("Reference Label", "Title", "Subtitle", "Panel 1", "Panel 2", "Panel 3", "Overall Figure")
  ),
  plot_B = list(
    label = "ESR1 Dx Landscape (B)",
    max_plots = 6,
    height = "800px",
    has_annotations = TRUE,
    num_annotations = 8,  # ← CHANGE: Now 8 annotations instead of 7
    annotation_configs = list(
      list(suffix = "1", label = "Reference Label:", placeholder = "fig-B-1, etc"),
      list(suffix = "2", label = "Title:", placeholder = "My Title"),
      list(suffix = "3", label = "Subtitle:", placeholder = "My Subtitle"),
      list(suffix = "4", label = "Panel 1 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "5", label = "Panel 2 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "6", label = "Panel 3 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "7", label = "Panel 4 Caption:", placeholder = "My Panel Caption"),
      list(suffix = "8", label = "Overall Figure Caption:", placeholder = "My Figure Caption")
    ),
    # ADD: Display labels for preview table (8 labels to match 8 annotations)
    display_labels = c("Reference Label", "Title", "Subtitle", "Panel 1", "Panel 2", "Panel 3", "Panel 4", "Overall Figure")
  )
)

REPORT_LOG_FILE <- "report_log.tsv"  # Tab-separated file for tracking reports

addResourcePath("css", "www")
addResourcePath("svg", "images")

# ────────────────────────────────────────────────────────────────
#### UI Support Functions for Server Integration ####
# ────────────────────────────────────────────────────────────────

# Helper function for null coalescing
`%||%` <- function(x, y) if (is.null(x)) y else x

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
  default_x <- c("wt", "hp", "disp")[min(i, 3)]
  
  # ← CHANGED: Updated label to use Plot.Figure hierarchy
  plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
  
  base_inputs <- list(
    if (i > 1) br(),
    selectInput(x_var_name, 
               paste0("X Variable for Plot ", plot_letter, ".", Roman_numerals[i], ":"), 
               choices = unique(mtcars$gear),
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
        # ← CHANGED: Updated to use Plot.Figure hierarchy
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
  
  for (j in seq_along(annotation_configs)) {  # Changed: use j as panel number
    config <- annotation_configs[[j]]
    input_name <- paste0(plot_type, "_annotation_", i, "_", j)  # Changed: i=figure, j=panel
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
#### Server-side UI Render Functions ####
# ────────────────────────────────────────────────────────────────

# Generate UI render functions for all plot types
generate_ui_renders <- function() {
  for (plot_type in names(PLOT_CONFIG)) {
    local({
      my_plot_type <- plot_type
      config <- PLOT_CONFIG[[my_plot_type]]
      
      # Create input UI render function
      output[[paste0(my_plot_type, "_inputs_ui")]] <- renderUI({
        req(input[[paste0(my_plot_type, "_n")]])
        create_plot_ui_config(my_plot_type, input[[paste0(my_plot_type, "_n")]], isolate(reactiveValuesToList(input)))
      })
      
      # Create output UI render function
      output[[paste0(my_plot_type, "_outputs_ui")]] <- renderUI({
        req(input[[paste0(my_plot_type, "_n")]])
        
        lapply(1:input[[paste0(my_plot_type, "_n")]], function(i) {
          plotOutput(paste0("Plot_", toupper(substring(my_plot_type, 6, 6)), "_Obj", i), 
                    height = config$height, width = "100%")
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
  if (!file.exists(REPORT_LOG_FILE)) {
    # Base header columns
    header_cols <- c(
      "timestamp", "format", "title", "subtitle", "name",
      "plot_A_n", "plot_B_n", 
      "plot_A_Notes_shared", "plot_B_Notes_shared",
      "bookmark_url"
    )
    
    # Add dynamic plot columns using configuration
    for (plot_type in names(PLOT_CONFIG)) {
      header_cols <- c(header_cols, generate_plot_columns_config(plot_type))
    }
    
    # Write header
    writeLines(paste(header_cols, collapse = "\t"), REPORT_LOG_FILE)
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
        ann_name <- paste0(plot_type, "_annotation_", i, "_", j)  # Changed: i=figure, j=panel
        ann_value <- input[[ann_name]]
        
        # Get the label for this annotation position
        label <- config$display_labels[j]
        
        # Provide defaults for different annotation types using new hierarchy
        if (label == "Reference Label") {
          if (is.null(ann_value) || ann_value == "") {
            # Generate default reference label using Plot hierarchy (A, B)
            plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
            default_ref <- paste0("fig-", plot_letter, "-", numerals[i])
            inputs[[ann_name]] <- default_ref
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else if (label == "Title") {
          if (is.null(ann_value) || ann_value == "") {
            # Generate default title using Plot.Figure hierarchy (A.I, A.II, etc.)
            label_clean <- gsub("[()]", "", config$label)
            plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
            default_title <- paste0("Plot ", plot_letter, ".", Roman_numerals[i])
            inputs[[ann_name]] <- default_title
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else if (label == "Subtitle") {
          if (is.null(ann_value) || ann_value == "") {
            # Generate default subtitle
            default_subtitle <- paste0("Figure ", Roman_numerals[i], " Analysis")
            inputs[[ann_name]] <- default_subtitle
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else if (grepl("Panel", label)) {
          if (is.null(ann_value) || ann_value == "") {
            # Extract panel number from label (e.g., "Panel 1" -> "1")
            panel_num <- gsub(".*Panel ([0-9]+).*", "\\1", label)
            # Generate default using Plot.Figure.Panel hierarchy (A.I.1, A.I.2, etc.)
            plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
            default_panel <- paste0("Panel ", plot_letter, ".", Roman_numerals[i], ".", panel_num)
            inputs[[ann_name]] <- default_panel
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else if (label == "Overall Figure") {
          if (is.null(ann_value) || ann_value == "") {
            # Generate default overall figure caption
            plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
            default_figure <- paste0("Figure ", plot_letter, ".", Roman_numerals[i])
            inputs[[ann_name]] <- default_figure
          } else {
            inputs[[ann_name]] <- ann_value
          }
        } else {
          # For any other annotations, only add if not null and not empty
          if (!is.null(ann_value) && ann_value != "") {
            inputs[[ann_name]] <- ann_value
          }
        }
      }
    }
  }
  
  # Add shared notes
  notes_name <- paste0(plot_type, "_Notes_shared")
  if (!is.null(input[[notes_name]])) {
    inputs[[notes_name]] <- input[[notes_name]]
  }
  
  return(inputs)
}

# Collect all active inputs for download
collect_all_active_inputs <- function(input) {
  all_inputs <- reactiveValuesToList(input)
  
  # Remove ALL plot-specific inputs first
  plot_patterns <- c(
    "^plot_A_x", "^plot_B_x", 
    "^plot_A_annotation", "^plot_B_annotation",
    "^plot_A_Notes_shared", "^plot_B_Notes_shared"
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

  return(all_inputs)
}

# ────────────────────────────────────────────────────────────────
#### Parameter Extraction Functions ####
# ────────────────────────────────────────────────────────────────

# Configuration for parameter patterns
PARAM_PATTERNS <- list(
  metadata = c("title", "subtitle", "name"),
  base_plot = c("plot_A_n", "plot_B_n", "plot_A_Notes_shared", "plot_B_Notes_shared"),
  dynamic_plot = c("^plot_A_x", "^plot_B_x", "^plot_A_annotation", "^plot_B_annotation")
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
    plot_A_Notes_shared = paste0("📊 ", PLOT_CONFIG$plot_A$label, " - Shared Notes"),
    plot_B_Notes_shared = paste0("📊 ", PLOT_CONFIG$plot_B$label, " - Shared Notes")
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
    plot_A_Notes_shared = all_inputs$plot_A_Notes_shared %||% "",
    plot_B_Notes_shared = all_inputs$plot_B_Notes_shared %||% "",
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
                ann_var <- input[[paste0(plot_type, "_annotation_", i, "_", j)]]  # FIXED: i=figure, j=panel
                
                # ← CHANGE: Use plot-specific labels from configuration
                annotation_labels <- config$display_labels
                
                if (!is.null(ann_var) && ann_var != "") {
                  plot_details <- rbind(plot_details, data.frame(
                    Setting = paste0(plot_label_clean, ".", i, " - ", annotation_labels[j]),
                    Value = if(nchar(ann_var) > 50) paste0(substr(ann_var, 1, 50), "...") else ann_var,
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
#### Plot Generation Functions for Updated UI ####
# ────────────────────────────────────────────────────────────────

# Generate Plot A content with configuration
generate_plot_A_content <- function(i, config, input) {
  # Get current values using helper function
  values <- get_plot_values("plot_A", i, input)
  
  # Create plots
  dat <- mtcars |> filter(gear == values$x_var)
  
  plots <- create_plot_A_components(dat, i, values, input)
  table_component <- create_table_component(dat, i, input)
  
  # Combine with patchwork
  combine_plot_A_components(plots, table_component, values$figOvCap,
    values$title, values$subtitle)
}

# Generate Plot B content with configuration
generate_plot_B_content <- function(i, config, input) {
  # Get current values
  values <- get_plot_values("plot_B", i, input)
  
  # Create the four plots for Plot B
  dat <- mtcars |> filter(gear == values$x_var)
  
  plots <- create_plot_B_components(dat, i, values)  # ← CHANGE: Pass values
  
  # Combine with patchwork for Plot B layout
  combine_plot_B_components(plots, values)  # ← CHANGE: Pass values instead of just title
}

# Helper function to get plot values
get_plot_values <- function(plot_type, i, input) {
  Roman_numerals <- as.roman(1:10)
  x_var <- input[[paste0(plot_type, "_x", i)]]
  
  # Helper to get annotation input or default
  get_ann <- function(panel_pos, default = "") {  # Changed: renamed to panel_pos for clarity
  val <- input[[paste0(plot_type, "_annotation_", i, "_", panel_pos)]]  # Changed: i=figure, panel_pos=panel
  if (is.null(val) || val == "") default else val
}
  
  # Common defaults using new hierarchy
  plot_letter <- toupper(substr(plot_type, nchar(plot_type), nchar(plot_type)))
  default_title <- paste0("Plot ", plot_letter, ".", Roman_numerals[i])
  default_figure <- paste0("Figure ", plot_letter, ".", Roman_numerals[i])
  default_ref <- paste0("@fig-", plot_letter, "-", i)
  
  if (plot_type == "plot_A") {
    list(
      x_var = x_var,
      figRef = if (get_ann(1) != "") paste0("@", gsub("[^a-zA-Z0-9]", "", get_ann(1))) else default_ref,
      title = get_ann(2, default_title),
      subtitle = get_ann(3),
      tbl1Cap = get_ann(4, paste0("Panel ", plot_letter, ".", Roman_numerals[i], ".1")),    # ← Panel hierarchy
      plot1Cap = get_ann(5, paste0("Panel ", plot_letter, ".", Roman_numerals[i], ".2")),   # ← Panel hierarchy
      plot2Cap = get_ann(6, paste0("Panel ", plot_letter, ".", Roman_numerals[i], ".3")),   # ← Panel hierarchy
      figOvCap = get_ann(7, default_figure)
    )
  } else if (plot_type == "plot_B") {
    list(
      x_var = x_var,
      figRef = if (get_ann(1) != "") paste0("@", gsub("[^a-zA-Z0-9]", "", get_ann(1))) else default_ref,
      title = get_ann(2, default_title),
      subtitle = get_ann(3),
      plot1Cap = get_ann(4, paste0("Panel ", plot_letter, ".", Roman_numerals[i], ".1")),   # ← Panel hierarchy
      plot2Cap = get_ann(5, paste0("Panel ", plot_letter, ".", Roman_numerals[i], ".2")),   # ← Panel hierarchy
      plot3Cap = get_ann(6, paste0("Panel ", plot_letter, ".", Roman_numerals[i], ".3")),   # ← Panel hierarchy
      plot4Cap = get_ann(7, paste0("Panel ", plot_letter, ".", Roman_numerals[i], ".4")),   # ← Panel hierarchy
      figOvCap = get_ann(8, default_figure)
    )
  }
}

# Create plot components for Plot A
create_plot_A_components <- function(dat, i, values, input) {
  base_theme <- create_base_plot_theme()
  values <- get_plot_values("plot_A", i, input)
  
  p1 <- ggplot(dat, aes(x = mpg)) +
    geom_histogram(fill = color_primary, color = color_secondary, alpha = 0.7, bins = 15) +
    base_theme +
    labs(
      title = paste0("Distribution of MPG (", i, ")"), 
      x = "MPG", 
      y = "Frequency",
      caption = values$plot1Cap 
    ) +
    theme(plot.margin = unit(c(0.8, 0.1, 0.1, 1), "cm"))
  
  p2 <- ggplot(dat, aes(x = hp)) +
    geom_histogram(fill = color_secondary, color = color_primary, alpha = 0.7, bins = 15) +
    base_theme +
    labs(
      title = paste0("Distribution of HP (", i, ")"), 
      x = "HP", 
      y = "Frequency",
      caption = values$plot2Cap 
    ) +
    theme(plot.margin = unit(c(2.2, 0.1, 0.05, 0.1), "cm"))
  
  list(p1 = p1, p2 = p2)
}

# Create plot components for Plot B
create_plot_B_components <- function(dat, i, values) {
  base_theme <- create_base_plot_theme()
  
  # Plot 1 - Use plot1Cap from annotations
  p1 <- ggplot(dat, aes(x = mpg)) +
    geom_histogram(fill = color_primary, color = color_secondary, alpha = 0.7, bins = 15) +
    base_theme +
    labs(
      title = paste0("ESR1 Expression Distribution (", i, ")"), 
      x = "Expression Level", 
      y = "Frequency",
      caption = values$plot1Cap  # ← CHANGE: Use annotation caption
    ) +
    theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))
  
  # Plot 2 - Use plot2Cap from annotations  
  p2 <- ggplot(dat, aes(x = hp)) +
    geom_histogram(fill = color_secondary, color = color_primary, alpha = 0.7, bins = 15) +
    base_theme +
    labs(
      title = paste0("ESR1 Mutation Frequency (", i, ")"), 
      x = "Mutation Count", 
      y = "Frequency",
      caption = values$plot2Cap  # ← CHANGE: Use annotation caption
    ) +
    theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))
  
  p3 <- ggplot(dat, aes(x = disp)) +
    geom_histogram(fill = color_primary, color = color_secondary, alpha = 0.7, bins = 15) +
    base_theme +
    labs(
      title = paste0("ESR1 Response Correlation (", i, ")"), 
      x = "Response Score", 
      y = "Frequency",
      caption = values$plot3Cap  
    ) +
    theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))
  
  p4 <- ggplot(dat, aes(x = wt)) +
    geom_histogram(fill = color_secondary, color = color_primary, alpha = 0.7, bins = 15) +
    base_theme +
    labs(
      title = paste0("ESR1 Diagnostic Accuracy (", i, ")"), 
      x = "Accuracy Score", 
      y = "Frequency",
      caption = values$plot4Cap  
    ) +
    theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt"))

  list(p1 = p1, p2 = p2, p3 = p3, p4 = p4)
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
      plot.title = element_text(face = "bold", color = color_primary)
    )
}

# Create table component
create_table_component <- function(dat, i, input) {
  # Get current values using helper function
  values <- get_plot_values("plot_A", i, input)
  table_data <- dat %>%
    select(mpg, cyl, hp) %>%
    head(5) %>%
    round(2)
  
  table_data <- cbind(Car = rownames(mtcars)[1:5], table_data) %>%
    as.data.frame()
  
  gt_tab <- create_styled_gt_table(table_data, i)
  
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

# Create styled GT table
create_styled_gt_table <- function(table_data, i) {
  table_data %>%
    gt() %>%
    tab_header(title = paste0("Sample Data (", i, ")")) %>%
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
        cell_text(size = px(20), weight = "normal"),
        cell_borders(sides = c("left", "right"), color = "transparent", weight = px(15))
      ),
      locations = cells_body()
    ) %>%
    tab_style(
      style = list(
        cell_text(size = px(22), weight = "bold"),
        cell_borders(sides = c("left", "right"), color = "transparent", weight = px(15))
      ),
      locations = cells_column_labels()
    ) %>%
    tab_style(
      style = cell_text(size = px(24), weight = "bold", color = "#cc4c02"),
      locations = cells_title()
    ) %>%
    cols_width(
      Car ~ px(200),      
      mpg ~ px(150),
      cyl ~ px(150),
      hp ~ px(150)
    )
}

# Combine plot components for Plot A
combine_plot_A_components <- function(plots, table_component, overall_caption,
    title, subtitle) {
  theme_border <- create_border_theme_A()
  
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

# Combine plot components for Plot B
combine_plot_B_components <- function(plots, values) {  # ← CHANGE: Accept values parameter
  theme_border <- create_border_theme_B()
  
  plots$p1 / (plots$p2 + (plots$p3 / plots$p4)) + 
    plot_annotation(
      title = values$title,      # ← CHANGE: Use annotation title
      subtitle = values$subtitle, # ← CHANGE: Use annotation subtitle
      caption = values$figOvCap,  # ← CHANGE: Use annotation overall caption
      tag_levels = '1',
      tag_suffix = ")",
      theme = theme_border
    ) +
    plot_layout(
      heights = c(0.4, 1),
      guides = "collect"
    )
}

# Create border theme for Plot A
create_border_theme_A <- function() {
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

# Create border theme for Plot B
create_border_theme_B <- function() {
  theme_void() +  # ← CHANGE: Use theme_void like Plot A
    theme(
      plot.background = element_rect(fill = NA, colour = '#f4dbcc', size = 1),
      plot.title = element_text(
        size = 18, hjust = 0.5, face = "bold", family = "Helvetica Neue",  # ← CHANGE: Match Plot A
        margin = margin(t = 15, b = 10, unit = "pt")
      ),
      plot.subtitle = element_text(  # ← ADD: Subtitle support
        size = 16, hjust = 0.5, family = "Helvetica Neue",
        margin = margin(t = 0, b = 10, unit = "pt")
      ),
      plot.caption = element_text(  # ← ADD: Caption support
        size = 17, colour = "#C0C0C0", hjust = 0.9,
        margin = margin(t = 10, b = 55, unit = "pt")
      ),
      plot.tag = element_text(
        size = 18, face = "bold", color = "#cc4c02"  # ← CHANGE: Match Plot A
      ),
      plot.margin = margin(t = 10, r = 5, b = 15, l = 5, unit = "pt")  # ← CHANGE: Match Plot A
    )
}

# ────────────────────────────────────────────────────────────────
#### Color and Style Constants ####
# ────────────────────────────────────────────────────────────────

# Define colors and fonts (these should be defined in your main file, but including here for completeness)
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
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
    ),
    # Add JavaScript for clipboard functionality
    tags$head(
      tags$link(rel = "icon", href = "svg/Blood_Drop_Full_Color.svg", type = "image/svg+xml"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
      # Add collapsible carrot for Plot Annotations 
      tags$script(HTML(
        "$(document).on('shown.bs.collapse', '.collapse', function() {
          var icon = $(this).prev().find('i');
          icon.removeClass('fa-caret-right').addClass('fa-caret-down');
        }).on('hidden.bs.collapse', '.collapse', function() {
          var icon = $(this).prev().find('i');
          icon.removeClass('fa-caret-down').addClass('fa-caret-right');
        });"
      )),
      # ADD CLIPBOARD JAVASCRIPT
      tags$script(HTML("
        // Enhanced clipboard functionality with better error handling
Shiny.addCustomMessageHandler('copyToClipboard', function(text) {
  console.log('Attempting to copy to clipboard:', text);
  
  if (navigator.clipboard && window.isSecureContext) {
    // Use modern clipboard API if available
    navigator.clipboard.writeText(text).then(function() {
      console.log('✅ Text copied to clipboard successfully using modern API');
    }).catch(function(err) {
      console.error('❌ Modern clipboard API failed:', err);
      // Fallback to older method
      fallbackCopyTextToClipboard(text);
    });
  } else {
    console.log('⚠️ Modern clipboard API not available, using fallback');
    // Fallback for older browsers or non-secure contexts
    fallbackCopyTextToClipboard(text);
  }
});

function fallbackCopyTextToClipboard(text) {
  console.log('Using fallback clipboard method');
  var textArea = document.createElement('textarea');
  textArea.value = text;
  textArea.style.top = '0';
  textArea.style.left = '0';
  textArea.style.position = 'fixed';
  textArea.style.width = '2em';
  textArea.style.height = '2em';
  textArea.style.padding = '0';
  textArea.style.border = 'none';
  textArea.style.outline = 'none';
  textArea.style.boxShadow = 'none';
  textArea.style.background = 'transparent';
  
  document.body.appendChild(textArea);
  textArea.focus();
  textArea.select();
  
  try {
    var successful = document.execCommand('copy');
    if (successful) {
      console.log('✅ Fallback: Copying text command was successful');
    } else {
      console.log('❌ Fallback: Copying text command was unsuccessful');
      alert('Copy failed. Please manually copy the URL from the browser address bar.');
    }
  } catch (err) {
    console.error('❌ Fallback: Oops, unable to copy', err);
    alert('Copy failed. Please manually copy the URL from the browser address bar.');
  }
  
  document.body.removeChild(textArea);
}
      "))
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
            /* Your existing responsive CSS... */
            @media (min-width: 768px) {
              .navbar { 
                 /* align-items: center !important; */
                display: flex !important;
              }
              .navbar .container-fluid {
                display: flex !important;
                align-items: center !important;
              }
              .navbar-header { 
                display: flex !important; 
                /* align-items: center !important; */
                float: left !important;
              }
              .navbar-collapse { 
                display: flex !important; 
                /* align-items: center !important; */
                float: right !important;
              }
              .navbar-nav { 
                display: flex !important; 
                /* align-items: center !important; */
                margin: 0 !important;
              }
              .navbar-nav > li > a { 
                font-size: 16px !important; 
                padding: 15px !important;
                display: flex !important;
                align-items: center !important;
              }
              .navbar-brand {
                font-size: 20px !important;
                padding: 15px !important;
                display: flex !important;
                align-items: center !important;
              }
            }
            
            @media (max-width: 767px) {
              .navbar-header {
                float: left !important;
                width: 100% !important;
              }
              .navbar-toggle {
                display: block !important;
                float: right !important;
                margin-right: 15px !important;
              }
              .navbar-collapse {
                display: none !important;
                width: 100% !important;
                clear: both !important;
              }
              .navbar-collapse.in {
                display: block !important;
              }
              .navbar-nav {
                margin: 0 !important;
                float: none !important;
              }
              .navbar-nav > li {
                float: none !important;
              }
              .navbar-nav > li > a {
                padding: 10px 15px !important;
              }
            }
            
            /* Active tab styling - choose your preferred option */
            .navbar-nav > li.active > a {
              background-color: transparent !important;
              border-bottom: 3px solid #cc4c02 !important;
              color: #cc4c02 !important;
            }
            
            .navbar-nav > li > a:hover,
            .navbar-nav > li > a:focus {
              background-color: rgba(204, 76, 2, 0.1) !important;
              color: #cc4c02 !important;
            }
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
  for (plot_type in names(PLOT_CONFIG)) {
    local({
      my_plot_type <- plot_type
      config <- PLOT_CONFIG[[my_plot_type]]
      
      # Create input UI render function
      output[[paste0(my_plot_type, "_inputs_ui")]] <- renderUI({
        req(input[[paste0(my_plot_type, "_n")]])
        create_plot_ui_config(my_plot_type, input[[paste0(my_plot_type, "_n")]], isolate(reactiveValuesToList(input)))
      })
      
      # Create output UI render function
      output[[paste0(my_plot_type, "_outputs_ui")]] <- renderUI({
        req(input[[paste0(my_plot_type, "_n")]])
        
        lapply(1:input[[paste0(my_plot_type, "_n")]], function(i) {
          plotOutput(paste0("Plot_", toupper(substring(my_plot_type, 6, 6)), "_Obj", i), 
                    height = config$height, width = "100%")
        })
      })
    })
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
  
  # Function to parse URL parameters (keeping your original)
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
          
          if (grepl("^[0-9]+$", value)) {
            value <- as.numeric(value)
          }
          
          param_list[[key]] <- value
        }
      }
    }
    
    return(param_list)
  }
  
  # Simplified preload function using configuration
  # Fixed loadPresetReport function that allows editing after loading
# Simpler approach: Store preset data and apply when UI is ready
loadPresetReport <- function(preset_url) {
  parsed <- parseBookmarkURL(preset_url)
  extracted <- extractReportParams(parsed)
  
  if (length(extracted) > 0) {
    # Update metadata inputs first
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
    
    # Store the extracted values in a reactive value for later use
    pending_preset_data(extracted)
  }
}

# Add this reactive value to your server function (put it near the top with other reactive values)
pending_preset_data <- reactiveVal(NULL)

# Add this observer to your server function (put it with other observers)
# This observer watches for UI changes and applies preset data when UI is ready
observe({
  preset_data <- pending_preset_data()
  
  if (!is.null(preset_data)) {
    # Try to apply preset data for each plot type
    for (plot_type in names(PLOT_CONFIG)) {
      config <- PLOT_CONFIG[[plot_type]]
      n_plots <- input[[paste0(plot_type, "_n")]]
      
      if (!is.null(n_plots) && n_plots > 0) {
        # Check if UI is ready for this plot type by checking if first input exists
        first_x_param <- paste0(plot_type, "_x1")
        
        if (!is.null(input[[first_x_param]])) {
          # UI is ready, apply preset data for this plot type
          for (i in 1:n_plots) {
            # Update X variables
            x_param <- paste0(plot_type, "_x", i)
            if (x_param %in% names(preset_data) && !is.na(preset_data[[x_param]])) {
              current_value <- input[[x_param]]
              preset_value <- preset_data[[x_param]]
              # Only update if current value is different from preset value
              if (is.null(current_value) || current_value != preset_value) {
                updateSelectInput(session, x_param, selected = preset_value)
              }
            }
            
            # Update annotation inputs if this plot type has them
            if (config$has_annotations) {
              for (j in 1:config$num_annotations) {
                ann_param <- paste0(plot_type, "_annotation_", j, "_", i)
                if (ann_param %in% names(preset_data) && !is.na(preset_data[[ann_param]])) {
                  current_value <- input[[ann_param]]
                  preset_value <- preset_data[[ann_param]]
                  # Only update if current value is different from preset value
                  if (is.null(current_value) || current_value != preset_value) {
                    updateTextInput(session, ann_param, value = preset_value)
                  }
                }
              }
            }
          }
          
          # Update shared notes
          notes_param <- paste0(plot_type, "_Notes_shared")
          if (notes_param %in% names(preset_data) && !is.na(preset_data[[notes_param]])) {
            current_value <- input[[notes_param]]
            preset_value <- preset_data[[notes_param]]
            # Only update if current value is different from preset value
            if (is.null(current_value) || current_value != preset_value) {
              updateTextAreaInput(session, notes_param, value = preset_value)
            }
          }
        }
      }
    }
    
    # Check if all plot types have been processed
    all_processed <- TRUE
    for (plot_type in names(PLOT_CONFIG)) {
      n_plots <- input[[paste0(plot_type, "_n")]]
      if (!is.null(n_plots) && n_plots > 0) {
        first_x_param <- paste0(plot_type, "_x1")
        if (is.null(input[[first_x_param]])) {
          all_processed <- FALSE
          break
        }
      }
    }
    
    # If all plot types have been processed, clear the pending data
    if (all_processed) {
      pending_preset_data(NULL)
    }
  }
})
  
  # Preload report observers (keeping your original button IDs)
  observeEvent(input$load_basic_report, {
    preset_url <- "http://127.0.0.1:6680/?_inputs_&nav=%22Report%20Export%22&plots_subtabs=%22Treatment%20Selection%20(A)%22&load_basic_report=0&load_basic_report_two=0&update_and_copy_url=0&download_trigger=0&parse_url_1=0&parse_url_2=0&load_report_1=0&load_report_2=0&clear_comparison=0&refresh_log=0&clear_log=0&format=%22PDF%22&plot_A_n=2&plot_B_n=1&bookmark_url_1=%22%22&bookmark_url_2=%22%22&title=%22Treatment%20Selection%20Analysis%22&subtitle=%22Comprehensive%20Treatment%20Evaluation%22&name=%22Research%20Team%22&plot_A_x1=3&plot_A_x2=4&plot_B_x1=3&plot_A_annotation_1_1=%22fig-A-1%22&plot_A_annotation_2_1=%22Plot%20A.I%22&plot_A_annotation_3_1=%22Figure%20I%20Analysis%22&plot_A_annotation_4_1=%22Panel%20A.I.1%22&plot_A_annotation_5_1=%22Panel%20A.I.2%22&plot_A_annotation_6_1=%22Panel%20A.I.3%22&plot_A_annotation_7_1=%22Figure%20A.I%22&plot_A_annotation_1_2=%22fig-A-2%22&plot_A_annotation_2_2=%22Plot%20A.II%22&plot_A_annotation_3_2=%22Figure%20II%20Analysis%22&plot_A_annotation_4_2=%22Panel%20A.II.1%22&plot_A_annotation_5_2=%22Panel%20A.II.2%22&plot_A_annotation_6_2=%22Panel%20A.II.3%22&plot_A_annotation_7_2=%22Figure%20A.II%22&plot_B_annotation_1_1=%22fig-B-1%22&plot_B_annotation_2_1=%22Plot%20B.I%22&plot_B_annotation_3_1=%22Figure%20I%20Analysis%22&plot_B_annotation_4_1=%22Panel%20B.I.1%22&plot_B_annotation_5_1=%22Panel%20B.I.2%22&plot_B_annotation_6_1=%22Panel%20B.I.3%22&plot_B_annotation_7_1=%22Panel%20B.I.4%22&plot_B_annotation_8_1=%22Figure%20B.I%22&plot_A_Notes_shared=%22These%20figures%20show%20comprehensive%20treatment%20selection%20analysis.%20See%20@fig-A-1%20and%20@fig-A-2%20for%20detailed%20results.%22&plot_B_Notes_shared=%22This%20analysis%20covers%20the%20complete%20ESR1%20diagnostic%20landscape.%20See%20@fig-B-1%20for%20comprehensive%20results.%22"
  
    loadPresetReport(preset_url)
    safe_showNotification("Basic Report configuration loaded successfully!", type = "default")
  })
  
  observeEvent(input$load_basic_report_two, {
    preset_url <- "http://127.0.0.1:6680/?_inputs_&nav=%22Report%20Export%22&plots_subtabs=%22Treatment%20Selection%20(A)%22&load_basic_report=0&load_basic_report_two=0&update_and_copy_url=0&download_trigger=0&parse_url_1=0&parse_url_2=0&load_report_1=0&load_report_2=0&clear_comparison=0&refresh_log=0&clear_log=0&format=%22PDF%22&plot_A_n=2&plot_B_n=1&bookmark_url_1=%22%22&bookmark_url_2=%22%22&title=%22Treatment%20Selection%20Analysis%22&subtitle=%22Comprehensive%20Treatment%20Evaluation%22&name=%22Research%20Team%22&plot_A_x1=3&plot_A_x2=4&plot_B_x1=3&plot_A_annotation_1_1=%22fig-A-1%22&plot_A_annotation_2_1=%22Plot%20A.I%22&plot_A_annotation_3_1=%22Figure%20I%20Analysis%22&plot_A_annotation_4_1=%22Panel%20A.I.1%22&plot_A_annotation_5_1=%22Panel%20A.I.2%22&plot_A_annotation_6_1=%22Panel%20A.I.3%22&plot_A_annotation_7_1=%22Figure%20A.I%22&plot_A_annotation_1_2=%22fig-A-2%22&plot_A_annotation_2_2=%22Plot%20A.II%22&plot_A_annotation_3_2=%22Figure%20II%20Analysis%22&plot_A_annotation_4_2=%22Panel%20A.II.1%22&plot_A_annotation_5_2=%22Panel%20A.II.2%22&plot_A_annotation_6_2=%22Panel%20A.II.3%22&plot_A_annotation_7_2=%22Figure%20A.II%22&plot_B_annotation_1_1=%22fig-B-1%22&plot_B_annotation_2_1=%22Plot%20B.I%22&plot_B_annotation_3_1=%22Figure%20I%20Analysis%22&plot_B_annotation_4_1=%22Panel%20B.I.1%22&plot_B_annotation_5_1=%22Panel%20B.I.2%22&plot_B_annotation_6_1=%22Panel%20B.I.3%22&plot_B_annotation_7_1=%22Panel%20B.I.4%22&plot_B_annotation_8_1=%22Figure%20B.I%22&plot_A_Notes_shared=%22These%20figures%20show%20comprehensive%20treatment%20selection%20analysis.%20See%20@fig-A-1%20and%20@fig-A-2%20for%20detailed%20results.%22&plot_B_Notes_shared=%22This%20analysis%20covers%20the%20complete%20ESR1%20diagnostic%20landscape.%20See%20@fig-B-1%20for%20comprehensive%20results.%22"
  
    loadPresetReport(preset_url)
    safe_showNotification("Basic Report Two configuration loaded successfully!", type = "default")
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
      
      # Add additional vars metadata
      vars_list$bookmark_url <- url
      vars_list$generated_at <- Sys.time()
      
      # Write to YAML with separated structure
      yaml_content <- yaml::as.yaml(list(
        params = metadata_params,
        vars = vars_list
      ))
      writeLines(yaml_content, "report_vars.yaml")
      
      isolate({
        fmt <- switch(input$format,
          PDF = list(input = "pdf_13.qmd", format = "pdf", ext = "pdf"),
          HTML = list(input = "html.qmd", format = "html", ext = "html"),
          Word = list(input = "word.qmd", format = "docx", ext = "docx")
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
        
        output[[output_name]] <- renderPlot({
          req(input[[paste0(my_plot_type, "_x", my_i)]])
          
          if (my_plot_type == "plot_A") {
            generate_plot_A_content(my_i, my_config, input)
          } else {
            generate_plot_B_content(my_i, my_config, input)
          }
        })
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
    if (!file.exists(REPORT_LOG_FILE)) {
      return(data.frame(Message = "No reports generated yet"))
    }
    
    tryCatch({
      log_data <- read.delim(REPORT_LOG_FILE, sep = "\t", stringsAsFactors = FALSE)
      
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
  
  # Function to load parameters from comparison
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
      
      invalidateLater(500, session)
      
      observe({
        # Update inputs for all configured plot types
        for (plot_type in names(PLOT_CONFIG)) {
          # Update X variables
          x_pattern <- paste0("^", plot_type, "_x")
          x_params <- params[grepl(x_pattern, names(params))]
          for (param_name in names(x_params)) {
            if (!is.na(x_params[[param_name]])) {
              updateSelectInput(session, param_name, selected = x_params[[param_name]])
            }
          }
          
          # Update shared notes
          notes_param <- paste0(plot_type, "_Notes_shared")
          if (!is.na(params[[notes_param]])) {
            updateTextAreaInput(session, notes_param, value = params[[notes_param]])
          }
        }
      })
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
