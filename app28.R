# ────────────────────────────────────────────────────────────────
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
#### Global Configuration Parameters ####
# ────────────────────────────────────────────────────────────────
MAX_PLOTS_A <- 6  # Maximum number of Plot A plots
MAX_PLOTS_B <- 6  # Maximum number of Plot B plots

# Configurable Plot Labels (change these to customize all UI text)
PLOT_A_LABEL <- "Treatment Selection"   # Display name for Plot A everywhere
PLOT_B_LABEL <- "ESR1 Dx Landscape"  # Display name for Plot B everywhere

REPORT_LOG_FILE <- "report_log.tsv"  # Tab-separated file for tracking reports

addResourcePath("css", "www")
addResourcePath("svg", "images")


# ────────────────────────────────────────────────────────────────
#### Data Structure Overview ####
# ────────────────────────────────────────────────────────────────
# PARAMS: Metadata passed via execute_params (title, subtitle, name)
# VARS: Plot configurations stored in report_vars.yaml (plot settings, captions, etc.)
# This separation allows clean handling of document metadata vs analysis parameters

# ────────────────────────────────────────────────────────────────
#### Define Colors and Fonts (LSHTM-inspired) ####
# ────────────────────────────────────────────────────────────────
color_bg <- "#F8F9FA"
color_panel_bg <- "#FFFFFF"
color_outer_bg <- "#ECECEC"
color_fg <- "#212529"
color_primary <- "#cc4c02"
color_secondary <- "#636363"

# ────────────────────────────────────────────────────────────────
#### UI ####
# ────────────────────────────────────────────────────────────────
ui <- function(request) {
  fluidPage(
    # Add JavaScript for clipboard functionality
    tags$head(
      tags$link(rel = "icon", href = "svg/Blood_Drop_Full_Color.svg", type = "image/svg+xml"),
      tags$link(rel = "stylesheet", type = "text/css", href = "css/styles.css"),
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
    
    navbarPage(
      theme = shinytheme("flatly"),
      collapsible = TRUE,
      id = "nav",
      title = div(
        tags$img(src = "svg/Blood_Drop_Full_Color.svg", height = "40px", style = "margin-right:10px;"),
        "DeepSight™"
      ),

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
),
      
      ##### User Guide #####
      tabPanel("User Guide",
        h4("Instructions for Use", class = "text-primary"),
        tableOutput("guideTable"),
        
        # Add Tips section
        br(),
        h4("Tips", class = "text-primary"),
        tags$ul(
          tags$li("Plots will be arranged in the report as they appear, top to bottom in the dashboard"),
          tags$li("Your configuration is automatically saved in the URL - bookmark it to save your work")
        ),
        
        # Add Report Metadata section
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
      
      ##### Preload Reports #####
      tabPanel("Preload Reports",
        h4("Quick Start with Predefined Reports", class = "text-primary"),
        p("Load pre-configured report templates to get started quickly. Click any button below to load the report configuration."),
        
        br(),
        
        # Basic Report Row
        fluidRow(
          column(8,
            div(
              class = "panel panel-default",
              style = "padding: 15px; margin-bottom: 15px; border: 1px solid #ddd; border-radius: 5px;",
              h5("Basic Report", class = "text-info", style = "margin-top: 0;"),
              p("A sample report with 2 Plot A configurations and 1 Plot B configuration. Includes example captions and notes.", 
                style = "margin-bottom: 10px; color: #666;"),
              tags$ul(
                tags$li("Plot A: 2 plots (mpg vs mpg, cyl vs mpg)"),
                tags$li("Plot B: 1 plot (mpg vs mpg)"),
                tags$li("Includes sample captions and figure references")
              )
            )
          ),
          column(4,
            div(style = "padding-top: 30px;",
              actionButton("load_basic_report", "Load Basic Report", 
                          class = "btn btn-primary btn-lg", 
                          style = "width: 100%;")
            )
          )
        ),
        
        # Basic Report Two Row
        fluidRow(
          column(8,
            div(
              class = "panel panel-default",
              style = "padding: 15px; margin-bottom: 15px; border: 1px solid #ddd; border-radius: 5px;",
              h5("Basic Report Two", class = "text-info", style = "margin-top: 0;"),
              p("Another sample report configuration with the same settings as Basic Report. Useful for testing or as an alternative starting point.", 
                style = "margin-bottom: 10px; color: #666;"),
              tags$ul(
                tags$li("Plot A: 2 plots (mpg vs mpg, cyl vs mpg)"),
                tags$li("Plot B: 1 plot (mpg vs mpg)"),
                tags$li("Includes sample captions and figure references")
              )
            )
          ),
          column(4,
            div(style = "padding-top: 30px;",
              actionButton("load_basic_report_two", "Load Basic Report Two", 
                          class = "btn btn-primary btn-lg", 
                          style = "width: 100%;")
            )
          )
        ),
        
        br(),
        
        div(
          class = "alert alert-info",
          style = "margin-top: 20px; padding: 15px;",
          h6("💡 How it works:", class = "text-info", style = "margin-bottom: 5px;"),
          p("These preload buttons will automatically configure all the plot settings, captions, and metadata based on predefined templates. After loading, you can modify any settings in the Plot A, Plot B, or User Guide tabs before generating your report.", 
            style = "margin-bottom: 0; font-size: 14px;")
        )
      ),
      
      ##### Plots (Main Tab with Sub-tabs) #####
      tabPanel("Plots",
        tabsetPanel(
          id = "plots_subtabs",
          
          ##### Plot A (Sub-tab) #####
          tabPanel(PLOT_A_LABEL,
            sidebarLayout(
              sidebarPanel(width = 3,
                h4(paste("Inputs for", PLOT_A_LABEL), class = "text-primary"),
                numericInput("plot_A_n", paste0("Number of plots (up to ", MAX_PLOTS_A ,"):"), value = 1, min = 1, max = MAX_PLOTS_A),
                
                # Dynamic inputs generated by server
                uiOutput("plot_A_inputs_ui")
              ),
              mainPanel(
                # Dynamic plot outputs generated by server
                uiOutput("plot_A_outputs_ui")
              )
            )
          ),
          
          ##### Plot B (Sub-tab) #####
          tabPanel(PLOT_B_LABEL,
            sidebarLayout(
              sidebarPanel(width = 3,
                h4(paste("Inputs for", PLOT_B_LABEL), class = "text-primary"),
                numericInput("plot_B_n", paste0("Number of plots (up to ", MAX_PLOTS_B ,"):"), value = 1, min = 1, max = MAX_PLOTS_B),
                
                # Dynamic inputs generated by server
                uiOutput("plot_B_inputs_ui")
              ),
              mainPanel(
                # Dynamic plot outputs generated by server
                uiOutput("plot_B_outputs_ui")
              )
            )
          )
        )
      ),
      
      ##### Report Export #####
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
      ),
      
      ##### Compare Reports #####
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
      ),

      ##### Report Log #####
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
    )
  )
}

# ────────────────────────────────────────────────────────────────
#### Server ####
# ────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # DEBUG FUNCTION
  safe_showNotification <- function(message, type = "default", duration = 5, ...) {
    cat("DEBUG: showNotification called with message:", message, "\n")
    cat("DEBUG: showNotification called with type:", type, "\n")
    cat("DEBUG: Valid types are: default, message, warning, error\n")
    
    # Check if type is valid
    valid_types <- c("default", "message", "warning", "error")
    if (!type %in% valid_types) {
      cat("ERROR: Invalid type '", type, "' provided to showNotification\n")
      cat("This call came from:\n")
      print(sys.calls())
      # Use default instead
      type <- "default"
    }
    
    showNotification(message, type = type, duration = duration, ...)
  }


# NOTE: This observer can be placed anywhere the server function to handle the close button
observeEvent(input$close_download_modal, {
  removeModal()
})
  
  # Function to initialize log file if it doesn't exist
initialize_log_file <- function() {
  if (!file.exists(REPORT_LOG_FILE)) {
    # Create header row with all possible columns
    header_cols <- c(
      "timestamp", "format", "title", "subtitle", "name",
      "plot_A_n", "plot_B_n", 
      "plot_A_caption_shared", "plot_A_Notes_shared",
      "plot_B_caption_shared", "plot_B_Notes_shared",
      "bookmark_url"
    )
    
    # Add dynamic plot columns (up to MAX_PLOTS for each type)
    for (i in 1:MAX_PLOTS_A) {
      header_cols <- c(header_cols, 
                      paste0("plot_A_x", i),
                      paste0("plot_A_SubCaption", i))
    }
    
    for (i in 1:MAX_PLOTS_B) {
      header_cols <- c(header_cols,
                      paste0("plot_B_x", i), 
                      paste0("plot_B_SubCaption", i))
    }
    
    # Write header
    writeLines(paste(header_cols, collapse = "\t"), REPORT_LOG_FILE)
  }
}
  
  # ────────────────────────────────────────────────────────────────
#### Add Report Log Server Logic ####
# ────────────────────────────────────────────────────────────────

# Add these reactive expressions and observers in your server function:

# Reactive value to trigger log table refresh
log_refresh_trigger <- reactiveVal(0)

# Render the report log table
output$reportLogTable <- DT::renderDataTable({
  # Trigger refresh when log_refresh_trigger changes
  log_refresh_trigger()
  
  log_data <- read_log_file()

  # Convert bookmark_url column to clickable links if it exists
  if ("bookmark_url" %in% names(log_data) && nrow(log_data) > 0) {
    # Create clickable links for non-empty bookmark URLs
    log_data$bookmark_url <- sapply(log_data$bookmark_url, function(url) {
      if (is.na(url) || url == "" || url == "Not set") {
        return(url)  # Return as-is for empty/missing URLs
      } else {
        # Create a shortened display text (first 50 characters + ...)
        display_text <- if (nchar(url) > 50) {
          paste0(substr(url, 1, 50), "...")
        } else {
          url
        }
        # Return HTML link
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
        list(width = "150px", targets = 0),  # timestamp column
        list(width = "80px", targets = 1),   # format column
        list(width = "200px", targets = 2)   # title column
      )
    ),
    filter = "top",
    rownames = FALSE,
    escape = FALSE
  )
})

# Refresh log table
observeEvent(input$refresh_log, {
  log_refresh_trigger(log_refresh_trigger() + 1)
  showNotification("Report log refreshed", type = "message", duration = 2)
})

# Clear log file
observeEvent(input$clear_log, {
  if (file.exists(REPORT_LOG_FILE)) {
    file.remove(REPORT_LOG_FILE)
    log_refresh_trigger(log_refresh_trigger() + 1)
    showNotification("Report log cleared", type = "warning", duration = 3)
  }
})


#### Log Function ####
# Function to log report generation
log_report_generation <- function(all_inputs, format, bookmark_url) {
  initialize_log_file()
  
  # Create log entry
  log_entry <- list()
  log_entry$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  log_entry$format <- format
  log_entry$title <- ifelse(is.null(all_inputs$title), "", all_inputs$title)
  log_entry$subtitle <- ifelse(is.null(all_inputs$subtitle), "", all_inputs$subtitle)
  log_entry$name <- ifelse(is.null(all_inputs$name), "", all_inputs$name)
  log_entry$plot_A_n <- ifelse(is.null(all_inputs$plot_A_n), "", all_inputs$plot_A_n)
  log_entry$plot_B_n <- ifelse(is.null(all_inputs$plot_B_n), "", all_inputs$plot_B_n)
  log_entry$plot_A_caption_shared <- ifelse(is.null(all_inputs$plot_A_caption_shared), "", all_inputs$plot_A_caption_shared)
  log_entry$plot_A_Notes_shared <- ifelse(is.null(all_inputs$plot_A_Notes_shared), "", all_inputs$plot_A_Notes_shared)
  log_entry$plot_B_caption_shared <- ifelse(is.null(all_inputs$plot_B_caption_shared), "", all_inputs$plot_B_caption_shared)
  log_entry$plot_B_Notes_shared <- ifelse(is.null(all_inputs$plot_B_Notes_shared), "", all_inputs$plot_B_Notes_shared)
  log_entry$bookmark_url <- ifelse(is.null(bookmark_url), "", bookmark_url)
  
  # Add dynamic plot variables
  for (i in 1:MAX_PLOTS_A) {
    x_var_name <- paste0("plot_A_x", i)
    subcaption_name <- paste0("plot_A_SubCaption", i)
    log_entry[[x_var_name]] <- ifelse(is.null(all_inputs[[x_var_name]]), "", all_inputs[[x_var_name]])
    log_entry[[subcaption_name]] <- ifelse(is.null(all_inputs[[subcaption_name]]), "", all_inputs[[subcaption_name]])
  }
  
  for (i in 1:MAX_PLOTS_B) {
    x_var_name <- paste0("plot_B_x", i)
    subcaption_name <- paste0("plot_B_SubCaption", i)
    log_entry[[x_var_name]] <- ifelse(is.null(all_inputs[[x_var_name]]), "", all_inputs[[x_var_name]])
    log_entry[[subcaption_name]] <- ifelse(is.null(all_inputs[[subcaption_name]]), "", all_inputs[[subcaption_name]])
  }
  
  # Convert to tab-separated line
  log_line <- paste(unlist(log_entry), collapse = "\t")
  
  # Append to file
  write(log_line, file = REPORT_LOG_FILE, append = TRUE)
}

# Function to read log file
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
    # Remove empty columns
    log_data <- log_data[, !apply(log_data, 2, function(x) all(x == "" | is.na(x)))]
    
    # Reorder columns for better display
    priority_cols <- c("timestamp", "format", "title", "subtitle", "name", 
                      "plot_A_n", "plot_B_n", "bookmark_url")
    
    # Get columns that exist in the data
    existing_priority_cols <- priority_cols[priority_cols %in% names(log_data)]
    other_cols <- setdiff(names(log_data), existing_priority_cols)
    
    # Reorder
    log_data <- log_data[, c(existing_priority_cols, other_cols)]
    
    return(log_data)
  }, error = function(e) {
    return(data.frame(Error = paste("Failed to read log file:", e$message)))
  })
}
  

  # ─────────────────────────────────────────────────
  #### Initialize Crew Controller for Async Processing ####
  # ─────────────────────────────────────────────────
  
  # Create crew controller with multiple workers
  controller <- crew_controller_local(
    workers = 3,           # Number of background workers
    seconds_idle = 30,     # Keep workers alive for 30 seconds
    seconds_timeout = 60   # Maximum task timeout
  )
  controller$start()
  
  # Clean up crew controller when session ends
  session$onSessionEnded(function() {
    controller$terminate()
  })
  
  # Store the current bookmark URL
  current_bookmark_url <- reactiveVal(NULL)
  download_requested <- reactiveVal(FALSE)
  copy_after_bookmark <- reactiveVal(FALSE)
  
  # Reactive values for async UI generation (separate for each plot)
  plot_A_ui_generation_in_progress <- reactiveVal(FALSE)
  plot_B_ui_generation_in_progress <- reactiveVal(FALSE)
  bookmark_in_progress <- reactiveVal(FALSE)
  
  # ─────────────────────────────────────────────────
  #### Simplified Async Bookmark Processing ####
  # ─────────────────────────────────────────────────
  
  # Poll for completed bookmark tasks
  observe({
    invalidateLater(300, session)  # Check every 300ms
    
    # Check if any bookmark tasks are complete
    if (controller$nonempty()) {
      result <- controller$pop()
      if (!is.null(result) && !is.null(result$result) && grepl("^bookmark_", result$name)) {
        # Bookmark task completed, now actually do the bookmark
        session$doBookmark()
        bookmark_in_progress(FALSE)
      }
    }
  })
  
  # ─────────────────────────────────────────────────
  #### Dynamic UI Generation (Fully Scalable) ####
  # ─────────────────────────────────────────────────
  
  # Plot A dynamic inputs - FIXED VERSION with stable UI
output$plot_A_inputs_ui <- renderUI({
  # Only trigger re-render when the NUMBER of plots changes, not when text changes
  req(input$plot_A_n)
  
  # Create a reactive trigger that only changes when plot count changes
  plot_count_trigger <- input$plot_A_n
  
  # Generate UI with preserved values
  plot_inputs <- lapply(1:plot_count_trigger, function(i) {
    x_var_name <- paste0("plot_A_x", i)
    subcaption_name <- paste0("plot_A_SubCaption", i)
    
    # GET CURRENT VALUES OR USE DEFAULTS
    current_x <- isolate(input[[x_var_name]])  # Use isolate() to prevent reactivity
    current_subcaption <- isolate(input[[subcaption_name]])  # Use isolate()
    default_x <- if (i == 1) "wt" else if (i == 2) "hp" else "disp"
    
    div(
      if (i > 1) br(),
      selectInput(x_var_name, 
                 paste0("X Variable for ", PLOT_A_LABEL, " (", i, ")", ":"), 
                 choices = unique(mtcars$gear), # was names(mtcars) 
                 selected = if(!is.null(current_x)) current_x else default_x),
      h6(paste0("Sub-Caption for ", PLOT_A_LABEL, " (", i, ")"), 
         class = "text-secondary", style = "font-weight: bold;"),
      #p("Sub-Captions are used only in the presence of multiple plots per figure", 
      #  class = "text-muted", style = "font-size: 12px; margin-bottom: 5px;"),
      textInput(subcaption_name, NULL, 
               value = if(!is.null(current_subcaption)) current_subcaption else "",
               placeholder = "Brief description of this plot")
    )
  })

     # PRESERVE SHARED INPUT VALUES with isolate()
  current_caption <- isolate(input[["plot_A_caption_shared"]])
  current_notes <- isolate(input[["plot_A_Notes_shared"]])
  
  shared_inputs <- div(
    br(),
    h5(paste("Shared Settings for All", PLOT_A_LABEL), class = "text-info"),
    textInput("plot_A_caption_shared", 
             paste("Shared Caption for All", PLOT_A_LABEL), 
             value = if(!is.null(current_caption)) current_caption else "",
             placeholder = paste("Overall caption for the", PLOT_A_LABEL, "figure group")),
    textAreaInput("plot_A_Notes_shared", 
                 paste("Notes for", PLOT_A_LABEL, "Plots"), 
                 value = if(!is.null(current_notes)) current_notes else "",
                 placeholder = paste("Add your comments about", PLOT_A_LABEL, "here. When referencing in text, use @fig-plot_A when nplots=1; otherwise, use @fig-plot_A-1, @fig-plot_A-2, etc."), 
                 height = "100px", width = "100%")
  )
  
  return(div(plot_inputs, shared_inputs))
})

  # Plot A dynamic outputs
  output$plot_A_outputs_ui <- renderUI({
    req(input$plot_A_n)
    
    lapply(1:input$plot_A_n, function(i) {
      plotOutput(paste0("Plot_A_Obj", i), height = "655px", width = "100%")
    })
  }) 

   # Plot B dynamic inputs - FIXED VERSION with stable UI
output$plot_B_inputs_ui <- renderUI({
  # Only trigger re-render when the NUMBER of plots changes, not when text changes
  req(input$plot_B_n)
  
  # Create a reactive trigger that only changes when plot count changes
  plot_count_trigger <- input$plot_B_n
  
  # Generate individual plot inputs with preserved values
  plot_inputs <- lapply(1:plot_count_trigger, function(i) {
    x_var_name <- paste0("plot_B_x", i)
    subcaption_name <- paste0("plot_B_SubCaption", i)
    
    # GET CURRENT VALUES OR USE DEFAULTS with isolate()
    current_x <- isolate(input[[x_var_name]])  # Use isolate()
    current_subcaption <- isolate(input[[subcaption_name]])  # Use isolate()
    default_x <- if (i == 1) "wt" else if (i == 2) "hp" else "disp"
    
    div(
      key = paste0("plot_B_input_", i),
      if (i > 1) br(),
      div(
        id = paste0("plot_B_select_container_", i),
        selectInput(
          inputId = x_var_name, 
          label = paste0("X Variable for ", PLOT_B_LABEL, " (",i, ")", ":"), 
          choices = unique(mtcars$gear), # was names(mtcars) 
          selected = if(!is.null(current_x)) current_x else default_x
        )
      ),
      h6(paste0("Sub-Caption for ", PLOT_B_LABEL, " (",i, ")"), 
         class = "text-secondary", 
         style = "font-weight: bold;"),
      p("Sub-Captions are used only in the presence of multiple plots per figure", 
        class = "text-muted", 
        style = "font-size: 12px; margin-bottom: 5px;"),
      textInput(
        inputId = subcaption_name, 
        label = NULL, 
        value = if(!is.null(current_subcaption)) current_subcaption else "",
        placeholder = "Brief description of this plot"
      )
    )
  })
  
  # PRESERVE SHARED INPUT VALUES with isolate()
  current_caption <- isolate(input[["plot_B_caption_shared"]])
  current_notes <- isolate(input[["plot_B_Notes_shared"]])
  
  shared_inputs <- div(
    key = "plot_B_shared_inputs",
    br(),
    h5(paste("Shared Settings for All", PLOT_B_LABEL), class = "text-info"),
    textInput("plot_B_caption_shared", 
             paste("Shared Caption for All", PLOT_B_LABEL), 
             value = if(!is.null(current_caption)) current_caption else "",
             placeholder = paste("Overall caption for the", PLOT_B_LABEL, "figure group")),
    textAreaInput("plot_B_Notes_shared", 
                 paste("Notes for", PLOT_B_LABEL, "Plots"), 
                 value = if(!is.null(current_notes)) current_notes else "",
                 placeholder = paste("Add your comments about", PLOT_B_LABEL, "here. When referencing in text, use @fig-plot_B when nplots=1; otherwise, use @fig-plot_B-1, @fig-plot_B-2, etc."), 
                 height = "100px", 
                 width = "100%")
  )
  
  return(div(plot_inputs, shared_inputs))
})

   # Plot B dynamic outputs
  output$plot_B_outputs_ui <- renderUI({
    req(input$plot_B_n)
    
    lapply(1:input$plot_B_n, function(i) {
      plotOutput(paste0("Plot_B_Obj", i), height = "900px")
    })
  })
  
 # ─────────────────────────────────────────────────
#### Auto-bookmark on input change (IMPROVED VERSION) ####
# ─────────────────────────────────────────────────

# Create a reactive that tracks ONLY non-text inputs (for immediate bookmarking)
inputs_to_track_stable <- reactive({
  all_inputs <- list()
  
  # Static inputs (these are safe to auto-bookmark immediately)
  all_inputs$nav <- input$nav
  all_inputs$plots_subtabs <- input$plots_subtabs
  all_inputs$format <- input$format
  all_inputs$title <- input$title
  all_inputs$subtitle <- input$subtitle
  all_inputs$name <- input$name
  
  # Plot numbers (these are safe - only bookmark when count changes)
  all_inputs$plot_A_n <- input$plot_A_n
  all_inputs$plot_B_n <- input$plot_B_n
  
  return(all_inputs)
})

# Debounced version for stable inputs
inputs_stable_debounced <- debounce(inputs_to_track_stable, 1000)  # 1 second delay

# Auto-bookmark for stable inputs only
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

# Separate reactive for text and dropdown inputs (longer delay to avoid interrupting typing)
text_and_dropdown_inputs <- reactive({
  all_vals <- list()
  
  # Only track dropdown and text variables with longer delay
  if (!is.null(input$plot_A_n) && input$plot_A_n > 0) {
    for (i in 1:input$plot_A_n) {
      x_var_name <- paste0("plot_A_x", i)
      subcaption_name <- paste0("plot_A_SubCaption", i)
      all_vals[[x_var_name]] <- input[[x_var_name]]
      all_vals[[subcaption_name]] <- input[[subcaption_name]]
    }
  }
  
  if (!is.null(input$plot_B_n) && input$plot_B_n > 0) {
    for (i in 1:input$plot_B_n) {
      x_var_name <- paste0("plot_B_x", i)
      subcaption_name <- paste0("plot_B_SubCaption", i)
      all_vals[[x_var_name]] <- input[[x_var_name]]
      all_vals[[subcaption_name]] <- input[[subcaption_name]]
    }
  }
  
  # Add shared text inputs
  all_vals$plot_A_caption_shared <- input$plot_A_caption_shared
  all_vals$plot_A_Notes_shared <- input$plot_A_Notes_shared
  all_vals$plot_B_caption_shared <- input$plot_B_caption_shared
  all_vals$plot_B_Notes_shared <- input$plot_B_Notes_shared
  
  return(all_vals)
})

# Much longer debounce for text inputs to avoid interrupting typing
text_debounced <- debounce(text_and_dropdown_inputs, 5000)  # 5 seconds - only bookmark after user stops typing

# Separate observer for text and dropdown changes with longer delay
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
  
 
  
  # ─────────────────────────────────────────────────
  #### Reactive values to check section completion ####
  # ─────────────────────────────────────────────────
  plot_A_visited <- reactive({
    if (!is.null(input$plot_A_n) && input$plot_A_n > 0) {
      for (i in 1:input$plot_A_n) {
        if (!is.null(input[[paste0("plot_A_x", i)]])) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  })
  
  plot_B_visited <- reactive({
    if (!is.null(input$plot_B_n) && input$plot_B_n > 0) {
      for (i in 1:input$plot_B_n) {
        if (!is.null(input[[paste0("plot_B_x", i)]])) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  })
  
  # ─────────────────────────────────────────────────
  #### Preload Reports Functionality ####
  # ─────────────────────────────────────────────────
  
  # Function to load predefined report settings (scalable version)
  loadPresetReport <- function(preset_url) {
    # Parse the URL parameters
    parsed <- parseBookmarkURL(preset_url)
    extracted <- extractReportParams(parsed)
    
    if (length(extracted) > 0) {
      # Update metadata inputs
      if (!is.na(extracted$title)) updateTextInput(session, "title", value = extracted$title)
      if (!is.na(extracted$subtitle)) updateTextInput(session, "subtitle", value = extracted$subtitle)
      if (!is.na(extracted$name)) updateTextInput(session, "name", value = extracted$name)
      
      # Update plot numbers
      if (!is.na(extracted$plot_A_n)) updateNumericInput(session, "plot_A_n", value = extracted$plot_A_n)
      if (!is.na(extracted$plot_B_n)) updateNumericInput(session, "plot_B_n", value = extracted$plot_B_n)
      
      # Use invalidateLater to delay the input updates
      invalidateLater(500, session)
      
      # Schedule the dynamic input updates
      observe({
        # Update all dynamic inputs using scalable approach
        update_dynamic_inputs <- function(param_prefix, input_type) {
          matching_params <- extracted[grepl(paste0("^", param_prefix), names(extracted))]
          
          if (length(matching_params) > 0) {
            for (param_name in names(matching_params)) {
              param_value <- matching_params[[param_name]]
              
              if (!is.na(param_value)) {
                if (input_type == "select") {
                  updateSelectInput(session, param_name, selected = param_value)
                } else if (input_type == "text") {
                  updateTextInput(session, param_name, value = param_value)
                } else if (input_type == "textarea") {
                  updateTextAreaInput(session, param_name, value = param_value)
                }
              }
            }
          }
        }
        
        # Update all dynamic inputs
        update_dynamic_inputs("plot_A_x", "select")
        update_dynamic_inputs("plot_A_SubCaption", "text")
        update_dynamic_inputs("plot_B_x", "select")
        update_dynamic_inputs("plot_B_SubCaption", "text")
        
        # Update shared settings
        shared_inputs <- list(
          list(name = "plot_A_caption_shared", type = "text"),
          list(name = "plot_A_Notes_shared", type = "textarea"),
          list(name = "plot_B_caption_shared", type = "text"),
          list(name = "plot_B_Notes_shared", type = "textarea")
        )
        
        for (input_info in shared_inputs) {
          param_name <- input_info$name
          if (!is.na(extracted[[param_name]])) {
            if (input_info$type == "text") {
              updateTextInput(session, param_name, value = extracted[[param_name]])
            } else if (input_info$type == "textarea") {
              updateTextAreaInput(session, param_name, value = extracted[[param_name]])
            }
          }
        }
      })
    }
  }
  
  # Load Basic Report
  observeEvent(input$load_basic_report, {
    preset_url <- "http://127.0.0.1:4001/?_inputs_&nav=%22Report%20Export%22&plots_subtabs=%22My%20Second%20Plot%22&load_basic_report=0&load_basic_report_two=0&update_and_copy_url=0&download_trigger=1&parse_url_1=0&parse_url_2=0&load_report_1=0&load_report_2=0&clear_comparison=0&refresh_log=0&clear_log=0&format=%22PDF%22&plot_A_n=1&plot_B_n=1&bookmark_url_1=%22%22&bookmark_url_2=%22%22&title=%22My%20Report%20Title%22&subtitle=%22Analysis%20Report%22&name=%22John%20Doe%22&plot_A_x1=%22wt%22&plot_A_Notes_shared=%22%22&plot_A_SubCaption1=%22%22&plot_A_caption_shared=%22%22&plot_B_x1=%22wt%22&plot_B_Notes_shared=%22%22&plot_B_SubCaption1=%22%22&plot_B_caption_shared=%22%22"
    
    loadPresetReport(preset_url)
    showNotification("Basic Report configuration loaded successfully!", type = "default")
  })
  
  # Load Basic Report Two
  observeEvent(input$load_basic_report_two, {
    preset_url <- "http://127.0.0.1:4001/?_inputs_&nav=%22Report%20Export%22&plots_subtabs=%22My%20Second%20Plot%22&load_basic_report=0&load_basic_report_two=0&update_and_copy_url=0&download_trigger=1&parse_url_1=0&parse_url_2=0&load_report_1=0&load_report_2=0&clear_comparison=0&refresh_log=0&clear_log=0&format=%22PDF%22&plot_A_n=3&plot_B_n=3&bookmark_url_1=%22%22&bookmark_url_2=%22%22&title=%22My%20Report%20Title%22&subtitle=%22Analysis%20Report%22&name=%22John%20Doe%22&reportLogTable_rows_selected=null&reportLogTable_columns_selected=null&reportLogTable_cells_selected=%5B%5D&reportLogTable_rows_current=1&reportLogTable_rows_all=1&reportLogTable_state=%7B%22time%22%3A1750956379937%2C%22start%22%3A0%2C%22length%22%3A10%2C%22order%22%3A%5B%5D%2C%22search%22%3A%7B%22search%22%3A%22%22%2C%22smart%22%3Atrue%2C%22regex%22%3Afalse%2C%22caseInsensitive%22%3Atrue%7D%2C%22columns%22%3A%5B%7B%22visible%22%3Atrue%2C%22search%22%3A%7B%22search%22%3A%22%22%2C%22smart%22%3Atrue%2C%22regex%22%3Afalse%2C%22caseInsensitive%22%3Atrue%7D%7D%2C%7B%22visible%22%3Atrue%2C%22search%22%3A%7B%22search%22%3A%22%22%2C%22smart%22%3Atrue%2C%22regex%22%3Afalse%2C%22caseInsensitive%22%3Atrue%7D%7D%2C%7B%22visible%22%3Atrue%2C%22search%22%3A%7B%22search%22%3A%22%22%2C%22smart%22%3Atrue%2C%22regex%22%3Afalse%2C%22caseInsensitive%22%3Atrue%7D%7D%2C%7B%22visible%22%3Atrue%2C%22search%22%3A%7B%22search%22%3A%22%22%2C%22smart%22%3Atrue%2C%22regex%22%3Afalse%2C%22caseInsensitive%22%3Atrue%7D%7D%2C%7B%22visible%22%3Atrue%2C%22search%22%3A%7B%22search%22%3A%22%22%2C%22smart%22%3Atrue%2C%22regex%22%3Afalse%2C%22caseInsensitive%22%3Atrue%7D%7D%2C%7B%22visible%22%3Atrue%2C%22search%22%3A%7B%22search%22%3A%22%22%2C%22smart%22%3Atrue%2C%22regex%22%3Afalse%2C%22caseInsensitive%22%3Atrue%7D%7D%2C%7B%22visible%22%3Atrue%2C%22search%22%3A%7B%22search%22%3A%22%22%2C%22smart%22%3Atrue%2C%22regex%22%3Afalse%2C%22caseInsensitive%22%3Atrue%7D%7D%2C%7B%22visible%22%3Atrue%2C%22search%22%3A%7B%22search%22%3A%22%22%2C%22smart%22%3Atrue%2C%22regex%22%3Afalse%2C%22caseInsensitive%22%3Atrue%7D%7D%2C%7B%22visible%22%3Atrue%2C%22search%22%3A%7B%22search%22%3A%22%22%2C%22smart%22%3Atrue%2C%22regex%22%3Afalse%2C%22caseInsensitive%22%3Atrue%7D%7D%2C%7B%22visible%22%3Atrue%2C%22search%22%3A%7B%22search%22%3A%22%22%2C%22smart%22%3Atrue%2C%22regex%22%3Afalse%2C%22caseInsensitive%22%3Atrue%7D%7D%5D%7D&reportLogTable_search=%22%22&reportLogTable_search_columns=%5B%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%2C%22%22%5D&reportLogTable_cell_clicked=%7B%7D&plot_A_x1=%22wt%22&plot_A_Notes_shared=%22f%22&plot_A_SubCaption1=%22a%22&plot_A_caption_shared=%22e%22&plot_A_x2=%22hp%22&plot_A_SubCaption2=%22b%22&plot_A_x3=%22disp%22&plot_A_SubCaption3=%22c%22&plot_A_x4=%22disp%22&plot_A_SubCaption4=%22d%22&plot_B_x1=%22wt%22&plot_B_Notes_shared=%22%22&plot_B_SubCaption1=%22%22&plot_B_caption_shared=%22%22&plot_B_x2=%22hp%22&plot_B_SubCaption2=%22%22&plot_B_x3=%22disp%22&plot_B_SubCaption3=%22%22"
    
    loadPresetReport(preset_url)
    showNotification("Basic Report Two configuration loaded successfully!", type = "default")
  })
  
  # ─────────────────────────────────────────────────
  #### Download Report with Advanced Quarto Integration ####
  # ─────────────────────────────────────────────────
  
  observeEvent(input$download_trigger, {
    # Check if all conditions are met before proceeding
    if (plot_A_visited() && plot_B_visited()) {
      download_requested(TRUE)
      session$doBookmark()
    } else {
      showModal(modalDialog(
        title = "Cannot Download Report",
        p("Please complete the checklist before downloading your report:"),
        tags$ul(
          if(!plot_A_visited()) tags$li("Visit Plot A tab to configure plots"),
          if(!plot_B_visited()) tags$li("Visit Plot B tab to configure plots")
        ),
        easyClose = TRUE,
        footer = actionButton("close_modal", "OK", class = "btn btn-primary", 
                            `data-dismiss` = "modal")
      ))
    }
  })
  
  # Add a reactive value to track if we want to copy after bookmark
copy_after_bookmark <- reactiveVal(FALSE)

# Combined button handler (simpler version)
observeEvent(input$update_and_copy_url, {
  if (!bookmark_in_progress()) {
    bookmark_in_progress(TRUE)
    copy_after_bookmark(TRUE)  # Set flag to copy after bookmark completes
    
    showNotification("Updating bookmark URL...", type = "message", duration = 2)
    
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
    
  # If copy flag is set, copy the URL
  if (copy_after_bookmark()) {
    copy_after_bookmark(FALSE)  # Reset flag
    session$sendCustomMessage("copyToClipboard", url)
    showNotification("Bookmark URL updated and copied to clipboard!", type = "message", duration = 3)
  }
    
    if (download_requested()) {
      download_requested(FALSE)
      
      # Start with base reactive values but filter dynamically
      all_inputs <- reactiveValuesToList(input)
      
      # Remove ALL plot-specific inputs first, then add back only the active ones
      # This prevents hidden inputs from being included
      plot_patterns <- c("^plot_A_x", "^plot_B_x", "^plot_A_SubCaption", "^plot_B_SubCaption")
      for (pattern in plot_patterns) {
        to_remove <- names(all_inputs)[grepl(pattern, names(all_inputs))]
        all_inputs[to_remove] <- NULL
      }
      
      # Now dynamically capture ONLY the active plot inputs (scalable)
      for (plot_type in c("plot_A", "plot_B")) {
        n_plots <- input[[paste0(plot_type, "_n")]]
        if (!is.null(n_plots) && n_plots > 0) {
          # Only capture inputs for the actual number of plots selected
          for (i in 1:n_plots) {
            # Capture X variables (only for active plots)
            x_var_name <- paste0(plot_type, "_x_", i)
            if (!is.null(input[[x_var_name]])) {
              all_inputs[[x_var_name]] <- input[[x_var_name]]
            }
            
            # Capture SubCaptions (only for active plots)
            subcaption_name <- paste0(plot_type, "_SubCaption_", i)
            if (!is.null(input[[subcaption_name]])) {
              all_inputs[[subcaption_name]] <- input[[subcaption_name]]
            }
          }
        }
        
        # Capture shared settings (always include these)
        caption_shared <- paste0(plot_type, "_caption_shared")
        if (!is.null(input[[caption_shared]])) {
          all_inputs[[caption_shared]] <- input[[caption_shared]]
        }
        
        notes_shared <- paste0(plot_type, "_notes_shared")
        if (!is.null(input[[notes_shared]])) {
          all_inputs[[notes_shared]] <- input[[notes_shared]]
        }
      }
      
      # Separate metadata (params) from plot configurations (vars)
      metadata_params <- list(
        title = input$title,
        subtitle = input$subtitle,
        name = input$name
      )
      
      # Get all plot-related variables (everything except metadata)
      vars_list <- all_inputs[!names(all_inputs) %in% c("title", "subtitle", "name")]
      
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
          PDF = list(input = "pdf_7.qmd", format = "pdf", ext = "pdf"),
          HTML = list(input = "html.qmd", format = "html", ext = "html"),
          Word = list(input = "word.qmd", format = "docx", ext = "docx")
        )

        out_file <- paste0("report.", fmt$ext)
        
        tryCatch({
          withProgress(message = "Rendering report", value = 0.3, {
            Sys.sleep(0.5)
            # Render with execute_params to pass metadata to YAML header
            # Plot configurations come from report_vars.yaml
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

          # Log the report generation after successful render
          log_report_generation(all_inputs, input$format, url)
          
          # Trigger log table refresh
          log_refresh_trigger(log_refresh_trigger() + 1)

          showModal(modalDialog(
            title = "📄 Report Ready for Download",
            div(
              style = "text-align: center; padding: 20px;",
              h4("✅ Your report has been generated successfully!", style = "color: #28a745; margin-bottom: 20px;"),
              p(paste("Format:", input$format), style = "font-weight: bold; margin-bottom: 15px;"),
              p(paste("Title:", input$title), style = "margin-bottom: 20px;"),
              
              # Make the download button more prominent
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
              ),
              
              #p("The file will be saved with a timestamp in the filename.", 
              #  style = "font-size: 12px; color: #6c757d; margin-top: 15px;")
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


  
# Add a reactive value to trigger plot refresh
#plot_A_refresh_trigger <- reactiveVal(0)

## Add the refresh button observer
# observeEvent(input$refresh_plot_A, {
#   plot_A_refresh_trigger(plot_A_refresh_trigger() + 1)
#   showNotification("Plot A refreshed with current settings!", type = "message", duration = 2)
# })
  
  # ─────────────────────────────────────────────────
  #### Plot Outputs (Scalable) ####
  # ─────────────────────────────────────────────────
  
  # Modify the Plot A outputs to include the refresh trigger
for (i in 1:MAX_PLOTS_A) {
  local({
    my_i <- i
    output[[paste0("Plot_A_Obj", my_i)]] <- renderPlot({
      # Add the refresh trigger as a dependency
      #plot_A_refresh_trigger()
      
      # Ensure we have the required inputs
      req(input[[paste0("plot_A_x", my_i)]])
      
      # CORRECTED: Get the specific subcaption for this plot number
      current_subcaption <- if(!is.null(input[[paste0("plot_A_SubCaption", my_i)]])) {
        input[[paste0("plot_A_SubCaption", my_i)]]
      } else {
        ""
      }
      
      # Also get the shared caption if you want to use both
      shared_caption <- if(!is.null(input$plot_A_caption_shared)) {
        input$plot_A_caption_shared
      } else {
        ""
      }
      
      # Decide which caption to use - you can combine them or choose one
      final_caption <- if(current_subcaption != "" && shared_caption != "") {
        paste0(shared_caption, " - ", current_subcaption)  # Combine both
      } else if(current_subcaption != "") {
        current_subcaption  # Use subcaption only
      } else {
        shared_caption  # Use shared caption only
      }
      
      # Create the two plots
      dat <- mtcars |> filter(gear == input[[paste0("plot_A_x", my_i)]])
      p1 <- ggplot(dat, aes(x = mpg)) +
        geom_histogram(fill = color_primary, color = color_secondary, alpha = 0.7, bins = 15) +
        theme_minimal(base_family = "Helvetica Neue") +
        labs(
          title = paste0("Distribution of MPG (", my_i, ")"), 
          x = "MPG", 
          y = "Frequency",
          caption = "Plot 1"
        ) +
        theme(
          plot.background = element_rect(fill = color_panel_bg, color = NA),
          plot.caption = element_text(
            size = 15,
            margin = margin(t = 2, b = 2, unit = "pt")  
          ),
          panel.background = element_rect(fill = color_panel_bg, color = NA),
          text = element_text(color = color_fg),
          axis.text = element_text(color = color_fg),
          axis.title = element_text(color = color_fg),
          plot.title = element_text(face = "bold", color = color_primary),
          plot.margin = unit(c(0.8, 0.1, 0.1, 1), "cm")  # VERY TIGHT margins
        )
      
      p2 <- ggplot(dat, aes(x = hp)) +
        geom_histogram(fill = color_secondary, color = color_primary, alpha = 0.7, bins = 15) +
        theme_minimal(base_family = "Helvetica Neue") +
        labs(
          title = paste0("Distribution of HP (", my_i, ")"), 
          x = "HP", 
          y = "Frequency",
          caption = "Plot 2"
        ) +
        theme(
          plot.background = element_rect(fill = color_panel_bg, color = NA),
          plot.caption = element_text(
            size = 15,
            margin = margin(t = 2, b = 2, unit = "pt")  
          ),
          panel.background = element_rect(fill = color_panel_bg, color = NA),
          text = element_text(color = color_fg),
          axis.text = element_text(color = color_fg),
          axis.title = element_text(color = color_fg),
          plot.title = element_text(face = "bold", color = color_primary),
          plot.margin = unit(c(2.2, 0.1, 0.05, 0.1), "cm")  # EVEN TIGHTER bottom margin
        )
      
      # Create GT table with custom CSS for width control
      table_data <- dat %>%
        select(mpg, cyl, hp) %>%
        head(5) %>%
        round(2)
      
      table_data <- cbind(Car = rownames(mtcars)[1:5], table_data) %>%
        as.data.frame()
      
      gt_tab <- table_data %>%
        gt() %>%
        tab_header(title = paste0("Sample Data (", my_i, ")")) %>%
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
      
      table_with_caption <- wrap_table(gt_tab, panel = "full", space = "fixed") +
        labs(caption = "Table 1") + 
        theme_minimal() +
        theme(
          plot.margin = unit(c(0.8, 0.8, 0.1, 0.1), "cm"),
          plot.caption = element_text(
            size = 15,
            hjust = 1,
            family = "Helvetica Neue",  
            margin = margin(t = 2, b = 2, unit = "pt")  
          ),
          text = element_text(family = "Helvetica Neue")
      )  # TIGHT margins
      
      
  theme_border <- theme_void() + 
    theme(
      plot.background = element_rect(fill = NA, colour = '#f4dbcc', size = 1),
      plot.title = element_text(
        size = 18,
        hjust = 0.5,
        face = "bold",
        family = "Helvetica Neue",
        margin = margin(t = 15, b = 95, unit = "pt")  # Add bottom margin to title
      ),
      plot.caption = element_text(
        size = 18,
        hjust = 0.5,
        margin = margin(t = 10, b = 55, unit = "pt")  # INCREASE bottom margin for caption
      ),
      plot.tag = element_text(
        size = 18,        # FONT SIZE for tag_levels (A, B, C)
        face = "bold",    # Make tags bold
        color = "#cc4c02" # Use your primary color
      ),
      plot.margin = margin(t = 15, r = 5, b = 15, l = 5, unit = "pt")  # INCREASE bottom margin
    )
      
      # Combine with patchwork and use the final caption
      (table_with_caption + p1) / p2 + 
        plot_annotation(
          title = paste0(PLOT_A_LABEL, " (", my_i, ")"),
          caption = final_caption,  # Use the correctly constructed caption
          tag_levels = 'A',
          theme = theme_border
        ) +
        plot_layout(
          heights = c(0.4, 1),  # Table, p1, p2 (p2 gets most height)
            guides = "collect"
        ) 
    })
  })
}
  
  # Plot B outputs (scalable to any number)
  for (i in 1:MAX_PLOTS_B) {  # Uses global parameter
  local({
    my_i <- i
    output[[paste0("Plot_B_Obj", my_i)]] <- renderPlot({
      # Create the two plots
      dat <- mtcars |> filter(gear == input[[paste0("plot_B_x", my_i)]])
      p1 <- ggplot(dat, aes(x = mpg)) +
        geom_histogram(fill = color_primary, color = color_secondary, alpha = 0.7, bins = 15) +
        theme_minimal(base_family = "Helvetica Neue") +
        labs(
          title = paste0("Distribution of MPG (", my_i, ")"), 
          x = "MPG", 
          y = "Frequency",
          caption = "Plot 1"
        ) +
        theme(
          plot.background = element_rect(fill = color_panel_bg, color = NA),
          panel.background = element_rect(fill = color_panel_bg, color = NA),
          text = element_text(color = color_fg),
          axis.text = element_text(color = color_fg),
          axis.title = element_text(color = color_fg),
          plot.title = element_text(face = "bold", color = color_primary),
          plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
        )
      p2 <- ggplot(dat, aes(x = mpg)) +
        geom_histogram(fill = color_primary, color = color_secondary, alpha = 0.7, bins = 15) +
        theme_minimal(base_family = "Helvetica Neue") +
        labs(
          title = paste0("Distribution of MPG (", my_i, ")"), 
          x = "MPG", 
          y = "Frequency",
          caption = "Plot 1"
        ) +
        theme(
          plot.background = element_rect(fill = color_panel_bg, color = NA),
          panel.background = element_rect(fill = color_panel_bg, color = NA),
          text = element_text(color = color_fg),
          axis.text = element_text(color = color_fg),
          axis.title = element_text(color = color_fg),
          plot.title = element_text(face = "bold", color = color_primary),
          plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
        )
      
      p3 <- ggplot(dat, aes(x = mpg)) +
        geom_histogram(fill = color_primary, color = color_secondary, alpha = 0.7, bins = 15) +
        theme_minimal(base_family = "Helvetica Neue") +
        labs(
          title = paste0("Distribution of MPG (", my_i, ")"), 
          x = "MPG", 
          y = "Frequency",
          caption = "Plot 1"
        ) +
        theme(
          plot.background = element_rect(fill = color_panel_bg, color = NA),
          panel.background = element_rect(fill = color_panel_bg, color = NA),
          text = element_text(color = color_fg),
          axis.text = element_text(color = color_fg),
          axis.title = element_text(color = color_fg),
          plot.title = element_text(face = "bold", color = color_primary),
          plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
        )
      
      p4 <- ggplot(dat, aes(x = mpg)) +
        geom_histogram(fill = color_primary, color = color_secondary, alpha = 0.7, bins = 15) +
        theme_minimal(base_family = "Helvetica Neue") +
        labs(
          title = paste0("Distribution of MPG (", my_i, ")"), 
          x = "MPG", 
          y = "Frequency",
          caption = "Plot 1"
        ) +
        theme(
          plot.background = element_rect(fill = color_panel_bg, color = NA),
          panel.background = element_rect(fill = color_panel_bg, color = NA),
          text = element_text(color = color_fg),
          axis.text = element_text(color = color_fg),
          axis.title = element_text(color = color_fg),
          plot.title = element_text(face = "bold", color = color_primary),
          plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
        )

      theme_border <- theme_gray() + 
        theme(plot.background = element_rect(fill = NA, colour = '#f4dbcc', size = 1),
              plot.title = element_text(
                size = 16,
                hjust = 0.5,           
                face = "bold",         
                family = "Arial"     
                ),
                plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "pt")
            )
      
      # Combine with patchwork and control sizing
      p1 / (p2 + (p3 / p4)) + 
        plot_annotation(title = paste0(PLOT_B_LABEL, " (", my_i, ")"), tag_levels = 'A', theme = theme_border) +
        plot_layout(
          heights = c(1, 1, 1), 
          widths = c(1, 1, 2),    # Equal widths - table should now be wider,
          guides = "collect"  # This can help reduce some spacing
        ) 
    })
  })
}
  
  # ─────────────────────────────────────────────────
  #### Configuration Display ####
  # ─────────────────────────────────────────────────
  
  # Show current URL for user
  output$current_url <- renderText({
    url <- current_bookmark_url()
    if (is.null(url)) {
      "No bookmark URL yet - change an input to generate"
    } else {
      url
    }
  })
  
  # ─────────────────────────────────────────────────
  #### User Guide Table ####
  # ─────────────────────────────────────────────────
  output$guideTable <- renderTable({
    data.frame(
      Step = c(
        "Enter report metadata",
        "Select number of plots and X for Plot A",
        "Select number of plots and X for Plot B",
        "Review plots",
        "Choose export format",
        "Download report"
      ),
      Description = c(
        "Fill in report title, subtitle, and your name",
        "Choose how many plots and X variables for Plot A",
        "Choose how many plots and X variables for Plot B",
        "Review the generated regression plots",
        "Pick a format (PDF, HTML, Word)",
        "Download the generated report"
      )
    )
  })
  
  # ─────────────────────────────────────────────────────────────────────
  #### Checklist Table for Report Export ####
  # ─────────────────────────────────────────────────────────────────────
  output$checklistTable <- renderTable({
    # Create checklist data frame
    checklist_data <- data.frame(
      Section = c(PLOT_A_LABEL, PLOT_B_LABEL),
      Status = c(
        if(plot_A_visited()) "✅ Completed" else "❌ Not Visited",
        if(plot_B_visited()) "✅ Completed" else "❌ Not Visited"
      ),
      Description = c(
        if(plot_A_visited()) paste(PLOT_A_LABEL, "section has been configured") else paste("Please visit", PLOT_A_LABEL, "tab to configure plots"),
        if(plot_B_visited()) paste(PLOT_B_LABEL, "section has been configured") else paste("Please visit", PLOT_B_LABEL, "tab to configure plots")
      ),
      stringsAsFactors = FALSE
    )
    
    return(checklist_data)
  }, striped = TRUE, hover = TRUE)
  
  # ─────────────────────────────────────────────────────────────────────
  #### Check if all conditions are met for download ####
  # ─────────────────────────────────────────────────────────────────────
  output$allConditionsMet <- reactive({
    # Return TRUE only if both sections have been visited
    return(plot_A_visited() && plot_B_visited())
  })
  
  # Make the reactive output available to UI
  outputOptions(output, "allConditionsMet", suspendWhenHidden = FALSE)

  # ─────────────────────────────────────────────────────────────────────
  #### Input Preview Table (Scalable) ####
  # ─────────────────────────────────────────────────────────────────────
  output$inputPreviewTable <- renderTable({
    # Create summary data - only show if inputs actually exist
    summary_data <- data.frame(
      Setting = character(),
      Value = character(),
      stringsAsFactors = FALSE
    )
    
    # Add report metadata first
    summary_data <- rbind(summary_data, data.frame(
      Setting = "Report Title",
      Value = if(!is.null(input$title)) input$title else "Not set",
      stringsAsFactors = FALSE
    ))
    
    summary_data <- rbind(summary_data, data.frame(
      Setting = "Report Subtitle", 
      Value = if(!is.null(input$subtitle)) input$subtitle else "Not set",
      stringsAsFactors = FALSE
    ))
    
    summary_data <- rbind(summary_data, data.frame(
      Setting = "Author Name",
      Value = if(!is.null(input$name)) input$name else "Not set",
      stringsAsFactors = FALSE
    ))
    
    # Use reactive values instead of duplicating logic
    plot_A_section_visited <- plot_A_visited()
    plot_B_section_visited <- plot_B_visited()
    
    # Add summary only if tabs have been visited
    if (plot_A_section_visited) {
      summary_data <- rbind(summary_data, data.frame(
        Setting = paste(PLOT_A_LABEL, "- Number of Plots"),
        Value = as.character(input$plot_A_n),
        stringsAsFactors = FALSE
      ))
    }
    
    if (plot_B_section_visited) {
      summary_data <- rbind(summary_data, data.frame(
        Setting = paste(PLOT_B_LABEL, "- Number of Plots"), 
        Value = as.character(input$plot_B_n),
        stringsAsFactors = FALSE
      ))
    }
    
    # Add individual plot variables (scalable approach)
    plot_details <- data.frame(
      Setting = character(),
      Value = character(),
      stringsAsFactors = FALSE
    )
    
    # Function to add plot details scalably
    add_plot_details <- function(plot_type, section_visited) {
      if (section_visited && !is.null(input[[paste0(plot_type, "_n")]]) && input[[paste0(plot_type, "_n")]] > 0) {
        n_plots <- input[[paste0(plot_type, "_n")]]
        
        # Create proper plot type label (plot_A → Plot A, plot_B → Plot B)
        plot_label <- if (plot_type == "plot_A") PLOT_A_LABEL else if (plot_type == "plot_B") PLOT_B_LABEL else plot_type
        
        for (i in 1:n_plots) {
          x_var <- input[[paste0(plot_type, "_x_", i)]]
          caption_var <- input[[paste0(plot_type, "_SubCaption_", i)]]
          
          if (!is.null(x_var)) {
            plot_details <<- rbind(plot_details, data.frame(
              Setting = paste0(plot_label, ".", i, " - X Variable"),
              Value = x_var,
              stringsAsFactors = FALSE
            ))
            
            # Add caption if it exists
            if (!is.null(caption_var) && caption_var != "") {
              plot_details <<- rbind(plot_details, data.frame(
                Setting = paste0(plot_label, ".", i, " - Caption"),
                Value = if(nchar(caption_var) > 50) paste0(substr(caption_var, 1, 50), "...") else caption_var,
                stringsAsFactors = FALSE
              ))
            }
          }
        }
        
        # Add shared caption
        shared_caption_var <- input[[paste0(plot_type, "_caption_shared")]]
        if (!is.null(shared_caption_var) && shared_caption_var != "") {
          plot_details <<- rbind(plot_details, data.frame(
            Setting = paste0(plot_label, " - Shared Caption"),
            Value = if(nchar(shared_caption_var) > 50) paste0(substr(shared_caption_var, 1, 50), "...") else shared_caption_var,
            stringsAsFactors = FALSE
          ))
        }
        
        # Add shared notes
        shared_notes_var <- input[[paste0(plot_type, "_notes_shared")]]
        if (!is.null(shared_notes_var) && shared_notes_var != "") {
          plot_details <<- rbind(plot_details, data.frame(
            Setting = paste0(plot_label, " - Shared Notes"),
            Value = if(nchar(shared_notes_var) > 50) paste0(substr(shared_notes_var, 1, 50), "...") else shared_notes_var,
            stringsAsFactors = FALSE
          ))
        }
      }
    }
    
    # Add details for both plot types
    add_plot_details("plot_A", plot_A_section_visited)
    add_plot_details("plot_B", plot_B_section_visited)
    
    # Combine summary and details
    final_data <- rbind(summary_data, plot_details)
    
    # If no plots have been configured/visited, still show metadata
    if (nrow(plot_details) == 0) {
      final_data <- rbind(final_data, data.frame(
        Setting = "Plots Status",
        Value = "Visit Plot A or Plot B tabs to configure plots for the report",
        stringsAsFactors = FALSE
      ))
    }
    
    return(final_data)
  }, striped = TRUE, hover = TRUE)

  # ─────────────────────────────────────────────────
  #### Compare Reports Functionality (Scalable) ####
  # ─────────────────────────────────────────────────

  # Function to parse URL parameters
  parseBookmarkURL <- function(url) {
    if (is.null(url) || url == "" || !grepl("\\?", url)) {
      return(list())
    }
    
    # Extract query string
    query_string <- sub(".*\\?", "", url)
    
    # Split by & and then by =
    params <- strsplit(query_string, "&")[[1]]
    param_list <- list()
    
    for (param in params) {
      if (grepl("=", param)) {
        parts <- strsplit(param, "=")[[1]]
        if (length(parts) == 2) {
          key <- URLdecode(parts[1])
          value <- URLdecode(parts[2])
          
          # Remove quotes from values
          value <- gsub('^"|"$', '', value)
          
          # Convert numeric strings to numbers
          if (grepl("^[0-9]+$", value)) {
            value <- as.numeric(value)
          }
          
          param_list[[key]] <- value
        }
      }
    }
    
    return(param_list)
  }

  # Function to extract relevant parameters for comparison (fully scalable)
  extractReportParams <- function(param_list) {
    # Metadata parameters
    metadata_params <- c("title", "subtitle", "name")
    
    # Base plot configuration variables
    base_plot_vars <- c("plot_A_n", "plot_B_n", "plot_A_Notes_shared", "plot_B_Notes_shared",
                        "plot_A_caption_shared", "plot_B_caption_shared")
    
    # Find all dynamic plot variables that actually exist in the parameter list
    all_param_names <- names(param_list)
    
    # Extract all plot_A_x, plot_B_x, plot_A_SubCaption, plot_B_SubCaption parameters
    dynamic_patterns <- c("^plot_A_x", "^plot_B_x", "^plot_A_SubCaption", "^plot_B_SubCaption")
    dynamic_plot_vars <- c()
    
    for (pattern in dynamic_patterns) {
      matching_params <- all_param_names[grepl(pattern, all_param_names)]
      dynamic_plot_vars <- c(dynamic_plot_vars, matching_params)
    }
    
    # Combine all relevant parameters
    all_relevant_params <- c(metadata_params, base_plot_vars, dynamic_plot_vars)
    
    # Extract parameters
    result <- list()
    for (param in all_relevant_params) {
      if (param %in% names(param_list)) {
        result[[param]] <- param_list[[param]]
      } else {
        result[[param]] <- NA
      }
    }
    
    return(result)
  }

  # Helper function to create readable parameter names (scalable)
  create_readable_param_name <- function(param) {
    # Handle metadata parameters
    if (param == "title") return("📄 Report Title")
    if (param == "subtitle") return("📄 Report Subtitle")
    if (param == "name") return("📄 Author Name")
    
    # Handle base plot variables
    if (param == "plot_A_n") return(paste0("📊 ", PLOT_A_LABEL, " - Number of Plots"))
    if (param == "plot_B_n") return(paste0("📊 ", PLOT_B_LABEL, " - Number of Plots"))
    if (param == "plot_A_caption_shared") return(paste0("📊 ", PLOT_A_LABEL, " - Shared Caption"))
    if (param == "plot_A_Notes_shared") return(paste0("📊 ", PLOT_A_LABEL, " - Shared Notes"))
    if (param == "plot_B_caption_shared") return(paste0("📊 ", PLOT_B_LABEL, " - Shared Caption"))
    if (param == "plot_B_Notes_shared") return(paste0("📊 ", PLOT_B_LABEL, " - Shared Notes"))
    
    # Handle dynamic plot variables
    if (grepl("^plot_A_x", param)) {
      plot_num <- gsub("plot_A_x", "", param)
      return(paste0("📊 ", PLOT_A_LABEL, " ", plot_num, " - X Variable"))
    }
    if (grepl("^plot_B_x", param)) {
      plot_num <- gsub("plot_B_x", "", param)
      return(paste0("📊 ", PLOT_B_LABEL, " ", plot_num, " - X Variable"))
    }
    if (grepl("^plot_A_SubCaption", param)) {
      plot_num <- gsub("plot_A_SubCaption", "", param)
      return(paste0("📊 ", PLOT_A_LABEL, " ", plot_num, " - Sub-Caption"))
    }
    if (grepl("^plot_B_SubCaption", param)) {
      plot_num <- gsub("plot_B_SubCaption", "", param)
      return(paste0("📊 ", PLOT_B_LABEL, " ", plot_num, " - Sub-Caption"))
    }
    
    # Default case
    return(param)
  }

  # Reactive values to store parsed parameters
  report1_params <- reactiveVal(list())
  report2_params <- reactiveVal(list())

  # Parse URL 1
  observeEvent(input$parse_url_1, {
    parsed <- parseBookmarkURL(input$bookmark_url_1)
    extracted <- extractReportParams(parsed)
    report1_params(extracted)
  })

  # Parse URL 2
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

  ##### Load Report parameters (Fully Scalable) #####
  load_report_params <- function(params) {
    if (length(params) > 0) {
      # Update metadata inputs
      if (!is.na(params$title)) updateTextInput(session, "title", value = params$title)
      if (!is.na(params$subtitle)) updateTextInput(session, "subtitle", value = params$subtitle)
      if (!is.na(params$name)) updateTextInput(session, "name", value = params$name)
      
      # Update plot numbers
      if (!is.na(params$plot_A_n)) updateNumericInput(session, "plot_A_n", value = params$plot_A_n)
      if (!is.na(params$plot_B_n)) updateNumericInput(session, "plot_B_n", value = params$plot_B_n)
      
      # Use invalidateLater to delay the input updates
      invalidateLater(500, session)
      
      # Schedule the dynamic input updates (fully scalable)
      observe({
        # Function to update inputs dynamically
        update_dynamic_inputs <- function(param_prefix, input_type) {
          matching_params <- params[grepl(paste0("^", param_prefix), names(params))]
          
          if (length(matching_params) > 0) {
            for (param_name in names(matching_params)) {
              param_value <- matching_params[[param_name]]
              
              # Only update if parameter is not NA
              if (!is.na(param_value)) {
                if (input_type == "select") {
                  updateSelectInput(session, param_name, selected = param_value)
                } else if (input_type == "text") {
                  updateTextInput(session, param_name, value = param_value)
                } else if (input_type == "textarea") {
                  updateTextAreaInput(session, param_name, value = param_value)
                }
              }
            }
          }
        }
        
        # Update all dynamic inputs (scalable to any number)
        update_dynamic_inputs("plot_A_x", "select")
        update_dynamic_inputs("plot_A_SubCaption", "text")
        update_dynamic_inputs("plot_B_x", "select")
        update_dynamic_inputs("plot_B_SubCaption", "text")
        
        # Update shared settings
        shared_inputs <- list(
          list(name = "plot_A_caption_shared", type = "text"),
          list(name = "plot_A_Notes_shared", type = "textarea"),
          list(name = "plot_B_caption_shared", type = "text"),
          list(name = "plot_B_Notes_shared", type = "textarea")
        )
        
        for (input_info in shared_inputs) {
          param_name <- input_info$name
          if (!is.na(params[[param_name]])) {
            if (input_info$type == "text") {
              updateTextInput(session, param_name, value = params[[param_name]])
            } else if (input_info$type == "textarea") {
              updateTextAreaInput(session, param_name, value = params[[param_name]])
            }
          }
        }
      })
    }
  }

  # Load Report 1 parameters into current session
  observeEvent(input$load_report_1, {
    load_report_params(report1_params())
    showNotification("Report 1 parameters loaded successfully!", type = "default")
  })

  # Load Report 2 parameters into current session
  observeEvent(input$load_report_2, {
    load_report_params(report2_params())
    showNotification("Report 2 parameters loaded successfully!", type = "default")
  })

  ##### Generate comparison table (scalable) #####
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
      
      # Determine if values match
      match_status <- if (val1 == val2) "✅" else "❌"

      # Create readable parameter names (scalable)
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

  # ─────────────────────────────────────────────────
  ##### Serve the rendered file #####
  # ─────────────────────────────────────────────────
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
}

# ────────────────────────────────────────────────────────────────
#### Run App ####
# ────────────────────────────────────────────────────────────────
enableBookmarking(store = "url")
shinyApp(ui, server)