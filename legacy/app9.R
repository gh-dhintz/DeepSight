# ────────────────────────────────────────────────────────────────
#### Load Libraries ####
# ────────────────────────────────────────────────────────────────
library(shiny)
library(ggplot2)
library(quarto)
library(shinythemes)
library(yaml)

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
ui <- fluidPage(
  tags$head(tags$link(rel = "icon", href = "company_logo.png", type = "image/png")),
  navbarPage(
    theme = shinytheme("flatly"),
    collapsible = TRUE,
    id = "nav",
    title = div(
      #tags$img(src = "company_logo.png", height = "30px", style = "margin-right:10px;"),
      "Breast Cancer GH Patients"
    ),
    ##### User Guide #####
    tabPanel("User Guide",
      h4("Instructions for Use", class = "text-primary"),
      tableOutput("guideTable"),

      # Add Tips section
      br(),
      h4("Tips", class = "text-primary"),
      tags$ul(
        tags$li("Plots will be arranged in the report as they appear, top to bottom in the dashboard")
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
    ##### Plot A #####
    tabPanel("Plot A",
      sidebarLayout(
        sidebarPanel(width = 3,
          h4("Inputs for Plot A", class = "text-primary"),
          numericInput("plotA_n", "Number of plots (max 3):", value = 1, min = 1, max = 3),
          uiOutput("plotA_inputs")
        ),
        mainPanel(uiOutput("plotA_outputs"))
      )
    ),
    ##### Plot B #####
    tabPanel("Plot B",
      sidebarLayout(
        sidebarPanel(width = 3,
          h4("Inputs for Plot B", class = "text-primary"),
          numericInput("plotB_n", "Number of plots (max 3):", value = 1, min = 1, max = 3),
          uiOutput("plotB_inputs")
        ),
        mainPanel(uiOutput("plotB_outputs"))
      )
    ),
##### Report Export #####
tabPanel("Report Export",
h4("Export Report", class = "text-primary"),

# Report format selection
selectInput("format", "Report Format:", choices = c("PDF", "HTML", "Word")),

###### Checklist Section ######
br(),
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
      )
  )
)

# ────────────────────────────────────────────────────────────────
#### Server ####
# ────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  bookmark_url <- reactiveVal(NULL)
  download_requested <- reactiveVal(FALSE)
  url_restored <- reactiveVal(FALSE)  # Flag to track if URL restoration has been done
  
  # ─────────────────────────────────────────────────────
  #### URL Bookmark Restoration Logic ####
  # ─────────────────────────────────────────────────────
  # This handles the restoration of dynamic UI elements when loading from URL
  observe({
    # Get URL parameters
    query <- parseQueryString(session$clientData$url_search)
    
    # Only run this restoration logic once when the app starts with URL parameters
    if (length(query) > 0 && is.null(isolate(input$plotA_n)) && !url_restored()) {
      url_restored(TRUE)  # Set flag to prevent re-execution
      
      # Small delay to ensure UI is ready
      invalidateLater(100, session)
      
      # Restore plot numbers first
      if ("plotA_n" %in% names(query)) {
        updateNumericInput(session, "plotA_n", value = as.numeric(query$plotA_n))
      }
      if ("plotB_n" %in% names(query)) {
        updateNumericInput(session, "plotB_n", value = as.numeric(query$plotB_n))
      }
      
      # Schedule restoration of dynamic inputs after UI is created
      invalidateLater(500, session)
      
      observeEvent(url_restored(), {
        if (url_restored()) {
          # Restore dynamic Plot A inputs
          if ("plotA_n" %in% names(query)) {
            n_plots_A <- as.numeric(query$plotA_n)
            for (i in 1:n_plots_A) {
              x_param <- paste0("plotA_x_", i)
              caption_param <- paste0("plotA_SubCaption_", i)
              
              if (x_param %in% names(query)) {
                updateSelectInput(session, x_param, selected = query[[x_param]])
              }
              if (caption_param %in% names(query)) {
                updateTextInput(session, caption_param, value = query[[caption_param]])
              }
            }
          }
          
          # Restore dynamic Plot B inputs
          if ("plotB_n" %in% names(query)) {
            n_plots_B <- as.numeric(query$plotB_n)
            for (i in 1:n_plots_B) {
              x_param <- paste0("plotB_x_", i)
              caption_param <- paste0("plotB_SubCaption_", i)
              
              if (x_param %in% names(query)) {
                updateSelectInput(session, x_param, selected = query[[x_param]])
              }
              if (caption_param %in% names(query)) {
                updateTextInput(session, caption_param, value = query[[caption_param]])
              }
            }
          }
          
          # Restore shared settings
          shared_params <- c("plotA_caption_shared", "plotA_notes_shared", 
                            "plotB_caption_shared", "plotB_notes_shared")
          for (param in shared_params) {
            if (param %in% names(query)) {
              if (grepl("notes", param)) {
                updateTextAreaInput(session, param, value = query[[param]])
              } else {
                updateTextInput(session, param, value = query[[param]])
              }
            }
          }
          
          # Restore metadata
          if ("title" %in% names(query)) {
            updateTextInput(session, "title", value = query$title)
          }
          if ("subtitle" %in% names(query)) {
            updateTextInput(session, "subtitle", value = query$subtitle)
          }
          if ("name" %in% names(query)) {
            updateTextInput(session, "name", value = query$name)
          }
          if ("format" %in% names(query)) {
            updateSelectInput(session, "format", selected = query$format)
          }
        }
      }, once = TRUE, ignoreInit = TRUE)
    }
  })
  
  # ─────────────────────────────────────────────────────
  #### Reactive values to check section completion ####
  # ─────────────────────────────────────────────────────
  plotA_visited <- reactive({
    if (!is.null(input$plotA_n) && input$plotA_n > 0) {
      for (i in 1:input$plotA_n) {
        if (!is.null(input[[paste0("plotA_x_", i)]])) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  })
  
  plotB_visited <- reactive({
    if (!is.null(input$plotB_n) && input$plotB_n > 0) {
      for (i in 1:input$plotB_n) {
        if (!is.null(input[[paste0("plotB_x_", i)]])) {
          return(TRUE)
        }
      }
    }
    return(FALSE)
  })

  # ─────────────────────────────────────────────────────
  #### Preload Reports Functionality ####
  # ─────────────────────────────────────────────────────
  
  # Function to load predefined report settings
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
      if (!is.na(extracted$plotA_n)) updateNumericInput(session, "plotA_n", value = extracted$plotA_n)
      if (!is.na(extracted$plotB_n)) updateNumericInput(session, "plotB_n", value = extracted$plotB_n)
      
      # Use invalidateLater to delay the input updates
      invalidateLater(500, session)
      
      # Schedule the dynamic input updates
      observe({
        # Function to update inputs dynamically
        update_dynamic_inputs <- function(param_prefix, input_type) {
          # Find all parameters that match the pattern
          matching_params <- extracted[grepl(paste0("^", param_prefix), names(extracted))]
          
          if (length(matching_params) > 0) {
            for (param_name in names(matching_params)) {
              param_value <- matching_params[[param_name]]
              
              # Only update if parameter is not NA and the corresponding input exists
              if (!is.na(param_value)) {
                # Wait a bit for UI to be ready
                Sys.sleep(0.1)
                
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
        update_dynamic_inputs("plotA_x_", "select")
        update_dynamic_inputs("plotA_SubCaption_", "text")
        update_dynamic_inputs("plotB_x_", "select")
        update_dynamic_inputs("plotB_SubCaption_", "text")
        
        # Update shared settings
        shared_inputs <- list(
          list(name = "plotA_caption_shared", type = "text"),
          list(name = "plotA_notes_shared", type = "textarea"),
          list(name = "plotB_caption_shared", type = "text"),
          list(name = "plotB_notes_shared", type = "textarea")
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
    preset_url <- "http://127.0.0.1:5173/?_inputs_&nav=%22Report%20Export%22&download_trigger=2&parse_url_1=0&parse_url_2=0&load_report_1=0&load_report_2=0&clear_comparison=0&format=%22PDF%22&plotA_n=2&plotB_n=1&bookmark_url_1=%22%22&bookmark_url_2=%22%22&title=%22My%20Report%20Title%22&subtitle=%22Analysis%20Report%22&name=%22John%20Doe%22&plotA_x_1=%22mpg%22&plotA_notes_shared=%22In%20Figure%20%40fig-plotA%20I%20have%5Cnplots%20%40fig-plotA-1%20and%20plots%20%40fig-plotA-2%22&plotA_SubCaption_1=%22this%20is%20a%20nice%20plot%22&plotA_caption_shared=%22I%20love%20my%20figure%22&plotB_x_1=%22mpg%22&plotB_notes_shared=%22%22&plotB_SubCaption_1=%22%22&plotB_caption_shared=%22%22&plotA_x_2=%22cyl%22&plotA_SubCaption_2=%22this%20is%20cyl%22"
    
    loadPresetReport(preset_url)
    showNotification("Basic Report configuration loaded successfully!", type = "message")
  })
  
  # Load Basic Report Two
  observeEvent(input$load_basic_report_two, {
    preset_url <- "http://127.0.0.1:5173/?_inputs_&nav=%22Report%20Export%22&download_trigger=2&parse_url_1=0&parse_url_2=0&load_report_1=0&load_report_2=0&clear_comparison=0&format=%22PDF%22&plotA_n=2&plotB_n=1&bookmark_url_1=%22%22&bookmark_url_2=%22%22&title=%22My%20Report%20Title%22&subtitle=%22Analysis%20Report%22&name=%22John%20Doe%22&plotA_x_1=%22mpg%22&plotA_notes_shared=%22In%20Figure%20%40fig-plotA%20I%20have%5Cnplots%20%40fig-plotA-1%20and%20plots%20%40fig-plotA-2%22&plotA_SubCaption_1=%22this%20is%20a%20nice%20plot%22&plotA_caption_shared=%22I%20love%20my%20figure%22&plotB_x_1=%22mpg%22&plotB_notes_shared=%22%22&plotB_SubCaption_1=%22%22&plotB_caption_shared=%22%22&plotA_x_2=%22cyl%22&plotA_SubCaption_2=%22this%20is%20cyl%22"
    
    loadPresetReport(preset_url)
    showNotification("Basic Report Two configuration loaded successfully!", type = "message")
  })

  observeEvent(input$download_trigger, {
    # Check if all conditions are met before proceeding
    if (plotA_visited() && plotB_visited()) {
      download_requested(TRUE)
      session$doBookmark()
    } else {
      showModal(modalDialog(
        title = "Cannot Download Report",
        p("Please complete the checklist before downloading your report:"),
        tags$ul(
          if(!plotA_visited()) tags$li("Visit Plot A tab to configure plots"),
          if(!plotB_visited()) tags$li("Visit Plot B tab to configure plots")
        ),
        easyClose = TRUE,
        footer = actionButton("close_modal", "OK", class = "btn btn-primary", 
                            `data-dismiss` = "modal")
      ))
    }
  })

  onBookmarked(function(url) {
    bookmark_url(url)
    if (download_requested()) {
      download_requested(FALSE)
      
      # Convert input to a regular list (only non-NULL values)
      all_inputs <- reactiveValuesToList(input)
      
      # Manually ensure all dynamic plot inputs are captured
      # Force capture of all possible plot inputs
      for (plot_type in c("plotA", "plotB")) {
        n_plots <- input[[paste0(plot_type, "_n")]]
        if (!is.null(n_plots) && n_plots > 0) {
          for (i in 1:n_plots) {
            # Capture X variables
            x_var_name <- paste0(plot_type, "_x_", i)
            if (!is.null(input[[x_var_name]])) {
              all_inputs[[x_var_name]] <- input[[x_var_name]]
            }
            
            # Capture SubCaptions
            subcaption_name <- paste0(plot_type, "_SubCaption_", i)
            if (!is.null(input[[subcaption_name]])) {
              all_inputs[[subcaption_name]] <- input[[subcaption_name]]
            }
          }
        }
        
        # Capture shared settings
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
          PDF = list(input = "pdf_6.qmd", format = "pdf", ext = "pdf"),
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

          showModal(modalDialog(
            title = "Report Ready",
            p("The report has been generated."),
            downloadLink("download_report_link", "Click here to download", class = "btn btn-success"),
            easyClose = TRUE
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

  # ─────────────────────────────────────────────────────
  #### Dynamic UI for Plot A ####
  # ─────────────────────────────────────────────────────
  output$plotA_inputs <- renderUI({
    plot_inputs <- lapply(1:input$plotA_n, function(i) {
      div(
        selectInput(paste0("plotA_x_", i), paste("X Variable for Plot A", i), choices = names(mtcars)),
        br(),
        h6(paste("Sub-Caption for Plot A", i), class = "text-secondary", style = "font-weight: bold;"),
        p("Sub-Captions are used only in the presence of multiple plots per figure", 
          class = "text-muted", style = "font-size: 12px; margin-bottom: 5px;"),
        textInput(paste0("plotA_SubCaption_", i), NULL, 
                 placeholder = "Brief description of this plot"),
        if (i < input$plotA_n) br() else NULL
      )
    })
    
    # Add shared caption and notes boxes at the end (label input removed)
    shared_inputs <- div(
      br(),
      h5("Shared Settings for All Plot A", class = "text-info"),
      textInput("plotA_caption_shared", "Shared Caption for All Plot A", 
               placeholder = "Overall caption for the Plot A figure group"),
      textAreaInput("plotA_notes_shared", "Notes for A Plots", 
                   placeholder = "Add your comments about Plot A here. When referencing in text, use @fig-plotA when nplots=1; otherwise, use @fig-plotA-1, @fig-plotA-2, etc.", 
                   height = "100px", width = "100%")
    )
    
    return(div(plot_inputs, shared_inputs))
  })

  output$plotA_outputs <- renderUI({
    lapply(1:input$plotA_n, function(i) {
      plotOutput(paste0("regPlotA_", i), height = "300px")
    })
  })

  for (i in 1:3) {
    local({
      my_i <- i
      output[[paste0("regPlotA_", my_i)]] <- renderPlot({
        req(input[[paste0("plotA_x_", my_i)]])
        ggplot(mtcars, aes_string(x = input[[paste0("plotA_x_", my_i)]], y = "mpg")) +
          geom_point(color = color_primary, size = 3) +
          geom_smooth(method = "lm", se = FALSE, color = color_secondary) +
          theme_minimal(base_family = "Helvetica Neue") +
          labs(title = paste("Plot A", my_i), x = input[[paste0("plotA_x_", my_i)]], y = "mpg") +
          theme(
            plot.background = element_rect(fill = color_panel_bg, color = NA),
            panel.background = element_rect(fill = color_panel_bg, color = NA),
            text = element_text(color = color_fg),
            axis.text = element_text(color = color_fg),
            axis.title = element_text(color = color_fg),
            plot.title = element_text(face = "bold", color = color_primary)
          )
      })
    })
  }

  # ─────────────────────────────────────────────────────
  #### Dynamic UI for Plot B ####
  # ─────────────────────────────────────────────────────
  output$plotB_inputs <- renderUI({
    plot_inputs <- lapply(1:input$plotB_n, function(i) {
      div(
        selectInput(paste0("plotB_x_", i), paste("X Variable for Plot B", i), choices = names(mtcars)),
        br(),
        h6(paste("Sub-Caption for Plot B", i), class = "text-secondary", style = "font-weight: bold;"),
        p("Sub-Captions are used only in the presence of multiple plots per figure", 
          class = "text-muted", style = "font-size: 12px; margin-bottom: 5px;"),
        textInput(paste0("plotB_SubCaption_", i), NULL, 
                 placeholder = "Brief description of this plot"),
        if (i < input$plotB_n) br() else NULL
      )
    })
    
    # Add shared caption and notes boxes at the end (label input removed)
    shared_inputs <- div(
      br(),
      h5("Shared Settings for All Plot B", class = "text-info"),
      textInput("plotB_caption_shared", "Shared Caption for All Plot B", 
               placeholder = "Overall caption for the Plot B figure group"),
      textAreaInput("plotB_notes_shared", "Notes for B Plots", 
                   placeholder = "Add your comments about Plot B here. When referencing in text, use @fig-plotB when nplots=1; otherwise, use @fig-plotB-1, @fig-plotB-2, etc.", 
                   height = "100px", width = "100%")
    )
    
    return(div(plot_inputs, shared_inputs))
  })

  output$plotB_outputs <- renderUI({
    lapply(1:input$plotB_n, function(i) {
      plotOutput(paste0("regPlotB_", i), height = "300px")
    })
  })

  for (i in 1:3) {
    local({
      my_i <- i
      output[[paste0("regPlotB_", my_i)]] <- renderPlot({
        req(input[[paste0("plotB_x_", my_i)]])
        ggplot(mtcars, aes_string(x = input[[paste0("plotB_x_", my_i)]], y = "mpg")) +
          geom_point(color = color_primary, size = 3) +
          geom_smooth(method = "lm", se = FALSE, color = color_secondary) +
          theme_minimal(base_family = "Helvetica Neue") +
          labs(title = paste("Plot B", my_i), x = input[[paste0("plotB_x_", my_i)]], y = "mpg") +
          theme(
            plot.background = element_rect(fill = color_panel_bg, color = NA),
            panel.background = element_rect(fill = color_panel_bg, color = NA),
            text = element_text(color = color_fg),
            axis.text = element_text(color = color_fg),
            axis.title = element_text(color = color_fg),
            plot.title = element_text(face = "bold", color = color_primary)
          )
      })
    })
  }

  # ─────────────────────────────────────────────────────
  #### User Guide Table ####
  # ─────────────────────────────────────────────────────
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
      Section = c("Plot A", "Plot B"),
      Status = c(
        if(plotA_visited()) "✅ Completed" else "❌ Not Visited",
        if(plotB_visited()) "✅ Completed" else "❌ Not Visited"
      ),
      Description = c(
        if(plotA_visited()) "Plot A section has been configured" else "Please visit Plot A tab to configure plots",
        if(plotB_visited()) "Plot B section has been configured" else "Please visit Plot B tab to configure plots"
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
    return(plotA_visited() && plotB_visited())
  })
  
  # Make the reactive output available to UI
  outputOptions(output, "allConditionsMet", suspendWhenHidden = FALSE)

  # ─────────────────────────────────────────────────────────────────────
  #### Input Preview Table for Report Export (More Detailed Preview) ####
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
    plotA_section_visited <- plotA_visited()
    plotB_section_visited <- plotB_visited()
    
    # Add summary only if tabs have been visited
    if (plotA_section_visited) {
      summary_data <- rbind(summary_data, data.frame(
        Setting = "Plot A - Number of Plots",
        Value = as.character(input$plotA_n),
        stringsAsFactors = FALSE
      ))
    }
    
    if (plotB_section_visited) {
      summary_data <- rbind(summary_data, data.frame(
        Setting = "Plot B - Number of Plots", 
        Value = as.character(input$plotB_n),
        stringsAsFactors = FALSE
      ))
    }
    
    # Add individual plot variables
    plot_details <- data.frame(
      Setting = character(),
      Value = character(),
      stringsAsFactors = FALSE
    )
    
    # Plot A variables - only if visited
    if (plotA_section_visited && !is.null(input$plotA_n) && input$plotA_n > 0) {
      for (i in 1:input$plotA_n) {
        x_var <- input[[paste0("plotA_x_", i)]]
        caption_var <- input[[paste0("plotA_SubCaption_", i)]]
        
        if (!is.null(x_var)) {
          plot_details <- rbind(plot_details, data.frame(
            Setting = paste("Plot A", i, "- X Variable"),
            Value = x_var,
            stringsAsFactors = FALSE
          ))
          
          # Add caption if it exists
          if (!is.null(caption_var) && caption_var != "") {
            plot_details <- rbind(plot_details, data.frame(
              Setting = paste("Plot A", i, "- Caption"),
              Value = if(nchar(caption_var) > 50) paste0(substr(caption_var, 1, 50), "...") else caption_var,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      # Add shared caption
      shared_caption_var <- input$plotA_caption_shared
      if (!is.null(shared_caption_var) && shared_caption_var != "") {
        plot_details <- rbind(plot_details, data.frame(
          Setting = "Plot A - Shared Caption",
          Value = if(nchar(shared_caption_var) > 50) paste0(substr(shared_caption_var, 1, 50), "...") else shared_caption_var,
          stringsAsFactors = FALSE
        ))
      }
      
      # Add shared notes (removed shared label section)
      shared_notes_var <- input$plotA_notes_shared
      if (!is.null(shared_notes_var) && shared_notes_var != "") {
        plot_details <- rbind(plot_details, data.frame(
          Setting = "Plot A - Shared Notes",
          Value = if(nchar(shared_notes_var) > 50) paste0(substr(shared_notes_var, 1, 50), "...") else shared_notes_var,
          stringsAsFactors = FALSE
        ))
      }
    }
    
    # Plot B variables - only if visited (similar structure)
    if (plotB_section_visited && !is.null(input$plotB_n) && input$plotB_n > 0) {
      for (i in 1:input$plotB_n) {
        x_var <- input[[paste0("plotB_x_", i)]]
        caption_var <- input[[paste0("plotB_SubCaption_", i)]]
        
        if (!is.null(x_var)) {
          plot_details <- rbind(plot_details, data.frame(
            Setting = paste("Plot B", i, "- X Variable"),
            Value = x_var,
            stringsAsFactors = FALSE
          ))
          
          # Add caption if it exists
          if (!is.null(caption_var) && caption_var != "") {
            plot_details <- rbind(plot_details, data.frame(
              Setting = paste("Plot B", i, "- Caption"),
              Value = if(nchar(caption_var) > 50) paste0(substr(caption_var, 1, 50), "...") else caption_var,
              stringsAsFactors = FALSE
            ))
          }
        }
      }
      
      # Add shared caption
      shared_caption_var <- input$plotB_caption_shared
      if (!is.null(shared_caption_var) && shared_caption_var != "") {
        plot_details <- rbind(plot_details, data.frame(
          Setting = "Plot B - Shared Caption",
          Value = if(nchar(shared_caption_var) > 50) paste0(substr(shared_caption_var, 1, 50), "...") else shared_caption_var,
          stringsAsFactors = FALSE
        ))
      }
      
      # Add shared notes (removed shared label section)
      shared_notes_var <- input$plotB_notes_shared
      if (!is.null(shared_notes_var) && shared_notes_var != "") {
        plot_details <- rbind(plot_details, data.frame(
          Setting = "Plot B - Shared Notes",
          Value = if(nchar(shared_notes_var) > 50) paste0(substr(shared_notes_var, 1, 50), "...") else shared_notes_var,
          stringsAsFactors = FALSE
        ))
      }
    }
    
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

  # ─────────────────────────────────────────────────────
  #### Compare Reports Functionality ####
  # ─────────────────────────────────────────────────────

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

  # Function to extract relevant parameters for comparison (fully dynamic version)
  extractReportParams <- function(param_list) {
    # Metadata parameters
    metadata_params <- c("title", "subtitle", "name")
    
    # Base plot configuration variables
    base_plot_vars <- c("plotA_n", "plotB_n", "plotA_notes_shared", "plotB_notes_shared",
                        "plotA_caption_shared", "plotB_caption_shared")
    
    # Find all dynamic plot variables that actually exist in the parameter list
    all_param_names <- names(param_list)
    
    # Extract all plotA_x_, plotB_x_, plotA_SubCaption_, plotB_SubCaption_ parameters
    dynamic_patterns <- c("^plotA_x_", "^plotB_x_", "^plotA_SubCaption_", "^plotB_SubCaption_")
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

  # Helper function to create readable parameter names
  create_readable_param_name <- function(param) {
    # Handle metadata parameters
    if (param == "title") return("📄 Report Title")
    if (param == "subtitle") return("📄 Report Subtitle")
    if (param == "name") return("📄 Author Name")
    
    # Handle base plot variables
    if (param == "plotA_n") return("📊 Plot A - Number of Plots")
    if (param == "plotB_n") return("📊 Plot B - Number of Plots")
    if (param == "plotA_caption_shared") return("📊 Plot A - Shared Caption")
    if (param == "plotA_notes_shared") return("📊 Plot A - Shared Notes")
    if (param == "plotB_caption_shared") return("📊 Plot B - Shared Caption")
    if (param == "plotB_notes_shared") return("📊 Plot B - Shared Notes")
    
    # Handle dynamic plot variables
    if (grepl("^plotA_x_", param)) {
      plot_num <- gsub("plotA_x_", "", param)
      return(paste0("📊 Plot A ", plot_num, " - X Variable"))
    }
    if (grepl("^plotB_x_", param)) {
      plot_num <- gsub("plotB_x_", "", param)
      return(paste0("📊 Plot B ", plot_num, " - X Variable"))
    }
    if (grepl("^plotA_SubCaption_", param)) {
      plot_num <- gsub("plotA_SubCaption_", "", param)
      return(paste0("📊 Plot A ", plot_num, " - Sub-Caption"))
    }
    if (grepl("^plotB_SubCaption_", param)) {
      plot_num <- gsub("plotB_SubCaption_", "", param)
      return(paste0("📊 Plot B ", plot_num, " - Sub-Caption"))
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

  ##### Load Report 1 parameters into current session (Dynamic version) #####
  observeEvent(input$load_report_1, {
    params <- report1_params()
    if (length(params) > 0) {
      # Update metadata inputs
      if (!is.na(params$title)) updateTextInput(session, "title", value = params$title)
      if (!is.na(params$subtitle)) updateTextInput(session, "subtitle", value = params$subtitle)
      if (!is.na(params$name)) updateTextInput(session, "name", value = params$name)
      
      # Update plot numbers
      if (!is.na(params$plotA_n)) updateNumericInput(session, "plotA_n", value = params$plotA_n)
      if (!is.na(params$plotB_n)) updateNumericInput(session, "plotB_n", value = params$plotB_n)
      
      # Use invalidateLater to delay the input updates
      invalidateLater(500, session)
      
      # Schedule the dynamic input updates
      observe({
        # Function to update inputs dynamically
        update_dynamic_inputs <- function(param_prefix, input_type) {
          # Find all parameters that match the pattern
          matching_params <- params[grepl(paste0("^", param_prefix), names(params))]
          
          if (length(matching_params) > 0) {
            for (param_name in names(matching_params)) {
              param_value <- matching_params[[param_name]]
              
              # Only update if parameter is not NA and the corresponding input exists
              if (!is.na(param_value) && !is.null(input[[param_name]])) {
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
        
        # Update all Plot A X variables (plotA_x_1, plotA_x_2, plotA_x_3, ... plotA_x_n)
        update_dynamic_inputs("plotA_x_", "select")
        
        # Update all Plot A sub-captions (plotA_SubCaption_1, plotA_SubCaption_2, ... plotA_SubCaption_n)
        update_dynamic_inputs("plotA_SubCaption_", "text")
        
        # Update all Plot B X variables (plotB_x_1, plotB_x_2, plotB_x_3, ... plotB_x_n)
        update_dynamic_inputs("plotB_x_", "select")
        
        # Update all Plot B sub-captions (plotB_SubCaption_1, plotB_SubCaption_2, ... plotB_SubCaption_n)
        update_dynamic_inputs("plotB_SubCaption_", "text")
        
        # Update shared settings (these are singular, not numbered)
        shared_inputs <- list(
          list(name = "plotA_caption_shared", type = "text"),
          list(name = "plotA_notes_shared", type = "textarea"),
          list(name = "plotB_caption_shared", type = "text"),
          list(name = "plotB_notes_shared", type = "textarea")
        )
        
        for (input_info in shared_inputs) {
          param_name <- input_info$name
          if (!is.na(params[[param_name]]) && !is.null(input[[param_name]])) {
            if (input_info$type == "text") {
              updateTextInput(session, param_name, value = params[[param_name]])
            } else if (input_info$type == "textarea") {
              updateTextAreaInput(session, param_name, value = params[[param_name]])
            }
          }
        }
      })
      
      showNotification("Report 1 parameters loaded successfully!", type = "message")
    }
  })

  ##### Load Report 2 parameters into current session (Dynamic version) #####
  observeEvent(input$load_report_2, {
    params <- report2_params()
    if (length(params) > 0) {
      # Update metadata inputs
      if (!is.na(params$title)) updateTextInput(session, "title", value = params$title)
      if (!is.na(params$subtitle)) updateTextInput(session, "subtitle", value = params$subtitle)
      if (!is.na(params$name)) updateTextInput(session, "name", value = params$name)
      
      # Update plot numbers
      if (!is.na(params$plotA_n)) updateNumericInput(session, "plotA_n", value = params$plotA_n)
      if (!is.na(params$plotB_n)) updateNumericInput(session, "plotB_n", value = params$plotB_n)
      
      # Use invalidateLater to delay the input updates
      invalidateLater(500, session)
      
      # Schedule the dynamic input updates
      observe({
        # Function to update inputs dynamically
        update_dynamic_inputs <- function(param_prefix, input_type) {
          # Find all parameters that match the pattern
          matching_params <- params[grepl(paste0("^", param_prefix), names(params))]
          
          if (length(matching_params) > 0) {
            for (param_name in names(matching_params)) {
              param_value <- matching_params[[param_name]]
              
              # Only update if parameter is not NA and the corresponding input exists
              if (!is.na(param_value) && !is.null(input[[param_name]])) {
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
        
        # Update all Plot A X variables (plotA_x_1, plotA_x_2, plotA_x_3, ... plotA_x_n)
        update_dynamic_inputs("plotA_x_", "select")
        
        # Update all Plot A sub-captions (plotA_SubCaption_1, plotA_SubCaption_2, ... plotA_SubCaption_n)
        update_dynamic_inputs("plotA_SubCaption_", "text")
        
        # Update all Plot B X variables (plotB_x_1, plotB_x_2, plotB_x_3, ... plotB_x_n)
        update_dynamic_inputs("plotB_x_", "select")
        
        # Update all Plot B sub-captions (plotB_SubCaption_1, plotB_SubCaption_2, ... plotB_SubCaption_n)
        update_dynamic_inputs("plotB_SubCaption_", "text")
        
        # Update shared settings (these are singular, not numbered)
        shared_inputs <- list(
          list(name = "plotA_caption_shared", type = "text"),
          list(name = "plotA_notes_shared", type = "textarea"),
          list(name = "plotB_caption_shared", type = "text"),
          list(name = "plotB_notes_shared", type = "textarea")
        )
        
        for (input_info in shared_inputs) {
          param_name <- input_info$name
          if (!is.na(params[[param_name]]) && !is.null(input[[param_name]])) {
            if (input_info$type == "text") {
              updateTextInput(session, param_name, value = params[[param_name]])
            } else if (input_info$type == "textarea") {
              updateTextAreaInput(session, param_name, value = params[[param_name]])
            }
          }
        }
      })
      
      showNotification("Report 2 parameters loaded successfully!", type = "message")
    }
  })

  ##### Generate comparison table (dynamic version) #####
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

      # Create readable parameter names (dynamic version)
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

 # ─────────────────────────────────────────────────────
 ##### Serve the rendered file #####
 # ─────────────────────────────────────────────────────
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