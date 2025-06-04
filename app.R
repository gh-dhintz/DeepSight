# ────────────────────────────────────────────────────────────────
#### Load Libraries ####
# ────────────────────────────────────────────────────────────────
library(shiny)
library(ggplot2)
library(quarto)
library(shinythemes)

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
      tags$img(src = "company_logo.png", height = "30px", style = "margin-right:10px;"),
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
    tags$li("Tip 1: For Plots to be Generated, you have to have visted its respected tab"),
    tags$li("Tip 2: Plots will be arranged in the report as they appear, top to bottom in the dashboard")
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

# Preview section
br(),
h4("Report Preview", class = "text-primary"),
p("The following inputs will be included in your report:"),

# Input values table
tableOutput("inputPreviewTable"),

br(),
actionButton("download_trigger", "Download Report", class = "btn btn-primary"),
downloadLink("download_report_link", "Download Link", style = "display:none;")
    )
  )
)

# ────────────────────────────────────────────────────────────────
#### Server ####
# ────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  bookmark_url <- reactiveVal(NULL)
  download_requested <- reactiveVal(FALSE)

  observeEvent(input$download_trigger, {
    download_requested(TRUE)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    bookmark_url(url)
    if (download_requested()) {
      download_requested(FALSE)
      isolate({
        fmt <- switch(input$format,
          PDF = list(input = "pdf_simple.qmd", format = "pdf", ext = "pdf"),
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
              execute_params = list(
                plotA_x_1 = input$plotA_x_1,
                plotA_x_2 = input$plotA_x_2,
                plotA_x_3 = input$plotA_x_3,
                plotB_x_1 = input$plotB_x_1,
                plotB_x_2 = input$plotB_x_2,
                plotB_x_3 = input$plotB_x_3,
                bookmark_url = url
              ),
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
  # Dynamic UI for Plot A
  # ─────────────────────────────────────────────────────
  output$plotA_inputs <- renderUI({
    lapply(1:input$plotA_n, function(i) {
      selectInput(paste0("plotA_x_", i), paste("X Variable for Plot A", i), choices = names(mtcars))
    })
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
  # Dynamic UI for Plot B
  # ─────────────────────────────────────────────────────
  output$plotB_inputs <- renderUI({
    lapply(1:input$plotB_n, function(i) {
      selectInput(paste0("plotB_x_", i), paste("X Variable for Plot B", i), choices = names(mtcars))
    })
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
  # User Guide Table
  # ─────────────────────────────────────────────────────
  output$guideTable <- renderTable({
    data.frame(
      Step = c(
        "Select number of plots and X for Plot A",
        "Select number of plots and X for Plot B",
        "Review plots",
        "Choose export format",
        "Download report"
      ),
      Description = c(
        "Choose how many plots and X variables for Plot A",
        "Choose how many plots and X variables for Plot B",
        "Review the generated regression plots",
        "Pick a format (PDF, HTML, Word)",
        "Download the generated report"
      )
    )
  })

  # ─────────────────────────────────────────────────────
# Input Preview Table for Report Export (More Detailed Preview)
# ─────────────────────────────────────────────────────
output$inputPreviewTable <- renderTable({
  # Create summary data - only show if inputs actually exist
  summary_data <- data.frame(
    Setting = character(),
    Value = character(),
    stringsAsFactors = FALSE
  )
  
  # Check if Plot A has been visited (inputs exist)
  plotA_visited <- FALSE
  if (!is.null(input$plotA_n) && input$plotA_n > 0) {
    # Check if any Plot A inputs exist
    for (i in 1:input$plotA_n) {
      if (!is.null(input[[paste0("plotA_x_", i)]])) {
        plotA_visited <- TRUE
        break
      }
    }
  }
  
  # Check if Plot B has been visited (inputs exist)
  plotB_visited <- FALSE
  if (!is.null(input$plotB_n) && input$plotB_n > 0) {
    # Check if any Plot B inputs exist
    for (i in 1:input$plotB_n) {
      if (!is.null(input[[paste0("plotB_x_", i)]])) {
        plotB_visited <- TRUE
        break
      }
    }
  }
  
  # Add summary only if tabs have been visited
  if (plotA_visited) {
    summary_data <- rbind(summary_data, data.frame(
      Setting = "Plot A - Number of Plots",
      Value = as.character(input$plotA_n),
      stringsAsFactors = FALSE
    ))
  }
  
  if (plotB_visited) {
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
  if (plotA_visited && !is.null(input$plotA_n) && input$plotA_n > 0) {
    for (i in 1:input$plotA_n) {
      x_var <- input[[paste0("plotA_x_", i)]]
      if (!is.null(x_var)) {
        plot_details <- rbind(plot_details, data.frame(
          Setting = paste("Plot A", i, "- X Variable"),
          Value = x_var,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Plot B variables - only if visited
  if (plotB_visited && !is.null(input$plotB_n) && input$plotB_n > 0) {
    for (i in 1:input$plotB_n) {
      x_var <- input[[paste0("plotB_x_", i)]]
      if (!is.null(x_var)) {
        plot_details <- rbind(plot_details, data.frame(
          Setting = paste("Plot B", i, "- X Variable"),
          Value = x_var,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  # Combine summary and details
  final_data <- rbind(summary_data, plot_details)
  
  # If no plots have been configured/visited, show message
  if (nrow(final_data) == 0) {
    final_data <- data.frame(
      Setting = "No plots configured",
      Value = "Visit Plot A or Plot B tabs to configure plots for the report",
      stringsAsFactors = FALSE
    )
  }
  
  return(final_data)
}, striped = TRUE, hover = TRUE)

  # ─────────────────────────────────────────────────────
  # Serve the rendered file
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
