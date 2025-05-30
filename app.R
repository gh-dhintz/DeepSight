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
color_bg <- "#F8F9FA"           # Light background
color_panel_bg <- "#FFFFFF"
color_outer_bg <- "#ECECEC"
color_fg <- "#212529"
color_primary <- "#cc4c02"      # Orange
color_secondary <- "#636363"    # Grey for secondary elements

# ────────────────────────────────────────────────────────────────
#### UI ####
# ────────────────────────────────────────────────────────────────
ui <- fluidPage(
  # Set favicon (optional)
  tags$head(
    tags$link(rel = "icon", href = "company_logo.png", type = "image/png")
  ),
  
  # Set custom window title and inject logo+title into navbar
  navbarPage(
    theme = shinytheme("flatly"),
    collapsible = TRUE,
    id = "nav",
    title = div(
      tags$img(src = "company_logo.png", height = "30px", style = "margin-right:10px;"),
      "Breast Cancer GH Patients"
    ),
    
    # Tab content follows...
    tabPanel("User Guide",
      h4("Instructions for Use", class = "text-primary"),
      tableOutput("guideTable")
    ),

    tabPanel("Plot A",
      sidebarLayout(
        sidebarPanel(width = 3,
          h4("Inputs for Plot A", class = "text-primary"),
          selectInput("plotA_x", "X Variable:", choices = names(mtcars), selected = "hp")
        ),
        mainPanel(
          plotOutput("regPlotA", height = "600px")
        )
      )
    ),

    tabPanel("Plot B",
      sidebarLayout(
        sidebarPanel(width = 3,
          h4("Inputs for Plot B", class = "text-primary"),
          selectInput("plotB_x", "X Variable:", choices = names(mtcars), selected = "wt")
        ),
        mainPanel(
          plotOutput("regPlotB", height = "600px")
        )
      )
    ),

    tabPanel("Report Export",
      h4("Export Report", class = "text-primary"),
      selectInput("format", "Report Format:", choices = c("PDF", "HTML", "Word")),
      downloadButton("downloadReport", "Download Report", class = "btn btn-primary")
    )
  )
)

# ────────────────────────────────────────────────────────────────
#### Server ####
# ────────────────────────────────────────────────────────────────
server <- function(input, output) {

  # Reactive formulas
  regFormulaA <- reactive({
    as.formula(paste("mpg ~", input$plotA_x))
  })

  regFormulaB <- reactive({
    as.formula(paste("mpg ~", input$plotB_x))
  })

  ##### Plot A #####
  output$regPlotA <- renderPlot({
    ggplot(mtcars, aes_string(x = input$plotA_x, y = "mpg")) +
      geom_point(color = color_primary, size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = color_secondary) +
      theme_minimal(base_family = "Helvetica Neue") +
      labs(title = "Plot A", x = input$plotA_x, y = "mpg") +
      theme(
        plot.background = element_rect(fill = color_panel_bg, color = NA),
        panel.background = element_rect(fill = color_panel_bg, color = NA),
        text = element_text(color = color_fg),
        axis.text = element_text(color = color_fg),
        axis.title = element_text(color = color_fg),
        plot.title = element_text(face = "bold", color = color_primary)
      )
  })

  ##### Plot B #####
  output$regPlotB <- renderPlot({
    ggplot(mtcars, aes_string(x = input$plotB_x, y = "mpg")) +
      geom_point(color = color_primary, size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = color_secondary) +
      theme_minimal(base_family = "Helvetica Neue") +
      labs(title = "Plot B", x = input$plotB_x, y = "mpg") +
      theme(
        plot.background = element_rect(fill = color_panel_bg, color = NA),
        panel.background = element_rect(fill = color_panel_bg, color = NA),
        text = element_text(color = color_fg),
        axis.text = element_text(color = color_fg),
        axis.title = element_text(color = color_fg),
        plot.title = element_text(face = "bold", color = color_primary)
      )
  })

  ##### Guide Table #####
  output$guideTable <- renderTable({
    data.frame(
      Step = c("Select X for Plot A", "Select X for Plot B", "Review plots", "Choose export format", "Download report"),
      Description = c(
        "Choose a variable for X-axis in Plot A",
        "Choose a variable for X-axis in Plot B",
        "Review the generated regression plots",
        "Pick a format (PDF, HTML, Word)",
        "Download the generated report"
      )
    )
  })

  ##### Report Download #####
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("my-report", switch(input$format,
        PDF = "pdf",
        HTML = "html",
        Word = "docx"), sep = ".")
    },
    content = function(file) {
      fmt <- switch(input$format,
        PDF = list(input = "my_pdf_doc.qmd", format = "pdf", ext = "pdf"),
        HTML = list(input = "html.qmd", format = "html", ext = "html"),
        Word = list(input = "word.qmd", format = "docx", ext = "docx"))

      out_file <- paste0("report.", fmt$ext)

      result <- tryCatch({
        quarto::quarto_render(
          input = fmt$input,
          output_file = out_file,
          output_format = fmt$format,
          execute_params = list(
            plotA_x = input$plotA_x,
            plotB_x = input$plotB_x
          ),
          execute_dir = getwd()
        )
      }, error = function(e) {
        stop("Document generation failed: ", e$message)
      })

      if (!file.exists(out_file)) {
        stop("Document generation failed: output not found.")
      }

      file.copy(out_file, file, overwrite = TRUE)
    }
  )
}
#### shinyApp(ui, server) ####
shinyApp(ui, server)
