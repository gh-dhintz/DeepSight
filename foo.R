library(shiny)
library(quarto)
library(bslib)
library(ggplot2)

# ────────────────────────────────────────────────────────────────
# 🟦 Define Global Color Variables
# ────────────────────────────────────────────────────────────────
color_bg <- "#FFFFFF"
color_plot_background <- "#FFFFFF"
color_fg <- "#0B1C72"
color_primary <- "#D62727"
color_success <- "#8A9C40"
color_warning <- "#FFA500"
color_plotA_point <- color_primary
color_plotA_line <- color_warning
color_plotB_point <- color_success
color_plotB_line <- color_warning

# ────────────────────────────────────────────────────────────────
# 🌐 UI
# ────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "morph",
    base_font = font_google("Roboto"),
    heading_font = font_google("Roboto"),
    bg = color_bg,
    fg = color_fg,
    primary = color_primary,
    success = color_success,
    warning = color_warning
  ),
  titlePanel("Multiple Regression Plots"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Inputs", class = "text-primary"),
      selectInput("plotA_x", "X Variable for Plot A:", choices = names(mtcars), selected = "hp"),
      selectInput("plotB_x", "X Variable for Plot B:", choices = names(mtcars), selected = "wt"),
      tags$hr(),
      h4("Report Export", class = "text-primary"),
      selectInput("format", "Report Format:", choices = c("PDF", "HTML", "Word")),
      downloadButton("downloadReport", "Download Report", class = "btn btn-primary")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot A",
          card(
            card_header("Regression Plot A"),
            card_body(plotOutput("regPlotA"))
          )
        ),
        tabPanel("Plot B",
          card(
            card_header("Regression Plot B"),
            card_body(plotOutput("regPlotB"))
          )
        )
      )
    )
  )
)

# ────────────────────────────────────────────────────────────────
# 🧠 Server
# ────────────────────────────────────────────────────────────────
server <- function(input, output) {

  regFormulaA <- reactive({
    as.formula(paste("mpg ~", input$plotA_x))
  })
  
  regFormulaB <- reactive({
    as.formula(paste("mpg ~", input$plotB_x))
  })

  output$regPlotA <- renderPlot({
    ggplot(mtcars, aes_string(x = input$plotA_x, y = "mpg")) +
      geom_point(color = color_plotA_point, size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = color_plotA_line) +
      theme_minimal(base_family = "Roboto") +
      labs(title = "Plot A", x = input$plotA_x, y = "mpg") +
      theme(
        plot.background = element_rect(fill = color_plot_background, color = NA),
        panel.background = element_rect(fill = color_plot_background, color = NA),
        text = element_text(color = color_fg),
        axis.text = element_text(color = color_fg),
        axis.title = element_text(color = color_fg),
        plot.title = element_text(face = "bold")
      )
  })

  output$regPlotB <- renderPlot({
    ggplot(mtcars, aes_string(x = input$plotB_x, y = "mpg")) +
      geom_point(color = color_plotB_point, size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = color_plotB_line) +
      theme_minimal(base_family = "Roboto") +
      labs(title = "Plot B", x = input$plotB_x, y = "mpg") +
      theme(
        plot.background = element_rect(fill = color_plot_background, color = NA),
        panel.background = element_rect(fill = color_plot_background, color = NA),
        text = element_text(color = color_fg),
        axis.text = element_text(color = color_fg),
        axis.title = element_text(color = color_fg),
        plot.title = element_text(face = "bold")
      )
  })

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("my-report", switch(input$format,
                                PDF = "pdf",
                                HTML = "html",
                                Word = "docx"), sep = ".")
    },
    content = function(file) {
      fmt <- switch(input$format,
                    PDF = list(input = "my_pdf_doc.qmd",  format = "pdf",  ext = "pdf"),
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

shinyApp(ui, server)