library(shiny)

max_plots <- 5

ui <- fluidPage(

  headerPanel("Dynamic number of plots"),

  sidebarPanel(
    numericInput("n", "Number of plots", value = 1, min = 1, max = max_plots, step = 1)
  ),

  mainPanel(
    uiOutput("plots")
  )
)

server <- function(input, output, session) {

  # Dynamic UI for plots and select inputs
  output$plots <- renderUI({
    plot_ui_list <- lapply(1:input$n, function(i) {
      select_id <- paste0("select_", i)
      plot_id <- paste0("plot", i)

      tagList(
        selectInput(select_id, paste("Plot", i, "type"),
                    choices = c("Linear", "Quadratic", "Random"), selected = "Linear"),
        plotOutput(plot_id, height = 280, width = 250)
      )
    })
    do.call(tagList, plot_ui_list)
  })

  # Generate plots based on select input
  for (i in 1:max_plots) {
    local({
      my_i <- i
      select_id <- paste0("select_", my_i)
      plot_id <- paste0("plot", my_i)

      output[[plot_id]] <- renderPlot({
        req(input[[select_id]])  # ensure the select input exists

        type <- input[[select_id]]
        x <- 1:max_plots

        y <- switch(type,
          "Linear" = x * my_i,
          "Quadratic" = (x^2) / my_i,
          "Random" = runif(max_plots, 0, my_i)
        )

        plot(x, y, type = "b", main = paste("Plot", my_i, "-", type),
             xlim = c(1, max_plots), ylim = range(y))
      })
    })
  }
}

shinyApp(ui, server)