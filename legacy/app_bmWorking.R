# ────────────────────────────────────────────────────────────────
#### Automatic Bookmarking on Input Changes ####
# ────────────────────────────────────────────────────────────────
library(shiny)
library(ggplot2)

# ────────────────────────────────────────────────────────────────
#### UI ####
# ────────────────────────────────────────────────────────────────
ui <- function(request) {
  fluidPage(
    # Add JavaScript for clipboard functionality
    tags$head(
      tags$script(HTML("
        Shiny.addCustomMessageHandler('copyToClipboard', function(text) {
          // Try modern clipboard API first
          if (navigator.clipboard && window.isSecureContext) {
            navigator.clipboard.writeText(text).then(function() {
              console.log('URL copied to clipboard via Clipboard API');
            }).catch(function(err) {
              console.error('Clipboard API failed: ', err);
              fallbackCopy(text);
            });
          } else {
            // Fallback for older browsers or non-HTTPS
            fallbackCopy(text);
          }
        });
        
        function fallbackCopy(text) {
          // Create temporary textarea
          var textarea = document.createElement('textarea');
          textarea.value = text;
          textarea.style.position = 'fixed';
          textarea.style.opacity = '0';
          document.body.appendChild(textarea);
          textarea.select();
          try {
            var successful = document.execCommand('copy');
            if (successful) {
              console.log('URL copied to clipboard via fallback');
            } else {
              console.error('Fallback copy failed');
            }
          } catch (err) {
            console.error('Fallback copy error: ', err);
          }
          document.body.removeChild(textarea);
        }
      "))
    ),
    
    titlePanel("Breast Cancer GH Patients - Auto Bookmark"),
    
    tabsetPanel(
      id = "tabs",
      
      tabPanel("Plot A",
               sidebarLayout(
                 sidebarPanel(width = 3,
                              h4("Inputs for Plot A"),
                              numericInput("plotA_n", "Number of plots (max 3):", value = 1, min = 1, max = 3),
                              
                              # Conditional inputs based on number of plots
                              conditionalPanel(
                                condition = "input.plotA_n >= 1",
                                selectInput("plotA_x_1", "X Variable for Plot A1:", 
                                            choices = names(mtcars), selected = "wt"),
                                textAreaInput("plotA_notes_1", "Notes for Plot A1:", 
                                              placeholder = "Add your comments...", height = "60px")
                              ),
                              conditionalPanel(
                                condition = "input.plotA_n >= 2",
                                selectInput("plotA_x_2", "X Variable for Plot A2:", 
                                            choices = names(mtcars), selected = "hp"),
                                textAreaInput("plotA_notes_2", "Notes for Plot A2:", 
                                              placeholder = "Add your comments...", height = "60px")
                              ),
                              conditionalPanel(
                                condition = "input.plotA_n >= 3",
                                selectInput("plotA_x_3", "X Variable for Plot A3:", 
                                            choices = names(mtcars), selected = "disp"),
                                textAreaInput("plotA_notes_3", "Notes for Plot A3:", 
                                              placeholder = "Add your comments...", height = "60px")
                              )
                 ),
                 mainPanel(
                   # Conditional plot outputs
                   conditionalPanel(
                     condition = "input.plotA_n >= 1",
                     plotOutput("plotA_1", height = "300px")
                   ),
                   conditionalPanel(
                     condition = "input.plotA_n >= 2",
                     plotOutput("plotA_2", height = "300px")
                   ),
                   conditionalPanel(
                     condition = "input.plotA_n >= 3",
                     plotOutput("plotA_3", height = "300px")
                   )
                 )
               )
      ),
      
      tabPanel("Plot B",
               sidebarLayout(
                 sidebarPanel(width = 3,
                              h4("Inputs for Plot B"),
                              numericInput("plotB_n", "Number of plots (max 3):", value = 1, min = 1, max = 3),
                              
                              conditionalPanel(
                                condition = "input.plotB_n >= 1",
                                selectInput("plotB_x_1", "X Variable for Plot B1:", 
                                            choices = names(mtcars), selected = "wt"),
                                textAreaInput("plotB_notes_1", "Notes for Plot B1:", 
                                              placeholder = "Add your comments...", height = "60px")
                              ),
                              conditionalPanel(
                                condition = "input.plotB_n >= 2",
                                selectInput("plotB_x_2", "X Variable for Plot B2:", 
                                            choices = names(mtcars), selected = "hp"),
                                textAreaInput("plotB_notes_2", "Notes for Plot B2:", 
                                              placeholder = "Add your comments...", height = "60px")
                              ),
                              conditionalPanel(
                                condition = "input.plotB_n >= 3",
                                selectInput("plotB_x_3", "X Variable for Plot B3:", 
                                            choices = names(mtcars), selected = "disp"),
                                textAreaInput("plotB_notes_3", "Notes for Plot B3:", 
                                              placeholder = "Add your comments...", height = "60px")
                              )
                 ),
                 mainPanel(
                   conditionalPanel(
                     condition = "input.plotB_n >= 1",
                     plotOutput("plotB_1", height = "300px")
                   ),
                   conditionalPanel(
                     condition = "input.plotB_n >= 2",
                     plotOutput("plotB_2", height = "300px")
                   ),
                   conditionalPanel(
                     condition = "input.plotB_n >= 3",
                     plotOutput("plotB_3", height = "300px")
                   )
                 )
               )
      ),
      
      tabPanel("Report Export",
               h4("Export Report"),
               
               selectInput("format", "Report Format:", 
                           choices = c("PDF", "HTML", "Word"), selected = "PDF"),
               
               br(),
               
               # Prominent bookmark URL display
               div(
                 style = "background-color: #f8f9fa; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
                 h5("Current Bookmark URL:", style = "color: #495057; margin-bottom: 10px;"),
                 div(
                   style = "background-color: white; padding: 10px; border: 1px solid #dee2e6; border-radius: 3px;",
                   verbatimTextOutput("current_url", placeholder = TRUE)
                 ),
                 br(),
                 actionButton("copy_url", "Copy URL to Clipboard", 
                              class = "btn btn-outline-secondary btn-sm"),
                 tags$small(" (Click to copy the bookmark URL)", 
                            style = "color: #6c757d; margin-left: 10px;")
               ),
               
               h5("Current Configuration:"),
               tableOutput("config_table"),
               
               br(),
               actionButton("download_trigger", "Download Report", 
                            class = "btn btn-primary")
      )
    )
  )
}

# ────────────────────────────────────────────────────────────────
#### Server ####
# ────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  
  # Store the current bookmark URL
  current_bookmark_url <- reactiveVal(NULL)
  
  # Copy URL to clipboard functionality
  observeEvent(input$copy_url, {
    current_url <- current_bookmark_url()
    if (!is.null(current_url)) {
      # Send the URL to JavaScript for copying
      session$sendCustomMessage("copyToClipboard", current_url)
      showNotification("Bookmark URL copied to clipboard!", type = "message", duration = 3)
    } else {
      showNotification("No bookmark URL available yet.", type = "warning")
    }
  })
  
  # ─────────────────────────────────────────────────
  #### Auto-bookmark on any input change ####
  # ─────────────────────────────────────────────────
  
  # Create a reactive that tracks all inputs we care about
  inputs_to_track <- reactive({
    # List all the inputs that should trigger bookmarking
    list(
      tabs = input$tabs,
      plotA_n = input$plotA_n,
      plotA_x_1 = input$plotA_x_1,
      plotA_x_2 = input$plotA_x_2,
      plotA_x_3 = input$plotA_x_3,
      plotA_notes_1 = input$plotA_notes_1,
      plotA_notes_2 = input$plotA_notes_2,
      plotA_notes_3 = input$plotA_notes_3,
      plotB_n = input$plotB_n,
      plotB_x_1 = input$plotB_x_1,
      plotB_x_2 = input$plotB_x_2,
      plotB_x_3 = input$plotB_x_3,
      plotB_notes_1 = input$plotB_notes_1,
      plotB_notes_2 = input$plotB_notes_2,
      plotB_notes_3 = input$plotB_notes_3,
      format = input$format
    )
  })
  
  # Auto-bookmark whenever tracked inputs change
  observeEvent(inputs_to_track(), {
    # Add a small delay to avoid rapid-fire bookmarking
    invalidateLater(500, session)
    session$doBookmark()
  }, ignoreInit = TRUE) # Important: don't trigger on app startup
  
  # Handle bookmark completion
  onBookmarked(function(url) {
    current_bookmark_url(url)
    # Optionally show subtle notification
    # showNotification("State saved", type = "message", duration = 2)
  })
  
  # ─────────────────────────────────────────────────
  #### Plot Outputs ####
  # ─────────────────────────────────────────────────
  
  # Plot A outputs
  output$plotA_1 <- renderPlot({
    req(input$plotA_x_1)
    ggplot(mtcars, aes_string(x = input$plotA_x_1, y = "mpg")) +
      geom_point(color = "#cc4c02", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "#636363") +
      theme_minimal() +
      labs(title = "Plot A1", x = input$plotA_x_1, y = "mpg")
  })
  
  output$plotA_2 <- renderPlot({
    req(input$plotA_x_2)
    ggplot(mtcars, aes_string(x = input$plotA_x_2, y = "mpg")) +
      geom_point(color = "#cc4c02", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "#636363") +
      theme_minimal() +
      labs(title = "Plot A2", x = input$plotA_x_2, y = "mpg")
  })
  
  output$plotA_3 <- renderPlot({
    req(input$plotA_x_3)
    ggplot(mtcars, aes_string(x = input$plotA_x_3, y = "mpg")) +
      geom_point(color = "#cc4c02", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "#636363") +
      theme_minimal() +
      labs(title = "Plot A3", x = input$plotA_x_3, y = "mpg")
  })
  
  # Plot B outputs
  output$plotB_1 <- renderPlot({
    req(input$plotB_x_1)
    ggplot(mtcars, aes_string(x = input$plotB_x_1, y = "mpg")) +
      geom_point(color = "#cc4c02", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "#636363") +
      theme_minimal() +
      labs(title = "Plot B1", x = input$plotB_x_1, y = "mpg")
  })
  
  output$plotB_2 <- renderPlot({
    req(input$plotB_x_2)
    ggplot(mtcars, aes_string(x = input$plotB_x_2, y = "mpg")) +
      geom_point(color = "#cc4c02", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "#636363") +
      theme_minimal() +
      labs(title = "Plot B2", x = input$plotB_x_2, y = "mpg")
  })
  
  output$plotB_3 <- renderPlot({
    req(input$plotB_x_3)
    ggplot(mtcars, aes_string(x = input$plotB_x_3, y = "mpg")) +
      geom_point(color = "#cc4c02", size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "#636363") +
      theme_minimal() +
      labs(title = "Plot B3", x = input$plotB_x_3, y = "mpg")
  })
  
  # ─────────────────────────────────────────────────
  #### Configuration Display ####
  # ─────────────────────────────────────────────────
  
  output$config_table <- renderTable({
    config_data <- data.frame(
      Setting = character(),
      Value = character(),
      stringsAsFactors = FALSE
    )
    
    # Add Plot A info
    if (!is.null(input$plotA_n) && input$plotA_n > 0) {
      config_data <- rbind(config_data, data.frame(
        Setting = "Plot A - Number of Plots",
        Value = as.character(input$plotA_n)
      ))
      
      for (i in 1:input$plotA_n) {
        x_var <- input[[paste0("plotA_x_", i)]]
        if (!is.null(x_var)) {
          config_data <- rbind(config_data, data.frame(
            Setting = paste("Plot A", i, "- X Variable"),
            Value = x_var
          ))
        }
      }
    }
    
    # Add Plot B info
    if (!is.null(input$plotB_n) && input$plotB_n > 0) {
      config_data <- rbind(config_data, data.frame(
        Setting = "Plot B - Number of Plots", 
        Value = as.character(input$plotB_n)
      ))
      
      for (i in 1:input$plotB_n) {
        x_var <- input[[paste0("plotB_x_", i)]]
        if (!is.null(x_var)) {
          config_data <- rbind(config_data, data.frame(
            Setting = paste("Plot B", i, "- X Variable"),
            Value = x_var
          ))
        }
      }
    }
    
    # Add format
    if (!is.null(input$format)) {
      config_data <- rbind(config_data, data.frame(
        Setting = "Report Format",
        Value = input$format
      ))
    }
    
    if (nrow(config_data) == 0) {
      config_data <- data.frame(
        Setting = "No configuration set",
        Value = "Configure plots in Plot A or Plot B tabs"
      )
    }
    
    return(config_data)
  }, striped = TRUE, hover = TRUE)
  
  # Show current URL for debugging
  output$current_url <- renderText({
    url <- current_bookmark_url()
    if (is.null(url)) {
      "No bookmark URL yet - change an input to generate"
    } else {
      url
    }
  })
  
  # ─────────────────────────────────────────────────
  #### Download Handler ####
  # ─────────────────────────────────────────────────
  
  observeEvent(input$download_trigger, {
    current_url <- current_bookmark_url()
    if (!is.null(current_url)) {
      showModal(modalDialog(
        title = "Report Generation",
        p("In a full implementation, this would generate a report using the current bookmark URL:"),
        p(tags$code(current_url)),
        p("All current settings would be passed to the report generation process."),
        easyClose = TRUE
      ))
    } else {
      showNotification("No bookmark URL available yet.", type = "warning")
    }
  })
}

# ────────────────────────────────────────────────────────────────
#### Run App ####
# ────────────────────────────────────────────────────────────────
enableBookmarking(store = "url")
shinyApp(ui, server)