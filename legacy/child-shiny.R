library(shiny)
library(rmarkdown)
library(knitr)

# Define UI
ui <- fluidPage(
  titlePanel("Report with Child Documents"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("generate", "Generate Report", class = "btn-primary"),
      br(), br(),
      checkboxGroupInput("files", 
                        "Select child documents to include:",
                        choices = c("Child One" = "child-one.Rmd", 
                                   "Child Two" = "child-two.Rmd"),
                        selected = c("child-one.Rmd", "child-two.Rmd")),
      br(),
      downloadButton("download", "Download Report", class = "btn-success")
    ),
    
    mainPanel(
      h2("Generated Report"),
      htmlOutput("report_html")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to store report content
  report_content <- reactiveVal()
  
  # Generate report when button is clicked
  observeEvent(input$generate, {
    
    withProgress(message = 'Generating report...', value = 0, {
      
      # Create temporary main Rmd file
      temp_main <- tempfile(fileext = ".Rmd")
      temp_html <- tempfile(fileext = ".html")
      
      # Build the main document content
      main_content <- '---
title: "Generated Report"
output: 
  html_document:
    self_contained: true
---

# Summary

'
      
      # Add child document inclusion
      if (length(input$files) > 0) {
        child_files_str <- paste0("c('", paste(input$files, collapse = "', '"), "')")
        main_content <- paste0(main_content, 
'```{r, results="asis", echo=FALSE, warning=FALSE, message=FALSE}
child_files <- ', child_files_str, '
for (file in child_files) {
  if (file.exists(file)) {
    cat(knitr::knit_child(file, quiet = TRUE), sep = "\\n\\n")
  } else {
    cat("**Note:** File", file, "not found.\\n\\n")
  }
}
```

')
      }
      
      main_content <- paste0(main_content, '
# Conclusion

Report generated successfully at ', Sys.time(), '
')
      
      incProgress(0.3, detail = "Writing files...")
      
      # Write the main file
      writeLines(main_content, temp_main)
      
      incProgress(0.6, detail = "Rendering...")
      
      # Render to HTML
      tryCatch({
        rmarkdown::render(temp_main, 
                         output_file = temp_html,
                         quiet = TRUE,
                         envir = new.env())
        
        # Read the generated HTML
        html_content <- readLines(temp_html, warn = FALSE)
        html_content <- paste(html_content, collapse = "\n")
        
        # Extract body content (remove head, html tags)
        body_start <- regexpr("<body[^>]*>", html_content)
        body_end <- regexpr("</body>", html_content)
        
        if (body_start > 0 && body_end > 0) {
          body_content <- substr(html_content, 
                               body_start + attr(body_start, "match.length"), 
                               body_end - 1)
          report_content(body_content)
        } else {
          report_content(html_content)
        }
        
        incProgress(1, detail = "Complete!")
        
      }, error = function(e) {
        report_content(paste("<div class='alert alert-danger'>",
                           "<strong>Error:</strong>", e$message,
                           "</div>"))
      })
      
      # Clean up temp files
      unlink(c(temp_main, temp_html))
    })
  })
  
  # Display the report
  output$report_html <- renderUI({
    if (is.null(report_content())) {
      HTML("<p>Click 'Generate Report' to create the report.</p>")
    } else {
      HTML(report_content())
    }
  })
  
  # Download handler
  output$download <- downloadHandler(
    filename = function() {
      paste("report-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      if (!is.null(report_content())) {
        # Create a complete HTML document
        full_html <- paste0(
          '<!DOCTYPE html>
<html>
<head>
<title>Generated Report</title>
<style>
body { font-family: Arial, sans-serif; margin: 40px; }
h1, h2, h3 { color: #333; }
</style>
</head>
<body>',
          report_content(),
          '</body></html>'
        )
        writeLines(full_html, file)
      }
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)