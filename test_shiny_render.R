# test_shiny_render.R
# This replicates EXACTLY how your Shiny app renders the PDF

library(yaml)
library(quarto)

cat("=== TESTING SHINY APP RENDERING PROCESS ===\n")

# Step 1: Create mock YAML data (simulating your report_vars.yaml)
mock_vars <- list(
  vars = list(
    plot_C_n = 1,
    plot_C_x1 = 3,
    plot_C_annotation_1_1 = "fig-C-1",
    plot_C_annotation_1_2 = "Shiny Render Test Survival Analysis",
    plot_C_annotation_1_3 = "Testing exact Shiny app rendering",
    plot_C_annotation_1_4 = "Treatment Survival Panel",
    plot_C_annotation_1_5 = "Stage Survival Panel", 
    plot_C_annotation_1_6 = "Complete Survival Analysis",
    plot_C_Notes_shared = "This survival analysis tests the exact Shiny rendering process. See @fig-C-1 for details.",
    bookmark_url = "http://test.com",
    generated_at = Sys.time()
  ),
  params = list(
    title = "Shiny Render Test",
    subtitle = "Testing exact rendering process",
    name = "Test Author"
  )
)

# Write YAML file (exactly like your Shiny app does)
yaml_content <- yaml::as.yaml(mock_vars)
writeLines(yaml_content, "report_vars.yaml")
cat("✓ Created report_vars.yaml\n")

# Step 2: Use the EXACT quarto_render call from your Shiny app
cat("✓ Starting quarto_render with exact Shiny parameters...\n")

tryCatch({
  # This is EXACTLY your Shiny app's quarto_render call
  quarto::quarto_render(
    input = "production_replica_test.qmd",  # Use our test file
    output_file = "shiny_render_test.pdf",
    output_format = "pdf", 
    execute_params = list(
      title = mock_vars$params$title,
      subtitle = mock_vars$params$subtitle, 
      name = mock_vars$params$name
    ),
    execute_dir = getwd()
  )
  
  cat("✅ SUCCESS: PDF rendered using exact Shiny process\n")
  cat("✓ Output file: shiny_render_test.pdf\n")
  
  # Check if file exists and get size
  if (file.exists("shiny_render_test.pdf")) {
    file_size <- file.size("shiny_render_test.pdf")
    cat("✓ File size:", file_size, "bytes\n")
    
    if (file_size > 10000) {  # Reasonable PDF size
      cat("✅ PDF appears to be properly generated\n")
    } else {
      cat("⚠️  PDF file seems too small - might be incomplete\n")
    }
  } else {
    cat("❌ PDF file was not created\n")
  }
  
}, error = function(e) {
  cat("❌ FAILED: quarto_render failed with error:\n")
  cat("Error message:", e$message, "\n")
  cat("This matches the issue in your Shiny app!\n")
})

cat("\n=== COMPARISON TEST ===\n")
cat("Now compare:\n")
cat("1. shiny_render_test.pdf (using quarto_render - Shiny method)\n")
cat("2. production_replica_test.pdf (using quarto preview - Direct method)\n")
cat("\nIf Plot C is missing in #1 but present in #2, we've found the issue!\n")

# Step 3: Clean up
cat("\n=== CLEANUP ===\n")
if (file.exists("report_vars.yaml")) {
  cat("✓ Keeping report_vars.yaml for inspection\n")
}