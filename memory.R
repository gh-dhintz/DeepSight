#### maps ####
maps <- map(cc, function(cancer) {
  df <- gh_ps_lt
  
  # Tested at 1L (newly dx on TRF)
  c1 <- df %>%
    filter(
      serial_test == 1,
      line == 1,
      new_cancer_category == cancer
    ) 
  
  # Tested at later line (not responding to treatment on TRF)
  c2 <- df %>%
    group_by(upk_key) %>%
    mutate(n_tests = n_distinct(request_id)) %>%
    ungroup() %>%
    filter(
      n_tests > 1 & line > 1,
      new_cancer_category == cancer
    )
  
  # with multiple G360 tests
  c3 <- df %>%
    group_by(upk_key) %>%
    mutate(n_tests = n_distinct(request_id)) %>%
    ungroup() %>%
    filter(
      product %in% c(
        'Guardant360 LDT Infinity',"Guardant360 LDT 2.12","Guardant360 CDx",
        "Guardant360","Guardant Response"),
      n_tests > 1,
      new_cancer_category == cancer
    ) 
  
  lst <- list(c1, c2, c3)
  fns <- list(upk)
  
  map2_dfr(lst, fns, ~ .y(.x), .id = "Count")
})




#### Arrow ####
cancer_by_cancer_approach <- function() {
  map_dfr(cc, function(cancer) {
    # Read and filter for this cancer only
    rwe_subset <- open_dataset("/mnt/imported/data/inform/2025Q2/rwe/gh_inform.parquet/") %>%
      filter(new_cancer_category == cancer) %>%
      select(upk_key, serial_test, request_id, product)
    
    # Join with summaries
    with_summaries <- rwe_subset %>%
      inner_join(
        open_dataset("/mnt/imported/data/inform/2025Q2/rwe/patient_summaries.parquet/") %>%
          select(upk_key, patient_ethnicity_omb, patient_race_omb),
        by = "upk_key"
      )
    
    # Try each LOT dataset until we find matches
    lot_paths <- c(
      "/mnt/imported/data/inform/2025Q2/rwe_disease/pancancer_lines_of_therapy.parquet/",
      "/mnt/imported/data/inform/2025Q2/rwe_disease/crc_lines_of_therapy.parquet/", 
      "/mnt/imported/data/inform/2025Q2/rwe_disease/breast_lines_of_therapy.parquet/",
      "/mnt/imported/data/inform/2025Q2/rwe_disease/lung_lines_of_therapy.parquet/",
      "/mnt/imported/data/inform/2025Q2/rwe_disease/prostate_lines_of_therapy.parquet/"
    )
    
    # Combine results from all LOT datasets for this cancer
    cancer_data <- map_dfr(lot_paths, function(lot_path) {
      tryCatch({
        with_summaries %>%
          inner_join(
            open_dataset(lot_path) %>% select(upk_key, line, regimen),
            by = "upk_key"
          ) %>%
          collect()
      }, error = function(e) {
        tibble()  # Return empty if no matches
      })
    })
    
    # Analyze this cancer's data
    if (nrow(cancer_data) > 0) {
      cancer_data %>%
        group_by(upk_key) %>%
        summarise(
          n_tests = n_distinct(request_id),
          has_first_line = any(serial_test == 1 & line == 1),
          has_later_line = any(line > 1),
          has_guardant = any(str_detect(product, "Guardant360")),
          .groups = "drop"
        ) %>%
        summarise(
          c1 = sum(has_first_line, na.rm = TRUE),
          c2 = sum(has_later_line & n_tests > 1, na.rm = TRUE),
          c3 = sum(has_guardant & n_tests > 1, na.rm = TRUE)
        ) %>%
        mutate(cancer = cancer)
    } else {
      tibble(c1 = 0L, c2 = 0L, c3 = 0L, cancer = cancer)
    }
  })
}
arrow <- cancer_by_cancer_approach()  