##################################################################
##                        Global Utility                        ##
##################################################################

# Resampling: Pairwise ----------------------------------------------------

gu_results <- lapply(file_name, function(fn) {
  
  # Load synthetic data
  sds <- readRDS(file.path(input_path, fn))
  
  # Calculate pMSE ratio for this synthesis setting
  res <- gu_micro(
    ods = data,
    sds = sds$syn,
    resampling = "pairwise",
    future_plan = "multicore",
    workers = 10L,
    ignore_na = TRUE,
    rpart_control = rpart::rpart.control(cp = 1e-3),
    future.seed = TRUE,
    future.packages = NULL
  )

  # Output
  res

})

# Name each list regarding to the synthesis setting
names(gu_results) <- suffix_name
saveRDS(gu_results, file = file.path(output_path, "micro_gu_detailed.rds"))

# Adjust results in a desired format for the correlation analysis
out <- purrr::imap_dfr(gu_results, function(mat, nm) {
  
  # mat: matrix array in gu_results
  # nm: list names
  
  # Transpose matrix (3x5 -> 5x3) and build the average for all metrics
  tib <- tibble::as_tibble_row(colMeans(t(mat)))
  
  # Additional information: Predictors and Levels as separate columns
  parsed <- stringr::str_match(nm, "pred(\\d+)_lv(\\d+)")
  pred_val <- as.integer(parsed[1, 2])
  lv_val <- as.integer(parsed[1, 3])
  
  tib |> 
    dplyr::mutate(predictors = pred_val, levels = lv_val)
}, .id = NULL)


saveRDS(out, file = file.path(output_path, "micro_gu.rds"))
