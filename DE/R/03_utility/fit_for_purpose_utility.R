#################################################################
##                   Fit-for-purpose utility                   ##
#################################################################

# Cramer V Matrix ---------------------------------------------------------

# Cramer's V Matrix for original data
cv_orig <- calc_cramerV(
  data = data, 
  future_plan = "multicore", 
  workers = 75L, 
  future.seed = TRUE,
  future.packages = NULL
)
saveRDS(cv_orig, file = file.path("~/WP13/DE/output/tmp", "cramer_original.rds"))

cv_mat_results <-  lapply(file_name, function(fn) {
  
  # Load synthetic data
  sds <- readRDS(file.path(input_path, fn))
  
  # Calculate Cramer's V for synthetic dataset
  cv_syn <- lapply(sds$syn, function(sds) {
    calc_cramerV(
      data = sds,
      future_plan = "multicore",
      workers = 75L,
      future.seed = TRUE,
      future.packages = NULL
    )
  })
  
  # Results
  cv_diff <- lapply(cv_syn, function(cv_s) {
    cv_orig - cv_s
  })
  cv_diff_mean <- Reduce("+", cv_diff) / length(cv_diff)
  
  # Output
  list(cv_syn = cv_syn,
       cv_diff = cv_diff,
       cv_diff_mean = cv_diff_mean)
})

# Name each list regarding to the synthesis setting
names(cv_mat_results) <- suffix_name
saveRDS(cv_mat_results, file = file.path(output_path, "cramer_results.rds"))
