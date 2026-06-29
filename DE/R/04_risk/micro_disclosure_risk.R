#################################################################
##                Disclosure Risk for microdata                ##
##                          TCAP ratio                         ##
#################################################################

# Calculate TCAP ratio ----------------------------------------------------

out <- future.apply::future_lapply(
  file_name,
  function(fn) {
    
    # Load synthetic data
    sds <- readRDS(file.path(input_path, fn))
    
    # Prepare data for TCAP calculation
    data_prep <- prepare_data(
      ods = data,
      sds = sds$syn,
      key_variables = key_variables,
      target_variables = target_variables,
      bin_variables = bin_variables,
      breaks = 5L
    )
    
    # TCAP ratio
    res <- tcap_ratio(
      ods = data_prep$ods_bin,
      sds = data_prep$sds_bin,
      key_variables = key_variables,
      target_variables = target_variables,
      exclusion_rate = 0.9,
      exception_variables = NULL,
      exception_values = NULL,
      weap_threshold = 1
    )
    
    # Number of predictors and levels as separate column
    pred_lv <- tidyr::extract(
      tibble(file = fn),
      col = file,
      into = c("predictors", "levels"),
      regex = "sds_preds(\\d+)_lv(\\d+)",
      convert = TRUE
    )
    
    # Only averaged TCAP ratio over all m synthetic dataset
    tibble(pred_lv, tcap_ratio = res$tcap_ratio)
    
  },
  future.seed = TRUE,
  future.packages = NULL
)

out <- do.call(rbind, out)

saveRDS(out, file = file.path(output_path, "micro_tcap_ratios.rds"))

# Plot --------------------------------------------------------------------

# require(ggplot2)
# out |> 
#   ggplot(aes(x = as.factor(predictors), y = tcap_ratio, group = levels)) +
#   geom_line(linewidth = 1) +
#   geom_point(size = 6) +
#   labs(x = "# predictor", y = "TCAP_ratio") +
#   facet_grid(~ levels)

