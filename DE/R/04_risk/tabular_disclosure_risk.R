#####################################################################
##                Disclosure risk for tabular data                 ##
##  Compare disclosive cells between synthetic and original table  ##
#####################################################################

# (1) Build a official table ----------------------------------------------

# Description: Assume that this is a table or table constellations based on
# key and target variables that should be published. 

# In practice: The table structure may come from the experts of a specific
# domain department that published exactly these tables or want to publish it in 
# future. 

# Prepare data for tabulation
#   injuries_fatal: convert into factor (since it only has four unique values)
#   crash_hour: bin for tabulation:
#     6 - 10: morning, 
#     11 - 15: noon, 
#     16 - 20: evening, 
#     21 - 5: night
tab_data <- build_official_table(data = data,
                                 key = key_official_table,
                                 target = target_official_table)

# All n-way table constellations
combinations <- n_way_tables(keys = key_official_table, targets = target_official_table, sizes = 2:4)


# (2) Risk calculation ----------------------------------------------------

risk_res <- lapply(seq_along(combinations), function(i) {
  
  sensitive_vars <- combinations[[i]]
  if(length(intersect(combinations[[i]], key_official_table)) > 0) sensitive_vars <- intersect(combinations[[i]], key_official_table)
  
  # Determine disclosive cells in original dataset
  dc_orig <- find_disclosive_cells(
    data = tab_data,
    vars = c(combinations[[i]]),
    sensitive_vars = sensitive_vars,
    gd_exclusion_rate = 0.9
  )

  # Calculate tabular risk 
  out <- future.apply::future_lapply(
    seq_along(file_name),
    function(fn) {
      
      # Load synthetic data
      sds <- readRDS(file.path(input_path, file_name[fn]))
      
      # Determine risk
      tab_sy <- lapply(sds$syn, function(syn_data) {
        
        # Prepare data for tabulation
        tab_sy_m <- build_official_table(data = syn_data,
                                         key = key_official_table,
                                         target = target_official_table)
        
        # Determine disclosive cells in synthetic dataset
        dc_syn <- find_disclosive_cells(
          data = tab_sy_m,
          vars = combinations[[i]],
          sensitive_vars = sensitive_vars,
          gd_exclusion_rate = 0.9 
        )
        
        # Calculate tabular risk
        res_risk <- calc_tabular_risk(
          sensitive_vars = sensitive_vars,
          ls_sy = dc_syn,
          ls_or = dc_orig
        )
        
        # Risk output
        res_risk
      })
      
      # Average risk across all m synthetic dataset
      tab_sy_mean <- tab_sy |> 
        dplyr::bind_rows() |> 
        dplyr::summarise(dplyr::across(tidyselect::where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
      
      # Number of predictors and levels as separate column + Table-ID + n_way dimension
      additional_info <- tidyr::extract(
        tibble::tibble(file = suffix_name[fn]),
        col = file,
        into = c("predictors", "levels"),
        regex = "pred(\\d+)_lv(\\d+)",
        convert = TRUE
      ) |> 
        dplyr::mutate(
          table_id = i,
          n_way = length(combinations[[i]])
        )
      
      # Add risk with additional infos and indicators of used targets
      out <- cbind(additional_info, tab_sy_mean) |>
        (\(df) bind_cols(df, tibble::as_tibble(
          matrix(
            FALSE,
            nrow = nrow(df),
            ncol = length(target_official_table),
            dimnames = list(NULL, target_official_table)
          )
        )))()
      
      # Indicator that shows which target were used in this tabulation
      tmp_ind <- intersect(combinations[[i]], target_official_table)
      out[tmp_ind] <- TRUE
      out
    },
    future.seed = TRUE,
    future.packages = NULL
  )
  
  # Output: Concatenate all calculated risks from each synthesis setting into one tibble
  tibble::as_tibble(do.call(rbind, out))
})

# Concatenate all list object into one tibble
risk_res <- do.call(rbind, risk_res)

# Replace risk values with NA to 0 
# Can happen, if denominator (number of synthetic group disclosure) is 0 (division by 0)
risk_res <- risk_res |> 
  tidyr::replace_na(list(tab_risk = 0))

saveRDS(risk_res, file = file.path(output_path, "tabular_risk.rds"))
