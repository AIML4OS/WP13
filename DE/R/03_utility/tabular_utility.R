#################################################################
##             Tabular utility: Hellinger distance             ##
#################################################################

# 1. Possible combinations ------------------------------------------------

# Step 1: Key- and target variables to build the tables (same as for TCAP)
key_variables <- c("crash_type","most_severe_injury", "injuries_no_indication") # known/unsynth. variables
target_variables <- c("trafficway_type", "damage", "injuries_fatal") # synthesized variables
bin_variables <- c("injuries_no_indication") # numeric variable(s) to be binned

# Step 2: All combinations of key and target variables with at least one target
combinations <- n_way_tables(key = key_variables,
                            target = target_variables,
                            sizes = 2:5)

# Step 3: Calculates tabular utility based on a normalized Hellinger distance

# Future plan settings 
init_plan <- future::plan()
on.exit(future::plan(init_plan), add = TRUE)
future::plan(multicore, workers = 52L)

# Calculate tabular utility for each synthesis setting
# Note: Warnings regarding to "only unique breaks" are ok.
tab_gu_results <- lapply(seq_along(file_name), function(fn) {

  # Load synthetic data
  sds <- readRDS(file.path(input_path, file_name[fn]))

  # Calculate normalized Hellinger distance
  tmp <- future.apply::future_lapply(
    seq_along(combinations),
    function(x) {
      
      # Prepare data for calculation (bin numerical variables)
      data_prep <- prepare_data(
        ods = data,
        sds = sds$syn,
        key_variables = combinations[[x]][!(combinations[[x]] %in% target_variables)],
        target_variables = combinations[[x]][combinations[[x]] %in% target_variables],
        bin_variables = bin_variables,
        breaks = 5L
      )
      
      # Step 4: Build count table 
      tab_orig <- count_table(data = data_prep$ods_bin, vars = combinations[[x]])
      hellinger_m <- vapply(data_prep$sds_bin, function(sb_data) {
        tab_syn <- count_table(data = sb_data, vars = combinations[[x]])
        
        # Step 5: Calculate Hellinger distance
        hellinger_dt(dt1 = tab_orig, dt2 = tab_syn, vars = combinations[[x]])
      }, FUN.VALUE = numeric(1))
      
      # Prepare output
      out <- dplyr::tibble(hellinger_dist = hellinger_m) |>
        (\(df) dplyr::bind_cols(df, dplyr::as_tibble(
          matrix(
            FALSE,
            nrow = nrow(df),
            ncol = length(target_variables),
            dimnames = list(NULL, target_variables)
          )
        )))()
      
      # Indicator that shows which target were used in this tabulation
      tmp_ind <- intersect(combinations[[x]], target_variables)
      out[tmp_ind] <- TRUE
      
      # Add table_id and n_way
      out |> 
        dplyr::mutate(
          table_id = x,
          n_way = length(combinations[[x]])
        )
    },
    future.seed = TRUE,
    future.packages = NULL
  )
  
  # Prepare output
  out <- do.call(rbind, tmp)
  
  # Number of predictors and levels as separate column
  additional_info <- tidyr::extract(
    tibble(file = suffix_name[fn]),
    col = file,
    into = c("predictors", "levels"),
    regex = "pred(\\d+)_lv(\\d+)",
    convert = TRUE
  )
  
  # Output
  additional_info |> 
    cbind(out)
})

# Save output in the needed format 
res_output <- data.table::rbindlist(tab_gu_results)
saveRDS(res_output, file = file.path(output_path, "tabular_utility.rds"))

