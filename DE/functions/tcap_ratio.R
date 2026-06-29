##################################################################
##                          TCAP ratio                          ##
##################################################################


# Auxiliary ---------------------------------------------------------------
# Calculates TCAP ratio for one synthetic dataset
tcap_ratio_m <- function(ods, 
                         sds, 
                         key_variables, 
                         target_variables, 
                         exclusion_rate = 0.9,
                         exception_variables = NULL, 
                         exception_values = NULL,
                         weap_threshold = 1){

  # Calculate TCAP for each WEAP unique combinations with all keys and each target separately
  tcapUnique <- lapply(seq_along(target_variables), function(x){
    
    # Calculate WEAP 
    df_weap <- weap(sds = sds, 
                    key_variables = key_variables, 
                    target_variables = target_variables[x],
                    exclusion_rate = exclusion_rate,
                    exception_variables = exception_variables,
                    exception_values = exception_values)
    
    # Message if the chosen WEAP threshold < maximum of the calculated WEAP
    if(max(df_weap$WEAP_sj) < weap_threshold){
      message(paste0(">>\tThe maximum WEAP with key variables '", paste0(key_variables, collapse = ", "), "' and target variable '", paste0(target_variables, collapse = ", "), "' is ", round(max(df_weap$WEAP_sj), digits = 4),
                     ".\n\tThe WEAP threshold is set to the mentioned maximum WEAP value."))
      weap_threshold <- max(df_weap$WEAP_sj)
    }
    
    # Calculate TCAP only for combis which WEAP > chosen WEAP threshold
    df_weap |>
      dplyr::filter(WEAP_sj >= weap_threshold) |>
      tcap_j(ods = ods, 
             sds = _, 
             key_variables = key_variables, 
             target_variables = target_variables[x],
             exclusion_rate = exclusion_rate,
             exception_variables = exception_variables,
             exception_values = exception_values)
  })
  
  # Calculate TCAP ratio in two steps
  
  # Step 1: Calculate for each list object which contains unique combinations for all keys with each target separately filtered by WEAP == weap_threshold (== 1)
  tcapRatio <- foreach(x = seq_along(tcapUnique), .combine = "rbind") %do% {
    
    # Since tcapUnique only contains unique combinations with WEAP == weap_threshold (== 1)
    denominator <- nrow(tcapUnique[[x]]) 
    
    # Number of unique combinations which has TCAP == 1 --> True group disclosure
    numerator <- tcapUnique[[x]] |>
      filter(TCAP_oj == 1) |>
      nrow()
    data.frame(numerator, denominator)
  }
  
  # Step 2: Stack the results in order to calculate the final TCAP ratio
  tcapRatio <- colSums(tcapRatio)
  
  # Final TCAP ratio
  unname(tcapRatio[1]/tcapRatio[2])
}


# Function ----------------------------------------------------------------

#--- Arguments
# ods: data.frame of original dataset
# sds: list of synthetic dataset(s)
# key_variables: non-synthesized (non-sensitive) variables for tabulation (assumes to be known)
# target_variables: synthesized (sensitive) variables for tabulation
# exclusion_rate: at which rate shouldn't a value disclosure counts as a harmful disclose? 
#   e.g. If fatal injuries are 95% equal to 0, then in this case, a disclose of 0 wouldn't 
#   be counted as disclosure for the TCAP calculation. Currently, only one numerical value for all 
#   exclusion_variables will be accepted.
# exclusion_variables: all variables where a disclosure of a majority class (> exclusion_rate) shouldn't
#   be counted as disclose.
# exclusion_values: majority class value that shouldn't be counted as disclose
# weap_threshold: filter only unique combinations in the synthetic dataset with WEAP >= weap_threshold to 
#   calculate the TCAP. 

# TCAP ratio for list of synthetic datasets
tcap_ratio <- function(ods, 
                       sds, 
                       key_variables, 
                       target_variables , 
                       exclusion_rate = 0.9,
                       exception_variables = NULL, 
                       exception_values = NULL,
                       weap_threshold = 1){
  
  # Caluclate TCAP ratio for each m synthetic dataset
  res <- vapply(sds, function(sd) {
    tcap_ratio_m(
      ods = ods, 
      sds = sd, 
      key_variables = key_variables, 
      target_variables = target_variables , 
      exclusion_rate = exclusion_rate,
      exception_variables = exception_variables, 
      exception_values = exception_values,
      weap_threshold = weap_threshold
    )
  }, FUN.VALUE = numeric(1))

  # Output
  out <- list(
    tcap_ratio_m = res,
    tcap_ratio = mean(res)
  )
  
}
