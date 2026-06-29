########################################################################
##  Auxiliary functions for functions using key and target variables  ##
##      Normalized Hellinger distance, TCAP ratio, Tabular risk       ##
########################################################################

# Bin ods & sds with same bin length --------------------------------------

#--- Auxiliary used in prepare_data()
# Arguments
# ods: data.frame of original dataset
# sds: list of synthetic dataset(s)
# bin_variables: numerical variables to be binned
# breaks: number of levels for the binned variable(s)

same_disc <- function(ods, sds, bin_variables, breaks = 10) {
  
  # Extract bin variables once
  ods_bin_src <- ods[, bin_variables, drop = FALSE]
  sds_bin_src <- lapply(sds, `[`, , bin_variables, drop = FALSE)
  
  # Join for shared discretization
  joined <- do.call(rbind, c(list(ods_bin_src), sds_bin_src))
  
  # Compute cuts
  cuts <- lapply(joined,
                 arules::discretize,
                 breaks = breaks,
                 onlycuts = TRUE)
  
  # Convert cuts to matrix-like structure for indexing consistency
  cuts_mat <- do.call(cbind, cuts)
  
  # Discretize original data
  ods[, bin_variables] <- as.data.frame(lapply(seq_along(bin_variables), function(j) {
    arules::discretize(ods_bin_src[[j]], method = "fixed", breaks = cuts_mat[, j])
  }), stringsAsFactors = FALSE)
  
  # Discretize synthetic data
  sds <- lapply(sds, function(sd) {
    sd_bin <- sd[, bin_variables, drop = FALSE]
    sd[, bin_variables] <- as.data.frame(lapply(seq_along(bin_variables), function(j) {
      arules::discretize(sd_bin[[j]], method = "fixed", breaks = cuts_mat[, j])
    }), stringsAsFactors = FALSE)
    sd
  })
  
  # Output
  list(ods = ods, sds = sds)
}

#--- Main function to bin each variable (bin_variables) in a dataset 
#--- Arguments
# ods: data.frame of original dataset
# sds: list of synthetic dataset(s)
# key_variables: non-synthesized (non-sensitive) variables for tabulation (assumes to be known)
# target_variables: synthesized (sensitive) variables for tabulation
# bin_variables: numerical variables to be binned
# breaks: number of levels for the binned variable(s)

prepare_data <- function(
    ods,
    sds,
    key_variables,
    target_variables,
    bin_variables,
    breaks = 10L
    ) {

  # Build relevant subsets based on needed variables to calculate TCAP
  vars_used <- c(key_variables, target_variables)
  ods_sub <- ods[, vars_used, drop = FALSE]
  sds_sub <- lapply(sds, `[`, , vars_used, drop = FALSE)
  
  out <- vector(mode = "list", length = 2L)
  names(out) <- c("ods_bin", "sds_bin")
  
  # Discretize variables in bin_variables
  if(length(intersect(bin_variables, c(key_variables, target_variables))) > 0) {
    
    # Shared discretization
    tmp_list <- same_disc(
      ods = ods_sub,
      sds = sds_sub,
      bin_variables = intersect(bin_variables, c(key_variables, target_variables)),
      breaks = breaks
    )
    
    out$ods_bin <- tmp_list$ods
    out$sds_bin <- tmp_list$sds
    
  } else {
    out$ods_bin <- ods_sub
    out$sds_bin <- sds_sub
  }
  
  # Output
  out
}


# Function to build count table -------------------------------------------

#--- Arguments
# data: prepared data (numerical variables already binned)
# vars: all variables needed for tabulation
# df_output: Should output be a data.frame? If FALSE, then a data.table will be returned

count_table <- function(data, vars, df_output = FALSE) {
  dt <- data.table::as.data.table(data)
  res <- dt[, .N, by = vars]
  if(df_output) res <- as.data.frame(res)
  res
}
