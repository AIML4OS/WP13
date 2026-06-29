#################################################################
##   Within Equivalence Class Attribution Probability (WEAP)   ##
#################################################################

require(plyr)
require(dplyr)

# Calculate WEAP ----------------------------------------------------------
weap <- function(
    sds, 
    key_variables, 
    target_variables,
    exclusion_rate = 0.9,
    exception_variables = NULL, 
    exception_values = NULL) {

  # Warning message
  if(length(exception_variables) != length(exception_values)) {
    stop("'exception_variables' and 'exception_values' must have the same length.")
  }

  # Exclude levels as disclosure case, if the disclosed level appears with a frequency >= exclusion_rate
  excl_case <- NULL
  if(sum(prop.table(table(sds[[target_variables]], useNA = "ifany")) >= exclusion_rate) > 0) excl_case <- names(which(prop.table(table(sds[[target_variables]], useNA = "ifany")) >= exclusion_rate))
  
  # Frequencies of unique combinations based on key variables (denominator of weap)
  key_freq <- plyr::count(sds, key_variables)
  
  # Frequencies of unique combinations based on key AND target variables (numerator of weap)
  key_target_freq <- plyr::count(sds, c(key_variables, target_variables))
  
  # Rename the columns
  colnames(key_freq)[ncol(key_freq)] <- "k_freq"
  colnames(key_target_freq)[ncol(key_target_freq)] <- "kt_freq"

  # Exceptional cases that should not be count as disclosure,
  # e.g. one variable has more then 95% missing values, then it could be 
  # exclude as disclosure case
  if (!is.null(exception_variables)) {
    for (i in seq_along(exception_variables)) {
      if (exception_variables[i] %in% names(key_freq)) {
        key_freq <- key_freq |> 
          dplyr::filter(!(.data[[exception_variables[i]]] %in% exception_values[i]))
      }
      if (exception_variables[i] %in% names(key_target_freq)) {
        key_target_freq <- key_target_freq |> 
          dplyr::filter(!(.data[[exception_variables[i]]] %in% exception_values[i]))
      }
    }
  }

  # Calculate WEAP for each unique combination of key and target variables from the synthetic dataset
  key_target_freq |> 
    dplyr::left_join(key_freq, by = key_variables) |> 
    dplyr::filter(!(.data[[target_variables]] %in% excl_case)) |> # exclude excl_case levels since these won't be count as disclosure
    dplyr::mutate(WEAP_sj = kt_freq/k_freq) |> 
    dplyr::select(-c(k_freq, kt_freq))
}
