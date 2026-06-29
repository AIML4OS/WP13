#################################################################
##       Targeted Correct Attribution Probability (TCAP)       ##
#################################################################

require(plyr)
require(dplyr)
require(tidyr)

# TCAP for each obs./row --------------------------------------------------
tcap_j <- function(ods, 
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
  if(sum(prop.table(table(ods[[target_variables]], useNA = "ifany")) >= exclusion_rate) > 0) excl_case <- names(which(prop.table(table(ods[[target_variables]], useNA = "ifany")) >= exclusion_rate))

  ### Denominator
  
  # Unique combinations of all key_variable in the synthetic dataset
  k_freq <- sds |>
    dplyr::select(all_of(key_variables)) |>
    dplyr::distinct() |> 
    dplyr::arrange_all()
  
  # Frequencies of all unique combination of all key_variables in the original dataset
  k_freq_orig <- plyr::count(ods, c(key_variables))
  
  # Join original unique counts to the unique combis in the sds
  denominator <- k_freq |>
    dplyr::left_join(k_freq_orig, by = key_variables) |>
    tidyr::replace_na(list(freq = 0)) |> # if we have some unique comb. which doen't exist in ods
    dplyr::rename(denominator = freq)
  
  ### Numerator
  
  # Unique combinations of all key_variables AND target_variables in the synthetic dataset
  kt_freq <- sds |>
    dplyr::select(all_of(c(key_variables, target_variables))) |>
    dplyr::distinct() |> 
    dplyr::arrange_all()
  
  # Frequencies of all unique combination of all key_variables AND target_variables in the original dataset
  kt_freq_orig <- plyr::count(ods, c(key_variables, target_variables))
  
  # Join original unique counts to the unique combis in the sds
  numerator <- kt_freq |>
    dplyr::left_join(kt_freq_orig, by = c(key_variables, target_variables)) |>
    tidyr::replace_na(list(freq = 0)) |> # if we have some unique comb. which doen't exist in ods
    dplyr::rename(numerator = freq)
  
  # Exceptional cases that should not be count as disclosure,
  # e.g. one variable has more then 95% missing values, then it could be 
  # exclude as disclosure case
  if (!is.null(exception_variables)) {
    for (i in seq_along(exception_variables)) {
      if (exception_variables[i] %in% names(denominator)) {
        denominator <- denominator |> 
          dplyr::filter(!(.data[[exception_variables[i]]] %in% exception_values[i]))
      }
      if (exception_variables[i] %in% names(numerator)) {
        numerator <- numerator |> 
          dplyr::filter(!(.data[[exception_variables[i]]] %in% exception_values[i]))
      }
    }
  }

  # Calculate TCAP_j for each unique combi
  numerator |>
    dplyr::left_join(denominator, by = key_variables) |>
    dplyr::filter(!(.data[[target_variables]] %in% excl_case)) |> # exclude excl_case levels since these won't be count as disclosure
    dplyr::mutate(TCAP_oj = numerator/denominator) |>
    dplyr::select(-c(denominator, numerator))
}
