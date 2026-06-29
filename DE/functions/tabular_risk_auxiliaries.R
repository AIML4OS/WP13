#################################################################
##             Auxiliary function for tabular risk             ##
#################################################################

require(dplyr)
require(reshape2)

# Auxiliary functions -----------------------------------------------------

# Example to showcase the code: 
# Function to prepare the data for tabulation

#--- Arguments
# data: data.frame of dataset to be transformed into "official" table
# key: non-synthesized (non-sensitive) variables for tabulation (assumes to be known)
# target: synthesized (sensitive) variables for tabulation

build_official_table <- function(data, key, target) {
  
  # Convert to factor
  data$injuries_fatal <- as.factor(data$injuries_fatal)
  
  # Bin crash hour variable
  data |>
    dplyr::mutate(crash_hour =
                    factor(
                      dplyr::case_when(
                        crash_hour >= 6  & crash_hour <= 10 ~ "morning",
                        crash_hour >= 11 & crash_hour <= 15 ~ "noon",
                        crash_hour >= 16 & crash_hour <= 20 ~ "evening",
                        crash_hour >= 21 | crash_hour <= 5  ~ "night",
                        TRUE ~ NA_character_
                      )
                    )) |>
    dplyr::select(dplyr::any_of(c(key, target))) |> 
    tibble::as_tibble()
}

# Functions ---------------------------------------------------------------

#--- Arguments
# data: data.frame of original dataset
# vars: string of names of all variable used for tabulation
# sensitive_vars: synthesized (sensitive) variables for tabulation
# gd_exclusion_rate: exclude GD cases, when the disclosed value appears with a frequency >= "gd_exclusion_rate"


# Find disclosive cells: Unique variable constellations (table with cell = 1)
find_disclosive_cells <- function(
    data, 
    vars = NULL,
    sensitive_vars = NULL,
    gd_exclusion_rate = 0.9 
    ) {
  
  if(is.null(sensitive_vars)) sensitive_vars <- vars

  out <- lapply(sensitive_vars, function(tar) {

    # Exclude GD cases, when the disclosed value appears with a frequency >= "gd_exclusion_rate"
    gd_excl_case <- NULL
    if(sum(prop.table(table(data[[tar]])) >= gd_exclusion_rate) > 0) gd_excl_case <- names(which(prop.table(table(data[[tar]])) >= gd_exclusion_rate))
    
    # Check whether a level in tar is a group disclosure case based on unique 
    # combinations of the remaining variables
    dcast_formula <- as.formula(paste(paste0(setdiff(vars, tar), collapse = "+"), tar, sep = "~"))
    
    suppressMessages(
      data |> 
        select(all_of(vars)) |> 
        reshape2::dcast(dcast_formula, fun.aggregate = length) |>
        dplyr::filter(rowSums(across(
          -dplyr::any_of(setdiff(vars, tar)), ~ dplyr::coalesce(as.numeric(.), 0) != 0
        )) == 1) |> 
        dplyr::mutate(gd_cases = rowSums(dplyr::across(-dplyr::any_of(setdiff(vars, tar))))) |> 
        dplyr::filter(dplyr::if_all(dplyr::all_of(gd_excl_case), ~ . != .data$gd_cases))
    )
  })
  
  # Names should be the variable name that was use as target
  names(out) <- sensitive_vars
  
  # Output
  out
}

# Calculate tabular risk
#--- Arguments
# sensitive_vars: synthesized (sensitive) variables for tabulation
# ls_sy: output of find_disclosive_cells() for synthetic data
# ls_or: output of find_disclosive_cells() for original data

calc_tabular_risk <- function(sensitive_vars, ls_sy, ls_or) {
  
  gd_cases <- Reduce("+", lapply(sensitive_vars, function(sv) {
    
    # Variables needed for joining the original data
    join_vars <- setdiff(names(ls_sy[[sv]]), "gd_cases")
    
    # Calculate tabular risk
    tmp_res <- ls_sy[[sv]] |>  
      dplyr::left_join(ls_or[[sv]], by = join_vars, suffix = c("_sy", "_or")) |>
      tidyr::replace_na(list(gd_cases_or = 0L)) |> 
      dplyr::summarise(gd_cases_sy = sum(gd_cases_sy),
                       gd_cases_or = sum(gd_cases_or))
  }))
  
  # Tabular risk metric
  tibble(tab_risk = ifelse(is.na(gd_cases[,2]/gd_cases[,1]), 0, gd_cases[,2]/gd_cases[,1]))
  
}
