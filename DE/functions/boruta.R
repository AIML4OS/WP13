##################################################################
##                      Feature Selection                       ##
##                    Via 'Boruta' algorithm                    ##
##################################################################

# Feature Selection via Boruta algorithm ----------------------------------

#' Boruta: A feature selection algorithm based on random forests.
#' @description This function is a wrapper build around \code{\link[Boruta]{Boruta}} that use the feature selection algorithm Boruta.
#' @param data A `data.frame` containing the original dataset.
#' @param targetVariable A single `string` containing the target variable for Boruta.
#' @param seed A single `integer` value for reproducibility.
#' @param n.cores An `integer` specifying the number of cores to use for parallelization.
#' @param maxRuns An `integer` specifying the maximal number of importance source runs, see  \code{\link[Boruta]{Boruta}}.
#' @param ... Additional arguments passed to `Boruta::Boruta(...)`. For more details, see \code{\link[Boruta]{Boruta}}.
#'
#' @returns A `list` of 2 elements.

boruta <- function(data,
                   targetVariable,
                   seed,
                   n.cores,
                   maxRuns,
                   model = FALSE,
                   ...) {
  #--- Split data into target and predictors
  
  # Feature matrix X
  X <- data |>
    dplyr::select(-dplyr::any_of(targetVariable))
  
  # Variable with missing values
  if (sum(apply(X, 2, function(x) {
    sum(is.na(x))
  })) > 0) {
    vars_na <- names(which(apply(X, 2, function(x) {
      sum(is.na(x))
    }) > 0))
    X <- data  |>
      dplyr::select(-dplyr::any_of(vars_na))
    message(
      paste0(
        ">> Feature variables <",
        paste0(vars_na, collapse = ", "),
        "> have missing variables and will not be considered in Boruta."
      )
    )
  }
  
  # Target vector y
  y <- data  |>
    dplyr::pull(targetVariable)
  
  #--- Boruta
  set.seed(seed)
  start <- Sys.time()
  boruta_model <- Boruta::Boruta(
    x = X,
    y = y,
    doTrace = 2,
    maxRuns = maxRuns,
    num.threads = n.cores,
    ...
  )
  end <- Sys.time()
  
  # Print info (progress)
  cat(">> Finished <",
      targetVariable,
      ">\n",
      "\tTime elapsed:",
      format(end - start))
  
  # Output
  boruta_model
}
