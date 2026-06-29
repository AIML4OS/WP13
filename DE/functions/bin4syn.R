#' Bin feature variables with high cardinality
#'
#' @description This function was primarily designed to handle factors with high cardinality, since the
#' CART algorithm in \code{\link[rpart]{rpart}}, which is the most commonly used synthesizer in `synthpop`,
#' is prone to multiple factors with high cardinality. It bins each categorical variable in the `feature`
#' with a cardinality `> (n_levels + 1)`.
#'
#' @param data A `data.frame` containing the original dataset.
#' @param n_levels A single `integer` containing the most common `n` values to be preserved. A negative `n`
#' preserves the `n` least common values. This argument is pass to the function `forcats::fct_lump_n`.
#' @param features A `character` vector containing all variable names that will be used as predictors.
#' @param keep_unbinned By default, if the value is `TRUE`, the unbinned variable(s) will be kept. Otherwise, they will be replaced by the binned version.
#' @param suffix_binned A `string` to characterize the binned variables. If you keep the unbinned variables, a suffix will be added to the names of the binned ones.
#' @param ... Further arguments that will be passed to `forcats::fct_lump_n`.
#' 
#' @returns A `data.frame` containing the binned original dataset.

bin4syn <- function(data, n_levels, features, keep_unbinned = FALSE, suffix_binned = "_bin", ...) {
  
  # Warning when categorical variabels aren't of type factor
  if(any(sapply(data, inherits, "character"))) warning("There are categorical variables of type 'character' in the dataset that are ignored. If you want to include them, convert them to factors.")
  
  # number of features
  n_predictors <- length(features)
  
  # Which factors have a cardinality higher then n_levels
  over_maxlevels <- data[features] |>
    dplyr::select(dplyr::where( ~ is.factor(.) && nlevels(.) > n_levels + 1)) |>
    names()
  
  # Variables to be binned because they are in the predictor set (used to synthesize a target)
  if(sum(features %in% over_maxlevels) == 0) {
    warning(">> There are no features with a cardinality > n_levels. Therefore, the initial dataset is returned.")
    return(data)
  } else {
    message(paste0(">> Binned features: <",
                   paste0(features[features %in% over_maxlevels], collapse = ", "),
                   ">"))
  }
  binningFactors <- features[features %in% over_maxlevels]
  
  # Lump levels of factors with high cardinality together into "Other"
  tmp <- apply(data[binningFactors], MARGIN = 2, function(x){
    x |>
      # forcats::fct_na_value_to_level(data[[binningFactors[1]]]) |>
      forcats::fct_lump_n(f = _, n = n_levels, ...)
  }) |>
    unclass() |>
    as.data.frame(stringsAsFactors = TRUE)
  
  
  if(keep_unbinned == TRUE) {
    # Add binned variables as new columns
    names(tmp) <- paste0(names(tmp), suffix_binned)
    data <- dplyr::bind_cols(data, tmp)
  } else {
    # Replace original high cardinality variables
    data[,binningFactors] <- tmp
  }
  
  # Output
  data
}

