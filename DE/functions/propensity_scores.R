#################################################################
##                      Propensity Scores                      ##
#################################################################

#' Calculating propensity scores
#' @description The aim is to find out whether \code{\link[rpart]{rpart}} can distinguish whether an observation is original or synthetic.
#' The predicted probabilities are called propensity scores.
#' @param ods  A `data.frame` containing the original dataset
#' @param sds A `data.frame`or `list` containing the synthetic dataset(s)
#' @param future_plan An `FutureStrategy` object (see \code{\link[future]{future}) to set the parallelization framework.
#' @param workers An `integer` specifying the number of cores to use for parallelization.
#' @param ignore_na If `TRUE`, rows with missing values are omitted. The default is `FALSE`.
#' @param rpart_control Various parameters that control aspects of the rpart fit, see  \code{\link[rpart]{rpart.control}}.
#' @param ... (optional) Additional arguments passed to \code{future_lapply()}. For details, see \code{\link[future]{future::future_lapply}}.
#' @returns A `list` for each synthetic dataset containing a `data.frame` with the estimated propensity scores and an indicator
#' showing whether the observation comes from the synthetic or original dataset.

propensity_score <- function(ods,
                             sds,
                             future_plan = NULL,
                             workers = future::availableCores(),
                             ignore_na = FALSE,
                             rpart_control = rpart::rpart.control(),
                             ...
                             ) {
  # If there are missing values, an error is displayed as some classifiers
  # cannot handle them (e.g. Logit, RF, SVM)
  if (ignore_na == FALSE && sum(is.na(ods)) != 0) {
    stop(
      ">> There are missing values in the original dataset that cannot be handled by some classifier.
      Fix these before you continue with the calculation, as some classifiers cannot handle missing values.
      Otherwise, set ignore_na = TRUE to omit rows with NAs or define NA as an additional category if the NAs are in a categorical variable."
    )
  }
  
  # If sds only contains one dataset that is not in a list (e.g. data.frame), convert sds into a list object
  if (!inherits(sds, "list"))
    sds <- list(sds)
  
  # Store original plan
  init_plan <- future::plan()
  
  # Number of cores to be used
  workers <- ifelse(length(sds) < workers, length(sds), workers)
  
  # If user provided a plan, set it temporarily
  if (!is.null(future_plan)) {
    # If a character like "multicore" or "multisession":
    if (is.character(future_plan)) {
      future::plan(future_plan, workers = workers)
    } else {
      # If they passed a plan object directly:
      future::plan(future_plan)
    }
    on.exit(future::plan(init_plan), add = TRUE)
  }
  
  # Calculate propensity scores via cart/rpart
  propensityScores <- future.apply::future_lapply(
    seq(1, length(sds)),
    function(m) {
      
      # Merge original and synthetic dataset and create an indicator variable 'synth'
      stackedData <- rbind(cbind(ods, synth = 0), cbind(sds[[m]], synth = 1))
      stackedData$synth <- as.factor(stackedData$synth)
      
      # If there are missing values and igonore_na = TRUE, only complete cases will be
      # used and rows with missing values will be dropped
      if (sum(apply(stackedData, MARGIN = 1, function(x) {
        sum(is.na(x))
      }) > 0) > 0 && ignore_na == TRUE) {
        naIndex <- which(apply(stackedData, MARGIN = 1, function(x) {
          sum(is.na(x))
        }) > 0)
        stackedData <- stackedData[-naIndex, ]
      }
      
      # Fit propensity model
      fit <- rpart::rpart(synth ~ ., data = stackedData, control = rpart_control)
      ps <- unname(predict(fit, newdata = stackedData[, -ncol(stackedData)], type = "prob")[, 2])
      
      # Return object
      return(data.frame(ps = ps, synth = stackedData$synth))
    },
    ...
  )

  # Output
  return(propensityScores)
}