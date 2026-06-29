#################################################################
##            Calculation of global utility metrics            ##
##                  pMSE, pMSE ratio, SPECKS                   ##
#################################################################

# Main function -----------------------------------------------------------

#--- Arguments
# ods: data.frame of original dataset
# sds: list of synthetic dataset(s)
# future_plan: based on future package, strategy of future::plan()
# workers: number of CPU cores for parallelization via future
# ignore_na: For calculation of propensity scores: if `TRUE`, rows with missing values are omitted. The default is `FALSE`.
# rpart_control: Arguments for rpart as proepnsity model; input format via rpart::rpart.control(cp = 1e-5, minbucket = 10) (for example)
# resampling: Resampling technique used to estimate the null expectation in order to calculate pMSE ratio (see synthpop::utility.gen, resamp.method argument);
# --> Here you can choose between permutation and pairwise
# nperms: Only necessary, if you use resampling = "permutation". Number of permutations.
# `...`: further arguments passed to future.apply::future_lapply(...)

gu_micro <- function(ods,
                     sds,
                     future_plan = NULL,
                     workers = future::availableCores(),
                     ignore_na = FALSE,
                     rpart_control = rpart::rpart.control(),
                     resampling = NULL,
                     nperms = 50,
                     seed = sample(1:100000, 1),
                     ...) {
  # Check resampling method
  if (!(resampling %in% c("permutation", "pairwise") || is.null(resampling)))
    stop(
      ">> The input of argument 'resampling' is invalid. To calculate a pmse ratio, please use 'permutation' or 'pairwise' as resampling method. Otherwise, assign NULL to resampling in order to skip the pmse ratio caluclation."
    )
  
  # Statement is TRUE if m = 1 (only one synth. ds)
  if (!inherits(sds, "list"))
    sds <- list(sds)
  
  # Estimate propensity scores
  ps <- propensity_score(
    ods = ods,
    sds = sds,
    future_plan = future_plan,
    workers = workers,
    ignore_na = ignore_na,
    rpart_control = rpart_control,
    ...
  )
  
  # Progress information
  message(
    paste(
      ">> Calculation of the propensity scores with m =",
      length(sds),
      "synthetic dataset(s) is done.\n"
    )
  )
  
  # Calculate utility measures based on propensity scores
  pmseValue <- pmse(ods = ods, sds = sds, propensity_scores = ps)
  specksValue <- specks(propensity_scores = ps)
  
  # Which resampling method to use for estimating the null pMSE with a CART propensity model
  # -> Snoke et al. (2018, Table 3)
  # Fully synthesized data: Pairwise (additional computation) or Permutation
  # Partial synthesized data: Pairwise
  
  # Calculate pmse ratio - resampling methods to determine the null expectation of the pmse
  if (resampling %in% c("permutation", "pairwise")) {
    if (resampling == "permutation") {
      message(">> Calculate pmse ratio with permutation.")
      pmseRatio <- null_pmse_permutation(
        ods = ods,
        sds = sds,
        nperms = 50,
        pmse_obs = pmseValue,
        future_plan = future_plan,
        workers = workers,
        seed = seed,
        rpart_control = rpart_control,
        ...
      )
    } else {
      message(">> Calculate pmse ratio with pairwise resampling")
      pmseRatio <- null_pmse_pairwise(
        sds = sds,
        pmse_obs = pmseValue,
        future_plan = future_plan,
        workers = workers,
        rpart_control = rpart_control,
        ...
      )
    }
    
    # Output
    return(rbind(
      pMSE = pmseValue,
      SPECKS = specksValue,
      pMSE_ratio = pmseRatio$S_pMSE
    ))
  } else {
    message(">> Calculation of pmse ratio is skipped since resampling = NULL.")
    return(rbind(pMSE = pmseValue, SPECKS = specksValue))
  }
}