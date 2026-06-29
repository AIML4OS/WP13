#################################################################
##     Calculating null pMSE with CART as propensity model     ##
##                   Resampling: permutation                   ##
#################################################################

# Reference: 
# - GitHub Repository: https://github.com/cran/synthpop/blob/master/R/utility.syn.r
# - Paper: https://doi.org/10.1111/rssa.12358

#--- Arguments
# ods: data.frame of original dataset
# sds: list of synthetic dataset(s)
# nperms: number of permutations
# pmse_obs: observed pMSE, i.e. calculated pMSE based on original and synthetic dataset
# future_plan: based on future package, strategy of future::plan()
# workers: number of CPU cores for parallelization via future
# rpart_control: Arguments for rpart as proepnsity model; input format via rpart::rpart.control(cp = 1e-5, minbucket = 10) (for example)
# `...`: further arguments passed to future.apply::future_lapply(...)

null_pmse_permutation <- function(
    ods,
    sds,
    nperms = 50,
    pmse_obs,
    future_plan = NULL,
    workers = future::availableCores(),
    seed = sample(1:100000, 1),
    rpart_control = rpart::rpart.control(),
    ...
) {
  # Error message
  if(length(sds) == 1) stop(">> Resampling is not possible with m = 1 synthesized dataset")
  else message(">> Resampling via permutation: Simulate null pMSE via ", nperms, " permutations for m = ", length(sds), " synthetic datasets")
  
  # Store original plan
  init_plan <- future::plan()
  
  # Number of cores to be used
  workers <- ifelse(nperms < workers, nperms, workers)
  
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
  
  # Alg. 2: Step 1-3: Create random permutation
  set.seed(seed)
  random_m <- sample(seq_along(sds), nperms, replace = T)
  
  # Fit model to calculate the propensity scores
  pmsePermutation <- future.apply::future_lapply(seq(1, nperms), function(i) {
    # Merge original and synthetic dataset and create an indicator variable 'synth'
    stackedData <- rbind(cbind(ods, synth = 0), cbind(sds[[random_m[i]]], synth = 1))
    stackedData$synth <- as.factor(stackedData$synth)
    
    # Required values for the calculation
    nOds <- sum(stackedData$synth == 0)
    nSds <- sum(stackedData$synth == 1)
    c <- nSds / (nOds + nSds)
    
    # Randomly shuffle the group indicator 'synth'
    dataPermutation <- stackedData
    dataPermutation$synth <- sample(dataPermutation$synth)
    
    # Fit propensity model (CART via rpart package)
    fit <- rpart::rpart(synth ~ ., data = dataPermutation, control = rpart_control)
    psPerm <- predict(fit, newdata = stackedData[, -ncol(stackedData)], type = "prob")[, 2]
    
    # Calculate pMSE
    sum((psPerm - c)^2, na.rm = T) / (nOds + nSds) / 2
  }, ...)
  pmsePermutation <- unlist(pmsePermutation)
  
  # Calculate S_pMSE for each m synth dataset
  pmsePermMean <- mean(pmsePermutation)
  out <- pmse_obs / pmsePermMean
  
  return(list(null_pmse = pmsePermMean, S_pMSE = out))
}