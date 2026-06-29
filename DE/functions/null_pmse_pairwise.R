#################################################################
##     Calculating null pMSE with CART as propensity model     ##
##                    Resampling: pairwise                     ##
#################################################################

# Reference: 
# - GitHub Repository: https://github.com/cran/synthpop/blob/master/R/utility.syn.r
# - Paper: https://doi.org/10.1111/rssa.12358

#--- Arguments
# sds: list of synthetic dataset(s)
# pmse_obs: observed pMSE, i.e. calculated pMSE based on original and synthetic dataset
# future_plan: based on future package, strategy of future::plan()
# workers: number of CPU cores for parallelization via future
# rpart_control: Arguments for rpart as proepnsity model; input format via rpart::rpart.control(cp = 1e-5, minbucket = 10) (for example)
# `...`: further arguments passed to future.apply::future_lapply(...)


null_pmse_pairwise <- function(
    sds, 
    pmse_obs,
    future_plan = NULL,
    workers = future::availableCores(), 
    rpart_control = rpart::rpart.control(),
    ...
) {
  
  # Error message
  if(length(sds) == 1) stop(">> Resampling is not possible with m = 1 synthesized dataset")
  
  # Each possible pair combination between m = 1, ..., n
  combi <- combn(x = seq_along(sds), m = 2, simplify = F)
  
  # Progress message/Warning by too less pairs
  if(length(combi) < 6) warning(">> The number of pairs is ", length(combi), " which is < 6. Increasing the number of synthesized dataset m is recommended")
  else message(">> Resampling pairwise: Simulate null pMSE from ", length(combi), " pair(s) of synthesis.")
  
  # Store original plan
  init_plan <- future::plan()
  
  # Number of cores to be used
  workers <- ifelse(length(combi) < workers, length(combi), workers)
  
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
  
  # Calculate propensity scores
  propensityScores <- future.apply::future_lapply(
    seq(1, length(combi)),
    function(p) {
      # Merge original and synthetic dataset and create an indicator variable 'synth'
      stackedData <- rbind(cbind(sds[[combi[[p]][1]]], synth = 0), cbind(sds[[combi[[p]][2]]], synth = 1))
      stackedData$synth <- as.factor(stackedData$synth)
      
      # Store synth indicator (Is observation i synthesized)
      synthIndexNaOmit <- stackedData$synth
      
      # Fit model
      fit <- rpart::rpart(synth ~ ., data = stackedData, control = rpart_control)
      ps <- unname(predict(fit, newdata = stackedData[, -ncol(stackedData)], type = "prob")[, 2])
      
      # Return object
      data.frame(ps = ps, synth = synthIndexNaOmit)
    }, ...
  )
  
  # Calculate pmse for any combination
  pmse_pairwise <- vapply(seq_along(propensityScores), function(x) {
    n0 <- unname(table(propensityScores[[x]]$synth)[names(table(propensityScores[[x]]$synth)) == "0"])
    n1 <- unname(table(propensityScores[[x]]$synth)[names(table(propensityScores[[x]]$synth)) == "1"])
    c <- n1 / (n0 + n1)
    sum((propensityScores[[x]][, 1] - c)^2, na.rm = T) / (n0 + n1)
  }, FUN.VALUE = double(1))
  
  # Output
  return(list(null_pmse = mean(pmse_pairwise), S_pMSE = pmse_obs * 2 / mean(pmse_pairwise)))
}