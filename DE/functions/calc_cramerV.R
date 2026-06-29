#################################################################
##                      Cramer's V Matrix                      ##
#################################################################


# Auxiliary ---------------------------------------------------------------

# Converts numeric variables into categorical one with NA as separate level
factorWithNa <- function(x){addNA(as.factor(x), ifany = T)}


# Function ----------------------------------------------------------------

#--- Arguments
# data: one original or snythetic dataset as data.frame or tibble
# future_plan: based on future package, strategy of future::plan()
# workers: number of CPU cores for parallelization via future
# `...`: further arguments passed to future.apply::future_lapply(...)


# Calculate Cramer's V for each pair of variables
calc_cramerV <- function(data, future_plan, workers, ...) {
  
  # Factor conversion
  cv_data <- data  |> 
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), factorWithNa))
  
  # Extract columns once (avoid repeated slicing)
  cols <- as.list(cv_data)
  rm(cv_data); gc()
  p <- length(cols)
  
  # Parallel plan
  init_plan <- future::plan()
  workers <- ifelse(p < workers, p, workers)
  
  if (!is.null(future_plan)) {
    if (is.character(future_plan)) {
      future::plan(future_plan, workers = workers)
    } else {
      future::plan(future_plan)
    }
    on.exit(future::plan(init_plan), add = TRUE)
  }
  
  # Computation
  res <- future.apply::future_lapply(
    seq_len(p),
    function(i) {
      xi <- cols[[i]]
      vapply(i:p,
             function(j) DescTools::CramerV(xi, cols[[j]]),
             FUN.VALUE = numeric(1)
      )
    },
    ...
  )
  
  # 5. Assemble symmetric matrix
  mat <- matrix(NA_real_, p, p)
  for (i in seq_len(p)) {mat[i, i:p] <- res[[i]]}
  mat[lower.tri(mat)] <- t(mat)[lower.tri(mat)]
  colnames(mat) <- rownames(mat) <- names(cols)
  
  # 6. Replace NaN values to 0 --> Occurs if variance for this pair of variable is 0
  mat[is.na(mat)] <- 0
  mat
}

  