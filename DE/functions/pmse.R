##################################################################
##          Propensity Score Mean Squared Error (pMSE)          ##
##################################################################

# Source:
# Raab et al. (2021): Assessing, visualizing and improving the utility of synthetic data, Table 4

#--- Arguments:
# ods: data.frame of original dataset
# sds: list of synthetic dataset(s)
# propensity_scores: Calculated propensity scores; Output of propensity_scores() function

pmse <- function(ods, sds, propensity_scores) {
  nOds <- nrow(ods)
  out <- vapply(seq(1, length(sds)), function(x) {
    nSds <- nrow(sds[[x]])
    c <- nSds / (nOds + nSds)
    sum((propensity_scores[[x]][, 1] - c)^2, na.rm = T) / (nOds + nSds)
  }, FUN.VALUE = double(1))
  return(out)
}
