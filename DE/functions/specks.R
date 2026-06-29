###################################################################
##  Kolmogorov-Smirnov-Test based on propensity scores (SPECKS)  ##
###################################################################

# Source:
# Raab et al. (2021): Assessing, visualizing and improving the utility of synthetic data, Table 4

#--- Arguments:
# propensity_scores: Calculated propensity scores; Output of propensity_scores() function

specks <- function(propensity_scores) {
  vapply(seq(1, length(propensity_scores)), function(x) {
    unname(suppressWarnings(
      ks.test(propensity_scores[[x]][which(propensity_scores[[x]]$synth == 1), 1],
              propensity_scores[[x]][which(propensity_scores[[x]]$synth == 0), 1],
              alternative = "two.sided")
    )$statistic)
  }, FUN.VALUE = double(1))
}