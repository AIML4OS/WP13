#################################################################
##                    Load needed R-Scripts                    ##
#################################################################

# Notes -------------------------------------------------------------------

# Bug not | synthpop version 1.9-2
# Note regarding to CART parameter for synthesis without stratification:
# We used synthpop::syn() in the synthesis process for these variants, but 
# there is an internal bug that does not allow these cart arguments to be passed 
# to the synthpop::syn() function. If you want to change the rpart CART arguments,
# you have to do that in the files: 
# hilbert_synthesis.R (~ Line 169), syn_no_strata_num.R (~ Line 46)

# Libraries ---------------------------------------------------------------

required_pkgs <- c(
  "dplyr", "tibble", "foreach", "Boruta", "forcats", "parallel", "doParallel",
  "synthpop", "rpart", "future", "future.apply", "purrr", "stringr", "rpart",
  "DescTools", "arules", "data.table", "tidyr", "reshape2", "ggplot2"
)

available_pkgs <- vapply(required_pkgs, function(pkg) pkg %in% .packages(all = TRUE), FUN.VALUE = logical(1))

not_installed <- names(which(available_pkgs == FALSE))
if(length(not_installed) > 0) install.packages(not_installed)

rm(required_pkgs, available_pkgs, not_installed)

require(dplyr)
require(foreach)
require(future)

# R-scripts ---------------------------------------------------------------

# Boruta function (feature selection)
source("~/WP13/DE/functions/boruta.R")

# Binning to deal with high cardinality 
source("~/WP13/DE/functions/bin4syn.R")

# Function to calculate global utility metrics, such as pMSE (ratio) & SPECKS (Kolmogorov-Smirnov)
source("~/WP13/DE/functions/gu_micro.R")

#--- Internal functions for global utility:
## Calculates propensity scores (parallelized via future)
source("~/WP13/DE/functions/propensity_scores.R")

## Calculates pMSE & SPECKS (Kolmogorov-Smirnov test to compare original and synthetic propensity score distribution)
source("~/WP13/DE/functions/pmse.R")
source("~/WP13/DE/functions/specks.R")

## Calculates null expectation for pMSE ratio with "permutation" or "pairwise" resampling method
source("~/WP13/DE/functions/null_pmse_permutation.R")
source("~/WP13/DE/functions/null_pmse_pairwise.R")
#---

# Calculates Cramer's V matrix
source("~/WP13/DE/functions/calc_cramerV.R")

# Auxiliary functions for functions using key and target variables
# Here: Normalized Hellinger distance, TCAP ratio, Tabular risk 
## Includes two main auxiliary functions:
## - prepare_data_4_tabular_utility(): prepares data for tabular utility calculations (binning) 
## - count_table(): builds a count table
source("~/WP13/DE/functions/key_target_auxiliary.R")

#--- Tabular utility

## Calculates normalized Hellinger distance metric
source("~/WP13/DE/functions/hellinger_dt.R")

## Determines all possible n-way table combinations 
source("~/WP13/DE/functions/n_way_tables.R")

# Risk

## Calculates WEAP (for each row/obs.)
source("~/WP13/DE/functions/weap.R")

## Calculates TCAP (for each row/obs.)
source("~/WP13/DE/functions/tcap.R")

## Calculates TCAP ratio (for each unique combination)
source("~/WP13/DE/functions/tcap_ratio.R")

#--- Tabular risk

# Contains three functions: 
# # build_official_table(): a function that builds an example for an official table (to showcase the code)
# find_disclosive_cells(): find all disclosive cell of a table (group disclosure, GD)
# calc_tabular_risk(): calculate the tabular risk (number of common GDs/number of synthetic GDs)
source("~/WP13/DE/functions/tabular_risk_auxiliaries.R")
