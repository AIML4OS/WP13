##################################################################
##           Utility metrics for microdata and tables           ##
##################################################################

# Preparations ------------------------------------------------------------

#--- Setup for Workbench Job
source("~/WP13/DE/R/source.R")
data <- readRDS("~/WP13/DE/data/traffic_accidents_subset_10pct.rds")

# Future plan: Max. memory for each worker
options(future.globals.maxSize = Inf)

# Paths
input_path <- "~/WP13/DE/output/sds"
output_path <- "~/WP13/DE/output/results/utility"

# For file names
syn_grid <- expand.grid(
  n_levels = c(4, 9, 14), # How many levels should be the maximum? --> Result: n_levels + 1 (cat. "Other")
  n_predictors = c(5, 7, 10) # How many predictors should be used for each target variable?
)

file_name <- vapply(seq(1, nrow(syn_grid)), function(x){
  paste0("sds_preds", syn_grid[x, "n_predictors"], "_lv", syn_grid[x, "n_levels"] + 1, ".rds")
}, FUN.VALUE = character(1))

suffix_name <- vapply(seq(1, nrow(syn_grid)), function(x){
  paste0("pred", syn_grid[x, "n_predictors"], "_lv", syn_grid[x, "n_levels"] + 1)
}, FUN.VALUE = character(1))


# Source R scripts --------------------------------------------------------

# Microdata: Fit-for-purpose utility
source("~/WP13/DE/R/03_utility/fit_for_purpose_utility.R")

# Microdata: Global utility
source("~/WP13/DE/R/03_utility/global_utility.R")

# Microdata: Tabular utility (outcome-specific utility)
source("~/WP13/DE/R/03_utility/tabular_utility.R")
