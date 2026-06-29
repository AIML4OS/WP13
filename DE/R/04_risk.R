#################################################################
##            Risk metrics for microdata and tables            ##
#################################################################

# Preparations ------------------------------------------------------------

#--- Setup for Workbench Job
source("~/WP13/DE/R/source.R")
data <- readRDS("~/WP13/DE/data/traffic_accidents_subset_10pct.rds")

# Future plan: Max. memory for each worker
options(future.globals.maxSize = Inf)

# Paths
input_path <- "~/WP13/DE/output/sds"
output_path <- "~/WP13/DE/output/results/risk"

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

# Used key and target variables (for tabulation) 
key_variables <- c("crash_type","most_severe_injury", "injuries_no_indication") # known/unsynth. variables
target_variables <- c("trafficway_type", "damage", "injuries_fatal") # synthesized variables
bin_variables <- c("injuries_no_indication") # numeric variable(s) to be binned

# Used key and target for official table (as example to calculate tabular risk)
# Example here: Table in each possible dimensions should be publish with variables:
# most_severe_injury, crash_hour, injuries_fatal, damage
key_official_table <- c("most_severe_injury", "crash_hour") # known/unsynth. variables
target_official_table <- c("damage", "injuries_fatal") # synthesized variables

# Future plan settings 
init_plan <- future::plan()
on.exit(future::plan(init_plan), add = TRUE)
future::plan(multicore, workers = 21L)

# Source R scripts --------------------------------------------------------

# Microdata risk: TCAP ratio
source("~/WP13/DE/R/04_risk/micro_disclosure_risk.R")

# Tabular risk: number of common group disclosure cases / number of synthetic gd cases
source("~/WP13/DE/R/04_risk/tabular_disclosure_risk.R")
