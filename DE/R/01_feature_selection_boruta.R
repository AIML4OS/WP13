##################################################################
##                              Job                             ##
##             Execute feature selection via Boruta             ##
##################################################################


# Setup -------------------------------------------------------------------
# File paths
input_path <- "~/WP13/DE/data"
output_path <- "~/WP13/DE/output/tmp"

source("~/WP13/DE/R/source.R")
data <- readRDS(file.path(input_path, "traffic_accidents_subset_10pct.rds"))



# Parameters --------------------------------------------------------------

# Variables to be synthesized
targets <- c("injuries_fatal", "damage", "trafficway_type")
seed <- 42
n.cores <- 80

# Boruta
maxRuns <- 100

# (1) Apply Boruta on each target variable --------------------------------
res_boruta <- lapply(targets, function(tar) {
  boruta(
    data = data,
    targetVariable = tar,
    seed = seed,
    n.cores = n.cores,
    maxRuns = maxRuns
  )
})
names(res_boruta) <- targets

# (2) Write list over the selected predictors -----------------------------

# Function: Selected features with 'normHits' value
# Definition 'normHits': How often was this variable declared as important? (range: 0 - 1)
printAttributeStatistics <- function(boruta_model){
  Boruta::attStats(boruta_model) |> 
    dplyr::filter(decision == "Confirmed") |> 
    dplyr::arrange(dplyr::desc(normHits), dplyr::desc(medianImp)) |> 
    dplyr::select(normHits, medianImp) |> 
    tibble::rownames_to_column(var = "variable_names") |> 
    tibble::as_tibble()
}

# Read output of the feature selection methods
for(tar in targets){
  
  # Fixed tentative variables with built-in function from Boruta package
  assign(paste0("fixed_", tar), Boruta::TentativeRoughFix(res_boruta[[tar]]))
  
  # Select important attributes with fixed tentative variables
  assign(paste0("selected_", tar), printAttributeStatistics(eval(parse(text = paste0("fixed_", tar)))))
}

# Output: Create selected feature list ------------------------------------

# Create output list
fs_boruta <- vector(mode = "list", length = length(targets))
names(fs_boruta) <- targets

# Assign results to the corresponding list element
for(tar in targets) fs_boruta[[tar]] <- eval(parse(text = paste0("selected_", tar)))

# Save Boruta results
saveRDS(fs_boruta, file = file.path(output_path, "results_boruta.rds"))
