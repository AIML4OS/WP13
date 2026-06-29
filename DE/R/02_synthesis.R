##################################################################
##            Micro data synthesization via synthpop            ##
##################################################################

# Libraries & Data --------------------------------------------------------

#--- Setup for Workbench Job
source("~/WP13/DE/R/source.R")
data <- readRDS("~/WP13/DE/data/traffic_accidents_subset_10pct.rds")
fs_boruta <- readRDS("~/WP13/DE/output/tmp/results_boruta.rds")

# Define path to store the output
output_path <- "~/WP13/DE/output/sds"

# Parameters --------------------------------------------------------------

# Variables to be synthesized
targets <- c("injuries_fatal", "damage", "trafficway_type")

# Number of synthetic datasets to be generated
m <- 5

# Seed
seed <- sample(1:100000, 1)

# CART parameter (complexity)
cart.cp <- 1e-3


# Modification to deal with high cardinality ------------------------------

# Description:
# (1) Use various number of predictors based on the results of Boruta (Feature Selection)
# (2) Use various number of maximum levels (in order to deal with categorical variables
#     of high cardinality while using CART as synthesizer)

# Synthetic outputs with different setups:
# Different parameter for number of factor levels and number of predictors
syn_grid <- expand.grid(
  n_levels = c(4, 9, 14), # How many levels should be the maximum? --> Result: n_levels + 1 (cat. "Other")
  n_predictors = c(5, 7, 10) # How many predictors should be used for each target variable?
)


# Synthesis with different setups  ----------------------------------------

# Setup for parallelization via future package
options(future.globals.maxSize = Inf)
init_plan <- future::plan()
workers <- nrow(syn_grid)
future::plan("multicore", workers = workers)
on.exit(future::plan(init_plan), add = TRUE)

# Variables that we need as predictors
needed_variables <- setdiff(unique(do.call(c, lapply(fs_boruta, function(x) {
  dplyr::pull(x, "variable_names")
}))), targets)

# Synthesization
sds_list <- future.apply::future_lapply(
  seq(1, nrow(syn_grid)),
  function(iter) {
    
    # Bin factors (each factor > n_levels were binned)
    data_bin <- bin4syn(
      data = data, 
      n_levels = syn_grid[iter, "n_levels"],
      features = needed_variables,
      keep_unbinned = FALSE)
    
    #--- Set synthpop parameters ---#
    
    ##  Initial synthpop framework
    tmp_synth <- synthpop::syn(data = data_bin, m = 0)
    
    ## Create method vector (Which variable should be synthesized and with which kind of method)
    method_vector <- tmp_synth$method
    method_vector[] <- ""
    method_vector[targets] <- "cart" # Use cart (rpart) for each variable
    
    ##  Synthesis order
    visit_sequence <- which(method_vector == "")
    target_position <- which(method_vector != "")
    
    # Raab et al. (2016): Guidelines for producing useful synthetic data (Ch 5 - Point 6)
    # "Move variables with many categories to the end of the synthesis [...]"
    data_bin |> dplyr::select(dplyr::all_of(targets)) |> str()
    target_syn_order <- c("injuries_fatal", "damage", "trafficway_type")
    target_position <- target_position[target_syn_order]
    visit_sequence <- c(visit_sequence, target_position)
    
    ##  Predictor Matrix
    
    # Create predictor matrix based on selected features
    predictor_matrix <- tmp_synth$predictor.matrix
    predictor_matrix[] <- 0
    
    # Update predictor matrix while excluding target features which are not already synthesized 
    sf <- vector(mode = "list", length = length(fs_boruta))
    names(sf) <- names(fs_boruta)
    
    for(i in seq_along(target_position)) {
      if(i < length(target_position)) {
        to_be_removed <- c(names(target_position)[seq(i+1, length(target_position))])
      } else to_be_removed <- ""
      
      # With head, we chooses the top n relevent predictors based on Boruta
      # n: n_predictors in syn_grid
      sf[[names(target_position[i])]] <- head(setdiff(fs_boruta[[names(target_position)[i]]]$variable_names, to_be_removed), n = syn_grid[iter, "n_predictors"])
      
      # Update predictor matrix
      featureId <- which(colnames(predictor_matrix) %in% sf[[names(target_position[i])]])
      targetId <- which(rownames(predictor_matrix) == names(target_position)[i])
      predictor_matrix[targetId, featureId] <- 1
    }
    
    #--- Synthesis via synthpop ---#
    synthpop::syn(
      data = data_bin,
      proper = FALSE,
      method = method_vector,
      visit.sequence = visit_sequence,
      predictor.matrix = predictor_matrix,
      m = m,
      seed = seed,
      cart.cp = cart.cp
    )
  },
  future.seed = TRUE,
  future.packages = NULL
)

# Save each synthpop output in separate rds-file
for(i in seq_along(sds_list)) {
  file_name <- paste0("sds_preds", syn_grid[i, "n_predictors"], "_lv", syn_grid[i, "n_levels"] + 1, ".rds")
  saveRDS(sds_list[[i]], file = file.path(output_path, file_name))
}