#################################################################
##       Stratified subset of our traffic accidents data       ##
#################################################################

# Description -------------------------------------------------------------

# Just to show how the code works (less computational cost)
# 
# Goal:
# Reduce the dataset to approximately 10% of the observations
# while preserving the distributions of the sensitive variables:
#
#   - trafficway_type (factor, 20 levels)
#   - damage (factor, 3 levels)
#   - injuries_fatal (numeric)
#
# Strategy:
# Perform stratified sampling using the joint distribution of
# trafficway_type and damage. Within each stratum we sample
# approximately 10% of observations.

# Load raw data -----------------------------------------------------------

path <- "~/WP13/DE/data"
accidents <- read.csv(file.path(path, "traffic_accidents.csv"), stringsAsFactors = TRUE) |> 
  tibble::as_tibble()


# Build subset via stratified sampling ------------------------------------

# The dataset is grouped by trafficway_type and damage.
# Within each stratum we draw approximately 10% of rows.
# A minimum of one observation per stratum is retained.

accidents_subset <- accidents |>
  dplyr::group_by(trafficway_type, damage) |>
  dplyr::group_modify(~ {
    n_stratum <- nrow(.x)
    # Target sample size (10% of the stratum)
    target_size <- max(1L, round(n_stratum * 0.10))
    dplyr::slice_sample(.x, n = min(target_size, n_stratum))
  }) |>
  dplyr::ungroup()

# Drop date variable "crash_date". All necessary informations are already in 
# other variables of the dataset, e.g. crash_hour, crash_day_of_the_week, etc.
accidents_subset <- accidents_subset |> 
  dplyr::select(-crash_date)

# [Optional] Compare distribution -----------------------------------------

# Optional diagnostics to check if key distributions
# are approximately preserved.

damage_original <- accidents |>
  dplyr::count(damage) |>
  dplyr::mutate(prop_original = n / sum(n))

damage_subset <- accidents_subset |>
  dplyr::count(damage) |>
  dplyr::mutate(prop_subset = n / sum(n))

print(damage_original)
print(damage_subset)

traffic_original <- accidents |>
  dplyr::count(trafficway_type) |>
  dplyr::mutate(prop_original = n / sum(n))

traffic_subset <- accidents_subset |>
  dplyr::count(trafficway_type) |>
  dplyr::mutate(prop_subset = n / sum(n))

print(traffic_original)
print(traffic_subset)

# Summary statistics for injuries_fatal
summary_original <- summary(accidents$injuries_fatal)
summary_subset   <- summary(accidents_subset$injuries_fatal)

print(summary_original)
print(summary_subset)


# Save subset -------------------------------------------------------------

saveRDS(accidents_subset, file =  file.path(path, "traffic_accidents_subset_10pct.rds"))
