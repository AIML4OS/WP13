##################################################################
##                            READ ME                           ##
##                        WP13: Use Case                        ##
##                            Germany                           ##
##################################################################

#  Folder
- data: contains dataset from kaggle (in order to show the code)
- functions: contains R functions used in our use case
- output: contains all outputs from our use case
- R: contains all main files (R scripts)

#  Data
Dataset from kaggle: https://www.kaggle.com/datasets/oktayrdeki/traffic-accidents/data
--> Accessed and downloaded: 20.02.2026

For our use case, we need to distinguish between sensitive and non-sensitive variables:
- Sensitive (variables to be synthesized):
  * trafficway_type (factor, 20 levels),
  * damage (factor, 3 levels),
  * injuries_fatal (num)

#  R scripts

source.R:
  - Contains a list of all needed libraries
  - Loads all needed R-scripts of functions needed in the main files

00_data_preprocessing.R:
  - Reduce the dataset to approximately 10% of the observations (stratified sampling)
  - Drop redundant variable

01_feature_selection_boruta.R:
  - Auxiliary method to deal with factors of high cardinality that are a problem when using CART as synthesizer.
  - Use Boruta algorithm to choose the most relevant predictors for each target variable.

02_synthesis.R:
  - Synthesis of our reduced sample with various synthesis setups
    (due to needed adjustments because of high cardinality: limit the number of predictors and levels of factors)
  - Synthesis via synthpop (CART) to generate m = 5 synthetic datasets for each setup

03_utility.R:
  - Calculates all microdata and table utiltiy metrics
  - 03_utility/fit_for_purpose_utility.R
  Calculates the Cramer's V matrix for original and synthetic data. The output is the difference between
  the original with each synthetic matrix (matrix_original - matrix_synthetic_m) for each m synthetic dataset and
  the average across all m matrices.

  - 03_utility/global_utility.R
  Calculates pMSE, SPECKS, and pMSE ratio in a parallelized framework (future)
  for each synthesis setups/settings from 02_synthesis.R

  - 03_utility/tabular_utility.R
  Calculates the normalized Hellinger distance based on each possible tabular combinations based on a set of
  non-synthesized (key) and synthesized (target) variables.

04_risk.R:
  - Calculates all microdata and table risk metrics
  - 04_risk/micro_disclosure_risk.R: Calculates the TCAP ratio metric
  - 04_risk/tabular_disclosure_risk.R: Calculates the tabular risk metric (for a specific table) based on
  group disclosures.

05_correlation_analysis (folder):

  - utility_correlation_analysis.R:
  Calculates the Pearson correlation coefficient between microdata utility (pMSE, pMSE ratio, SPECKS) with
  tabular utility (normalized Hellinger distance) group by table_id, n_way (table dimension), target variables included
  in table with table_id x.

  - risk_correlation_analysis.R:
  Calculates the Pearson correlation coefficient between microdata risk (TCAP ratio) with
  tabular risk (group disclosure based metric) group by table_id, n_way (table dimension), target variables included
  in table with table_id x.

  - plot_cor.R:
  Contains various options to plot the correlation results.
