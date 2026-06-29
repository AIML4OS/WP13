#################################################################
##            Correlation analysis of risk measures            ##
#################################################################

# Load micro and tabular risk results -------------------------------------

input_path <- "~/WP13/DE/output/results/risk"
output_path <- "~/WP13/DE/output/results/correlation"

micro_risk <- readRDS(file.path(input_path, "micro_tcap_ratios.rds"))
table_risk <- readRDS(file.path(input_path, "tabular_risk.rds"))

# Correlation analysis ----------------------------------------------------

# Join micro and tabular risk results
joined_risk <- table_risk |> 
  dplyr::left_join(micro_risk, by = c("predictors", "levels"))

# Set confidence interval level for correlation test
ci_rate <- 0.99

# Select target variable names from the joined tibble
param_names <- c("predictors", "levels", "table_id", "n_way", "tab_risk", "tcap_ratio")
tar_names <- setdiff(names(joined_risk), param_names)

# Pearson correlation
# Warning (correlation = NA because standard deviation is zero) occurs 
# if table risk is 0. If that is the case, we replace NAs with 0.
risk_cor <- joined_risk |> 
  dplyr::group_by(table_id, n_way, dplyr::across(dplyr::all_of(tar_names))) |> 
  dplyr::summarise(
    {
      cc <- complete.cases(tab_risk, tcap_ratio)
      if (sum(cc) >= 3) {
        ct <- cor.test(
          tab_risk[cc],
          tcap_ratio[cc],
          method = "pearson",
          conf.level = ci_rate
        )
        tibble::tibble(
          cor = unname(ct$estimate),
          # p_value = round(ct$p.value, digits = 5),
          conf_low = ct$conf.int[1],
          conf_high = ct$conf.int[2]
        )
      } else {
        tibble::tibble(
          cor = NA_real_,
          p_value = NA_real_,
          conf_low = NA_real_,
          conf_high = NA_real_
        )
      }
    },
    .groups = "drop"
  ) |> 
  tidyr::replace_na(list(cor = 0, conf_low = 0, conf_high = 0))

# Save results
saveRDS(risk_cor, file = file.path(output_path, "risk_correlations.rds"))
