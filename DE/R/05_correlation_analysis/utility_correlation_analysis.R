##################################################################
##           Correlation analysis of utility measures           ##
##################################################################

# Load micro and tabular utility results ----------------------------------

input_path <- "~/WP13/DE/output/results/utility"
output_path <- "~/WP13/DE/output/results/correlation"

micro_util <- readRDS(file.path(input_path, "micro_gu.rds"))
table_util <- readRDS(file.path(input_path, "tabular_utility.rds"))

# Correlation analysis ----------------------------------------------------

# Join micro and tabular utility results
joined_util <- table_util |> 
  dplyr::left_join(micro_util, by = c("predictors", "levels"))

# Set confidence interval level for correlation test
ci_rate <- 0.99

# Select target variable names from the joined tibble
param_names <- c("predictors", "levels", "table_id", "n_way", "hellinger_dist", "pMSE", "SPECKS", "pMSE_ratio")
tar_names <- setdiff(names(joined_util), param_names)

# Pearson correlation
util_cor <- joined_util |>
  tidyr::pivot_longer(
    cols = c(pMSE, SPECKS, pMSE_ratio),
    names_to = "metric",
    values_to = "metric_value"
  ) |>
  dplyr::group_by(table_id, n_way, across(all_of(tar_names)), metric) |>
  dplyr::summarise({
    cc <- complete.cases(hellinger_dist, metric_value)
    n <- sum(cc)
    
    if (n >= 3) { # for reasonable p-values and confidence intervals)
      ct <- cor.test(hellinger_dist[cc],
                     metric_value[cc],
                     method = "pearson",
                     conf.level = ci_rate)
      tibble::tibble(
        cor = unname(ct$estimate),
        p_value = ct$p.value,
        conf_low = ct$conf.int[1],
        conf_high = ct$conf.int[2]
      )
    } else if (n == 2) { # return only correlation value
      tibble::tibble(
        cor = cor(hellinger_dist[cc], metric_value[cc], use = "complete.obs"),
        p_value = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_
      )
    } else {
      tibble::tibble(
        cor = NA_real_,
        p_value = NA_real_,
        conf_low = NA_real_,
        conf_high = NA_real_
      )
    }
  }, .groups = "drop")

# Wider table format 
util_cor |>
  pivot_wider(
    names_from = metric,
    values_from = c(cor, p_value, conf_low, conf_high),
    names_glue = "{.value}_{metric}"
  )

# Save results
saveRDS(util_cor, file = file.path(output_path, "utility_correlations.rds"))
