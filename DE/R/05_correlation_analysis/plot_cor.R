##################################################################
##                   Plot correlation results                   ##
##################################################################

require(ggplot2)

# Load micro and tabular utility results ----------------------------------

source("~/WP13/DE/R/source.R")

input_path <- "~/WP13/DE/output/results/correlation"
output_path <- "~/WP13/DE/output/plots"

util_cor <- readRDS(file.path(input_path, "utility_correlations.rds"))
risk_cor <- readRDS(file.path(input_path, "risk_correlations.rds"))

# Target variables (synthesized variables)
col_names <- c("trafficway_type", "damage", "injuries_fatal")

# Prepare data for plotting -----------------------------------------------

# Create mapping for shorter target variable names (or to disclose the true 
# variable names)
short_labels <- LETTERS[seq_along(col_names)]
name_map <- setNames(short_labels, col_names)

# Prepare plot data
plot_data <- util_cor |>
  dplyr::mutate(metric = as.character(metric)) |>
  dplyr::rowwise() |>
  dplyr::mutate(flags_true = list(col_names[which(c_across(all_of(col_names)))]),
                group_flag = if (length(flags_true) == 0) {
                  "none"
                  } else {
                    paste(name_map[flags_true], collapse = "")
                    }) |>
  dplyr::ungroup() |>
  dplyr::mutate(metric = factor(metric, levels = unique(metric)), group_flag = factor(group_flag)) |>
  dplyr::select(
    table_id,
    n_way,
    all_of(col_names),
    group_flag,
    metric,
    cor,
    p_value,
    conf_low,
    conf_high
  )

#--- Forest plot
(forest_plot <- plot_data |>
  # optional: sortiere die Zeilen pro metric nach korrelation für bessere Lesbarkeit
  group_by(metric) |>
  arrange(metric, cor) |>
  ungroup() |>
  mutate(
    y_label = paste0("n_way=", n_way, " | ", group_flag),
    y_label = factor(y_label, levels = unique(y_label))
  ) |>
  ggplot(aes(x = cor, y = y_label, color = group_flag)) +
  geom_segment(aes(x = conf_low, xend = conf_high, y = y_label, yend = y_label), linewidth = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ metric, scales = "free_y", ncol = 1) +
  labs(x = "Pearson r", y = NULL, color = "group") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)))

#--- Heatmap / Tile
(heatmap_plot <- plot_data |>
  dplyr::mutate(n_way_f = as.factor(n_way)) |>
  ggplot2::ggplot(aes(x = group_flag, y = n_way_f, fill = cor)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Pearson r") +
    ggplot2::labs(x = "group (flags)", y = "n_way") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)))

#--- Individual plot of confidence intervals
my_theme <- theme_bw() +
  theme(
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    axis.text.x = element_text(size = 18, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 18),
    axis.title.x = element_text(size = 20, face = "bold"),
    axis.title.y = element_text(size = 20, face = "bold"),
    strip.text       = element_text(size = 18, face = "bold"),
    strip.text.x     = element_text(size = 18, face = "bold"),
    strip.text.y     = element_text(size = 18, face = "bold", angle = 0)
  )


(my_plot <- plot_data |>
    dplyr::mutate(n_way = factor(n_way)) |>
    ggplot(aes(x = group_flag, y = cor, group = table_id)) +
    geom_errorbar(
      aes(ymax = conf_high, ymin = conf_low),
      width = .25,
      position = position_dodge(width = .6),
      alpha = 0.8
    ) +
    # Punkte für die Mittelposition (sichtbarer)
    geom_point(
      aes(group = table_id),
      position = position_dodge(width = .6),
      size = 1.8,
      alpha = 0.8
    ) +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               color = "gray40") +
    labs(title = "", x = "Included targets", y = "Pearson's correlation coefficient") +
    coord_cartesian(ylim = c(-1, 1)) +
    scale_x_discrete(expand = expansion(add = .5)) +
    facet_grid(n_way ~ metric) +
    scale_y_continuous(sec.axis = sec_axis(
      ~ . ,
      name = "Table dimensions",
      breaks = NULL,
      labels = NULL
    )) +
    my_theme)
