# ============================================================
# Variability figures (REPLACEMENT: fixes Fig 2/3/5 to be interpretable)
#
# Reads from:
#   /Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/Soils_Proposal/data
#
# Saves to:
#   /Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/Soils_Proposal/outputs
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(fs)
  library(scales)
})

# ---- Paths ----
data_dir <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/Soils_Proposal/data"
out_dir  <- "/Users/sidneybush/Library/CloudStorage/Box-Box/Sidney_Bush/Soils_Proposal/outputs"
dir_create(out_dir, recurse = TRUE)

# ---- Input files ----
climate_path <- file.path(data_dir, "AI4PF_climate.csv")
landuse_path <- file.path(data_dir, "GLC_Annual_AI4PF.csv")

if (!file.exists(climate_path)) stop("Missing file: ", climate_path)
if (!file.exists(landuse_path)) stop("Missing file: ", landuse_path)

# ---- Read data ----
climate_raw <- readr::read_csv(climate_path, show_col_types = FALSE) %>% clean_names()
landuse_raw <- readr::read_csv(landuse_path, show_col_types = FALSE) %>% clean_names()

# ---- Standardize key fields ----
# Climate file has: plot_id_fullname, name
climate <- climate_raw %>%
  transmute(
    plot_id = plot_id_fullname,
    climate_name = name
  ) %>%
  filter(!is.na(plot_id), !is.na(climate_name)) %>%
  distinct()

# Landuse file has: plot_id_fu, year, land_class, area_m2 (also pix_count)
landuse <- landuse_raw %>%
  transmute(
    plot_id = plot_id_fu,
    year = as.integer(year),
    land_class = as.character(land_class),
    area_m2 = as.numeric(area_m2)
  ) %>%
  filter(!is.na(plot_id), !is.na(year), year >= 2000, year <= 2022)

# ---- Join + compute within-plot yearly proportions ----
# prop = area_m2 / total_area_m2 per plot-year
lu <- landuse %>%
  inner_join(climate, by = "plot_id") %>%
  group_by(plot_id, year) %>%
  mutate(
    total_area_m2 = sum(area_m2, na.rm = TRUE),
    prop = ifelse(total_area_m2 > 0, area_m2 / total_area_m2, NA_real_)
  ) %>%
  ungroup() %>%
  filter(is.finite(prop), prop >= 0, prop <= 1)

# ---- Helper: save plot ----
save_plot <- function(p, fname, w = 9, h = 6, dpi = 300) {
  out_path <- file.path(out_dir, fname)
  ggsave(filename = out_path, plot = p, width = w, height = h, dpi = dpi)
  cat("Saved:", out_path, "\n")
}

# ============================================================
# Optional: restrict to top climates by site count for readability
# (keeps figures interpretable if you have many climate names)
# ============================================================
top_climates <- lu %>%
  distinct(plot_id, climate_name) %>%
  count(climate_name, name = "n_sites") %>%
  arrange(desc(n_sites)) %>%
  slice_head(n = 10) %>%              # <-- change 10 if you want more/less
  pull(climate_name)

lu <- lu %>% filter(climate_name %in% top_climates)

# ============================================================
# Choose featured land classes (top by mean proportion in 2022)
# (more interpretable than "mean across 2000–2022")
# ============================================================
year_focus <- 2022

lu_y <- lu %>%
  filter(year == year_focus) %>%
  group_by(plot_id, climate_name, land_class) %>%
  summarise(prop = sum(prop, na.rm = TRUE), .groups = "drop")

top_classes <- lu_y %>%
  group_by(land_class) %>%
  summarise(mean_prop = mean(prop, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(mean_prop)) %>%
  slice_head(n = 6) %>%
  pull(land_class)

cat("\nTop climates (by sites):\n"); print(top_climates)
cat("\nTop land classes (by mean prop in ", year_focus, "):\n", sep=""); print(top_classes)

lu_top <- lu %>% filter(land_class %in% top_classes)

# ============================================================
# FIG 1 — Site coverage across climate types (context)
# ============================================================
fig1 <- lu %>%
  distinct(plot_id, climate_name) %>%
  count(climate_name, name = "n_sites") %>%
  mutate(climate_name = fct_reorder(climate_name, n_sites)) %>%
  ggplot(aes(x = n_sites, y = climate_name)) +
  geom_col() +
  labs(
    title = "Site coverage across climate types",
    x = "Number of sites",
    y = NULL
  ) +
  theme_minimal()

save_plot(fig1, "fig01_sites_by_climate.png", w = 9, h = 7)

# ============================================================
# FIG 2 (REPLACEMENT) — Across-site variability in land-use composition (2022)
# Uses a single year so “variability” is interpretable (no averaging away dynamics)
# ============================================================
fig2 <- lu_y %>%
  filter(land_class %in% top_classes) %>%
  mutate(climate_name = fct_infreq(climate_name)) %>%
  ggplot(aes(x = climate_name, y = prop)) +
  geom_boxplot(outlier.alpha = 0.15) +
  geom_point(position = position_jitter(width = 0.15), alpha = 0.20, size = 0.9) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~ land_class, scales = "free_y", ncol = 2) +
  labs(
    title = paste0("Across-site variability in land-use composition (", year_focus, ")"),
    x = "Climate type",
    y = "Proportion of plot area"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(fig2, "fig02_landuse_variability_2022_by_climate.png", w = 11, h = 8)

# ============================================================
# FIG 3 (REPLACEMENT) — Land-use change magnitude: Δ(2000 → 2022) by climate
# Clearer than 23-year ribbons + huge facet grid
# ============================================================
lu_2000_2022 <- lu %>%
  filter(year %in% c(2000, 2022)) %>%
  group_by(plot_id, climate_name, land_class, year) %>%
  summarise(prop = sum(prop, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = prop, values_fill = 0) %>%
  mutate(delta = `2022` - `2000`)

# choose land classes that actually changed the most (top 6 by mean |delta|)
top_classes_delta <- lu_2000_2022 %>%
  group_by(land_class) %>%
  summarise(m = mean(abs(delta), na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(m)) %>%
  slice_head(n = 6) %>%
  pull(land_class)

fig3 <- lu_2000_2022 %>%
  filter(land_class %in% top_classes_delta) %>%
  mutate(climate_name = fct_infreq(climate_name)) %>%
  ggplot(aes(x = climate_name, y = delta)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_boxplot(outlier.alpha = 0.15) +
  geom_point(position = position_jitter(width = 0.15), alpha = 0.20, size = 0.9) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~ land_class, scales = "free_y", ncol = 2) +
  labs(
    title = "Land-use change by climate (Δ 2000 → 2022)",
    subtitle = "Distributions across sites within each climate type",
    x = "Climate type",
    y = "Change in proportion of plot area"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(fig3, "fig03_delta_2000_2022_by_climate.png", w = 11, h = 8)

# ============================================================
# FIG 4 — “How mixed are sites?” Shannon diversity by climate
# Computed per plot-year across ALL land classes,
# then averaged per plot across 2000–2022
# ============================================================
shannon <- lu %>%
  group_by(plot_id, climate_name, year) %>%
  summarise(
    H = -sum(ifelse(prop > 0, prop * log(prop), 0), na.rm = TRUE),
    .groups = "drop"
  )

fig4 <- shannon %>%
  group_by(plot_id, climate_name) %>%
  summarise(H_mean = mean(H, na.rm = TRUE), .groups = "drop") %>%
  mutate(climate_name = fct_infreq(climate_name)) %>%
  ggplot(aes(x = climate_name, y = H_mean)) +
  geom_boxplot(outlier.alpha = 0.2) +
  geom_point(position = position_jitter(width = 0.15), alpha = 0.25, size = 1) +
  labs(
    title = "Variability in land-use mixture (Shannon diversity)",
    x = "Climate type",
    y = "Shannon diversity (higher = more mixed)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(fig4, "fig04_shannon_diversity_by_climate.png", w = 11, h = 7)

# ============================================================
# FIG 5a/5b (REPLACEMENT) — Temporal variability as “turnover”
# Mean year-to-year absolute change in proportion (compositional-friendly)
# Replaces SD/CV plots that can be misleading for rare classes
# ============================================================
turnover <- lu_top %>%
  group_by(plot_id, climate_name, land_class) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(dy = abs(prop - lag(prop))) %>%
  ungroup()

site_turnover <- turnover %>%
  group_by(plot_id, climate_name, land_class) %>%
  summarise(
    mean_abs_change = mean(dy, na.rm = TRUE),
    median_abs_change = median(dy, na.rm = TRUE),
    n_years = sum(!is.na(prop)),
    .groups = "drop"
  )

# 5a — mean |Δ|
fig5a <- site_turnover %>%
  mutate(climate_name = fct_infreq(climate_name)) %>%
  ggplot(aes(x = climate_name, y = mean_abs_change)) +
  geom_boxplot(outlier.alpha = 0.15) +
  geom_point(position = position_jitter(width = 0.15), alpha = 0.20, size = 0.9) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~ land_class, scales = "free_y", ncol = 2) +
  labs(
    title = "Temporal variability in land-use (mean year-to-year absolute change, 2000–2022)",
    subtitle = "Each point is one site; higher values mean more year-to-year change",
    x = "Climate type",
    y = "Mean |Δ proportion| per year"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(fig5a, "fig05a_turnover_mean_abs_change_by_climate.png", w = 11, h = 8)

# 5b — median |Δ| (more robust to spikes)
fig5b <- site_turnover %>%
  mutate(climate_name = fct_infreq(climate_name)) %>%
  ggplot(aes(x = climate_name, y = median_abs_change)) +
  geom_boxplot(outlier.alpha = 0.15) +
  geom_point(position = position_jitter(width = 0.15), alpha = 0.20, size = 0.9) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~ land_class, scales = "free_y", ncol = 2) +
  labs(
    title = "Temporal variability in land-use (median year-to-year absolute change, 2000–2022)",
    subtitle = "Median is more robust to one-off changes",
    x = "Climate type",
    y = "Median |Δ proportion| per year"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

save_plot(fig5b, "fig05b_turnover_median_abs_change_by_climate.png", w = 11, h = 8)

# End of script ----
