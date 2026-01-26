# ============================================================
# Variability figures
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

# ---- Read data ----
climate_raw <- readr::read_csv(climate_path, show_col_types = FALSE) %>% clean_names()
landuse_raw <- readr::read_csv(landuse_path, show_col_types = FALSE) %>% clean_names()

# ---- Standardize key fields ----
climate <- climate_raw %>%
  transmute(
    plot_id = plot_id_fullname,
    climate_name = name
  ) %>%
  filter(!is.na(plot_id), !is.na(climate_name)) %>%
  distinct()

landuse <- landuse_raw %>%
  transmute(
    plot_id   = plot_id_fu,
    year      = as.integer(year),
    land_class = as.character(land_class),
    area_m2   = as.numeric(area_m2)
  ) %>%
  filter(!is.na(plot_id), !is.na(year), year >= 2000, year <= 2022)

# ---- Join + compute within-plot yearly proportions ----
lu <- landuse %>%
  inner_join(climate, by = "plot_id") %>%
  group_by(plot_id, year) %>%
  mutate(
    total_area_m2 = sum(area_m2, na.rm = TRUE),
    prop = ifelse(total_area_m2 > 0, area_m2 / total_area_m2, NA_real_)
  ) %>%
  ungroup() %>%
  filter(is.finite(prop), prop >= 0, prop <= 1)

# ============================================================
# Collapse forest and cropland classes
# ============================================================
lu <- lu %>%
  mutate(
    land_class = case_when(
      str_detect(tolower(land_class), "forest")   ~ "Forest",
      str_detect(tolower(land_class), "cropland") ~ "Cropland",
      TRUE ~ land_class
    )
  )

# ============================================================
# Clean land-use names
# ============================================================
lu <- lu %>%
  mutate(
    land_class = land_class %>%
      str_replace_all("_", " ") %>%
      str_to_lower() %>%
      str_to_title()
  )

# ============================================================
# Manual land-use order (legend + stacking)
# ============================================================
landuse_order <- c(
  "Forest",
  "Shrubland",
  "Grassland",
  "Sparse Vegetation",
  "Cropland",
  "Impervious Surfaces",
  "Unconsolidated Bare Areas",
  "Bare Areas",
  "Marsh",
  "Swamp",
  "Flooded Flat",
  "Water Body"
)

# Enforce factor order once (affects downstream)
lu <- lu %>%
  mutate(land_class = factor(land_class, levels = landuse_order))

# ============================================================
# Land-use colors (muted + cohesive)
# ============================================================
landuse_colors <- c(
  "Forest"                    = "#2E6B3E",
  "Shrubland"                 = "#7A6A2B",
  "Grassland"                 = "#7FB069",
  "Sparse Vegetation"         = "#D8C99B",
  "Cropland"                  = "#D2A24C",
  "Impervious Surfaces"       = "#4A4A4A",
  "Unconsolidated Bare Areas" = "#CBBBA0",
  "Bare Areas"                = "#E2D9C8",
  "Marsh"                     = "#6EC4B0",
  "Swamp"                     = "#2FA79A",
  "Flooded Flat"              = "#4FA3D1",
  "Water Body"                = "#1F5FA8"
)

# ---- Helper: save plot ----
save_plot <- function(p, fname, w = 9, h = 6, dpi = 300) {
  ggsave(
    filename = file.path(out_dir, fname),
    plot = p,
    width = w,
    height = h,
    dpi = dpi
  )
}

# ============================================================
# Theme tweak:
# - remove ALL gridlines
# - keep x/y axes + ticks
# ============================================================
no_grid_with_axes <- theme(
  panel.grid        = element_blank(),
  axis.line         = element_line(color = "black", linewidth = 0.4),
  axis.ticks        = element_line(color = "black", linewidth = 0.4),
  axis.ticks.length = unit(0.15, "cm")
)

# ============================================================
# FIG 1 — Site coverage across climate types
# ============================================================
fig1 <- lu %>%
  distinct(plot_id, climate_name) %>%
  count(climate_name, name = "n_sites") %>%
  mutate(climate_name = fct_reorder(climate_name, n_sites)) %>%
  ggplot(aes(x = n_sites, y = climate_name)) +
  geom_col(color = "white", linewidth = 0.2) +
  labs(
    x = "Number of sites",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  no_grid_with_axes

save_plot(fig1, "fig01_sites_by_climate.png", w = 9, h = 7)

# ============================================================
# Site-level land-use signatures (2000–2022)
# ============================================================
site_signature <- lu %>%
  group_by(plot_id, climate_name, land_class) %>%
  summarise(
    mean_prop_2000_2022 = mean(prop, na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# Major land-use type per site (dominant across record)
# ============================================================
site_major_lulc <- site_signature %>%
  group_by(plot_id, climate_name) %>%
  slice_max(mean_prop_2000_2022, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  rename(
    major_lulc = land_class,
    major_lulc_prop = mean_prop_2000_2022
  ) %>%
  mutate(major_lulc = factor(major_lulc, levels = landuse_order))

# ============================================================
# FIG 2 — Major land-use types across climates
# (drop unused legend entries for THIS plot only)
# ============================================================
fig2 <- site_major_lulc %>%
  count(climate_name, major_lulc, name = "n_sites") %>%
  group_by(climate_name) %>%
  mutate(prop_sites = n_sites / sum(n_sites)) %>%
  ungroup() %>%
  mutate(climate_name = fct_infreq(climate_name)) %>%
  ggplot(aes(x = climate_name, y = prop_sites, fill = major_lulc)) +
  geom_col(color = "white", linewidth = 0.2) +
  scale_fill_manual(
    name   = "Major land-use type",
    values = landuse_colors,
    breaks = landuse_order,
    drop   = TRUE
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Climate type",
    y = "Proportion of sites"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  no_grid_with_axes

save_plot(fig2, "fig02_major_landuse_by_climate.png", w = 12, h = 7)

# ============================================================
# FIG 3 — Full-record land-use composition across climates
# (keep ALL land classes in legend)
# ============================================================
fig3 <- site_signature %>%
  group_by(climate_name, land_class) %>%
  summarise(
    mean_prop = mean(mean_prop_2000_2022, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(climate_name) %>%
  mutate(mean_prop = mean_prop / sum(mean_prop)) %>%
  ungroup() %>%
  mutate(climate_name = fct_infreq(climate_name)) %>%
  ggplot(aes(x = climate_name, y = mean_prop, fill = land_class)) +
  geom_col(color = "white", linewidth = 0.2) +
  scale_fill_manual(
    name   = "Land-use type",
    values = landuse_colors,
    breaks = landuse_order,
    drop   = FALSE
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    x = "Climate type",
    y = "Mean land-use proportion (2000–2022)"
  ) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  no_grid_with_axes

save_plot(fig3, "fig03_full_record_landuse_by_climate.png", w = 12, h = 7)

# ============================================================
# FIG 4 — Overall counts of sites by ALL land-use classes
# Definition:
#   number of sites where a class appears at least once (2000–2022)
# ============================================================
fig4 <- site_signature %>%
  rename(mean_prop = mean_prop_2000_2022) %>%
  filter(mean_prop > 0) %>%
  distinct(plot_id, land_class) %>%
  count(land_class, name = "n_sites") %>%
  mutate(land_class = fct_reorder(land_class, n_sites)) %>%
  ggplot(aes(x = n_sites, y = land_class, fill = land_class)) +
  geom_col(color = "white", linewidth = 0.2) +
  scale_fill_manual(
    name   = "Land-use type",
    values = landuse_colors,
    breaks = landuse_order,
    drop   = FALSE
  ) +
  labs(
    x = "Number of sites",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  no_grid_with_axes

save_plot(fig4, "fig04_site_counts_all_landclasses.png", w = 9, h = 7)

# ---- End of script ----
