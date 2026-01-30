# ============================================================
# 01_data_prep.R
# Reads, cleans, and combines all data sources into one CSV
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(fs)
})

# ---- Paths ----
data_dir <- "~/Library/CloudStorage/Box-Box/Hydrology_Lab/Soils_Proposal/data"
gdrive_dir <- file.path(data_dir, "drive-download-20260130T004300Z-3-001")
out_dir <- "~/Library/CloudStorage/Box-Box/Hydrology_Lab/Soils_Proposal/outputs"
dir_create(out_dir, recurse = TRUE)

# ---- Input files ----
climate_path <- file.path(data_dir, "AI4PF_climate.csv")
landuse_path <- file.path(data_dir, "GLC_Annual_AI4PF.csv")
ecoregions_path <- file.path(gdrive_dir, "sites_with_ecoregions.csv")
sand_path <- file.path(gdrive_dir, "sand_mean_by_basin.csv")
clay_path <- file.path(gdrive_dir, "clay_mean_by_basin.csv")
lai_high_path <- file.path(gdrive_dir, "LAI_high_2020.csv")
lai_low_path <- file.path(gdrive_dir, "LAI_low_2020.csv")
skin_temp_path <- file.path(gdrive_dir, "skin_temp_2020.csv")
snow_cover_path <- file.path(gdrive_dir, "snow_cover_2020.csv")
total_evap_path <- file.path(gdrive_dir, "total_evap_sum_2020.csv")

# ============================================================
# Read and clean climate data
# ============================================================
climate_raw <- read_csv(climate_path, show_col_types = FALSE) %>%
  clean_names()

climate <- climate_raw %>%
  transmute(
    plot_id = plot_id_fullname,
    climate_zone = climate_z,
    climate_name = name
  ) %>%
  filter(!is.na(plot_id), !is.na(climate_name)) %>%
  distinct()

# ============================================================
# Read and clean land use data
# ============================================================
landuse_raw <- read_csv(landuse_path, show_col_types = FALSE) %>%
  clean_names()

landuse <- landuse_raw %>%
  transmute(
    plot_id = plot_id_fu,
    year = as.integer(year),
    land_class = as.character(land_class),
    area_m2 = as.numeric(area_m2)
  ) %>%
  filter(!is.na(plot_id), !is.na(year), year >= 2000, year <= 2022)

# Clean land-use names
landuse <- landuse %>%
  mutate(
    land_class = land_class %>%
      str_replace_all("_", " ") %>%
      str_to_lower() %>%
      str_to_title()
  )

# Consolidate forest and cropland types into major categories
landuse <- landuse %>%
  mutate(
    land_class = case_when(
      str_detect(tolower(land_class), "forest") ~ "Forest",
      str_detect(tolower(land_class), "cropland") ~ "Cropland",
      TRUE ~ land_class
    )
  )

# Compute within-plot yearly proportions
landuse <- landuse %>%
  group_by(plot_id, year) %>%
  mutate(
    total_area_m2 = sum(area_m2, na.rm = TRUE),
    prop = ifelse(total_area_m2 > 0, area_m2 / total_area_m2, NA_real_)
  ) %>%
  ungroup() %>%
  filter(is.finite(prop), prop >= 0, prop <= 1)

# ============================================================
# Read and clean ecoregions data
# ============================================================
ecoregions <- read_csv(ecoregions_path, show_col_types = FALSE) %>%
  clean_names() %>%
  transmute(
    plot_id = site_id,
    latitude = latitude,
    longitude = longitude,
    realm = realm_name,
    biome = biome_name,
    ecoregion_id = ecoregion_id,
    ecoregion = ecoregion_name
  ) %>%
  filter(!is.na(plot_id)) %>%
  distinct(plot_id, .keep_all = TRUE)

# ============================================================
# Read and clean soil data
# ============================================================
sand <- read_csv(sand_path, show_col_types = FALSE) %>%
  clean_names() %>%
  transmute(
    plot_id = plot_id_fu,
    sand_pct = mean
  ) %>%
  filter(!is.na(plot_id)) %>%
  distinct(plot_id, .keep_all = TRUE)

clay <- read_csv(clay_path, show_col_types = FALSE) %>%
  clean_names() %>%
  transmute(
    plot_id = plot_id_fu,
    clay_pct = mean
  ) %>%
  filter(!is.na(plot_id)) %>%
  distinct(plot_id, .keep_all = TRUE)

# Combine soil data and calculate silt
soil <- sand %>%
  full_join(clay, by = "plot_id") %>%
  mutate(silt_pct = 100 - sand_pct - clay_pct) %>%
  mutate(silt_pct = ifelse(silt_pct < 0, NA_real_, silt_pct))

# ============================================================
# Helper function to process wide time-series data
# Converts wide format to long, then summarizes to annual mean per site
# ============================================================
process_timeseries <- function(file_path, var_name) {
  df <- read_csv(file_path, show_col_types = FALSE)

  # Pivot from wide to long
  df_long <- df %>%
    pivot_longer(
      cols = -dates,
      names_to = "plot_id",
      values_to = "value"
    ) %>%
    filter(!is.na(value), value != "")

  # Convert value to numeric
  df_long <- df_long %>%
    mutate(value = as.numeric(value)) %>%
    filter(!is.na(value))

  # Calculate annual mean per site
  df_summary <- df_long %>%
    group_by(plot_id) %>%
    summarise(
      !!paste0(var_name, "_mean") := mean(value, na.rm = TRUE),
      !!paste0(var_name, "_sd") := sd(value, na.rm = TRUE),
      .groups = "drop"
    )

  # Handle duplicate site names by averaging
  df_summary <- df_summary %>%
    group_by(plot_id) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  return(df_summary)
}

# ============================================================
# Process time-series data
# ============================================================
lai_high <- process_timeseries(lai_high_path, "lai_high")
lai_low <- process_timeseries(lai_low_path, "lai_low")
skin_temp <- process_timeseries(skin_temp_path, "skin_temp")
snow_cover <- process_timeseries(snow_cover_path, "snow_cover")
total_evap <- process_timeseries(total_evap_path, "total_evap")

# ============================================================
# Create site-level summary (combining all environmental variables)
# ============================================================
# Get unique sites from land use data
sites <- landuse %>%
  distinct(plot_id)

# Join all environmental variables
site_env <- sites %>%
  left_join(ecoregions, by = "plot_id") %>%
  left_join(climate, by = "plot_id") %>%
  left_join(soil, by = "plot_id") %>%
  left_join(lai_high, by = "plot_id") %>%
  left_join(lai_low, by = "plot_id") %>%
  left_join(skin_temp, by = "plot_id") %>%
  left_join(snow_cover, by = "plot_id") %>%
  left_join(total_evap, by = "plot_id")

# ============================================================
# Create site-level land use signature (mean proportion 2000-2022)
# FIXED: Include zeros for years when a land class is absent
# ============================================================

# Get all unique combinations we need
all_years <- 2000:2022
all_land_classes <- unique(landuse$land_class)
all_sites <- unique(landuse$plot_id)

# Create complete grid of site x year x land_class
complete_grid <- expand_grid(
  plot_id = all_sites,
  year = all_years,
  land_class = all_land_classes
)

# Join with actual data and fill missing with 0
landuse_complete <- complete_grid %>%
  left_join(
    landuse %>% select(plot_id, year, land_class, prop),
    by = c("plot_id", "year", "land_class")
  ) %>%
  mutate(prop = replace_na(prop, 0))

# Now calculate true mean proportion across ALL years
site_landuse <- landuse_complete %>%
  group_by(plot_id, land_class) %>%
  summarise(
    mean_prop = mean(prop, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(mean_prop > 0) # Only keep classes that appear at least once

# Pivot to wide format for site-level land use
site_landuse_wide <- site_landuse %>%
  mutate(
    land_class = paste0(
      "lc_",
      str_to_lower(str_replace_all(land_class, " ", "_"))
    )
  ) %>%
  pivot_wider(
    names_from = land_class,
    values_from = mean_prop,
    values_fill = 0
  )

# ============================================================
# Combine everything into final site-level dataset
# ============================================================
site_data <- site_env %>%
  left_join(site_landuse_wide, by = "plot_id")

# ============================================================
# Also create a long-format dataset for land use by site
# (useful for stacked bar plots)
# ============================================================
site_landuse_long <- site_landuse %>%
  left_join(site_env, by = "plot_id")

# ============================================================
# Write outputs
# ============================================================
write_csv(site_data, file.path(out_dir, "site_data_combined.csv"))
write_csv(site_landuse_long, file.path(out_dir, "site_landuse_long.csv"))
write_csv(landuse, file.path(out_dir, "landuse_annual.csv"))
