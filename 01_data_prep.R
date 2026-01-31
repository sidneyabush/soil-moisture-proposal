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
# Each site-year has ONE land class assignment (pixel_count is always 1)
landuse_raw <- read_csv(landuse_path, show_col_types = FALSE) %>%
  clean_names()

landuse <- landuse_raw %>%
  transmute(
    plot_id = plot_id_fu,
    year = as.integer(year),
    land_class = as.character(land_class)
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
# Helper functions to process wide time-series data
# Converts wide format to long, then summarizes per site
# ============================================================

# Helper to pivot and clean time-series data
pivot_timeseries <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)

  df_long <- df %>%
    pivot_longer(
      cols = -dates,
      names_to = "plot_id",
      values_to = "value"
    ) %>%
    filter(!is.na(value), value != "") %>%
    mutate(value = as.numeric(value)) %>%
    filter(!is.na(value))

  return(df_long)
}

# For STATE variables (temperature, snow cover, LAI): calculate MEAN per site
# These are instantaneous measurements - annual mean makes sense
process_timeseries_mean <- function(file_path, var_name) {
  df_long <- pivot_timeseries(file_path)

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

# For FLUX variables (evaporation): calculate SUM per site
# These are daily totals - annual sum gives total flux for the year
process_timeseries_sum <- function(file_path, var_name) {
  df_long <- pivot_timeseries(file_path)

  df_summary <- df_long %>%
    group_by(plot_id) %>%
    summarise(
      !!paste0(var_name, "_sum") := sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  # Handle duplicate site names
  df_summary <- df_summary %>%
    group_by(plot_id) %>%
    summarise(across(everything(), ~ mean(.x, na.rm = TRUE)), .groups = "drop")

  return(df_summary)
}

# ============================================================
# Process time-series data (ERA5-Land Daily Aggregates, 2020)
# ============================================================
# All variables are from ECMWF/ERA5_LAND/DAILY_AGGR via Google Earth Engine
# Raw data: 366 daily values per site (columns) for year 2020
#
# STATE variables (use MEAN - instantaneous measurements):
#   - skin_temp_mean:    Mean Annual Skin Temperature (K)
#   - snow_cover_mean:   Mean Annual Snow Cover (%)
#   - lai_high_veg_mean: Mean Annual LAI for high vegetation (m²/m²)
#   - lai_low_veg_mean:  Mean Annual LAI for low vegetation (m²/m²)
#
# FLUX variables (use SUM - daily totals accumulated to annual):
#   - total_evap_sum:    Annual Total Evaporation (mm/yr)
# ============================================================

# STATE variables: MEAN of daily values per site
lai_high <- process_timeseries_mean(lai_high_path, "lai_high_veg")
lai_low <- process_timeseries_mean(lai_low_path, "lai_low_veg")
skin_temp <- process_timeseries_mean(skin_temp_path, "skin_temp")
snow_cover <- process_timeseries_mean(snow_cover_path, "snow_cover")

# FLUX variable: SUM of daily values per site
total_evap <- process_timeseries_sum(total_evap_path, "total_evap")

# ============================================================
# Convert total evaporation units
# ============================================================
# ERA5-Land total_evaporation_sum: daily values in meters (negative = evaporation)
# After summing 366 days: total is in meters/year (negative = net evaporation)
# Convert to mm/year and flip sign so evaporation LOSS is positive
total_evap <- total_evap %>%
  mutate(
    total_evap_sum = total_evap_sum * -1 * 1000  # m/yr -> mm/yr, flip sign
  )

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
# Determine modal land cover per site (2000-2022)
# ============================================================
# Each site-year already has ONE land class (the dominant class for that year)
# Find the mode: which class appears most often across the 23 years

# Count how many years each land class appears per site
class_counts <- landuse %>%
  count(plot_id, land_class, name = "n_years")

# Pick the modal class (most frequent) per site
site_dominant_lc <- class_counts %>%
  group_by(plot_id) %>%
  slice_max(n_years, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(plot_id, dominant_lc = land_class)

# ============================================================
# Combine everything into final site-level dataset
# ============================================================
site_data <- site_env %>%
  left_join(site_dominant_lc, by = "plot_id")

# ============================================================
# Write outputs
# ============================================================
# Main site-level dataset with all variables
write_csv(site_data, file.path(out_dir, "site_data_combined.csv"))

# Copy README documentation to outputs folder
# (README.md should be in the outputs/ folder of the git repo)
readme_source <- "outputs/README.md"
if (file.exists(readme_source)) {
  file.copy(readme_source, file.path(out_dir, "README.md"), overwrite = TRUE)
  message("README.md copied to outputs folder")
}
