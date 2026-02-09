# -----------------------------------------------------------
# 01_data_prep.R
# Reads, cleans, and combines all data sources into one CSV
# -----------------------------------------------------------

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

# ---- Climate ----
climate <- read_csv(climate_path, show_col_types = FALSE) %>%
  clean_names() %>%
  transmute(
    plot_id = plot_id_fullname,
    climate_zone = climate_z,
    climate_name = name
  ) %>%
  filter(!is.na(plot_id), !is.na(climate_name)) %>%
  distinct()

# ---- Land use (one class per site per year, 2000-2022) ----
landuse <- read_csv(landuse_path, show_col_types = FALSE) %>%
  clean_names() %>%
  transmute(
    plot_id = plot_id_fu,
    year = as.integer(year),
    land_class = as.character(land_class)
  ) %>%
  filter(!is.na(plot_id), !is.na(year), year >= 2000, year <= 2022)

# Clean up class names and consolidate subtypes
landuse <- landuse %>%
  mutate(
    land_class = land_class %>%
      str_replace_all("_", " ") %>%
      str_to_lower() %>%
      str_to_title()
  )

landuse <- landuse %>%
  mutate(
    land_class = case_when(
      str_detect(tolower(land_class), "forest") ~ "Forest",
      str_detect(tolower(land_class), "cropland") ~ "Cropland",
      TRUE ~ land_class
    )
  )

# ---- Ecoregions ----
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

# ---- Soil texture ----
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

soil <- sand %>%
  full_join(clay, by = "plot_id") %>%
  mutate(silt_pct = 100 - sand_pct - clay_pct) %>%
  mutate(silt_pct = ifelse(silt_pct < 0, NA_real_, silt_pct))

# ---- ERA5 helpers ----
# Raw ERA5 data is wide (dates x sites), these pivot and summarize it.
# names_transform prevents a site literally called "Nan" from becoming NA
pivot_to_long <- function(file_path) {
  df <- read_csv(file_path, show_col_types = FALSE)

  df_long <- df %>%
    pivot_longer(
      cols = -dates,
      names_to = "plot_id",
      values_to = "value",
      names_transform = list(plot_id = as.character) # prevent "Nan" -> NA
    ) %>%
    filter(!is.na(value), value != "") %>%
    mutate(value = as.numeric(value)) %>%
    filter(!is.na(value))

  return(df_long)
}

process_timeseries_mean <- function(file_path, var_name) {
  df_long <- pivot_to_long(file_path)

  df_summary <- df_long %>%
    group_by(plot_id) %>%
    summarise(
      !!paste0(var_name, "_mean") := mean(value, na.rm = TRUE),
      !!paste0(var_name, "_sd") := sd(value, na.rm = TRUE),
      .groups = "drop"
    )

  return(df_summary)
}

process_timeseries_sum <- function(file_path, var_name) {
  df_long <- pivot_to_long(file_path)

  df_summary <- df_long %>%
    group_by(plot_id) %>%
    summarise(
      !!paste0(var_name, "_sum") := sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  return(df_summary)
}

# ---- Process ERA5 time series (366 daily values per site, 2020) ----
# State vars get averaged, flux vars get summed
lai_high <- process_timeseries_mean(lai_high_path, "lai_high_veg")
lai_low <- process_timeseries_mean(lai_low_path, "lai_low_veg")
skin_temp <- process_timeseries_mean(skin_temp_path, "skin_temp")
snow_cover <- process_timeseries_mean(snow_cover_path, "snow_cover")

total_evap <- process_timeseries_sum(total_evap_path, "total_evap")

# ERA5 reports evap as negative meters — flip sign and convert to mm/yr
total_evap <- total_evap %>%
  mutate(total_evap_sum = total_evap_sum * -1 * 1000)

# ---- Join everything together ----
site_env <- landuse %>%
  distinct(plot_id) %>%
  left_join(ecoregions, by = "plot_id") %>%
  left_join(climate, by = "plot_id") %>%
  left_join(soil, by = "plot_id") %>%
  left_join(lai_high, by = "plot_id") %>%
  left_join(lai_low, by = "plot_id") %>%
  left_join(skin_temp, by = "plot_id") %>%
  left_join(snow_cover, by = "plot_id") %>%
  left_join(total_evap, by = "plot_id")

# Modal land class per site across 2000-2022
class_counts <- landuse %>%
  count(plot_id, land_class, name = "n_years")

site_dominant_lc <- class_counts %>%
  group_by(plot_id) %>%
  slice_max(n_years, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(plot_id, dominant_lc = land_class)

site_data <- site_env %>%
  left_join(site_dominant_lc, by = "plot_id")

# ---- Write outputs ----
write_csv(site_data, file.path(out_dir, "site_data_combined.csv"))
write_csv(landuse, file.path(out_dir, "landuse_annual.csv"))
