# ============================================================
# 02_plotting.R
# Focused variability plots using 3 key groupings:
#   - Land Cover (LULC): bare to forested
#   - Climate Zone: dry to wet
#   - Biome: dry to wet (grouped into major categories)
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(fs)
  library(scales)
  library(viridis)
  library(ggtern)
})

# ---- Paths ----
out_dir <- "~/Library/CloudStorage/Box-Box/Hydrology_Lab/Soils_Proposal/outputs"

# ---- Read prepared data ----
site_data <- read_csv(
  file.path(out_dir, "site_data_combined.csv"),
  show_col_types = FALSE
)
site_landuse_long <- read_csv(
  file.path(out_dir, "site_landuse_long.csv"),
  show_col_types = FALSE
)

# ============================================================
# Define consistent ordering for categorical variables
# ============================================================

# LULC order: bare to forested (original classes)
lulc_order <- c(
  "Bare Areas",
  "Unconsolidated Bare Areas",
  "Impervious Surfaces",
  "Sparse Vegetation",
  "Grassland",
  "Shrubland",
  "Cropland",
  "Marsh",
  "Swamp",
  "Flooded Flat",
  "Water Body",
  "Forest"
)

# Climate zone order: dry to wet (matching actual data)
climate_order <- c(
  "Arid",
  "Mediterranean",
  "Humid Subtropical",
  "Tropical Savanna",
  "Humid Temperate",
  "Humid Continental",
  "Subarctic",
  "Tundra"
)

# Biome order: dry to wet (original categories)
biome_order <- c(
  "Deserts & Xeric Shrublands",
  "Mediterranean Forests, Woodlands & Scrub",
  "Montane Grasslands & Shrublands",
  "Temperate Grasslands, Savannas & Shrublands",
  "Tropical & Subtropical Grasslands, Savannas & Shrublands",
  "Tropical & Subtropical Dry Broadleaf Forests",
  "Temperate Broadleaf & Mixed Forests",
  "Temperate Conifer Forests",
  "Tropical & Subtropical Coniferous Forests",
  "Tropical & Subtropical Moist Broadleaf Forests",
  "Boreal Forests/Taiga",
  "Tundra",
  "Flooded Grasslands & Savannas",
  "Mangroves"
)

# ============================================================
# Biome groupings for plotting (singular names)
# ============================================================
biome_order_plot <- c(
  "Desert",
  "Mediterranean",
  "Temperate Grassland, Shrubland & Savanna",
  "Tropical Grassland, Shrubland & Savanna",
  "Tropical Forest",
  "Temperate Forest",
  "Boreal Forest"
)

# Colors for grouped biomes
biome_colors_plot <- c(
  "Desert" = "#E6B800",
  "Mediterranean" = "#CC6600",
  "Temperate Grassland, Shrubland & Savanna" = "#7FB069",
  "Tropical Grassland, Shrubland & Savanna" = "#9ACD32",
  "Tropical Forest" = "#2E8B57",
  "Temperate Forest" = "#1E5631",
  "Boreal Forest" = "#4A6741"
)

# Function to map original biome to grouped biome
group_biome <- function(biome) {
  case_when(
    biome %in% c("Deserts & Xeric Shrublands") ~ "Desert",
    biome %in% c("Mediterranean Forests, Woodlands & Scrub") ~ "Mediterranean",
    biome %in%
      c(
        "Montane Grasslands & Shrublands",
        "Temperate Grasslands, Savannas & Shrublands",
        "Tundra"
      ) ~ "Temperate Grassland, Shrubland & Savanna",
    biome %in%
      c(
        "Tropical & Subtropical Grasslands, Savannas & Shrublands",
        "Flooded Grasslands & Savannas"
      ) ~ "Tropical Grassland, Shrubland & Savanna",
    biome %in%
      c(
        "Tropical & Subtropical Dry Broadleaf Forests",
        "Tropical & Subtropical Moist Broadleaf Forests",
        "Tropical & Subtropical Coniferous Forests",
        "Mangroves"
      ) ~ "Tropical Forest",
    biome %in%
      c(
        "Temperate Broadleaf & Mixed Forests",
        "Temperate Conifer Forests"
      ) ~ "Temperate Forest",
    biome %in%
      c(
        "Boreal Forests/Taiga"
      ) ~ "Boreal Forest",
    TRUE ~ NA_character_
  )
}

# Variable order for faceted plots
var_order <- c(
  "Sand (%)",
  "Silt (%)",
  "Clay (%)",
  "Skin Temp (K)",
  "LAI High",
  "LAI Low",
  "Snow Cover (%)",
  "Total Evaporation"
)

# ============================================================
# Land-use colors (original)
# ============================================================
landuse_colors <- c(
  "Bare Areas" = "#E2D9C8",
  "Unconsolidated Bare Areas" = "#CBBBA0",
  "Impervious Surfaces" = "#4A4A4A",
  "Sparse Vegetation" = "#D8C99B",
  "Grassland" = "#7FB069",
  "Shrubland" = "#7A6A2B",
  "Cropland" = "#D2A24C",
  "Marsh" = "#6EC4B0",
  "Swamp" = "#2FA79A",
  "Flooded Flat" = "#4FA3D1",
  "Water Body" = "#1F5FA8",
  "Forest" = "#2E6B3E"
)
# ============================================================
# LULC merges for plotting:
#   - Bare Areas + Unconsolidated Bare Areas -> Bare
#   - Grassland + Shrubland -> Grassland & Shrubland
#   - Marsh + Swamp -> Wetland
# ============================================================
lulc_order_plot <- c(
  "Bare",
  "Impervious Surfaces",
  "Sparse Vegetation",
  "Grassland & Shrubland",
  "Cropland",
  "Wetland",
  "Flooded Flat",
  "Water Body",
  "Forest"
)

landuse_colors_plot <- c(
  "Bare" = "#E2D9C8",
  "Sparse Vegetation" = "#C4D4A0",
  "Grassland & Shrubland" = "#6B8E23",
  "Wetland" = "#39B7A5",
  "Impervious Surfaces" = landuse_colors[["Impervious Surfaces"]],
  "Cropland" = landuse_colors[["Cropland"]],
  "Flooded Flat" = landuse_colors[["Flooded Flat"]],
  "Water Body" = landuse_colors[["Water Body"]],
  "Forest" = landuse_colors[["Forest"]]
)

# ============================================================
# Apply ordering + create plotting variables (LC and biome)
# ============================================================
site_data <- site_data %>%
  mutate(
    climate_name = factor(climate_name, levels = climate_order),
    biome = factor(biome, levels = biome_order),
    biome_plot = group_biome(as.character(biome)),
    biome_plot = factor(biome_plot, levels = biome_order_plot)
  )

site_landuse_long <- site_landuse_long %>%
  mutate(
    land_class = str_squish(as.character(land_class)),
    land_class = factor(land_class, levels = lulc_order),
    climate_name = factor(climate_name, levels = climate_order),
    biome = factor(biome, levels = biome_order),
    biome_plot = group_biome(as.character(biome)),
    biome_plot = factor(biome_plot, levels = biome_order_plot),
    land_class_plot = case_when(
      land_class %in% c("Bare Areas", "Unconsolidated Bare Areas") ~ "Bare",
      land_class %in% c("Marsh", "Swamp") ~ "Wetland",
      land_class %in% c("Grassland", "Shrubland") ~ "Grassland & Shrubland",
      TRUE ~ as.character(land_class)
    ),
    land_class_plot = str_squish(land_class_plot),
    land_class_plot = factor(land_class_plot, levels = lulc_order_plot)
  )


# ============================================================
# Theme for plots
# ============================================================
theme_clean <- theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.6),
    axis.ticks = element_line(color = "black", linewidth = 0.6),
    axis.ticks.length = unit(0.2, "cm"),
    legend.position = "right",
    plot.title.position = "plot",
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    legend.key.size = unit(0.5, "cm"),
    strip.text = element_text(size = 12, face = "bold")
  )

tern_theme <- theme_bw(base_size = 12) +
  theme(
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11),
    legend.key.size = unit(0.45, "cm")
  )

# ---- Helper: save plot ----
save_plot <- function(p, fname, w = 10, h = 7, dpi = 300) {
  ggsave(
    filename = file.path(out_dir, fname),
    plot = p,
    width = w,
    height = h,
    dpi = dpi
  )
}

# ============================================================
# Variables
# ============================================================
cont_vars <- list(
  list(var = "sand_pct", label = "Sand (%)", name = "sand"),
  list(var = "clay_pct", label = "Clay (%)", name = "clay"),
  list(var = "silt_pct", label = "Silt (%)", name = "silt"),
  list(var = "skin_temp_mean", label = "Skin Temp (K)", name = "skin_temp"),
  list(var = "lai_high_mean", label = "LAI High", name = "lai_high"),
  list(var = "lai_low_mean", label = "LAI Low", name = "lai_low"),
  list(var = "snow_cover_mean", label = "Snow Cover (%)", name = "snow_cover"),
  list(
    var = "total_evap_mean",
    label = "Total Evaporation",
    name = "total_evap"
  )
)

label_var <- function(x) {
  case_when(
    x == "sand_pct" ~ "Sand (%)",
    x == "silt_pct" ~ "Silt (%)",
    x == "clay_pct" ~ "Clay (%)",
    x == "skin_temp_mean" ~ "Skin Temp (K)",
    x == "lai_high_mean" ~ "LAI High",
    x == "lai_low_mean" ~ "LAI Low",
    x == "snow_cover_mean" ~ "Snow Cover (%)",
    x == "total_evap_mean" ~ "Total Evaporation",
    TRUE ~ x
  )
}

# ============================================================
# 1. SITE COUNT BAR CHARTS
# ============================================================

# Sites by biome (grouped)
p <- site_data %>%
  filter(!is.na(biome_plot)) %>%
  count(biome_plot, name = "n") %>%
  ggplot(aes(x = n, y = biome_plot, fill = biome_plot)) +
  geom_col(color = "white", linewidth = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = biome_colors_plot) +
  labs(x = "Number of Sites", y = "Biome") +
  theme_clean
save_plot(p, "sites_by_biome.png", w = 10, h = 8)

# Sites by climate
p <- site_data %>%
  filter(!is.na(climate_name)) %>%
  count(climate_name, name = "n") %>%
  ggplot(aes(x = n, y = climate_name)) +
  geom_col(fill = "#D4A574", color = "white", linewidth = 0.2) +
  labs(x = "Number of Sites", y = "Climate Zone") +
  theme_clean
save_plot(p, "sites_by_climate.png", w = 10, h = 8)

# Sites by land cover (count sites where class appears with mean_prop > 0)
sites_lulc <- site_landuse_long %>%
  filter(!is.na(land_class_plot), !is.na(mean_prop), mean_prop > 0) %>%
  distinct(plot_id, land_class_plot) %>%
  count(land_class_plot, name = "n")

breaks_sites_lulc <- sites_lulc %>%
  pull(land_class_plot) %>%
  as.character() %>%
  unique() %>%
  sort()

p <- ggplot(
  sites_lulc,
  aes(x = n, y = land_class_plot, fill = land_class_plot)
) +
  geom_col(color = "white", linewidth = 0.2, show.legend = FALSE) +
  scale_fill_manual(values = landuse_colors_plot, breaks = breaks_sites_lulc) +
  labs(x = "Number of Sites", y = "Land Cover") +
  theme_clean
save_plot(p, "sites_by_lulc.png", w = 10, h = 9)

# ============================================================
# 2. LAND COVER PROPORTION PLOTS (composition)
# ============================================================

# --- By Biome (grouped) ---
lc_biome <- site_landuse_long %>%
  filter(!is.na(biome_plot), !is.na(land_class_plot), !is.na(mean_prop)) %>%
  group_by(biome_plot, land_class_plot) %>%
  summarise(mean_prop = mean(mean_prop, na.rm = TRUE), .groups = "drop") %>%
  group_by(biome_plot) %>%
  mutate(prop = mean_prop / sum(mean_prop, na.rm = TRUE)) %>%
  ungroup()

breaks_lc_biome <- lc_biome %>%
  filter(prop > 0) %>%
  pull(land_class_plot) %>%
  as.character() %>%
  unique() %>%
  sort()

# Stacked bar
p <- ggplot(lc_biome, aes(x = biome_plot, y = prop, fill = land_class_plot)) +
  geom_col(position = "stack", color = "white", linewidth = 0.2) +
  scale_fill_manual(
    name = "Land Cover",
    values = landuse_colors_plot,
    breaks = breaks_lc_biome
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Biome", y = "Proportion") +
  theme_clean +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
save_plot(p, "lulc_by_biome_stacked.png", w = 14, h = 9)

# Dodged bar
p <- ggplot(lc_biome, aes(x = biome_plot, y = prop, fill = land_class_plot)) +
  geom_col(position = "dodge", color = "white", linewidth = 0.1) +
  scale_fill_manual(
    name = "Land Cover",
    values = landuse_colors_plot,
    breaks = breaks_lc_biome
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Biome", y = "Proportion") +
  theme_clean +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
save_plot(p, "lulc_by_biome_dodged.png", w = 16, h = 9)

# Faceted by land cover
p <- ggplot(lc_biome, aes(x = biome_plot, y = prop, fill = land_class_plot)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = landuse_colors_plot, breaks = breaks_lc_biome) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~land_class_plot, scales = "free_y", ncol = 4) +
  labs(x = "Biome", y = "Proportion") +
  theme_clean +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10)
  )
save_plot(p, "lulc_by_biome_faceted.png", w = 14, h = 12)

# --- By Climate Zone ---
lc_climate <- site_landuse_long %>%
  filter(!is.na(climate_name), !is.na(land_class_plot), !is.na(mean_prop)) %>%
  group_by(climate_name, land_class_plot) %>%
  summarise(mean_prop = mean(mean_prop, na.rm = TRUE), .groups = "drop") %>%
  group_by(climate_name) %>%
  mutate(prop = mean_prop / sum(mean_prop, na.rm = TRUE)) %>%
  ungroup()

breaks_lc_climate <- lc_climate %>%
  filter(prop > 0) %>%
  pull(land_class_plot) %>%
  as.character() %>%
  unique() %>%
  sort()

# Stacked bar
p <- ggplot(
  lc_climate,
  aes(x = climate_name, y = prop, fill = land_class_plot)
) +
  geom_col(position = "stack", color = "white", linewidth = 0.2) +
  scale_fill_manual(
    name = "Land Cover",
    values = landuse_colors_plot,
    breaks = breaks_lc_climate
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Climate Zone", y = "Proportion") +
  theme_clean +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
save_plot(p, "lulc_by_climate_stacked.png", w = 14, h = 9)

# Dodged bar
p <- ggplot(
  lc_climate,
  aes(x = climate_name, y = prop, fill = land_class_plot)
) +
  geom_col(position = "dodge", color = "white", linewidth = 0.1) +
  scale_fill_manual(
    name = "Land Cover",
    values = landuse_colors_plot,
    breaks = breaks_lc_climate
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(x = "Climate Zone", y = "Proportion") +
  theme_clean +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
save_plot(p, "lulc_by_climate_dodged.png", w = 16, h = 9)

# Faceted by land cover
p <- ggplot(
  lc_climate,
  aes(x = climate_name, y = prop, fill = land_class_plot)
) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = landuse_colors_plot, breaks = breaks_lc_climate) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  facet_wrap(~land_class_plot, scales = "free_y", ncol = 4) +
  labs(x = "Climate Zone", y = "Proportion") +
  theme_clean +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10)
  )
save_plot(p, "lulc_by_climate_faceted.png", w = 14, h = 12)

# ============================================================
# 3. CONTINUOUS VARIABLES BY KEY GROUPINGS
# ============================================================

# --- Biome boxplots (site-level, grouped) ---
for (cv in cont_vars) {
  plot_data <- site_data %>%
    filter(!is.na(biome_plot), !is.na(.data[[cv$var]]))

  if (nrow(plot_data) > 0) {
    p <- ggplot(
      plot_data,
      aes(x = biome_plot, y = .data[[cv$var]], fill = biome_plot)
    ) +
      geom_boxplot(outlier.size = 0.7, show.legend = FALSE) +
      scale_fill_manual(values = biome_colors_plot, drop = TRUE) +
      labs(x = "Biome", y = cv$label) +
      theme_clean +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
    save_plot(p, paste0(cv$name, "_by_biome.png"), w = 12, h = 7)
  }
}

# --- Climate boxplots (site-level) ---
for (cv in cont_vars) {
  plot_data <- site_data %>%
    filter(!is.na(climate_name), !is.na(.data[[cv$var]]))

  if (nrow(plot_data) > 0) {
    p <- ggplot(
      plot_data,
      aes(x = climate_name, y = .data[[cv$var]], fill = climate_name)
    ) +
      geom_boxplot(outlier.size = 0.7, show.legend = FALSE) +
      scale_fill_viridis_d(option = "C", drop = TRUE) +
      labs(x = "Climate Zone", y = cv$label) +
      theme_clean +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
    save_plot(p, paste0(cv$name, "_by_climate.png"), w = 12, h = 7)
  }
}

# --- Land cover boxplots (using ALL land class data, not just dominant) ---
site_lc_vars <- site_landuse_long %>%
  select(plot_id, land_class_plot, mean_prop) %>%
  left_join(
    site_data %>%
      select(
        plot_id,
        sand_pct,
        silt_pct,
        clay_pct,
        skin_temp_mean,
        lai_high_mean,
        lai_low_mean,
        snow_cover_mean,
        total_evap_mean
      ),
    by = "plot_id"
  ) %>%
  filter(!is.na(land_class_plot), !is.na(mean_prop), mean_prop > 0)

# Boxplots for each variable by land cover (all classes)
for (cv in cont_vars) {
  plot_data <- site_lc_vars %>%
    filter(!is.na(.data[[cv$var]]))

  if (nrow(plot_data) > 0) {
    lc_breaks <- plot_data %>%
      pull(land_class_plot) %>%
      as.character() %>%
      unique() %>%
      sort()

    p <- ggplot(
      plot_data,
      aes(x = land_class_plot, y = .data[[cv$var]], fill = land_class_plot)
    ) +
      geom_boxplot(outlier.size = 0.7, show.legend = FALSE) +
      scale_fill_manual(values = landuse_colors_plot, breaks = lc_breaks) +
      labs(x = "Land Cover", y = cv$label) +
      theme_clean +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
    save_plot(p, paste0(cv$name, "_by_lulc.png"), w = 12, h = 7)
  }
}

# --- Calculate dominant land cover (needed for correlation matrix) ---
# First aggregate by land_class_plot (so Marsh+Swamp -> Wetland are combined)
site_dominant_lc <- site_landuse_long %>%
  filter(!is.na(land_class_plot), !is.na(mean_prop)) %>%
  group_by(plot_id, land_class_plot) %>%
  summarise(mean_prop = sum(mean_prop, na.rm = TRUE), .groups = "drop") %>%
  group_by(plot_id) %>%
  slice_max(mean_prop, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(plot_id, dominant_lc = land_class_plot)

# ============================================================
# 4. LAI HIGH VS LOW COMPARISON
# ============================================================

lai_long <- site_data %>%
  select(plot_id, biome_plot, climate_name, lai_high_mean, lai_low_mean) %>%
  pivot_longer(
    cols = c(lai_high_mean, lai_low_mean),
    names_to = "lai_type",
    values_to = "lai"
  ) %>%
  filter(!is.na(lai)) %>%
  mutate(lai_type = ifelse(lai_type == "lai_high_mean", "LAI High", "LAI Low"))

# By biome (grouped)
p <- lai_long %>%
  filter(!is.na(biome_plot)) %>%
  ggplot(aes(x = biome_plot, y = lai, fill = lai_type)) +
  geom_boxplot(outlier.size = 0.6, position = position_dodge(0.8)) +
  scale_fill_manual(
    values = c("LAI High" = "#2E8B57", "LAI Low" = "#8FBC8F"),
    name = "LAI Type",
    breaks = c("LAI High", "LAI Low")
  ) +
  labs(x = "Biome", y = "LAI") +
  theme_clean +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
save_plot(p, "lai_high_vs_low_by_biome.png", w = 14, h = 7)

# By climate
p <- lai_long %>%
  filter(!is.na(climate_name)) %>%
  ggplot(aes(x = climate_name, y = lai, fill = lai_type)) +
  geom_boxplot(outlier.size = 0.6, position = position_dodge(0.8)) +
  scale_fill_manual(
    values = c("LAI High" = "#2E8B57", "LAI Low" = "#8FBC8F"),
    name = "LAI Type",
    breaks = c("LAI High", "LAI Low")
  ) +
  labs(x = "Climate Zone", y = "LAI") +
  theme_clean +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
save_plot(p, "lai_high_vs_low_by_climate.png", w = 14, h = 7)

# Land cover (ALL classes; boxplot)
lai_lc_long <- site_landuse_long %>%
  select(plot_id, land_class_plot, mean_prop) %>%
  left_join(
    site_data %>% select(plot_id, lai_high_mean, lai_low_mean),
    by = "plot_id"
  ) %>%
  filter(!is.na(land_class_plot), !is.na(mean_prop), mean_prop > 0) %>%
  pivot_longer(
    cols = c(lai_high_mean, lai_low_mean),
    names_to = "lai_type",
    values_to = "lai"
  ) %>%
  filter(!is.na(lai)) %>%
  mutate(lai_type = ifelse(lai_type == "lai_high_mean", "LAI High", "LAI Low"))

p <- ggplot(lai_lc_long, aes(x = land_class_plot, y = lai, fill = lai_type)) +
  geom_boxplot(outlier.size = 0.6, position = position_dodge(0.8)) +
  scale_fill_manual(
    values = c("LAI High" = "#2E8B57", "LAI Low" = "#8FBC8F"),
    name = "LAI Type",
    breaks = c("LAI High", "LAI Low")
  ) +
  labs(x = "Land Cover", y = "LAI") +
  theme_clean +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
save_plot(p, "lai_high_vs_low_by_lulc.png", w = 14, h = 7)


# ============================================================
# 5. TERNARY SOIL TEXTURE PLOTS
# ============================================================

soil_ternary <- site_data %>%
  filter(!is.na(sand_pct), !is.na(clay_pct)) %>%
  mutate(
    silt_pct = 100 - sand_pct - clay_pct,
    silt_pct = ifelse(silt_pct < 0, 0, silt_pct)
  ) %>%
  filter(silt_pct >= 0)

# By biome (grouped)
p <- ggtern(
  filter(soil_ternary, !is.na(biome_plot)),
  aes(x = sand_pct, y = silt_pct, z = clay_pct, color = biome_plot)
) +
  geom_point(alpha = 0.65, size = 2.2) +
  scale_color_manual(values = biome_colors_plot, name = "Biome", drop = TRUE) +
  labs(x = "Sand", y = "Silt", z = "Clay") +
  tern_theme
save_plot(p, "ternary_soil_by_biome.png", w = 11, h = 9)

# By climate
p <- ggtern(
  filter(soil_ternary, !is.na(climate_name)),
  aes(x = sand_pct, y = silt_pct, z = clay_pct, color = climate_name)
) +
  geom_point(alpha = 0.65, size = 2.2) +
  scale_color_viridis_d(option = "C", name = "Climate", drop = TRUE) +
  labs(x = "Sand", y = "Silt", z = "Clay") +
  tern_theme
save_plot(p, "ternary_soil_by_climate.png", w = 11, h = 9)

# Land cover ALL classes (simple ternary)
soil_ternary_lc <- site_landuse_long %>%
  select(plot_id, land_class_plot, mean_prop) %>%
  filter(!is.na(land_class_plot), !is.na(mean_prop), mean_prop > 0) %>%
  left_join(
    soil_ternary %>% select(plot_id, sand_pct, silt_pct, clay_pct),
    by = "plot_id"
  ) %>%
  drop_na(sand_pct, silt_pct, clay_pct) %>%
  mutate(land_class_plot = droplevels(land_class_plot))

lc_breaks_tern <- soil_ternary_lc %>%
  pull(land_class_plot) %>%
  as.character() %>%
  unique() %>%
  sort()

p <- ggtern(
  soil_ternary_lc,
  aes(x = sand_pct, y = silt_pct, z = clay_pct, color = land_class_plot)
) +
  geom_point(alpha = 0.65, size = 2.2) +
  scale_color_manual(
    values = landuse_colors_plot,
    breaks = lc_breaks_tern,
    name = "Land Cover"
  ) +
  labs(x = "Sand", y = "Silt", z = "Clay") +
  tern_theme
save_plot(p, "ternary_soil_by_lulc.png", w = 11, h = 9)

# By continuous variables
ternary_cont <- list(
  list(var = "skin_temp_mean", label = "Skin Temp (K)", name = "skin_temp"),
  list(var = "lai_high_mean", label = "LAI High", name = "lai_high"),
  list(var = "snow_cover_mean", label = "Snow Cover (%)", name = "snow_cover"),
  list(
    var = "total_evap_mean",
    label = "Total Evaporation",
    name = "total_evap"
  )
)

for (cv in ternary_cont) {
  plot_data <- soil_ternary %>% filter(!is.na(.data[[cv$var]]))
  if (nrow(plot_data) > 0) {
    p <- ggtern(
      plot_data,
      aes(x = sand_pct, y = silt_pct, z = clay_pct, color = .data[[cv$var]])
    ) +
      geom_point(alpha = 0.7, size = 2.2) +
      scale_color_viridis_c(option = "C", name = cv$label) +
      labs(x = "Sand", y = "Silt", z = "Clay") +
      tern_theme
    save_plot(p, paste0("ternary_soil_by_", cv$name, ".png"), w = 10, h = 8)
  }
}

# ============================================================
# 6. FACETED DISTRIBUTION OF ALL CONTINUOUS VARIABLES
# ============================================================

dist_data <- site_data %>%
  select(
    plot_id,
    sand_pct,
    silt_pct,
    clay_pct,
    skin_temp_mean,
    lai_high_mean,
    lai_low_mean,
    snow_cover_mean,
    total_evap_mean
  ) %>%
  pivot_longer(cols = -plot_id, names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(
    variable = label_var(variable),
    variable = factor(variable, levels = var_order)
  )

# Histogram
p <- ggplot(dist_data, aes(x = value, fill = variable)) +
  geom_histogram(
    bins = 30,
    color = "white",
    linewidth = 0.2,
    show.legend = FALSE
  ) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  scale_fill_viridis_d(option = "D") +
  labs(x = "Value", y = "Count") +
  theme_clean
save_plot(p, "all_variables_distribution_histogram.png", w = 14, h = 10)

# Density
p <- ggplot(dist_data, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.7, color = "white", show.legend = FALSE) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  scale_fill_viridis_d(option = "D") +
  labs(x = "Value", y = "Density") +
  theme_clean
save_plot(p, "all_variables_distribution_density.png", w = 14, h = 10)

# ============================================================
# 7. CORRELATION MATRIX (Continuous variables only)
# ============================================================

cor_data <- site_data %>%
  select(
    sand_pct,
    silt_pct,
    clay_pct,
    skin_temp_mean,
    lai_high_mean,
    lai_low_mean,
    snow_cover_mean,
    total_evap_mean
  ) %>%
  drop_na()

names(cor_data) <- c(
  "Sand",
  "Silt",
  "Clay",
  "Skin Temp",
  "LAI High",
  "LAI Low",
  "Snow Cover",
  "Total Evap"
)

cor_matrix <- cor(cor_data, use = "complete.obs")

if (requireNamespace("GGally", quietly = TRUE)) {
  library(GGally)

  p <- ggpairs(
    cor_data,
    lower = list(continuous = wrap("points", alpha = 0.3, size = 0.6)),
    diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
    upper = list(continuous = wrap("cor", size = 3.5))
  ) +
    theme_clean +
    theme(
      strip.text = element_text(size = 11),
      axis.text = element_text(size = 10)
    )

  save_plot(p, "correlation_matrix_continuous.png", w = 14, h = 14)
}

# ============================================================
# 8. CORRELATION MATRICES WITH CATEGORICAL VARIABLES
# ============================================================
# Upper panel: plain correlation values (not colored by group)
# Lower panel: points colored by category
# Diagonal: density colored by category
# Legend: explains what colors mean
# Filter: top 5 categories with > 2 sites
# ============================================================

# For correlation matrices: show top 5 categories with > 2 sites
# (threshold filtering only applies to correlation matrices, not other plots)

if (requireNamespace("GGally", quietly = TRUE)) {
  library(GGally)

  # --- Prepare data with categorical variables ---
  # Use less restrictive filtering - only require continuous vars, not all categoricals
  site_data_cont <- site_data %>%
    left_join(site_dominant_lc, by = "plot_id") %>%
    mutate(
      dominant_lc = as.character(dominant_lc),
      dominant_lc = stringr::str_squish(dominant_lc),
      dominant_lc = dplyr::na_if(dominant_lc, ""),
      dominant_lc = factor(dominant_lc, levels = lulc_order_plot)
    ) %>%
    select(
      sand_pct,
      silt_pct,
      clay_pct,
      skin_temp_mean,
      lai_high_mean,
      lai_low_mean,
      snow_cover_mean,
      total_evap_mean,
      biome_plot,
      climate_name,
      dominant_lc
    )

  # Only drop NA for continuous variables (keep rows even if categorical is NA)
  site_data_cont <- site_data_cont %>%
    filter(
      !is.na(sand_pct),
      !is.na(silt_pct),
      !is.na(clay_pct),
      !is.na(skin_temp_mean),
      !is.na(lai_high_mean),
      !is.na(lai_low_mean),
      !is.na(snow_cover_mean),
      !is.na(total_evap_mean)
    )

  names(site_data_cont) <- c(
    "Sand",
    "Silt",
    "Clay",
    "Skin Temp",
    "LAI High",
    "LAI Low",
    "Snow Cover",
    "Total Evap",
    "Biome",
    "Climate",
    "Land Cover"
  )

  # --- Color by biome (top 5 with > 2 sites) ---
  biome_counts <- site_data_cont %>%
    filter(!is.na(Biome)) %>%
    count(Biome) %>%
    filter(n > 2) %>%
    slice_max(n, n = 5, with_ties = FALSE)
  site_biome <- site_data_cont %>%
    filter(!is.na(Biome), Biome %in% biome_counts$Biome) %>%
    mutate(Biome = droplevels(Biome))

  p <- ggpairs(
    site_biome,
    columns = 1:8,
    mapping = aes(color = Biome),
    lower = list(continuous = wrap("points", alpha = 0.4, size = 0.8)),
    diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
    upper = list(continuous = wrap("cor", size = 3.5))
  ) +
    scale_color_manual(values = biome_colors_plot, drop = TRUE) +
    scale_fill_manual(values = biome_colors_plot, drop = TRUE) +
    theme_clean +
    theme(
      strip.text = element_text(size = 11),
      axis.text = element_text(size = 9),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 11)
    ) +
    guides(
      color = guide_legend(nrow = 2, override.aes = list(size = 3, alpha = 1))
    )
  save_plot(p, "correlation_matrix_by_biome.png", w = 16, h = 16)

  # --- Color by climate (top 5 with > 2 sites) ---
  climate_counts <- site_data_cont %>%
    filter(!is.na(Climate)) %>%
    count(Climate) %>%
    filter(n > 2) %>%
    slice_max(n, n = 5, with_ties = FALSE)
  site_climate <- site_data_cont %>%
    filter(!is.na(Climate), Climate %in% climate_counts$Climate) %>%
    mutate(Climate = droplevels(Climate))

  p <- ggpairs(
    site_climate,
    columns = 1:8,
    mapping = aes(color = Climate),
    lower = list(continuous = wrap("points", alpha = 0.4, size = 0.8)),
    diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
    upper = list(continuous = wrap("cor", size = 3.5))
  ) +
    scale_color_viridis_d(option = "C", drop = TRUE) +
    scale_fill_viridis_d(option = "C", drop = TRUE) +
    theme_clean +
    theme(
      strip.text = element_text(size = 11),
      axis.text = element_text(size = 9),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 11)
    ) +
    guides(
      color = guide_legend(nrow = 2, override.aes = list(size = 3, alpha = 1))
    )
  save_plot(p, "correlation_matrix_by_climate.png", w = 16, h = 16)

  # --- Color by land cover (dominant, top 5 with > 2 sites) ---
  lc_counts <- site_data_cont %>%
    filter(!is.na(`Land Cover`)) %>%
    count(`Land Cover`) %>%
    filter(n > 2) %>%
    slice_max(n, n = 5, with_ties = FALSE)
  site_lc <- site_data_cont %>%
    filter(!is.na(`Land Cover`), `Land Cover` %in% lc_counts$`Land Cover`) %>%
    mutate(`Land Cover` = droplevels(`Land Cover`))

  # Build breaks from what's actually present (after threshold filter)
  dom_levels <- site_lc$`Land Cover` %>%
    as.character() %>%
    unique() %>%
    sort()

  # Build a palette that covers all present levels (fallback for anything missing)
  pal_dom <- landuse_colors_plot
  missing_levels <- setdiff(dom_levels, names(pal_dom))
  if (length(missing_levels) > 0) {
    pal_dom[missing_levels] <- "#9E9E9E"
  }

  p <- ggpairs(
    site_lc,
    columns = 1:8,
    mapping = aes(color = `Land Cover`),
    lower = list(continuous = wrap("points", alpha = 0.4, size = 0.8)),
    diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
    upper = list(continuous = wrap("cor", size = 3.5))
  ) +
    scale_color_manual(
      values = pal_dom,
      breaks = dom_levels,
      drop = TRUE,
      na.translate = FALSE
    ) +
    scale_fill_manual(
      values = pal_dom,
      breaks = dom_levels,
      drop = TRUE,
      na.translate = FALSE
    ) +
    theme_clean +
    theme(
      strip.text = element_text(size = 11),
      axis.text = element_text(size = 9),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      legend.title = element_text(size = 11)
    ) +
    guides(
      color = guide_legend(nrow = 2, override.aes = list(size = 3, alpha = 1))
    )

  save_plot(p, "correlation_matrix_by_lulc_dominant.png", w = 16, h = 16)
}

# Faceted density by biome (all categories, grouped)
dist_data_biome <- site_data %>%
  filter(!is.na(biome_plot)) %>%
  mutate(biome_plot = droplevels(biome_plot)) %>%
  select(
    plot_id,
    biome_plot,
    sand_pct,
    silt_pct,
    clay_pct,
    skin_temp_mean,
    lai_high_mean,
    lai_low_mean,
    snow_cover_mean,
    total_evap_mean
  ) %>%
  pivot_longer(
    cols = c(
      sand_pct,
      silt_pct,
      clay_pct,
      skin_temp_mean,
      lai_high_mean,
      lai_low_mean,
      snow_cover_mean,
      total_evap_mean
    ),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    variable = label_var(variable),
    variable = factor(variable, levels = var_order)
  )

p <- ggplot(dist_data_biome, aes(x = value, fill = biome_plot)) +
  geom_density(alpha = 0.5, color = NA) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  scale_fill_manual(values = biome_colors_plot, name = "Biome", drop = TRUE) +
  labs(x = "Value", y = "Density") +
  theme_clean +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  )
save_plot(p, "all_variables_distribution_by_biome.png", w = 14, h = 12)

# Faceted density by climate (all categories)
dist_data_climate <- site_data %>%
  filter(!is.na(climate_name)) %>%
  mutate(climate_name = droplevels(climate_name)) %>%
  select(
    plot_id,
    climate_name,
    sand_pct,
    silt_pct,
    clay_pct,
    skin_temp_mean,
    lai_high_mean,
    lai_low_mean,
    snow_cover_mean,
    total_evap_mean
  ) %>%
  pivot_longer(
    cols = c(
      sand_pct,
      silt_pct,
      clay_pct,
      skin_temp_mean,
      lai_high_mean,
      lai_low_mean,
      snow_cover_mean,
      total_evap_mean
    ),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    variable = label_var(variable),
    variable = factor(variable, levels = var_order)
  )

p <- ggplot(dist_data_climate, aes(x = value, fill = climate_name)) +
  geom_density(alpha = 0.5, color = NA) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  scale_fill_viridis_d(option = "C", name = "Climate", drop = TRUE) +
  labs(x = "Value", y = "Density") +
  theme_clean +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 11)
  )
save_plot(p, "all_variables_distribution_by_climate.png", w = 14, h = 12)
