# -----------------------------------------------------------
# 02_plotting.R
# Exploratory plots grouped by biome, climate, and land cover
# -----------------------------------------------------------

suppressPackageStartupMessages({
  library(tidyverse)
  library(janitor)
  library(fs)
  library(scales)
  library(viridis)
  library(ggtern)
})

# ---- Paths and data ----
out_dir <- "~/Library/CloudStorage/Box-Box/Hydrology_Lab/Soils_Proposal/outputs"

site_data <- read_csv(
  file.path(out_dir, "site_data_combined.csv"),
  show_col_types = FALSE
)

# ---- Colors and ordering ----

biome_levels <- c(
  "Desert",
  "Mediterranean",
  "Temperate Grassland, Shrubland & Savanna",
  "Tropical Grassland, Shrubland & Savanna",
  "Tropical Forest",
  "Temperate Forest",
  "Boreal Forest"
)

biome_colors <- c(
  "Desert" = "#E6B800",
  "Mediterranean" = "#CC6600",
  "Temperate Grassland, Shrubland & Savanna" = "#7FB069",
  "Tropical Grassland, Shrubland & Savanna" = "#9ACD32",
  "Tropical Forest" = "#2E8B57",
  "Temperate Forest" = "#1E5631",
  "Boreal Forest" = "#4A6741"
)

climate_levels <- c(
  "Arid",
  "Mediterranean",
  "Humid Subtropical",
  "Tropical Savanna",
  "Humid Temperate",
  "Humid Continental",
  "Subarctic",
  "Tundra"
)

lc_levels <- c(
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

lc_colors <- c(
  "Bare" = "#E2D9C8",
  "Sparse Vegetation" = "#C4D4A0",
  "Grassland & Shrubland" = "#6B8E23",
  "Wetland" = "#39B7A5",
  "Impervious Surfaces" = "#4A4A4A",
  "Cropland" = "#D2A24C",
  "Flooded Flat" = "#4FA3D1",
  "Water Body" = "#1F5FA8",
  "Forest" = "#2E6B3E"
)

# Map detailed WWF biomes to grouped categories
group_biome <- function(biome) {
  case_when(
    biome == "Deserts & Xeric Shrublands" ~ "Desert",
    biome == "Mediterranean Forests, Woodlands & Scrub" ~ "Mediterranean",
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
    biome == "Boreal Forests/Taiga" ~ "Boreal Forest",
    TRUE ~ NA_character_
  )
}

# ---- Prep plotting variables ----
site_data <- site_data %>%
  mutate(
    climate_name = factor(climate_name, levels = climate_levels),
    biome_plot = factor(group_biome(biome), levels = biome_levels),
    dominant_lc_plot = case_when(
      dominant_lc %in% c("Bare Areas", "Unconsolidated Bare Areas") ~ "Bare",
      dominant_lc %in% c("Marsh", "Swamp") ~ "Wetland",
      dominant_lc %in% c("Grassland", "Shrubland") ~ "Grassland & Shrubland",
      TRUE ~ as.character(dominant_lc)
    ),
    dominant_lc_plot = factor(dominant_lc_plot, levels = lc_levels)
  )

# ---- Continuous variables ----
cont_vars <- list(
  list(var = "sand_pct", label = "Sand (%)", name = "sand"),
  list(var = "clay_pct", label = "Clay (%)", name = "clay"),
  list(var = "silt_pct", label = "Silt (%)", name = "silt"),
  list(
    var = "skin_temp_mean",
    label = "Mean Annual Skin Temp (K)",
    name = "skin_temp"
  ),
  list(
    var = "lai_high_veg_mean",
    label = "Mean Annual LAI High Veg",
    name = "lai_high"
  ),
  list(
    var = "lai_low_veg_mean",
    label = "Mean Annual LAI Low Veg",
    name = "lai_low"
  ),
  list(
    var = "snow_cover_mean",
    label = "Mean Annual Snow Cover (%)",
    name = "snow_cover"
  ),
  list(
    var = "total_evap_sum",
    label = "Annual Evap Loss (mm/yr)",
    name = "total_evap"
  )
)

cont_cols <- c(
  "sand_pct",
  "silt_pct",
  "clay_pct",
  "skin_temp_mean",
  "lai_high_veg_mean",
  "lai_low_veg_mean",
  "snow_cover_mean",
  "total_evap_sum"
)

var_order <- c(
  "Sand (%)",
  "Silt (%)",
  "Clay (%)",
  "Mean Annual Skin Temp (K)",
  "Mean Annual LAI High Veg",
  "Mean Annual LAI Low Veg",
  "Mean Annual Snow Cover (%)",
  "Annual Evap Loss (mm/yr)"
)

label_var <- function(x) {
  case_when(
    x == "sand_pct" ~ "Sand (%)",
    x == "silt_pct" ~ "Silt (%)",
    x == "clay_pct" ~ "Clay (%)",
    x == "skin_temp_mean" ~ "Mean Annual Skin Temp (K)",
    x == "lai_high_veg_mean" ~ "Mean Annual LAI High Veg",
    x == "lai_low_veg_mean" ~ "Mean Annual LAI Low Veg",
    x == "snow_cover_mean" ~ "Mean Annual Snow Cover (%)",
    x == "total_evap_sum" ~ "Annual Evap Loss (mm/yr)",
    TRUE ~ x
  )
}

# ---- Grouping specs ----
# Each plot type loops over these three groupings.
# colors = NULL means use viridis instead of a manual palette.
groupings <- list(
  list(
    var = "biome_plot",
    label = "Biome",
    colors = biome_colors,
    suffix = "biome"
  ),
  list(
    var = "climate_name",
    label = "Climate Zone",
    colors = NULL,
    suffix = "climate"
  ),
  list(
    var = "dominant_lc_plot",
    label = "Dominant Land Cover (2000-2022)",
    colors = lc_colors,
    suffix = "lulc"
  )
)

# ---- Theme and helpers ----
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

save_plot <- function(p, fname, w = 10, h = 7, dpi = 300) {
  ggsave(
    filename = file.path(out_dir, fname),
    plot = p,
    width = w,
    height = h,
    dpi = dpi
  )
}

# Returns the right fill or color scale based on grouping
add_fill <- function(g, ...) {
  if (!is.null(g$colors)) {
    scale_fill_manual(values = g$colors, drop = TRUE, ...)
  } else {
    scale_fill_viridis_d(option = "C", drop = TRUE, ...)
  }
}

add_color <- function(g, ...) {
  if (!is.null(g$colors)) {
    scale_color_manual(values = g$colors, drop = TRUE, ...)
  } else {
    scale_color_viridis_d(option = "C", drop = TRUE, ...)
  }
}

# ---- 1. Site count bar charts ----
for (g in groupings) {
  plot_data <- site_data %>%
    filter(!is.na(.data[[g$var]])) %>%
    count(.data[[g$var]], name = "n")

  p <- ggplot(
    plot_data,
    aes(x = n, y = .data[[g$var]], fill = .data[[g$var]])
  ) +
    geom_col(color = "white", linewidth = 0.2, show.legend = FALSE) +
    add_fill(g) +
    labs(x = "Number of Sites", y = g$label) +
    theme_clean
  save_plot(p, paste0("sites_by_", g$suffix, ".png"), w = 10, h = 8)
}

# ---- 2. Dominant land cover stacked by biome/climate ----
for (g in groupings[1:2]) {
  plot_data <- site_data %>%
    filter(!is.na(.data[[g$var]]), !is.na(dominant_lc_plot)) %>%
    count(.data[[g$var]], dominant_lc_plot, name = "n")

  p <- ggplot(
    plot_data,
    aes(x = .data[[g$var]], y = n, fill = dominant_lc_plot)
  ) +
    geom_col(position = "stack", color = "white", linewidth = 0.2) +
    scale_fill_manual(name = "Dominant Land Cover", values = lc_colors) +
    labs(x = g$label, y = "Number of Sites") +
    theme_clean +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
  save_plot(p, paste0("lulc_by_", g$suffix, "_stacked.png"), w = 14, h = 9)
}

# ---- 3. Boxplots of each variable by grouping ----
for (g in groupings) {
  for (cv in cont_vars) {
    plot_data <- site_data %>%
      filter(!is.na(.data[[g$var]]), !is.na(.data[[cv$var]]))
    if (nrow(plot_data) == 0) {
      next
    }

    p <- ggplot(
      plot_data,
      aes(x = .data[[g$var]], y = .data[[cv$var]], fill = .data[[g$var]])
    ) +
      geom_boxplot(outlier.size = 0.7, show.legend = FALSE) +
      add_fill(g) +
      labs(x = g$label, y = cv$label) +
      theme_clean +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
    save_plot(p, paste0(cv$name, "_by_", g$suffix, ".png"), w = 12, h = 7)
  }
}

# ---- 4. LAI high vs low by grouping ----
lai_fill <- c("LAI High Veg" = "#2E8B57", "LAI Low Veg" = "#8FBC8F")

for (g in groupings) {
  lai_long <- site_data %>%
    filter(!is.na(.data[[g$var]])) %>%
    pivot_longer(
      cols = c(lai_high_veg_mean, lai_low_veg_mean),
      names_to = "lai_type",
      values_to = "lai"
    ) %>%
    filter(!is.na(lai)) %>%
    mutate(
      lai_type = ifelse(
        lai_type == "lai_high_veg_mean",
        "LAI High Veg",
        "LAI Low Veg"
      )
    )

  p <- ggplot(lai_long, aes(x = .data[[g$var]], y = lai, fill = lai_type)) +
    geom_boxplot(outlier.size = 0.6, position = position_dodge(0.8)) +
    scale_fill_manual(values = lai_fill, name = "Vegetation Type") +
    labs(x = g$label, y = "Mean Annual LAI") +
    theme_clean +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 11))
  save_plot(p, paste0("lai_high_vs_low_by_", g$suffix, ".png"), w = 14, h = 7)
}

# ---- 5. Ternary soil texture plots ----
soil_ternary <- site_data %>%
  filter(!is.na(sand_pct), !is.na(clay_pct)) %>%
  mutate(silt_pct = pmax(100 - sand_pct - clay_pct, 0))

# By categorical groupings
for (g in groupings) {
  plot_data <- soil_ternary %>% filter(!is.na(.data[[g$var]]))
  if (nrow(plot_data) == 0) {
    next
  }

  p <- ggtern(
    plot_data,
    aes(x = sand_pct, y = silt_pct, z = clay_pct, color = .data[[g$var]])
  ) +
    geom_point(alpha = 0.65, size = 2.2) +
    add_color(g, name = g$label) +
    labs(x = "Sand", y = "Silt", z = "Clay") +
    tern_theme
  save_plot(p, paste0("ternary_soil_by_", g$suffix, ".png"), w = 11, h = 9)
}

# By continuous variables
ternary_cont <- list(
  list(
    var = "skin_temp_mean",
    label = "Mean Annual Skin Temp (K)",
    name = "skin_temp"
  ),
  list(
    var = "lai_high_veg_mean",
    label = "Mean Annual LAI High Veg",
    name = "lai_high"
  ),
  list(
    var = "snow_cover_mean",
    label = "Mean Annual Snow Cover (%)",
    name = "snow_cover"
  ),
  list(
    var = "total_evap_sum",
    label = "Annual Evap Loss (mm/yr)",
    name = "total_evap"
  )
)

for (cv in ternary_cont) {
  plot_data <- soil_ternary %>% filter(!is.na(.data[[cv$var]]))
  if (nrow(plot_data) == 0) {
    next
  }

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

# ---- 6. Distributions ----
dist_data <- site_data %>%
  select(plot_id, all_of(cont_cols)) %>%
  pivot_longer(cols = -plot_id, names_to = "variable", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(variable = factor(label_var(variable), levels = var_order))

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

# Faceted density colored by biome/climate
for (g in groupings[1:2]) {
  plot_data <- site_data %>%
    filter(!is.na(.data[[g$var]])) %>%
    mutate(!!g$var := droplevels(.data[[g$var]])) %>%
    select(plot_id, all_of(g$var), all_of(cont_cols)) %>%
    pivot_longer(
      cols = all_of(cont_cols),
      names_to = "variable",
      values_to = "value"
    ) %>%
    filter(!is.na(value)) %>%
    mutate(variable = factor(label_var(variable), levels = var_order))

  p <- ggplot(plot_data, aes(x = value, fill = .data[[g$var]])) +
    geom_density(alpha = 0.5, color = NA) +
    facet_wrap(~variable, scales = "free", ncol = 4) +
    add_fill(g, name = g$label) +
    labs(x = "Value", y = "Density") +
    theme_clean +
    theme(legend.position = "bottom", legend.text = element_text(size = 10))
  save_plot(
    p,
    paste0("all_variables_distribution_by_", g$suffix, ".png"),
    w = 14,
    h = 12
  )
}

# ---- 7. Correlation matrices ----
if (requireNamespace("GGally", quietly = TRUE)) {
  library(GGally)

  cor_labels <- c(
    "Sand",
    "Silt",
    "Clay",
    "Skin Temp",
    "LAI High Veg",
    "LAI Low Veg",
    "Snow Cover",
    "Evap Loss"
  )

  # Base dataset: continuous vars only, drop NAs
  cor_base <- site_data %>%
    select(all_of(cont_cols), biome_plot, climate_name, dominant_lc_plot) %>%
    filter(across(all_of(cont_cols), ~ !is.na(.)))

  names(cor_base)[1:8] <- cor_labels
  names(cor_base)[9:11] <- c("Biome", "Climate", "Dominant LC")

  # Plain correlation matrix (no coloring)
  p <- ggpairs(
    cor_base[, 1:8],
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

  # Colored by each grouping (top 5 categories with > 2 sites)
  cor_groupings <- list(
    list(col = "Biome", colors = biome_colors, suffix = "biome"),
    list(col = "Climate", colors = NULL, suffix = "climate"),
    list(col = "Dominant LC", colors = lc_colors, suffix = "lulc_dominant")
  )

  for (cg in cor_groupings) {
    # Keep only top 5 categories with > 2 sites
    cat_counts <- cor_base %>%
      filter(!is.na(.data[[cg$col]])) %>%
      count(.data[[cg$col]]) %>%
      filter(n > 2) %>%
      slice_max(n, n = 5, with_ties = FALSE)

    plot_data <- cor_base %>%
      filter(.data[[cg$col]] %in% cat_counts[[1]]) %>%
      mutate(!!cg$col := droplevels(.data[[cg$col]]))

    # Build appropriate scales
    if (!is.null(cg$colors)) {
      present <- plot_data[[cg$col]] %>% as.character() %>% unique() %>% sort()
      pal <- cg$colors
      missing <- setdiff(present, names(pal))
      if (length(missing) > 0) {
        pal[missing] <- "#9E9E9E"
      }

      color_sc <- scale_color_manual(
        values = pal,
        drop = TRUE,
        na.translate = FALSE
      )
      fill_sc <- scale_fill_manual(
        values = pal,
        drop = TRUE,
        na.translate = FALSE
      )
    } else {
      color_sc <- scale_color_viridis_d(option = "C", drop = TRUE)
      fill_sc <- scale_fill_viridis_d(option = "C", drop = TRUE)
    }

    p <- ggpairs(
      plot_data,
      columns = 1:8,
      mapping = aes(color = .data[[cg$col]]),
      lower = list(continuous = wrap("points", alpha = 0.4, size = 0.8)),
      diag = list(continuous = wrap("densityDiag", alpha = 0.5)),
      upper = list(continuous = wrap("cor", size = 3.5))
    ) +
      color_sc +
      fill_sc +
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
    save_plot(
      p,
      paste0("correlation_matrix_by_", cg$suffix, ".png"),
      w = 16,
      h = 16
    )
  }
}
