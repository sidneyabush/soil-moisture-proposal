# Data Processing and Plotting Documentation

This document describes the data transformations applied from raw downloads through final plots.

---

## Data Sources

### ERA5-Land Variables (Google Earth Engine)
- **Source**: `ECMWF/ERA5_LAND/DAILY_AGGR`
- **Extraction**: Google Earth Engine, filtered to 2020-01-01 to 2020-12-31
- **Format**: Wide CSV with dates as rows, site IDs as columns
- **Variables extracted**:
  - `total_evaporation_sum` - Daily total evaporation (meters)
  - `snow_cover` - Daily snow cover (%)
  - `skin_temperature` - Daily skin temperature (Kelvin)
  - `leaf_area_index_high_vegetation` - Daily LAI for high vegetation (m²/m²)
  - `leaf_area_index_low_vegetation` - Daily LAI for low vegetation (m²/m²)

### Static Spatial Data
- **Climate zones**: Köppen climate classification (`AI4PF_climate.csv`)
- **Ecoregions**: WWF ecoregions (`sites_with_ecoregions.csv`)
- **Soil properties**: SoilGrids sand/clay percentages (`sand_mean_by_basin.csv`, `clay_mean_by_basin.csv`)

### Land Use/Land Cover
- **Source**: GLC Annual (`GLC_Annual_AI4PF.csv`)
- **Period**: 2000-2022 (annual data)
- **Format**: Site × year × land class with area proportions

---

## Data Transformations (01_data_prep.R)

### 1. ERA5 Time-Series Processing

Raw ERA5 data consists of **366 daily values per site** (2020 was a leap year). These are aggregated to a single annual value per site using different methods depending on variable type:

#### State Variables (instantaneous measurements)
**Method**: MEAN of daily values

| Variable | Raw Unit | Aggregation | Output Column | Output Unit |
|----------|----------|-------------|---------------|-------------|
| Skin temperature | K | Mean of 366 days | `skin_temp_mean` | K |
| Snow cover | % | Mean of 366 days | `snow_cover_mean` | % |
| LAI high vegetation | m²/m² | Mean of 366 days | `lai_high_veg_mean` | m²/m² |
| LAI low vegetation | m²/m² | Mean of 366 days | `lai_low_veg_mean` | m²/m² |

**Rationale**: For state variables, the annual mean characterizes the site's typical conditions throughout the year.

#### Flux Variables (accumulated totals)
**Method**: SUM of daily values

| Variable | Raw Unit | Aggregation | Output Column | Output Unit |
|----------|----------|-------------|---------------|-------------|
| Total evaporation | m/day | Sum of 366 days × (-1) × 1000 | `total_evap_sum` | mm/yr |

**Rationale**: For flux variables, we sum daily totals to get annual total. The sign flip (×-1) converts ERA5 convention (negative = evaporation) to positive values representing evaporation loss. Multiply by 1000 converts meters to millimeters.

### 2. Soil Properties

- Sand and clay percentages are joined directly from source files (one value per site)
- Silt percentage is calculated as: `silt_pct = 100 - sand_pct - clay_pct`

### 3. Land Use/Land Cover

**Raw data**: Annual dominant land class per site for 2000-2022 (from GLC Annual)
- One row per site-year with the dominant land class for that year
- 23 years of data per site (2000-2022)

**Example raw data**:
```
plot_id_fu  year  lc_id  pixel_count  land_class                              area_m2
AUKIW1      2000  72     1            Closed_evergreen_needle_leaved_forest   1e+06
AUKIW1      2001  72     1            Closed_evergreen_needle_leaved_forest   1e+06
AUKIW1      2002  72     1            Closed_evergreen_needle_leaved_forest   1e+06
...
```

**Transformation**:
1. Clean land class names (replace underscores, title case)
   - `Closed_evergreen_needle_leaved_forest` → `Closed Evergreen Needle Leaved Forest`
2. Consolidate forest/cropland subtypes into major categories
   - Any class containing "forest" → `Forest`
   - Any class containing "cropland" → `Cropland`
3. Count how many years each land class appears per site
4. Assign the **modal land class** (most frequent across years) as `dominant_lc`

**Example result**:
- Site AUKIW1: "Forest" for 23 years → `dominant_lc` = Forest
- Site X: Forest in 15 years, Cropland in 8 years → `dominant_lc` = Forest

**Rationale**: The modal land class captures the primary land use character over the 23-year period of record.

### 4. Static Classifications

The following are joined directly without transformation:
- Climate zone and name (Köppen classification)
- Biome, realm, ecoregion (WWF Ecoregions)
- Latitude, longitude

---

## Output Data Structure

### site_data_combined.csv

One row per site with the following columns:

| Column | Description | Source | Type |
|--------|-------------|--------|------|
| `plot_id` | Unique site identifier | All sources | ID |
| `latitude` | Site latitude | Ecoregions | Static |
| `longitude` | Site longitude | Ecoregions | Static |
| `realm` | Biogeographic realm | Ecoregions | Static |
| `biome` | WWF biome name | Ecoregions | Static |
| `ecoregion_id` | WWF ecoregion ID | Ecoregions | Static |
| `ecoregion` | WWF ecoregion name | Ecoregions | Static |
| `climate_zone` | Köppen climate code | Climate | Static |
| `climate_name` | Köppen climate name | Climate | Static |
| `sand_pct` | Sand content (%) | Soil | Static |
| `clay_pct` | Clay content (%) | Soil | Static |
| `silt_pct` | Silt content (%) | Calculated | Static |
| `skin_temp_mean` | Mean Annual Skin Temperature (K) | ERA5 2020 | Annual Mean |
| `skin_temp_sd` | Std dev of daily skin temp | ERA5 2020 | Variability |
| `snow_cover_mean` | Mean Annual Snow Cover (%) | ERA5 2020 | Annual Mean |
| `snow_cover_sd` | Std dev of daily snow cover | ERA5 2020 | Variability |
| `lai_high_veg_mean` | Mean Annual LAI High Vegetation | ERA5 2020 | Annual Mean |
| `lai_high_veg_sd` | Std dev of daily LAI high veg | ERA5 2020 | Variability |
| `lai_low_veg_mean` | Mean Annual LAI Low Vegetation | ERA5 2020 | Annual Mean |
| `lai_low_veg_sd` | Std dev of daily LAI low veg | ERA5 2020 | Variability |
| `total_evap_sum` | Annual Total Evaporation (mm/yr) | ERA5 2020 | Annual Sum |
| `dominant_lc` | Dominant Land Cover (2000-2022) | LULC | Categorical |

### site_landuse_proportions.csv

Detailed land use proportions for each site (optional, for detailed analysis):
- `plot_id`: Site identifier
- `land_class`: Land cover class name
- `mean_prop`: Mean proportion of site covered by this class (2000-2022)

---

## Plotting Transformations (02_plotting.R)

### Categorical Groupings for Visualization

#### Biome Grouping
Original WWF biomes are grouped into 7 major categories for clearer visualization:
- Desert (from Deserts & Xeric Shrublands)
- Mediterranean (from Mediterranean Forests, Woodlands & Scrub)
- Temperate Grassland, Shrubland & Savanna (from Montane Grasslands, Temperate Grasslands, Tundra)
- Tropical Grassland, Shrubland & Savanna (from Tropical Grasslands, Flooded Grasslands)
- Tropical Forest (from Tropical Dry/Moist Broadleaf, Tropical Coniferous, Mangroves)
- Temperate Forest (from Temperate Broadleaf & Mixed, Temperate Conifer)
- Boreal Forest (from Boreal Forests/Taiga)

#### Land Cover Grouping
Original land classes are merged for plotting (`dominant_lc_plot`):
- Bare Areas + Unconsolidated Bare Areas → **Bare**
- Marsh + Swamp → **Wetland**
- Grassland + Shrubland → **Grassland & Shrubland**
- All others remain unchanged

### Plot Types Generated

1. **Site Count Bar Charts**: Number of sites by biome, climate, and dominant land cover
2. **Land Cover Distribution**: Stacked bars showing dominant LC counts within each biome/climate
3. **Continuous Variable Boxplots**: Distribution of each variable by biome, climate, and land cover
4. **LAI Comparison**: High vs low vegetation LAI by grouping
5. **Ternary Soil Plots**: Sand-silt-clay composition colored by various factors
6. **Distribution Plots**: Histograms and density plots of all continuous variables
7. **Correlation Matrices**: Pairwise relationships colored by categorical groupings

---

## Variable Labels in Plots

| Variable | Plot Label |
|----------|-----------|
| `sand_pct` | Sand (%) |
| `silt_pct` | Silt (%) |
| `clay_pct` | Clay (%) |
| `skin_temp_mean` | Mean Annual Skin Temp (K) |
| `lai_high_veg_mean` | Mean Annual LAI High Veg |
| `lai_low_veg_mean` | Mean Annual LAI Low Veg |
| `snow_cover_mean` | Mean Annual Snow Cover (%) |
| `total_evap_sum` | Annual Total Evap (mm/yr) |
| `dominant_lc_plot` | Dominant Land Cover (2000-2022) |

---

## Notes

- **Year**: All ERA5 variables are from 2020 only (single year to reduce computation)
- **Sign Convention**: Evaporation is reported as positive values (mm of water lost)
- **Missing Data**: Sites with missing values for required variables are excluded from relevant plots
- **Boxplots**: Each point in a boxplot represents one site; the boxplot shows the distribution of site-level values within each category

---

## File Manifest

| File | Description |
|------|-------------|
| `site_data_combined.csv` | Main analysis dataset (one row per site) |
| `*.png` | Generated plots |
| `README.md` | This documentation |
