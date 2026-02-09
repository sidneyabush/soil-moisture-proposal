# Soil Moisture Proposal — Preliminary Figures

Exploratory data analysis and site characterization for NSF proposal.

## Data Sources

- **ERA5-Land** (via Google Earth Engine) — daily skin temperature, snow cover, LAI (high & low vegetation), and total evaporation for 2020
- **SoilGrids** — sand and clay percentages by basin
- **GLC Annual** — land use / land cover classes, 2000–2022
- **Köppen climate classification** — climate zones per site
- **WWF Ecoregions** — biome, realm, and ecoregion per site

Raw data lives on Box (`Hydrology_Lab/Soils_Proposal/data/`) for now - will likely be migrated to google drive for collaborating/ sharing later on.

## Scripts

Run these in order:

1. **`01_data_prep.R`** — reads all the raw CSVs, cleans and joins them into a single site-level dataset. ERA5 state variables get averaged across the year, evaporation gets summed. Land cover is simplified to a modal class across 2000–2022. Outputs go to the `outputs/` folder.

2. **`02_plotting.R`** — reads the prepped data and generates a bunch of exploratory plots: boxplots by biome/climate/land cover, ternary soil texture diagrams, distribution plots, correlation matrices, etc. Also saves to `outputs/`.

## Requirements

R packages: `tidyverse`, `janitor`, `fs`, `scales`, `viridis`, `ggtern`, `GGally`

## Outputs

Everything gets written to `outputs/`. Not tracked in git — re-run the scripts to regenerate.

- **`site_data_combined.csv`** — main dataset, one row per site with all environmental variables joined together
- **`landuse_annual.csv`** — cleaned annual land use data (one class per site per year)
- A bunch of PNGs: boxplots, ternary soil diagrams, distribution plots, correlation matrices, etc.

## Notes

- All ERA5 variables are from 2020 only (single year due to time limits in pulling data from GEE, more years will be added later)
- Evaporation is flipped to positive values (mm/yr of water lost) from ERA5's negative convention
- Land cover uses the modal (most frequent) class across 23 years rather than any single year
