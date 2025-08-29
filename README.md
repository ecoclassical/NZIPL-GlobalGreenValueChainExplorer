# NZIPL – Global Green Value Chain Explorer

Interactive Shiny app to explore green-technology trade:

- **Map**: country bubbles (color by Technology / Type / Stage) with filters, Top-10, and Trend.
- **Regions**: stacked exports by World Bank region.
- **Network**: BACI 2002 bilateral flows restricted to HS codes in the green dictionary.
- **Data**: green dictionary lookup.

## Quick Start (Local)

```r
# install deps
pkgs <- c("shiny","dplyr","readr","tidyr","ggplot2","sf","leaflet",
          "DT","visNetwork","rnaturalearth","janitor","countrycode","scales")
install.packages(setdiff(pkgs, rownames(installed.packages())))

# run
shiny::runApp("NetZeroValueChainExplorer")
```

## Project Tree 

NetZeroValueChainExplorer/
├── app.R
├── styles.css
├── modules/
│   ├── mod_map.R
│   ├── mod_regionplots.R
│   ├── mod_network.R
│   └── mod_dictionary.R
├── data/
│   ├── BACI_2002.RDS
│   ├── green_trade.RDS
│   ├── green_dictionary.csv
│   ├── product_codes_HS02_V202501.csv
│   └── country_codes_V202501.csv
└── aux/
    └── check_data.R
    
## Data Notes

- **Unilateral (Map/Regions)**: `green_trade.RDS` (2004–2023).
- **Bilateral (Network)**: `BACI_2002.RDS`, filtered to HS codes in `green_dictionary.csv`.
- **Regions**: added with `countrycode` (World Bank classification); centroids from `rnaturalearth`.

## Sanity Check Script

Run before deploying:

```r
source("aux/check_data.R")
```
    
## Deploy

```r
install.packages("rsconnect")
# setAccountInfo() once with your token/secret, then:
rsconnect::deployApp("NetZeroValueChainExplorer", appName = "nzip-ggvce")
```
