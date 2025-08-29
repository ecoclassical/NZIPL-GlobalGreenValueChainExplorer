# app.R
library(shiny)
library(dplyr)
library(stringr)
library(readr)
library(tidyr)
library(ggplot2)
library(sf)
library(leaflet)
library(DT)
library(visNetwork)
library(rnaturalearth)
library(janitor)
library(countrycode)

source("modules/mod_map.R")
source("modules/mod_regionplots.R")
source("modules/mod_network.R")
source("modules/mod_dictionary.R")

ui <- navbarPage(
  "NZIPL – Global Green Value-Chain Explorer",
  id = "top",
  header = tags$head(includeCSS("styles.css")),
  tabPanel("Map",     mod_map_ui("maptab")),
  tabPanel("Regions", mod_regionplots_ui("regions")),
  tabPanel("Network", mod_network_ui("network")),
  tabPanel("Data",    mod_dictionary_ui("dict")),
  tabPanel("About",
           fluidPage(
             h2("About this app"),
             HTML("<p>This explorer visualizes green-tech trade & dictionary mappings. Data sources:
      unilateral green trade (RDS), BACI 2002 (bilateral), green dictionary, HS product and country lookup.</p>"),
             tags$pre("
Project tree
.
├── app.R
├── styles.css
├── modules/
│   ├── mod_map.R
│   ├── mod_regionplots.R
│   ├── mod_network.R
│   └── mod_dictionary.R
└── data/
    ├── BACI_2002.RDS
    ├── green_trade.RDS
    ├── green_dictionary.csv
    ├── product_codes_HS02_V202501.csv
    └── country_codes_V202501.csv
")
           )
  )
)

server <- function(input, output, session) {
  
  # -------- Load & standardize once -----------------------------------------
  
  # Dictionary (robust-ish to minor header variants)
  dic <- read_csv("data/green_dictionary.csv", show_col_types = FALSE) |>
    clean_names()
  
  # Map common variants -> normalized columns
  col_map <- list(
    hs_code    = c("hs_code","hs_code_index","hs_code_idx","hs_codeindex","code","hs_code_id","hs_codeid"),
    technology = c("technology","tech"),
    type       = c("type","category"),
    stage      = c("stage","upstream_manufacturing_downstream","upstream_downstream","umds")
  )
  pick_col <- function(df, keys) { hit <- intersect(keys, names(df)); if (length(hit)) df[[hit[1]]] else NA }
  
  dic <- tibble(
    hs_code    = pick_col(dic, col_map$hs_code),
    technology = pick_col(dic, col_map$technology),
    type       = pick_col(dic, col_map$type),
    stage      = pick_col(dic, col_map$stage)
  ) |>
    mutate(hs_code = sprintf("%06s", gsub("\\D", "", as.character(hs_code)))) |>
    filter(!is.na(hs_code), !is.na(technology), !is.na(type))
  
  dic_unique <- dic |> distinct(hs_code, technology, type, stage)
  
  # Product descriptions (optional)
  hs_desc <- read_csv("data/product_codes_HS02_V202501.csv", show_col_types = FALSE) |>
    clean_names() |>
    transmute(hs_code = sprintf("%06s", as.character(code)), description)
  
  # Country meta (add region using ISO3)
  countries <- read_csv("data/country_codes_V202501.csv", show_col_types = FALSE) |>
    clean_names() |>
    mutate(
      country_code = as.character(country_code),
      country_code = stringr::str_pad(country_code, 3, pad = "0"),
      region = countrycode(country_iso3, origin = "iso3c", destination = "region")
    )
  countries$region[is.na(countries$region)] <- "Other"
  cty_uni <- countries |> distinct(country_iso3, .keep_all = TRUE)
  
  # Natural Earth centroids (avoid sf warnings; use label_x/label_y)
  world_xy <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
    sf::st_drop_geometry() |>
    transmute(country_code = iso_a3, lon = label_x, lat = label_y)
  ne_uni <- world_xy |> distinct(country_code, .keep_all = TRUE)
  valid_iso3 <- unique(ne_uni$country_code)
  
  # Unilateral trade (filter to valid ISO3; join categories, regions, centroids)
  uni_raw <- readRDS("data/green_trade.RDS") |>
    as_tibble() |>
    clean_names() |>
    mutate(country_code = toupper(country_code)) |>
    filter(country_code %in% valid_iso3)
  
  uni <- uni_raw |>
    mutate(hs_code = sprintf("%06s", as.character(product_code))) |>
    inner_join(dic_unique, by = "hs_code", relationship = "many-to-many") |>
    left_join(cty_uni |> select(country_iso3, region),
              by = c("country_code" = "country_iso3")) |>
    left_join(ne_uni |> rename(country_iso3 = country_code),
              by = c("country_code" = "country_iso3")) |>
    group_by(year, country_code, country_name, technology, type, stage, region, lon, lat) |>
    summarise(value = sum(if ("export_value" %in% names(uni_raw)) export_value else value,
                          na.rm = TRUE),
              .groups = "drop")
  
  # Bilateral (2002 only) — restrict to green HS codes, valid ISO3, add regions
  baci_raw <- readRDS("data/BACI_2002.RDS") |>
    as_tibble() |>
    clean_names() |>
    mutate(
      hs_code  = sprintf("%06s", as.character(product_code)),
      exporter = toupper(exporter),
      importer = toupper(importer)
    )
  
  baci2002 <- baci_raw |>
    semi_join(dic_unique, by = "hs_code") |>
    inner_join(dic_unique, by = "hs_code", relationship = "many-to-many") |>
    filter(exporter %in% valid_iso3, importer %in% valid_iso3) |>
    left_join(cty_uni |> select(country_iso3, region),
              by = c("exporter" = "country_iso3")) |>
    rename(region_exporter = region) |>
    left_join(cty_uni |> select(country_iso3, region),
              by = c("importer" = "country_iso3")) |>
    rename(region_importer = region)
  
  # Shared objects for modules
  shared <- reactiveValues(
    sd = uni,
    dictionary  = dic |> left_join(hs_desc, by = "hs_code"),
    country_meta= countries,
    baci2002    = baci2002,
    tech_levels = sort(unique(uni$technology)),
    type_levels = sort(unique(uni$type)),
    stage_levels= sort(unique(uni$stage)),
    region_levels = sort(unique(uni$region))
  )
  
  # -------- Modules ----------------------------------------------------------
  mod_map_server("maptab",    shared = shared)
  mod_regionplots_server("regions", shared = shared)
  mod_network_server("network", shared = shared)    # fixed to 2002
  mod_dictionary_server("dict", shared = shared)
}

shinyApp(ui, server)