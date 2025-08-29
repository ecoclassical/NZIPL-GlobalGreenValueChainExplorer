## -------- Data diagnostics for NetZeroValueChainExplorer --------
suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(janitor); library(sf); library(rnaturalearth)
})

root     <- "NetZeroValueChainExplorer"
data_dir <- file.path(root, "data")

req <- c("green_dictionary.csv","product_codes_HS02_V202501.csv",
         "country_codes_V202501.csv","green_trade.RDS","BACI_2002.RDS")
missing <- req[!file.exists(file.path(data_dir, req))]
if (length(missing)) stop("Missing files in data/: ", paste(missing, collapse=", "))

say <- function(...) cat(paste0(..., "\n"))

## --- Load helpers
ne_xy <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_drop_geometry() |>
  transmute(country_code = iso_a3, lon = label_x, lat = label_y)

ok <- TRUE

## --- Dictionary
dic_raw <- read_csv(file.path(data_dir, "green_dictionary.csv"), show_col_types = FALSE) |>
  clean_names()

col_map <- list(
  hs_code    = c("hs_code","hs_code_index","hs_code_idx","hs_codeindex","code","hs_code_id","hs_codeid"),
  technology = c("technology","tech"),
  type       = c("type","category"),
  stage      = c("stage","upstream_manufacturing_downstream","upstream_downstream","umds")
)

pick_col <- function(df, keys) {
  nm <- names(df)
  hit <- intersect(keys, nm)
  if (length(hit)) df[[hit[1]]] else NA
}

dic <- tibble(
  hs_code    = pick_col(dic_raw, col_map$hs_code),
  technology = pick_col(dic_raw, col_map$technology),
  type       = pick_col(dic_raw, col_map$type),
  stage      = pick_col(dic_raw, col_map$stage)
) |>
  mutate(
    hs_code = sprintf("%06s", gsub("\\D", "", as.character(hs_code)))
  ) |>
  filter(!is.na(hs_code), !is.na(technology), !is.na(type))

if (nrow(dic)==0) {
  ok <- FALSE
  say("✗ DICTIONARY: could not map required columns (hs_code/technology/type).")
} else {
  say("✓ DICTIONARY: rows=", nrow(dic),
      " | tech=", length(unique(dic$technology)),
      " | type=", length(unique(dic$type)),
      " | stage(maybe NA)=", sum(is.na(dic$stage)))
}


## --- Product descriptions (optional)
prod_raw <- read_csv(file.path(data_dir, "product_codes_HS02_V202501.csv"), show_col_types = FALSE) |>
  clean_names()

prod <- prod_raw |>
  transmute(
    hs_code = sprintf("%06s", as.character(code)),
    description
  )

say("• PRODUCTS: rows=", nrow(prod), " | desc avail=", sum(!is.na(prod$description)))

## --- Countries (enrich with region)
library(countrycode)

cty <- read_csv(file.path(data_dir, "country_codes_V202501.csv"), show_col_types = FALSE) |>
  clean_names() |>
  mutate(
    country_code = as.character(country_code),   # keep as string
    country_code = stringr::str_pad(country_code, 3, pad="0")  # harmonize to 3 digits if needed
  )

# Add region (World Bank classification)
cty <- cty |>
  mutate(
    region = countrycode(country_iso3, origin = "iso3c", destination = "region")
  ) |> 
  mutate(
    region = ifelse(is.na(region), "Other", region)
  )

say("• COUNTRIES: rows=", nrow(cty),
    " | regions=", length(unique(cty$region)),
    " | missing regions=", sum(is.na(cty$region)))

## --- Unilateral trade
uni_raw <- readRDS(file.path(data_dir, "green_trade.RDS")) |>
  as_tibble() |>
  clean_names() |>
  mutate(country_code = toupper(country_code)) |>
  filter(country_code %in% valid_iso3)   # <-- new

## OPTIONAL: unique dictionary rows per category tuple
dic_unique <- dic |> distinct(hs_code, technology, type, stage)

## Prepare distinct country meta and NE centroids
cty_uni <- cty |> distinct(country_iso3, .keep_all = TRUE)

# Natural Earth centroids (already built earlier as ne_xy with country_code = iso_a3)
ne_uni  <- ne_xy |> distinct(country_code, .keep_all = TRUE)
valid_iso3 <- unique(ne_uni$country_code)

## Filter unilateral trade to ISO3 present in NE (avoids lon/lat NA)
uni_raw <- readRDS(file.path(data_dir, "green_trade.RDS")) |>
  as_tibble() |>
  clean_names() |>
  mutate(country_code = toupper(country_code)) |>
  filter(country_code %in% valid_iso3)

## Build unilateral with explicit many-to-many on the dictionary
uni <- uni_raw |>
  mutate(
    hs_code = sprintf("%06s", as.character(product_code))
  ) |>
  inner_join(dic_unique, by = "hs_code", relationship = "many-to-many") |>
  # region via ISO3
  left_join(cty_uni |> select(country_iso3, region),
            by = c("country_code" = "country_iso3")) |>
  # centroids via ISO3
  left_join(ne_uni |> rename(country_iso3 = country_code),
            by = c("country_code" = "country_iso3")) |>
  # optional: ensure every row has a region bucket
  mutate(region = ifelse(is.na(region), "Other", region))

# Summaries
yrs <- sort(unique(uni$year))
say("✓ GREEN_TRADE: rows=", nrow(uni),
    " | years=", paste(range(yrs), collapse="–"),
    " | tech=", length(unique(uni$technology)),
    " | type=", length(unique(uni$type)),
    " | stage=", length(unique(uni$stage)),
    " | region NA=", sum(is.na(uni$region)),
    " | lon/lat NA=", sum(is.na(uni$lon) | is.na(uni$lat)))

# Aggregated frame (as used by the app)
uni_chk <- uni |>
  group_by(year, country_code, technology, type, stage, region) |>
  summarise(value = sum(.data[[ if ("export_value" %in% names(uni)) "export_value" else "value" ]], na.rm = TRUE),
            .groups="drop") |>
  arrange(desc(value))
say("• GREEN_TRADE agg sample:", paste(utils::head(uni_chk$value, 3), collapse=", "))

## --- BACI 2002 (bilateral)
baci_raw <- readRDS(file.path(data_dir, "BACI_2002.RDS")) |>
  as_tibble() |>
  clean_names() |>
  mutate(
    hs_code  = sprintf("%06s", as.character(product_code)),
    exporter = toupper(exporter),
    importer = toupper(importer)
  )

need_baci <- c("exporter","importer","product_code","value")
if (!all(need_baci %in% names(baci_raw))) {
  ok <- FALSE
  say("✗ BACI_2002: missing columns: ",
      paste(setdiff(need_baci, names(baci_raw)), collapse=", "))
} else {
  baci <- baci_raw |>
    # keep only HS codes in green dictionary
    semi_join(dic_unique, by = "hs_code") |>
    inner_join(dic_unique, by = "hs_code", relationship = "many-to-many") |>
    # filter to valid ISO3s (Natural Earth known countries)
    filter(exporter %in% valid_iso3, importer %in% valid_iso3) |>
    # add regions
    left_join(cty_uni |> select(country_iso3, region),
              by = c("exporter" = "country_iso3")) |>
    rename(region_exporter = region) |>
    left_join(cty_uni |> select(country_iso3, region),
              by = c("importer" = "country_iso3")) |>
    rename(region_importer = region)
  
  say("✓ BACI_2002: rows=", nrow(baci),
      " | exporters=", length(unique(baci$exporter)),
      " | importers=", length(unique(baci$importer)),
      " | tech=", length(unique(baci$technology)),
      " | region_exporter NA=", sum(is.na(baci$region_exporter)),
      " | region_importer NA=", sum(is.na(baci$region_importer)))
}

if (ok) say("ALL CHECKS PASSED ✔") else say("CHECKS FAILED – see messages above ❗")
## ---------------------------------------------------------------------------