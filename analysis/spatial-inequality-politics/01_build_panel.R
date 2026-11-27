# Build a country-year panel (1992-2024) merging ntlgini spatial-inequality
# measures with political outcomes from V-Dem, democracyData, and
# peacesciencer, plus WDI controls.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(countrycode)
  library(peacesciencer)
  library(ntlgini)
})

here  <- file.path("analysis", "spatial-inequality-politics")
cache <- file.path(here, "cache")
ld    <- function(f) { e <- new.env(); load(file.path(cache, f), envir = e); get(ls(e)[1], e) }

# ---- 1. ntlgini predictors -------------------------------------------------
g <- as.data.frame(ntl_gini_global)
wide <- g |>
  filter(level %in% c("adm1", "adm2")) |>
  select(iso3, year, level, n_groups, total_gini, between_gini, within_gini,
         overlap_gini, between_share) |>
  pivot_wider(names_from = level,
              values_from = c(n_groups, total_gini, between_gini, within_gini,
                              overlap_gini, between_share))
base <- g |>
  filter(level == "adm1") |>
  transmute(iso3, year, n_pixels, sum_of_lights,
            area_km2 = n_pixels * 0.25,
            log_mean_rad = log(sum_of_lights / n_pixels + 1e-6))
ntl <- base |>
  left_join(wide, by = c("iso3", "year")) |>
  rename(total_gini = total_gini_adm1) |>
  select(-total_gini_adm2)

# ---- 2. V-Dem (v16) --------------------------------------------------------
vdem_vars <- c(
  # geographic (urban-rural) political inequality
  "v2xpe_exlgeo", "v2pepwrgeo", "v2peapsgeo", "v2clgeocl",
  # regime
  "v2x_polyarchy", "v2x_libdem", "v2x_civlib",
  # governance / state capacity
  "v2svstterr", "v2x_corr", "v2x_rule", "v2stfisccap",
  # contention
  "v2cagenmob", "v2caviol",
  # coups (Powell & Thyne, distributed with V-Dem)
  "e_pt_coup_attempts",
  # region
  "e_regionpol_6C"
)
vd <- ld("vdem.RData") |>
  filter(year >= 1990) |>
  transmute(iso3 = country_text_id, year, across(all_of(vdem_vars))) |>
  mutate(coup_attempt = as.integer(e_pt_coup_attempts > 0)) |>
  select(-e_pt_coup_attempts)

# ---- 3. democracyData ------------------------------------------------------
to_iso <- function(gw) {
  suppressWarnings(countrycode(gw, "gwn", "iso3c",
                               custom_match = c("340" = "SRB", "345" = "SRB",
                                                "347" = "XKX", "260" = "DEU",
                                                "817" = "VNM", "678" = "YEM")))
}
dd_prep <- function(df, ...) {
  df |>
    filter(year >= 1990, !is.na(GWn)) |>
    mutate(iso3 = to_iso(GWn)) |>
    filter(!is.na(iso3)) |>
    group_by(iso3, year) |>
    summarise(..., .groups = "drop")
}
polity <- dd_prep(ld("polity5.rda"), polity2 = mean(polity2, na.rm = TRUE))
fh     <- dd_prep(ld("fh.rda"), fh_total_rev = mean(fh_total_reversed, na.rm = TRUE))
uds    <- dd_prep(ld("extended_uds.rda"), uds_z1 = mean(z1, na.rm = TRUE))
lied   <- dd_prep(ld("LIED.rda"),
                  lexical_index = max(lexical_index, na.rm = TRUE),
                  dem_breakdown = max(democratic_breakdown, na.rm = TRUE))

# ---- 4. peacesciencer: UCDP armed conflict, ethnic fractionalization, terrain
ps <- suppressMessages(
  create_stateyears(system = "gw", subset_years = 1990:2024) |>
    add_ucdp_acd(type = "intrastate") |>
    add_creg_fractionalization() |>
    add_rugged_terrain()
) |>
  mutate(iso3 = to_iso(gwcode)) |>
  filter(!is.na(iso3)) |>
  group_by(iso3, year) |>
  summarise(conflict_incidence = max(ucdpongoing),
            conflict_onset = max(ucdponset),
            ethfrac = mean(ethfrac, na.rm = TRUE),
            rugged = mean(rugged, na.rm = TRUE),
            .groups = "drop") |>
  mutate(across(c(ethfrac, rugged), ~ ifelse(is.nan(.x), NA, .x)))
# carry ethnic fractionalization forward (CREG ends before 2024)
ps <- ps |> group_by(iso3) |> arrange(year) |> fill(ethfrac, rugged) |> ungroup()

# ---- 5. WDI controls -------------------------------------------------------
wdi <- readRDS(file.path(cache, "wdi.rds")) |>
  filter(region != "Aggregates", !is.na(iso3c), iso3c != "") |>
  transmute(iso3 = iso3c, year, log_gdppc = log(gdppc), log_pop = log(pop),
            urban = urb, rents)

# ---- 6. merge --------------------------------------------------------------
panel <- ntl |>
  inner_join(vd, by = c("iso3", "year")) |>     # V-Dem defines the universe of polities
  left_join(polity, by = c("iso3", "year")) |>
  left_join(fh,     by = c("iso3", "year")) |>
  left_join(uds,    by = c("iso3", "year")) |>
  left_join(lied,   by = c("iso3", "year")) |>
  left_join(ps,     by = c("iso3", "year")) |>
  left_join(wdi,    by = c("iso3", "year")) |>
  mutate(
    across(where(is.numeric), ~ ifelse(is.infinite(.x), NA, .x)),
    log_area   = log(area_km2),
    log_ngroups1 = log(n_groups_adm1),
    log_ngroups2 = log(n_groups_adm2),
    log_density = log_pop - log_area,
    region = factor(e_regionpol_6C),
    era = ifelse(year <= 2012, "DMSP-era (1992-2012)", "VIIRS-era (2013-2024)")
  ) |>
  arrange(iso3, year)

saveRDS(panel, file.path(here, "cache", "panel.rds"))
message(sprintf("Panel: %d country-years, %d countries, %d-%d",
                nrow(panel), n_distinct(panel$iso3),
                min(panel$year), max(panel$year)))
print(colSums(!is.na(panel))[c("total_gini", "between_gini_adm1", "between_gini_adm2",
                               "v2x_polyarchy", "v2xpe_exlgeo", "polity2", "fh_total_rev",
                               "uds_z1", "lexical_index", "conflict_incidence",
                               "coup_attempt", "log_gdppc", "urban", "ethfrac")])
