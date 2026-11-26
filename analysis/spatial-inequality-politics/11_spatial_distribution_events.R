# Spatial distribution of light after democratization and autocratization.
#  A. Country-level event studies (Sun & Abraham), no time-varying controls,
#     country + region-by-year FE. Outcomes: Gini components (x100), core-region
#     share and dark-tercile share of national light (pp).
#  B. ADM1-unit triple difference: do initially dark regions gain light
#     relative to initially bright regions in the same country after the event?
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(fixest); library(ntlgini) })
here  <- file.path("analysis", "spatial-inequality-politics")
out   <- file.path(here, "output")
cache <- file.path(here, "cache")
panel <- readRDS(file.path(cache, "panel.rds"))

# ---- events (RoW first sustained crossings; LIED as robustness) ---------------
ve <- new.env(); load(file.path(cache, "vdem.RData"), envir = ve)
vd <- ve$vdem |> filter(year >= 1985) |> transmute(iso3 = country_text_id, year, v2x_regime)
crossing <- function(up) {
  vd |> arrange(iso3, year) |> group_by(iso3) |>
    mutate(cand = if (up) v2x_regime >= 2 & lag(v2x_regime) <= 1 else v2x_regime <= 1 & lag(v2x_regime) >= 2,
           ok3 = sapply(seq_along(year), function(i) {
             w <- v2x_regime[i:min(i + 2, n())]; if (up) all(w >= 2, na.rm = TRUE) else all(w <= 1, na.rm = TRUE) })) |>
    summarise(event_year = suppressWarnings(min(year[cand & ok3 & year >= 1995 & year <= 2021])),
              ever_dem = any(v2x_regime >= 2, na.rm = TRUE), ever_aut = any(v2x_regime <= 1, na.rm = TRUE),
              .groups = "drop") |>
    mutate(event_year = ifelse(is.infinite(event_year), NA, event_year))
}
up <- crossing(TRUE); dn <- crossing(FALSE)
always_aut <- up$iso3[is.na(up$event_year) & !up$ever_dem]
always_dem <- dn$iso3[is.na(dn$event_year) & !dn$ever_aut]
le <- new.env(); load(file.path(cache, "LIED.rda"), envir = le)
lied <- le$LIED |> filter(year >= 1995, year <= 2021, !is.na(GWn)) |>
  mutate(iso3 = suppressWarnings(countrycode::countrycode(
    GWn, "gwn", "iso3c", custom_match = c("340" = "SRB", "345" = "SRB", "347" = "XKX",
                                          "260" = "DEU", "817" = "VNM", "678" = "YEM")))) |>
  filter(!is.na(iso3))
lied_first <- function(var) lied |> filter(.data[[var]] == 1) |> group_by(iso3) |>
  summarise(event_year = min(year), .groups = "drop")
events <- list(
  "Democratization (RoW)"  = list(ev = up |> filter(!is.na(event_year)), ctrl = always_aut),
  "Autocratization (RoW)"  = list(ev = dn |> filter(!is.na(event_year)), ctrl = always_dem),
  "Democratization (LIED)" = list(ev = lied_first("democratic_transition"), ctrl = always_aut),
  "Autocratization (LIED)" = list(ev = lied_first("democratic_breakdown"), ctrl = always_dem)
)

# ---- ADM1 unit data: baseline brightness terciles within country --------------
u <- as.data.frame(ntl_gini_units("adm1", quiet = TRUE)) |>
  mutate(unit_light = mean_radiance * n_pixels)
base <- u |> filter(year %in% 1992:1994) |> group_by(iso3, gid) |>
  summarise(base_rad = mean(mean_radiance), base_share = mean(value_share), .groups = "drop") |>
  group_by(iso3) |>
  filter(n() >= 3) |>
  mutate(tercile = ntile(base_rad, 3),                         # 1 = darkest third of regions
         is_core = base_share == max(base_share)) |>
  ungroup()
shares <- u |> inner_join(base, by = c("iso3", "gid")) |>
  group_by(iso3, year) |>
  summarise(core_share = 100 * sum(value_share[is_core]),
            dark_share = 100 * sum(value_share[tercile == 1]),
            bright_share = 100 * sum(value_share[tercile == 3]), .groups = "drop")

d <- panel |>
  select(iso3, year, region, total_gini, between_gini_adm1, within_gini_adm1,
         overlap_gini_adm1, between_gini_adm2) |>
  mutate(across(c(total_gini, between_gini_adm1, within_gini_adm1, overlap_gini_adm1, between_gini_adm2),
                ~ 100 * .x)) |>
  left_join(shares, by = c("iso3", "year"))
outs <- c(total_gini = "Total Gini", between_gini_adm1 = "Between-region Gini (ADM1)",
          within_gini_adm1 = "Within-region Gini (ADM1)", overlap_gini_adm1 = "Overlap (ADM1)",
          between_gini_adm2 = "Between-region Gini (ADM2)",
          core_share = "Core-region share (pp)", dark_share = "Darkest-third regions' share (pp)",
          bright_share = "Brightest-third regions' share (pp)")

# ---- A. country-level event studies -------------------------------------------
es <- function(ev, ctrl, label, fe) {
  dd <- d |> left_join(ev |> select(iso3, event_year), by = "iso3") |>
    filter(!is.na(event_year) | iso3 %in% ctrl) |>
    mutate(cohort = ifelse(is.na(event_year), 10000, event_year),
           rel = ifelse(cohort == 10000, NA, year - cohort)) |>
    filter(is.na(rel) | (rel >= -6 & rel <= 10))
  bind_rows(lapply(names(outs), function(y) {
    dy <- dd[!is.na(dd[[y]]), ]
    m <- tryCatch(feols(as.formula(paste(y, "~ sunab(cohort, year) |", fe)), dy, vcov = ~iso3,
                        notes = FALSE, warn = FALSE), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    ct <- as.data.frame(coeftable(m)); ct <- ct[grepl("^year::", rownames(ct)), ]
    ag <- function(rx, nm) tryCatch(coeftable(summary(m, agg = setNames(rx, nm)))[nm, ], error = function(e) rep(NA, 4))
    a <- ag("year::[^-]", "ATT"); e5 <- ag("year::[0-4]:", "EARLY"); l5 <- ag("year::[5-9]:", "LATE")   # years +5..+9
    pr <- ag("year::-[2-9]", "PRE")
    tibble(event = label, fe = fe, outcome = y, outcome_label = outs[[y]],
           rel = as.numeric(sub("year::", "", rownames(ct))), est = ct[, 1], se = ct[, 2],
           att = a[1], att_se = a[2], att_p = a[4], early = e5[1], early_p = e5[4],
           late = l5[1], late_p = l5[4], pre = pr[1], pre_p = pr[4],
           base_mean = mean(dy[[y]][dy$cohort != 10000 & dy$rel < 0], na.rm = TRUE),
           n_treated = n_distinct(dy$iso3[dy$cohort != 10000]), n_control = n_distinct(dy$iso3[dy$cohort == 10000]))
  }))
}
A <- bind_rows(lapply(names(events), function(nm) bind_rows(
  es(events[[nm]]$ev, events[[nm]]$ctrl, nm, "iso3 + region^year"),
  es(events[[nm]]$ev, events[[nm]]$ctrl, nm, "iso3 + year"))))
write.csv(A, file.path(out, "spatial_events_country.csv"), row.names = FALSE)

# ---- B. ADM1-unit triple difference ---------------------------------------------
# log unit light ~ treated x period x tercile, with unit FE, country-by-year FE
# (absorbs every national shock incl. growth) and tercile-by-year FE (absorbs
# global convergence of dark regions). Bright tercile is the reference.
ud <- u |> inner_join(base |> select(iso3, gid, tercile, is_core), by = c("iso3", "gid")) |>
  filter(unit_light > 0) |>
  mutate(log_light = 100 * log(unit_light), terc = factor(tercile, 1:3, c("dark", "middle", "bright")))
ddd <- function(ev, ctrl, label) {
  dd <- ud |> left_join(ev |> select(iso3, event_year), by = "iso3") |>
    filter(!is.na(event_year) | iso3 %in% ctrl) |>
    mutate(rel = year - event_year,
           period = case_when(is.na(event_year) ~ "never",
                              rel < -6 | rel > 10 ~ "out",
                              rel <= -2 ~ "pre (-6..-2)", rel <= -1 ~ "ref",
                              rel <= 4 ~ "early (0..4)", TRUE ~ "late (5..10)")) |>
    filter(period != "out") |>
    mutate(dark = as.numeric(terc == "dark"), middle = as.numeric(terc == "middle"),
           pre_dark = (period == "pre (-6..-2)") * dark, pre_middle = (period == "pre (-6..-2)") * middle,
           early_dark = (period == "early (0..4)") * dark, early_middle = (period == "early (0..4)") * middle,
           late_dark = (period == "late (5..10)") * dark, late_middle = (period == "late (5..10)") * middle)
  m <- feols(log_light ~ pre_dark + pre_middle + early_dark + early_middle + late_dark + late_middle |
               iso3^gid + iso3^year + terc^year, dd, vcov = ~iso3, notes = FALSE)
  ct <- as.data.frame(coeftable(m))
  tibble(event = label, term = rownames(ct), est = ct[, 1], se = ct[, 2], p = ct[, 4],
         n_units = n_distinct(paste(dd$iso3, dd$gid)), n_treated = n_distinct(dd$iso3[!is.na(dd$event_year)]))
}
B <- bind_rows(lapply(names(events), function(nm) ddd(events[[nm]]$ev, events[[nm]]$ctrl, nm)))
write.csv(B, file.path(out, "spatial_events_unit_ddd.csv"), row.names = FALSE)

options(width = 230)
cat("\n== A. Country-level ATTs (region-by-year FE) ==\n")
print(A |> filter(fe == "iso3 + region^year") |>
        distinct(event, outcome_label, base_mean, att, att_se, att_p, early, early_p, late, late_p, pre_p, n_treated) |>
        mutate(across(where(is.double), ~ round(.x, 2))), n = 40)
cat("\n== A. same, country + year FE: ATT and p only ==\n")
print(A |> filter(fe == "iso3 + year") |> distinct(event, outcome, att, att_p, pre_p) |>
        mutate(cell = sprintf("%+.2f (p=%.2f, pre p=%.2f)", att, att_p, pre_p)) |>
        select(event, outcome, cell) |> pivot_wider(names_from = event, values_from = cell) |> as.data.frame())
cat("\n== B. Unit-level DDD (log light x100, dark/middle vs bright regions) ==\n")
print(B |> mutate(across(where(is.double), ~ round(.x, 2))), n = 40)
