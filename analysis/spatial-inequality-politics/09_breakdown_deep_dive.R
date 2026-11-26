# What happens to light after democratic breakdowns?
#  A. Year-by-year paths: light vs GDP, and the light-GDP gap
#  B. Influence: drop one breakdown country at a time
#  C. Heterogeneity: era, resource rents, income, coup-led vs incumbent-led, region
#  D. Where the light goes: core (baseline-brightest ADM1) vs periphery
#  E. Alternative event definition (LIED democratic breakdown)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(fixest); library(ntlgini) })
here  <- file.path("analysis", "spatial-inequality-politics")
out   <- file.path(here, "output")
cache <- file.path(here, "cache")
panel <- readRDS(file.path(cache, "panel.rds"))

ve <- new.env(); load(file.path(cache, "vdem.RData"), envir = ve)
vd <- ve$vdem |> filter(year >= 1985) |>
  transmute(iso3 = country_text_id, year, v2x_regime, coup = as.integer(e_pt_coup > 0))
le <- new.env(); load(file.path(cache, "LIED.rda"), envir = le)
lied <- le$LIED |>
  filter(year >= 1985, !is.na(GWn)) |>
  mutate(iso3 = suppressWarnings(countrycode::countrycode(
    GWn, "gwn", "iso3c", custom_match = c("340" = "SRB", "345" = "SRB", "347" = "XKX",
                                          "260" = "DEU", "817" = "VNM", "678" = "YEM")))) |>
  filter(!is.na(iso3)) |>
  group_by(iso3, year) |> summarise(lied_bd = max(democratic_breakdown, na.rm = TRUE),
                                    lied_type = max(breakdown_type, na.rm = TRUE), .groups = "drop")
# LIED breakdown_type: 1 incumbent-induced regression, 2 military coup, 3 foreign occupation,
# 4 self-coup, 5 coup/civil conflict led by opposition, 6 coup headed by monarch

# ---- D-prep: core vs periphery light from ADM1 unit table -------------------
u <- as.data.frame(ntl_gini_units("adm1", quiet = TRUE))
core_id <- u |> filter(year %in% 1992:1994) |> group_by(iso3, gid) |>
  summarise(s = mean(value_share), .groups = "drop") |>
  group_by(iso3) |> slice_max(s, n = 1, with_ties = FALSE) |> select(iso3, core_gid = gid)
core <- u |> inner_join(core_id, by = "iso3") |> filter(gid == core_gid) |>
  transmute(iso3, year, core_share = value_share)

d <- panel |>
  select(iso3, year, region, sum_of_lights, log_pop, log_gdppc, rents) |>
  left_join(vd, by = c("iso3", "year")) |>
  left_join(core, by = c("iso3", "year")) |>
  mutate(log_lights   = 100 * log(sum_of_lights),
         log_gdppc100 = 100 * log_gdppc,
         gap_light_gdp = 100 * (log(sum_of_lights) - log_pop - log_gdppc),   # light per $ of GDP
         log_core      = 100 * log(sum_of_lights * core_share),
         log_periphery = 100 * log(sum_of_lights * (1 - core_share)),
         core_share100 = 100 * core_share)
outs <- c(log_lights = "Total light", log_gdppc100 = "GDP per capita",
          gap_light_gdp = "Light per capita minus GDP per capita",
          log_core = "Light in core region", log_periphery = "Light outside core region",
          core_share100 = "Core-region share of light (pp)")

# ---- events ---------------------------------------------------------------------
bd <- vd |> arrange(iso3, year) |> group_by(iso3) |>
  mutate(cand = v2x_regime <= 1 & lag(v2x_regime) >= 2,
         ok3 = sapply(seq_along(year), function(i) all(v2x_regime[i:min(i + 2, n())] <= 1, na.rm = TRUE))) |>
  summarise(event_year = suppressWarnings(min(year[cand & ok3 & year >= 1995 & year <= 2021])),
            ever_aut = any(v2x_regime <= 1, na.rm = TRUE), .groups = "drop") |>
  mutate(event_year = ifelse(is.infinite(event_year), NA, event_year))
always_dem <- bd$iso3[is.na(bd$event_year) & !bd$ever_aut]
bd <- bd |> filter(!is.na(event_year)) |>
  left_join(panel |> select(iso3, year, log_gdppc, rents), by = c("iso3", "event_year" = "year")) |>
  rowwise() |>
  mutate(coup_led = any(vd$coup[vd$iso3 == iso3 & abs(vd$year - event_year) <= 1] == 1, na.rm = TRUE)) |>
  ungroup()
med_inc <- median(bd$log_gdppc, na.rm = TRUE)
lied_ev <- lied |> group_by(iso3) |>
  summarise(event_year = suppressWarnings(min(year[lied_bd == 1 & year >= 1995 & year <= 2021])), .groups = "drop") |>
  filter(is.finite(event_year)) |>
  left_join(lied |> select(iso3, year, lied_type), by = c("iso3", "event_year" = "year")) |>
  mutate(lied_incumbent = lied_type %in% c(1, 4))
write.csv(lied_ev, file.path(out, "breakdown_events_lied.csv"), row.names = FALSE)
write.csv(bd, file.path(out, "breakdown_events.csv"), row.names = FALSE)

es <- function(treated, label, fe = "iso3 + region^year", ys = names(outs), controls = always_dem,
               window = c(-6, 10)) {
  dd <- d |> left_join(treated |> select(iso3, event_year), by = "iso3") |>
    filter(!is.na(event_year) | iso3 %in% controls) |>
    mutate(cohort = ifelse(is.na(event_year), 10000, event_year),
           rel = ifelse(cohort == 10000, NA, year - cohort)) |>
    filter(is.na(rel) | (rel >= window[1] & rel <= window[2]))
  bind_rows(lapply(ys, function(y) {
    dy <- dd[!is.na(dd[[y]]), ]
    m <- tryCatch(feols(as.formula(paste(y, "~ sunab(cohort, year) |", fe)), dy, vcov = ~iso3,
                        notes = FALSE, warn = FALSE), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    ct <- as.data.frame(coeftable(m)); ct <- ct[grepl("^year::", rownames(ct)), ]
    ag <- function(rx, nm) tryCatch(coeftable(summary(m, agg = setNames(rx, nm)))[nm, ], error = function(e) rep(NA, 4))
    a <- ag("year::[^-]", "ATT"); pr <- ag("year::-[2-9]", "PRE")
    tibble(sample = label, fe = fe, outcome = y, outcome_label = outs[[y]],
           rel = as.numeric(sub("year::", "", rownames(ct))), est = ct[, 1], se = ct[, 2],
           att = a[1], att_se = a[2], att_p = a[4], pre = pr[1], pre_p = pr[4],
           n_treated = n_distinct(dy$iso3[dy$cohort != 10000]))
  }))
}

# A + D: full sample, two FE structures
paths <- bind_rows(es(bd, "All breakdowns (RoW)", fe = "iso3 + year"),
                   es(bd, "All breakdowns (RoW)", fe = "iso3 + region^year"))
write.csv(paths, file.path(out, "breakdown_paths.csv"), row.names = FALSE)

# B: drop-one-country influence (total light, region-year FE)
jk <- bind_rows(lapply(bd$iso3, function(i) {
  r <- es(bd |> filter(iso3 != i), paste("drop", i), ys = "log_lights")
  tibble(dropped = i, att = r$att[1], att_p = r$att_p[1])
}))
write.csv(jk, file.path(out, "breakdown_jackknife.csv"), row.names = FALSE)

# C: heterogeneity (each subset vs the same always-democracy controls)
het_full <- bind_rows(
  es(bd |> filter(event_year <= 2011), "Event 1995-2011 (DMSP era)", ys = c("log_lights", "log_gdppc100")),
  es(bd |> filter(event_year >= 2012), "Event 2012-2021 (VIIRS era)", ys = c("log_lights", "log_gdppc100")),
  es(bd |> filter(rents < 5), "Resource rents < 5% GDP", ys = c("log_lights", "log_gdppc100")),
  es(bd |> filter(rents >= 5), "Resource rents >= 5% GDP", ys = c("log_lights", "log_gdppc100")),
  es(bd |> filter(log_gdppc < med_inc), "Below-median income", ys = c("log_lights", "log_gdppc100")),
  es(bd |> filter(log_gdppc >= med_inc), "Above-median income", ys = c("log_lights", "log_gdppc100")),
  es(bd |> filter(!coup_led), "Incumbent-led (no coup within 1 yr)", ys = c("log_lights", "log_gdppc100")),
  es(bd |> filter(coup_led), "Coup-led (4 events)", ys = c("log_lights", "log_gdppc100")),
  es(bd |> filter(!iso3 %in% c("GUY", "LBY", "PSE")), "Excl. Guyana, Libya, Palestine", ys = c("log_lights", "log_gdppc100")),
  es(lied_ev, "LIED democratic breakdowns", ys = c("log_lights", "log_gdppc100")),
  es(lied_ev |> filter(lied_incumbent), "LIED: incumbent-led (regression / self-coup)", ys = c("log_lights", "log_gdppc100")),
  es(lied_ev |> filter(!lied_incumbent), "LIED: coups & civil conflict", ys = c("log_lights", "log_gdppc100"))
)
write.csv(het_full, file.path(out, "breakdown_heterogeneity_paths.csv"), row.names = FALSE)
het <- het_full |> distinct(sample, outcome, att, att_se, att_p, pre, pre_p, n_treated)
write.csv(het, file.path(out, "breakdown_heterogeneity.csv"), row.names = FALSE)

options(width = 220)
cat("\n== ATTs (all breakdowns) ==\n")
print(paths |> distinct(fe, outcome_label, att, att_se, att_p, pre, pre_p, n_treated) |>
        mutate(across(where(is.double), ~ round(.x, 2))))
cat("\n== Path: region-year FE ==\n")
print(paths |> filter(fe == "iso3 + region^year") |> select(outcome, rel, est) |>
        mutate(est = round(est, 1)) |> pivot_wider(names_from = outcome, values_from = est) |> as.data.frame())
cat("\n== Jackknife (total light ATT, region-year FE) ==\n")
print(summary(jk$att)); print(jk |> arrange(att) |> slice(c(1:3, (n() - 2):n())))
cat("\n== Heterogeneity ==\n"); print(het |> mutate(across(where(is.double), ~ round(.x, 2))), n = 30)
