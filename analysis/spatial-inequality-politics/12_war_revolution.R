# Effects of civil wars and revolutions on the level and spatial distribution of light.
# Events (first qualifying event per country, 1995-2021 so every event has >= 3
# pre-years of light):
#   - Civil-conflict onset: UCDP/PRIO intrastate conflict (>= 25 battle deaths)
#     after >= 3 conflict-free years            [peacesciencer::ucdp_acd]
#   - Civil-war onset: first year at war intensity (>= 1,000 deaths) after
#     >= 3 years without war
#   - Conflict termination: first conflict-free year after >= 2 conflict years,
#     peace lasting >= 3 years
#   - Revolution (popular uprising): regime ended by "popular uprising"
#     (V-Dem v2regendtype = 8); event = first year of the successor regime
#   - Revolution (armed): regime ended by "loss in civil war" (v2regendtype = 5)
#   - Robustness: leader removed by popular protest (Archigos, through 2015)
# Comparison groups: never-conflict countries (1989-2024) for conflict events;
# countries with no regime change 1985-2024 for revolutions.
# Sun & Abraham (2021) estimator, country + region-by-year FE, no time-varying
# controls; SEs clustered by country.
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(fixest); library(ntlgini); library(peacesciencer) })
here  <- file.path("analysis", "spatial-inequality-politics")
out   <- file.path(here, "output")
cache <- file.path(here, "cache")
panel <- readRDS(file.path(cache, "panel.rds"))
to_iso <- function(gw) suppressWarnings(countrycode::countrycode(
  gw, "gwn", "iso3c", custom_match = c("340" = "SRB", "345" = "SRB", "347" = "XKX",
                                       "260" = "DEU", "817" = "VNM", "678" = "YEM")))

# ---- conflict events -----------------------------------------------------------
ucdp <- suppressMessages(create_stateyears(system = "gw", subset_years = 1985:2024) |>
                           add_ucdp_acd(type = "intrastate")) |>
  mutate(iso3 = to_iso(gwcode)) |> filter(!is.na(iso3)) |>
  group_by(iso3, year) |>
  summarise(conf = max(ucdpongoing), war = as.integer(max(maxintensity, na.rm = TRUE) == 2), .groups = "drop") |>
  mutate(war = ifelse(is.na(war) | is.infinite(war), 0L, war))
first_event <- function(df, flag) df |> filter(flag) |> group_by(iso3) |>
  summarise(event_year = min(year), .groups = "drop")
u_l <- ucdp |> arrange(iso3, year) |> group_by(iso3) |>
  mutate(conf_prev3 = lag(conf, 1) + lag(conf, 2) + lag(conf, 3),
         war_prev3  = lag(war, 1) + lag(war, 2) + lag(war, 3),
         conf_prev2 = lag(conf, 1) + lag(conf, 2),
         peace_next3 = (conf == 0) & (lead(conf, 1, default = 0) == 0) & (lead(conf, 2, default = 0) == 0)) |>
  ungroup() |> filter(year >= 1995, year <= 2021)
ev_conf <- first_event(u_l, u_l$conf == 1 & u_l$conf_prev3 == 0)
ev_war  <- first_event(u_l, u_l$war == 1 & u_l$war_prev3 == 0)
ev_end  <- first_event(u_l, u_l$peace_next3 & u_l$conf_prev2 == 2)
never_conf <- ucdp |> filter(year >= 1989) |> group_by(iso3) |> summarise(any = any(conf == 1)) |>
  filter(!any) |> pull(iso3)

# ---- revolution events ---------------------------------------------------------
ve <- new.env(); load(file.path(cache, "vdem.RData"), envir = ve)
rg <- ve$vdem |> filter(year >= 1985) |>
  transmute(iso3 = country_text_id, year, regid = v2regidnr, endtype = v2regendtype) |>
  arrange(iso3, year) |> group_by(iso3) |>
  mutate(prev_end = lag(endtype), new_regime = !is.na(lag(regid)) & regid != lag(regid)) |> ungroup()
rev_up   <- first_event(rg |> filter(year >= 1995, year <= 2021), with(rg |> filter(year >= 1995, year <= 2021), new_regime & prev_end == 8))
rev_arm  <- first_event(rg |> filter(year >= 1995, year <= 2021), with(rg |> filter(year >= 1995, year <= 2021), new_regime & prev_end == 5))
stable   <- rg |> group_by(iso3) |> summarise(ch = any(new_regime, na.rm = TRUE)) |> filter(!ch) |> pull(iso3)
arch <- archigos |>
  filter(grepl("^Popular Protest", exitcode)) |>
  mutate(iso3 = to_iso(gwcode), year = as.numeric(format(enddate, "%Y"))) |>
  filter(!is.na(iso3), year >= 1995, year <= 2015) |>
  group_by(iso3) |> summarise(event_year = min(year), .groups = "drop")

events <- list(
  "Civil-conflict onset (UCDP)"    = list(ev = ev_conf, ctrl = never_conf),
  "Civil-war onset (1,000+ deaths)" = list(ev = ev_war,  ctrl = never_conf),
  "Conflict termination"           = list(ev = ev_end,  ctrl = never_conf),
  "Revolution: popular uprising (V-Dem)" = list(ev = rev_up,  ctrl = stable),
  "Revolution: armed (V-Dem, rebel victory)" = list(ev = rev_arm, ctrl = stable),
  "Leader ousted by popular protest (Archigos)" = list(ev = arch, ctrl = stable)
)
ev_tab <- bind_rows(lapply(names(events), function(nm) events[[nm]]$ev |> mutate(event = nm)))
write.csv(ev_tab, file.path(out, "war_revolution_events.csv"), row.names = FALSE)

# ---- outcomes ------------------------------------------------------------------
u <- as.data.frame(ntl_gini_units("adm1", quiet = TRUE))
base <- u |> filter(year %in% 1992:1994) |> group_by(iso3, gid) |>
  summarise(base_rad = mean(mean_radiance), base_share = mean(value_share), .groups = "drop") |>
  group_by(iso3) |> filter(n() >= 3) |>
  mutate(tercile = ntile(base_rad, 3), is_core = base_share == max(base_share)) |> ungroup()
shares <- u |> inner_join(base, by = c("iso3", "gid")) |> group_by(iso3, year) |>
  summarise(core_share = 100 * sum(value_share[is_core]),
            dark_share = 100 * sum(value_share[tercile == 1]), .groups = "drop")
d <- panel |>
  select(iso3, year, region, sum_of_lights, log_pop, log_gdppc, total_gini, between_gini_adm1, within_gini_adm1) |>
  left_join(shares, by = c("iso3", "year")) |>
  mutate(log_lights = 100 * log(sum_of_lights),
         log_gdppc100 = 100 * log_gdppc,
         log_core = 100 * log(sum_of_lights * core_share / 100),
         log_periph = 100 * log(sum_of_lights * (1 - core_share / 100)),
         across(c(total_gini, between_gini_adm1, within_gini_adm1), ~ 100 * .x))
outs <- c(log_lights = "Total light (log x100)", log_gdppc100 = "GDP per capita (log x100)",
          log_core = "Light in core region (log x100)", log_periph = "Light outside core (log x100)",
          core_share = "Core-region share (pp)", dark_share = "Darkest-third share (pp)",
          total_gini = "Total Gini (x100)", between_gini_adm1 = "Between-region Gini ADM1 (x100)",
          within_gini_adm1 = "Within-region Gini ADM1 (x100)")

es <- function(ev, ctrl, label, fe = "iso3 + region^year") {
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
    a <- ag("year::[^-]", "ATT"); e5 <- ag("year::[0-4]:", "EARLY"); l5 <- ag("year::[5-9]:", "LATE")
    pr <- ag("year::-[2-9]", "PRE")
    tibble(event = label, fe = fe, outcome = y, outcome_label = outs[[y]],
           rel = as.numeric(sub("year::", "", rownames(ct))), est = ct[, 1], se = ct[, 2],
           att = a[1], att_se = a[2], att_p = a[4], early = e5[1], early_p = e5[4],
           late = l5[1], late_p = l5[4], pre = pr[1], pre_p = pr[4],
           n_treated = n_distinct(dy$iso3[dy$cohort != 10000]), n_control = n_distinct(dy$iso3[dy$cohort == 10000]))
  }))
}
R <- bind_rows(lapply(names(events), function(nm) bind_rows(
  es(events[[nm]]$ev, events[[nm]]$ctrl, nm),
  es(events[[nm]]$ev, events[[nm]]$ctrl, nm, fe = "iso3 + year"))))
write.csv(R, file.path(out, "war_revolution_event_study.csv"), row.names = FALSE)

options(width = 230)
cat("\n== Events ==\n"); print(ev_tab |> group_by(event) |> summarise(n = n(), cases = paste(paste0(iso3, " ", event_year), collapse = ", ")) |> as.data.frame())
cat("\n== ATTs, region-by-year FE ==\n")
print(R |> filter(fe == "iso3 + region^year") |>
        distinct(event, outcome, att, att_se, att_p, early, early_p, late, late_p, pre_p, n_treated, n_control) |>
        mutate(across(where(is.double), ~ round(.x, 2))), n = 80)

# ---- influence: drop one event country at a time (region-by-year FE) -------------
jk_one <- function(evname, ys) {
  ev <- events[[evname]]
  bind_rows(lapply(ev$ev$iso3, function(i) {
    r <- es(ev$ev |> filter(iso3 != i), ev$ctrl, evname)
    r |> filter(outcome %in% ys) |> distinct(outcome, att, att_p, late, late_p) |> mutate(dropped = i)
  })) |> mutate(event = evname)
}
JK <- bind_rows(
  jk_one("Revolution: popular uprising (V-Dem)", c("core_share", "between_gini_adm1")),
  jk_one("Conflict termination", c("log_lights", "between_gini_adm1")),
  jk_one("Civil-war onset (1,000+ deaths)", c("log_lights", "log_core"))
)
write.csv(JK, file.path(out, "war_revolution_jackknife.csv"), row.names = FALSE)
cat("\n== Jackknife ranges ==\n")
print(JK |> group_by(event, outcome) |>
        summarise(att_min = min(att), att_max = max(att), share_p05 = mean(att_p < .05),
                  late_min = min(late), late_max = max(late), share_late_p05 = mean(late_p < .05),
                  most_influential = dropped[which.max(abs(att - median(att)))], .groups = "drop") |>
        mutate(across(where(is.double), ~ round(.x, 2))))
