# Screen: which political events/processes significantly change the spatial
# distribution of light?
# Same design for every event type: first qualifying event per country in
# 1995-2021, Sun & Abraham (2021) estimator, country + region-by-year FE,
# no time-varying controls, SEs clustered by country, window -6..+10.
# Outcomes: six spatial-inequality measures. Selection rule for "significant":
#   BH-adjusted q < .05 across the whole screen (events x outcomes x {ATT, years 5-9}),
#   pre-trend p > .10, and p < .05 in >= 80% of drop-one-country re-estimates.
# Election cycles (repeated events) are estimated separately as distributed leads/lags.
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(fixest); library(ntlgini); library(peacesciencer) })
here  <- file.path("analysis", "spatial-inequality-politics")
out   <- file.path(here, "output")
cache <- file.path(here, "cache")
panel <- readRDS(file.path(cache, "panel.rds"))
to_iso <- function(gw) suppressWarnings(countrycode::countrycode(
  gw, "gwn", "iso3c", custom_match = c("340" = "SRB", "345" = "SRB", "347" = "XKX",
                                       "260" = "DEU", "817" = "VNM", "678" = "YEM")))

# ---- source data ------------------------------------------------------------------
ve <- new.env(); load(file.path(cache, "vdem.RData"), envir = ve)
vd <- ve$vdem |> filter(year >= 1946) |>
  transmute(iso3 = country_text_id, year, regime = v2x_regime, regid = v2regidnr, endtype = v2regendtype,
            coup = e_pt_coup, coup_att = e_pt_coup_attempts, mob = v2cagenmob, clphy = v2x_clphy,
            reg_el = v2elsrgel, loc_el = v2ellocelc,
            elec = as.integer(v2xel_elecpres == 1 | v2xel_elecparl == 1), poly = v2x_polyarchy) |>
  arrange(iso3, year)
le <- new.env(); load(file.path(cache, "LIED.rda"), envir = le)
lied <- le$LIED |> filter(!is.na(GWn)) |> mutate(iso3 = to_iso(GWn)) |> filter(!is.na(iso3)) |>
  group_by(iso3, year) |>
  summarise(lt = max(democratic_transition, na.rm = TRUE), lb = max(democratic_breakdown, na.rm = TRUE),
            turn = max(turnover_event, na.rm = TRUE), .groups = "drop") |> arrange(iso3, year)
ucdp <- suppressMessages(create_stateyears(system = "gw", subset_years = 1985:2024) |>
                           add_ucdp_acd(type = "intrastate")) |>
  mutate(iso3 = to_iso(gwcode)) |> filter(!is.na(iso3)) |>
  group_by(iso3, year) |>
  summarise(conf = max(ucdpongoing), war = as.integer(any(maxintensity == 2, na.rm = TRUE)), .groups = "drop") |>
  mutate(war = ifelse(is.na(war) | is.infinite(war), 0L, war)) |> arrange(iso3, year)

# ---- event constructors -----------------------------------------------------------
win <- function(df) df |> filter(year >= 1995, year <= 2021)
first_ev <- function(df) df |> filter(flag) |> group_by(iso3) |> summarise(event_year = min(year), .groups = "drop")
sustain3 <- function(x, test) sapply(seq_along(x), function(i) all(test(x[i:min(i + 2, length(x))]), na.rm = TRUE))

V <- vd |> filter(year >= 1985) |> group_by(iso3) |>
  mutate(up = regime >= 2 & lag(regime) <= 1 & sustain3(regime, function(r) r >= 2),
         dn = regime <= 1 & lag(regime) >= 2 & sustain3(regime, function(r) r <= 1),
         coup_ev = coup > 0,
         fail_ev = coup_att > 0 & coup == 0,
         reg_ev = reg_el >= 1 & lag(reg_el) < 0.5 & sustain3(reg_el, function(x) x >= 0.5),
         loc_ev = loc_el >= 1 & lag(loc_el) < 0.5 & sustain3(loc_el, function(x) x >= 0.5),
         mob_ev = mob >= quantile(vd$mob[vd$year >= 1992], .95, na.rm = TRUE) &
           lag(mob) < quantile(vd$mob[vd$year >= 1992], .90, na.rm = TRUE),
         rep_ev = (lag(clphy) - clphy) >= 0.15,
         new_regime = !is.na(lag(regid)) & regid != lag(regid),
         rev_up = new_regime & lag(endtype) == 8, rev_arm = new_regime & lag(endtype) == 5) |>
  ungroup()
L <- lied |> group_by(iso3) |>
  mutate(prior_turn = cumsum(lag(turn, default = 0) == 1 & year >= 1946) > 0,
         turn_ev = turn == 1 & !prior_turn) |> ungroup()
U <- ucdp |> group_by(iso3) |>
  mutate(on = conf == 1 & lag(conf, 1) + lag(conf, 2) + lag(conf, 3) == 0,
         war_on = war == 1 & lag(war, 1) + lag(war, 2) + lag(war, 3) == 0,
         end = conf == 0 & lag(conf, 1) + lag(conf, 2) == 2 & lead(conf, 1, default = 0) == 0 & lead(conf, 2, default = 0) == 0) |>
  ungroup()

always_aut <- V |> group_by(iso3) |> summarise(a = all(regime <= 1, na.rm = TRUE)) |> filter(a) |> pull(iso3)
always_dem <- V |> group_by(iso3) |> summarise(a = all(regime >= 2, na.rm = TRUE)) |> filter(a) |> pull(iso3)
stable_reg <- V |> group_by(iso3) |> summarise(ch = any(new_regime, na.rm = TRUE)) |> filter(!ch) |> pull(iso3)
no_coup    <- V |> group_by(iso3) |> summarise(a = any(coup_att > 0, na.rm = TRUE)) |> filter(!a) |> pull(iso3)
no_conf    <- U |> filter(year >= 1989) |> group_by(iso3) |> summarise(a = any(conf == 1)) |> filter(!a) |> pull(iso3)
no_turn    <- lied |> filter(year >= 1946) |> group_by(iso3) |> summarise(a = any(turn == 1)) |> filter(!a) |> pull(iso3)
reg_stable <- V |> group_by(iso3) |> summarise(a = all(reg_el >= 0.5, na.rm = TRUE) | all(reg_el < 0.5, na.rm = TRUE)) |> filter(a) |> pull(iso3)
loc_stable <- V |> group_by(iso3) |> summarise(a = all(loc_el >= 0.5, na.rm = TRUE) | all(loc_el < 0.5, na.rm = TRUE)) |> filter(a) |> pull(iso3)
no_mob     <- V |> group_by(iso3) |> summarise(a = any(mob >= quantile(vd$mob[vd$year >= 1992], .95, na.rm = TRUE), na.rm = TRUE)) |> filter(!a) |> pull(iso3)
no_rep     <- V |> group_by(iso3) |> summarise(a = any((lag(clphy) - clphy) >= 0.10, na.rm = TRUE)) |> filter(!a) |> pull(iso3)

mk <- function(df, var) first_ev(win(df) |> mutate(flag = .data[[var]] %in% TRUE))
events <- list(
  "Democratization (RoW)"             = list(ev = mk(V, "up"),      ctrl = always_aut, family = "Regime change"),
  "Autocratization (RoW)"             = list(ev = mk(V, "dn"),      ctrl = always_dem, family = "Regime change"),
  "Democratization (LIED)"            = list(ev = first_ev(win(lied) |> mutate(flag = lt == 1)), ctrl = always_aut, family = "Regime change"),
  "Autocratization (LIED)"            = list(ev = first_ev(win(lied) |> mutate(flag = lb == 1)), ctrl = always_dem, family = "Regime change"),
  "Successful coup"                   = list(ev = mk(V, "coup_ev"), ctrl = no_coup,    family = "Coups & leaders"),
  "Failed coup attempt"               = list(ev = mk(V, "fail_ev"), ctrl = no_coup,    family = "Coups & leaders"),
  "First electoral turnover (LIED)"   = list(ev = mk(L, "turn_ev"), ctrl = no_turn,    family = "Coups & leaders"),
  "Elected regional government introduced" = list(ev = mk(V, "reg_ev"), ctrl = reg_stable, family = "Decentralization"),
  "Elected local government introduced"    = list(ev = mk(V, "loc_ev"), ctrl = loc_stable, family = "Decentralization"),
  "Mass-mobilization surge (top 5%)"  = list(ev = mk(V, "mob_ev"),  ctrl = no_mob,     family = "Contention"),
  "Repression surge (physical integrity -0.15)" = list(ev = mk(V, "rep_ev"), ctrl = no_rep, family = "Contention"),
  "Revolution: popular uprising"      = list(ev = mk(V, "rev_up"),  ctrl = stable_reg, family = "Contention"),
  "Revolution: armed (rebel victory)" = list(ev = mk(V, "rev_arm"), ctrl = stable_reg, family = "Violence"),
  "Civil-conflict onset"              = list(ev = mk(U, "on"),      ctrl = no_conf,    family = "Violence"),
  "Civil-war onset"                   = list(ev = mk(U, "war_on"),  ctrl = no_conf,    family = "Violence"),
  "Conflict termination"              = list(ev = mk(U, "end"),     ctrl = no_conf,    family = "Violence")
)
ev_tab <- bind_rows(lapply(names(events), function(nm) events[[nm]]$ev |> mutate(event = nm)))
write.csv(ev_tab, file.path(out, "screen_events.csv"), row.names = FALSE)

# ---- outcomes -------------------------------------------------------------------
u <- as.data.frame(ntl_gini_units("adm1", quiet = TRUE))
base <- u |> filter(year %in% 1992:1994) |> group_by(iso3, gid) |>
  summarise(base_rad = mean(mean_radiance), base_share = mean(value_share), .groups = "drop") |>
  group_by(iso3) |> filter(n() >= 3) |>
  mutate(tercile = ntile(base_rad, 3), is_core = base_share == max(base_share)) |> ungroup()
shares <- u |> inner_join(base, by = c("iso3", "gid")) |> group_by(iso3, year) |>
  summarise(core_share = 100 * sum(value_share[is_core]), dark_share = 100 * sum(value_share[tercile == 1]),
            .groups = "drop")
d <- panel |>
  select(iso3, year, region, total_gini, between_gini_adm1, within_gini_adm1, between_gini_adm2) |>
  mutate(across(c(total_gini, between_gini_adm1, within_gini_adm1, between_gini_adm2), ~ 100 * .x)) |>
  left_join(shares, by = c("iso3", "year"))
outs <- c(total_gini = "Total Gini", between_gini_adm1 = "Between-region Gini (ADM1)",
          within_gini_adm1 = "Within-region Gini (ADM1)", between_gini_adm2 = "Between-region Gini (ADM2)",
          core_share = "Core-region share of light", dark_share = "Darkest-third regions' share of light")
units <- c(total_gini = "Gini points x 100", between_gini_adm1 = "Gini points x 100",
           within_gini_adm1 = "Gini points x 100", between_gini_adm2 = "Gini points x 100",
           core_share = "Percentage points", dark_share = "Percentage points")

# ---- estimator --------------------------------------------------------------------
es <- function(ev, ctrl, label, ys = names(outs)) {
  dd <- d |> left_join(ev |> select(iso3, event_year), by = "iso3") |>
    filter(!is.na(event_year) | iso3 %in% ctrl) |>
    mutate(cohort = ifelse(is.na(event_year), 10000, event_year),
           rel = ifelse(cohort == 10000, NA, year - cohort)) |>
    filter(is.na(rel) | (rel >= -6 & rel <= 10))
  bind_rows(lapply(ys, function(y) {
    dy <- dd[!is.na(dd[[y]]), ]
    if (n_distinct(dy$iso3[dy$cohort != 10000]) < 5) return(NULL)
    m <- tryCatch(feols(as.formula(paste(y, "~ sunab(cohort, year) | iso3 + region^year")), dy,
                        vcov = ~iso3, notes = FALSE, warn = FALSE), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    ct <- as.data.frame(coeftable(m)); ct <- ct[grepl("^year::", rownames(ct)), ]
    ag <- function(rx, nm) tryCatch(coeftable(summary(m, agg = setNames(rx, nm)))[nm, ], error = function(e) rep(NA, 4))
    a <- ag("year::[^-]", "ATT"); l5 <- ag("year::[5-9]:", "LATE"); pr <- ag("year::-[2-9]", "PRE")
    tibble(event = label, outcome = y, rel = as.numeric(sub("year::", "", rownames(ct))), est = ct[, 1], se = ct[, 2],
           att = a[1], att_se = a[2], att_p = a[4], late = l5[1], late_se = l5[2], late_p = l5[4],
           pre = pr[1], pre_p = pr[4],
           n_treated = n_distinct(dy$iso3[dy$cohort != 10000]), n_control = n_distinct(dy$iso3[dy$cohort == 10000]))
  }))
}
S <- bind_rows(lapply(names(events), function(nm) es(events[[nm]]$ev, events[[nm]]$ctrl, nm))) |>
  mutate(family = sapply(event, function(e) events[[e]]$family))
write.csv(S, file.path(out, "screen_paths.csv"), row.names = FALSE)

# ---- selection ----------------------------------------------------------------------
summ <- S |> distinct(event, family, outcome, att, att_se, att_p, late, late_se, late_p, pre, pre_p, n_treated, n_control)
tests <- bind_rows(summ |> transmute(event, outcome, stat = "ATT (years 0-10)", est = att, se = att_se, p = att_p, pre_p),
                   summ |> transmute(event, outcome, stat = "Years 5-9", est = late, se = late_se, p = late_p, pre_p)) |>
  filter(!is.na(p)) |>
  mutate(q = p.adjust(p, "BH"),
         candidate = q < .05 & pre_p > .10,
         candidate_suggestive = q < .10 & pre_p > .10)

jk <- function(evname, y, stat) {
  ev <- events[[evname]]
  r <- sapply(ev$ev$iso3, function(i) {
    x <- es(ev$ev |> filter(iso3 != i), ev$ctrl, evname, ys = y)
    if (is.null(x) || !nrow(x)) return(NA)
    if (stat == "Years 5-9") x$late_p[1] else x$att_p[1]
  })
  mean(r < .05, na.rm = TRUE)
}
cand <- tests |> filter(candidate_suggestive)
cand$jk_share_p05 <- if (nrow(cand)) as.numeric(unlist(mapply(jk, cand$event, cand$outcome, cand$stat))) else numeric(0)
tests <- tests |> left_join(cand |> select(event, outcome, stat, jk_share_p05), by = c("event", "outcome", "stat")) |>
  mutate(selected = candidate & !is.na(jk_share_p05) & jk_share_p05 >= .8,
         suggestive = candidate_suggestive & !is.na(jk_share_p05) & jk_share_p05 >= .8)
write.csv(tests, file.path(out, "screen_tests.csv"), row.names = FALSE)
write.csv(summ, file.path(out, "screen_summary.csv"), row.names = FALSE)

# ---- election cycles (repeated events) -------------------------------------------------
el <- vd |> filter(year >= 1985) |> group_by(iso3) |>
  mutate(e_m2 = lead(elec, 2), e_m1 = lead(elec, 1), e_0 = elec, e_p1 = lag(elec, 1), e_p2 = lag(elec, 2),
         competitive = poly >= 0.5) |> ungroup() |>
  select(iso3, year, e_m2, e_m1, e_0, e_p1, e_p2, competitive)
de <- d |> inner_join(el, by = c("iso3", "year"))
EC <- bind_rows(lapply(names(outs), function(y) bind_rows(lapply(c("All", "Competitive (polyarchy >= .5)", "Non-competitive"), function(s) {
  dd <- switch(s, All = de, "Competitive (polyarchy >= .5)" = filter(de, competitive), "Non-competitive" = filter(de, !competitive))
  m <- feols(as.formula(paste(y, "~ e_m2 + e_m1 + e_0 + e_p1 + e_p2 | iso3 + region^year")), dd, vcov = ~iso3, notes = FALSE)
  ct <- as.data.frame(coeftable(m))
  tibble(outcome = y, sample = s, term = rownames(ct), rel = c(-2, -1, 0, 1, 2), est = ct[, 1], se = ct[, 2], p = ct[, 4])
}))))
EC <- EC |> group_by(sample) |> mutate(q = p.adjust(p, "BH")) |> ungroup()
write.csv(EC, file.path(out, "screen_election_cycles.csv"), row.names = FALSE)

options(width = 230)
cat("\n== Event counts ==\n"); print(summ |> distinct(event, n_treated, n_control) |> group_by(event) |> slice(1) |> as.data.frame())
cat("\n== Candidates (q < .10, pre p > .10) and jackknife ==\n")
print(tests |> filter(candidate_suggestive) |> arrange(q) |> mutate(across(where(is.double), ~ round(.x, 3))) |> as.data.frame())
cat("\nTests:", nrow(tests), " p<.05:", sum(tests$p < .05), " q<.05:", sum(tests$q < .05), " selected (strict):", sum(tests$selected), " suggestive:", sum(tests$suggestive), "\n")
cat("\n== Election cycles: q < .10 ==\n"); print(EC |> filter(q < .10) |> as.data.frame())
