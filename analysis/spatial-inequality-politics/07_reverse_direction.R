# Reverse-direction test: does regime change reshape the spatial distribution
# of light (democracy -> inequality), rather than inequality -> democracy?
#  A. Cross-lagged panel: both directions estimated in the same FE framework
#  B. Event studies (Sun & Abraham 2021) around democratizations and breakdowns
#  C. Mechanism: rural electrification (WDI)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(fixest) })
here  <- file.path("analysis", "spatial-inequality-politics")
out   <- file.path(here, "output")
cache <- file.path(here, "cache")
panel <- readRDS(file.path(cache, "panel.rds"))

elec <- readRDS(file.path(cache, "wdi_elec.rds")) |>
  filter(region != "Aggregates", !is.na(iso3c), iso3c != "") |>
  transmute(iso3 = iso3c, year, elec, elec_rural, elec_urban)
e <- new.env(); load(file.path(cache, "LIED.rda"), envir = e)
lied_tr <- e$LIED |>
  filter(year >= 1990, !is.na(GWn)) |>
  mutate(iso3 = suppressWarnings(countrycode::countrycode(
    GWn, "gwn", "iso3c", custom_match = c("340" = "SRB", "345" = "SRB", "347" = "XKX",
                                          "260" = "DEU", "817" = "VNM", "678" = "YEM")))) |>
  filter(!is.na(iso3)) |>
  group_by(iso3, year) |>
  summarise(lied_transition = max(democratic_transition, na.rm = TRUE), .groups = "drop")
vd <- local({ e <- new.env(); load(file.path(cache, "vdem.RData"), envir = e)
  e$vdem |> filter(year >= 1990) |> transmute(iso3 = country_text_id, year, v2x_regime) })

ineq  <- c(within_gini_adm1 = "Within-region Gini (ADM1)",
           between_gini_adm1 = "Between-region Gini (ADM1)",
           between_gini_adm2 = "Between-region Gini (ADM2)",
           total_gini = "Total spatial Gini")
dem   <- c(v2x_polyarchy = "Polyarchy", v2x_libdem = "Liberal democracy",
           fh_total_rev = "Freedom House", uds_z1 = "UDS", lexical_index = "LIED")
ctrl  <- c("log_gdppc", "log_pop", "urban", "rents")      # mean radiance omitted: it is an outcome-side quantity here
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

d <- panel |>
  left_join(elec, by = c("iso3", "year")) |>
  left_join(lied_tr, by = c("iso3", "year")) |>
  left_join(vd, by = c("iso3", "year")) |>
  mutate(across(all_of(c(names(ineq), names(dem))), z, .names = "{.col}_z"),
         trend = year - 2008)

shift <- function(df, vars, k) {
  s <- df |> select(iso3, year, all_of(vars)) |> mutate(year = year - k)
  names(s)[-(1:2)] <- paste0(vars, "_f", k)
  left_join(df, s, by = c("iso3", "year"))
}
zvars <- paste0(c(names(ineq), names(dem)), "_z")
for (k in c(1, 3, 5)) d <- shift(d, c(zvars, "elec_rural", "log_mean_rad"), k)

# ---- A. cross-lagged panel ---------------------------------------------------
C <- paste(ctrl, collapse = " + ")
cl <- list()
fit <- function(y, x, extra, fe, dir, k, variant, ivar, dvar) {
  fml <- as.formula(paste(y, "~", paste(c(x, extra), collapse = " + "), "|", fe))
  m <- tryCatch(feols(fml, d, vcov = ~iso3, notes = FALSE, warn = FALSE), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  ct <- coeftable(m)[x, ]
  tibble(direction = dir, horizon = k, variant = variant, inequality = ivar, democracy = dvar,
         est = ct[1], se = ct[2], p = ct[4], nobs = nobs(m))
}
for (iv in names(ineq)) for (dv in names(dem)) for (k in c(1, 3, 5)) {
  I0 <- paste0(iv, "_z"); D0 <- paste0(dv, "_z")
  Ik <- paste0(iv, "_z_f", k); Dk <- paste0(dv, "_z_f", k)
  for (v in c("TWFE + controls", "TWFE + controls + lagged DV", "TWFE + lagged DV + country trends")) {
    fe <- if (grepl("trends", v)) "iso3[trend] + year" else "iso3 + year"
    c1 <- c(C, if (grepl("lagged DV", v)) I0)
    c2 <- c(C, if (grepl("lagged DV", v)) D0)
    cl[[length(cl) + 1]] <- fit(Ik, D0, c1, fe, "Democracy(t) -> Inequality(t+k)", k, v, iv, dv)
    cl[[length(cl) + 1]] <- fit(Dk, I0, c2, fe, "Inequality(t) -> Democracy(t+k)", k, v, iv, dv)
  }
}
cl <- bind_rows(cl) |> group_by(direction, variant, horizon) |>
  mutate(p_bh = p.adjust(p, "BH")) |> ungroup()
write.csv(cl, file.path(out, "reverse_crosslag.csv"), row.names = FALSE)

# ---- B. event studies ---------------------------------------------------------
# Events: first sustained crossing of the RoW democracy threshold (v2x_regime
# >= 2) in 1995-2021, where the new regime lasts >= 3 years (or to data end).
# Robustness: LIED democratic_transition.
sustained_first <- function(df, flag_fun) {
  df |> arrange(year) |> group_by(iso3) |>
    mutate(cand = flag_fun(v2x_regime, lag(v2x_regime)),
           keep3 = cand & sapply(seq_along(year), function(i) {
             new_side <- v2x_regime[i] >= 2
             w <- v2x_regime[i:min(i + 2, n())]
             all((w >= 2) == new_side, na.rm = TRUE)
           })) |>
    summarise(event_year = suppressWarnings(min(year[keep3 & year >= 1995 & year <= 2021])),
              ever_dem = any(v2x_regime >= 2, na.rm = TRUE),
              ever_aut = any(v2x_regime <= 1, na.rm = TRUE), .groups = "drop") |>
    mutate(event_year = ifelse(is.infinite(event_year), NA, event_year))
}
rw <- d |> filter(!is.na(v2x_regime)) |> select(iso3, year, v2x_regime)
ev_dem <- sustained_first(rw, function(r, rl) r >= 2 & rl <= 1)
ev_bd  <- sustained_first(rw, function(r, rl) r <= 1 & rl >= 2)
ev_lied <- d |> group_by(iso3) |>
  summarise(event_year = suppressWarnings(min(year[lied_transition == 1 & year >= 1995 & year <= 2021])),
            .groups = "drop") |>
  mutate(event_year = ifelse(is.infinite(event_year), NA, event_year))

es_outcomes <- c(within_gini_adm1_z = "Within-region Gini (ADM1)",
                 between_gini_adm1_z = "Between-region Gini (ADM1)",
                 between_gini_adm2_z = "Between-region Gini (ADM2)",
                 total_gini_z = "Total spatial Gini",
                 log_mean_rad = "Log mean radiance (level)",
                 elec_rural = "Rural electricity access (%)")

run_es <- function(events, controls_filter, label) {
  dd <- d |> left_join(events |> select(iso3, event_year), by = "iso3") |>
    filter(controls_filter(iso3, event_year)) |>
    mutate(cohort = ifelse(is.na(event_year), 10000, event_year),
           rel = ifelse(cohort == 10000, NA, year - cohort)) |>
    filter(is.na(rel) | (rel >= -6 & rel <= 8))
  bind_rows(lapply(names(es_outcomes), function(y) {
    m <- feols(as.formula(paste(y, "~ sunab(cohort, year, ref.p = -1) +", C, "| iso3 + year")),
               dd, vcov = ~iso3, notes = FALSE, warn = FALSE)
    ct <- as.data.frame(coeftable(m))
    ct <- ct[grepl("^year::", rownames(ct)), ]
    att <- coeftable(summary(m, agg = "ATT"))["ATT", ]
    pre <- ct[as.numeric(sub("year::", "", rownames(ct))) < -1, ]
    # average pre-period deviation (t-6..t-2), aggregated across cohorts like the ATT
    pre_ct <- tryCatch(coeftable(summary(m, agg = c(PRE = "year::-[2-9]")))["PRE", ],
                       error = function(e) rep(NA_real_, 4))
    tibble(event = label, outcome = y, outcome_label = es_outcomes[[y]],
           rel = as.numeric(sub("year::", "", rownames(ct))), est = ct[, 1], se = ct[, 2]) |>
      bind_rows(tibble(event = label, outcome = y, outcome_label = es_outcomes[[y]], rel = NA,
                       est = att[1], se = att[2])) |>
      mutate(att_p = att[4], pre_avg = pre_ct[1], pre_avg_se = pre_ct[2], pre_avg_p = pre_ct[4],
             n_treated = n_distinct(dd$iso3[dd$cohort != 10000 & !is.na(dd[[y]])]),
             n_control = n_distinct(dd$iso3[dd$cohort == 10000 & !is.na(dd[[y]])]))
  }))
}
# democratization: treated = democratizers; controls = never-democratizing autocracies
aut_always <- ev_dem$iso3[is.na(ev_dem$event_year) & !ev_dem$ever_dem]
dem_always <- ev_bd$iso3[is.na(ev_bd$event_year) & !ev_bd$ever_aut]
es <- bind_rows(
  run_es(ev_dem, function(i, ey) !is.na(ey) | i %in% aut_always, "Democratization (RoW)"),
  run_es(ev_bd,  function(i, ey) !is.na(ey) | i %in% dem_always, "Democratic breakdown (RoW)"),
  run_es(ev_lied, function(i, ey) !is.na(ey) | i %in% aut_always, "Democratization (LIED)")
)
write.csv(es, file.path(out, "reverse_event_study.csv"), row.names = FALSE)
write.csv(bind_rows(ev_dem |> mutate(type = "democratization"), ev_bd |> mutate(type = "breakdown")) |>
            filter(!is.na(event_year)), file.path(out, "reverse_events.csv"), row.names = FALSE)

# ---- C. mechanism: rural electrification ---------------------------------------
mech <- list(
  "Polyarchy(t) -> rural elec. access(t+3)" =
    feols(as.formula(paste("elec_rural_f3 ~ v2x_polyarchy_z + elec_rural +", C, "| iso3 + year")), d, vcov = ~iso3),
  "Rural elec. access(t) -> within Gini(t)" =
    feols(as.formula(paste("within_gini_adm1_z ~ elec_rural +", C, "| iso3 + year")), d, vcov = ~iso3),
  "Polyarchy(t) -> within Gini(t+3), same sample" =
    feols(as.formula(paste("within_gini_adm1_z_f3 ~ v2x_polyarchy_z + within_gini_adm1_z +", C, "| iso3 + year")),
          d |> filter(!is.na(elec_rural), !is.na(elec_rural_f3)), vcov = ~iso3),
  "... + change in rural elec. access" =
    feols(as.formula(paste("within_gini_adm1_z_f3 ~ v2x_polyarchy_z + within_gini_adm1_z + I(elec_rural_f3 - elec_rural) +", C, "| iso3 + year")),
          d |> filter(!is.na(elec_rural), !is.na(elec_rural_f3)), vcov = ~iso3)
)
mech_tab <- bind_rows(lapply(names(mech), function(nm) {
  ct <- coeftable(mech[[nm]]); x <- rownames(ct)[1]
  tibble(model = nm, term = x, est = ct[1, 1], se = ct[1, 2], p = ct[1, 4], nobs = nobs(mech[[nm]]))
}))
write.csv(mech_tab, file.path(out, "reverse_mechanism.csv"), row.names = FALSE)

# ---- effect sizes in within-country SD units (TWFE residual SDs) ------------------
wsd <- function(v) { ok <- !is.na(d[[v]]); r <- d[[v]][ok] - ave(d[[v]][ok], d$iso3[ok]); sd(r - ave(r, d$year[ok])) }
sd_w <- sapply(c(paste0(names(ineq), "_z"), paste0(names(dem), "_z")), wsd)
write.csv(data.frame(var = names(sd_w), within_twfe_sd = sd_w), file.path(out, "reverse_within_sd.csv"), row.names = FALSE)
cl <- cl |> mutate(
  est_within_sd = ifelse(grepl("^Democracy", direction),
                         est * sd_w[paste0(democracy, "_z")] / sd_w[paste0(inequality, "_z")],
                         est * sd_w[paste0(inequality, "_z")] / sd_w[paste0(democracy, "_z")]))
write.csv(cl, file.path(out, "reverse_crosslag.csv"), row.names = FALSE)

# ---- print ---------------------------------------------------------------------
options(width = 220)
cat("\n== A. Cross-lagged, polyarchy x within-region Gini ==\n")
print(cl |> filter(democracy == "v2x_polyarchy", inequality == "within_gini_adm1") |>
        select(direction, variant, horizon, est, se, p, est_within_sd) |> arrange(variant, direction, horizon), n = 30)
cat("\n== A. Count p<.05 (BH) across 4 inequality x 5 democracy measures, by direction/variant/horizon ==\n")
print(cl |> group_by(direction, variant, horizon) |>
        summarise(n = n(), p05 = sum(p < .05), bh05 = sum(p_bh < .05),
                  mean_est = mean(est), .groups = "drop"), n = 30)
cat("\n== B. Event-study ATTs ==\n")
print(es |> filter(is.na(rel)) |> select(event, outcome_label, est, se, att_p, pre_avg, pre_avg_p, n_treated, n_control), n = 30)
cat("\n== C. Mechanism ==\n"); print(mech_tab)
