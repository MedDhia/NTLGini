# Does democratization increase the *amount* of light?
# Outcome-side analogue of Acemoglu, Naidu, Restrepo & Robinson (2019, JPE),
# "Democracy Does Cause Growth", with nighttime lights in place of GDP.
#  A. Sun & Abraham event studies, NO time-varying controls (GDP is the
#     mediator of interest, so controlling for it would be a bad control)
#  B. ANRR-style dynamic panel: y_it = sum_j rho_j y_i,t-j + beta D_it + a_i + d_t,
#     long-run effect = beta / (1 - sum rho)
# Benchmark outcome: log GDP per capita (WDI) on the same countries.
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(fixest) })
here  <- file.path("analysis", "spatial-inequality-politics")
out   <- file.path(here, "output")
cache <- file.path(here, "cache")
panel <- readRDS(file.path(cache, "panel.rds"))

vd <- local({ e <- new.env(); load(file.path(cache, "vdem.RData"), envir = e)
  e$vdem |> filter(year >= 1985) |> transmute(iso3 = country_text_id, year, v2x_regime) })
e <- new.env(); load(file.path(cache, "LIED.rda"), envir = e)
lied <- e$LIED |>
  filter(year >= 1985, !is.na(GWn)) |>
  mutate(iso3 = suppressWarnings(countrycode::countrycode(
    GWn, "gwn", "iso3c", custom_match = c("340" = "SRB", "345" = "SRB", "347" = "XKX",
                                          "260" = "DEU", "817" = "VNM", "678" = "YEM")))) |>
  filter(!is.na(iso3)) |>
  group_by(iso3, year) |>
  summarise(lied_transition = max(democratic_transition, na.rm = TRUE),
            lied_dem = as.integer(max(lexical_index, na.rm = TRUE) >= 5), .groups = "drop")

d <- panel |>
  select(iso3, year, sum_of_lights, log_pop, log_gdppc, v2x_polyarchy) |>
  left_join(vd, by = c("iso3", "year")) |>
  left_join(lied, by = c("iso3", "year")) |>
  mutate(log_lights = log(sum_of_lights),
         log_lights_pc = log_lights - log_pop,
         row_dem = as.integer(v2x_regime >= 2)) |>
  mutate(across(c(log_lights, log_lights_pc, log_gdppc), ~ 100 * .x)) |>  # effects read as approx. % (log points x 100)
  left_join(panel |> distinct(iso3, region), by = "iso3")
inc0 <- d |> filter(year <= 1996) |> group_by(iso3) |> summarise(g0 = mean(log_gdppc, na.rm = TRUE)) |>
  mutate(inc3 = ifelse(is.nan(g0), NA, dplyr::ntile(ifelse(is.nan(g0), NA, g0), 3)))
d <- d |> left_join(inc0 |> select(iso3, inc3), by = "iso3")

outs <- c(log_lights = "Log total light (x100)", log_lights_pc = "Log light per capita (x100)",
          log_gdppc = "Log GDP pc, WDI (x100)")

# ---- A. event studies ---------------------------------------------------------
first_sustained <- function(flag_up) {
  vd |> arrange(iso3, year) |> group_by(iso3) |>
    mutate(prev = lag(v2x_regime),
           cand = if (flag_up) v2x_regime >= 2 & prev <= 1 else v2x_regime <= 1 & prev >= 2,
           ok3 = sapply(seq_along(year), function(i) {
             w <- v2x_regime[i:min(i + 2, n())]
             if (flag_up) all(w >= 2, na.rm = TRUE) else all(w <= 1, na.rm = TRUE) })) |>
    summarise(event_year = suppressWarnings(min(year[cand & ok3 & year >= 1995 & year <= 2021])),
              ever_dem = any(v2x_regime >= 2, na.rm = TRUE),
              ever_aut = any(v2x_regime <= 1, na.rm = TRUE), .groups = "drop") |>
    mutate(event_year = ifelse(is.infinite(event_year), NA, event_year))
}
ev_up <- first_sustained(TRUE); ev_dn <- first_sustained(FALSE)
ev_lied <- lied |> group_by(iso3) |>
  summarise(event_year = suppressWarnings(min(year[lied_transition == 1 & year >= 1995 & year <= 2021])),
            .groups = "drop") |>
  mutate(event_year = ifelse(is.infinite(event_year), NA, event_year))
always_aut <- ev_up$iso3[is.na(ev_up$event_year) & !ev_up$ever_dem]
always_dem <- ev_dn$iso3[is.na(ev_dn$event_year) & !ev_dn$ever_aut]

es_run <- function(events, ctrl_ids, label, window = c(-6, 10), drop_seam = FALSE,
                   fe = "iso3 + year", fe_label = "Country + year FE") {
  ev <- events
  if (drop_seam) ev <- ev |> filter(is.na(event_year) | !(event_year %in% 2009:2015))
  dd <- d |> left_join(ev |> select(iso3, event_year), by = "iso3") |>
    filter(!is.na(event_year) | iso3 %in% ctrl_ids) |>
    mutate(cohort = ifelse(is.na(event_year), 10000, event_year),
           rel = ifelse(cohort == 10000, NA, year - cohort)) |>
    filter(is.na(rel) | (rel >= window[1] & rel <= window[2]))
  bind_rows(lapply(names(outs), function(y) {
    dy <- dd[!is.na(dd[[y]]), ]
    m <- feols(as.formula(paste(y, "~ sunab(cohort, year) |", fe)), dy, vcov = ~iso3, notes = FALSE)
    ct <- as.data.frame(coeftable(m)); ct <- ct[grepl("^year::", rownames(ct)), ]
    agg <- function(rx, nm) tryCatch(coeftable(summary(m, agg = setNames(rx, nm)))[nm, ], error = function(e) rep(NA, 4))
    a_att  <- agg("year::[^-]", "ATT")
    a_late <- agg("year::([5-9]|10)$|year::([5-9]|10):", "LATE")       # years +5..+10
    a_pre  <- agg("year::-[2-9]", "PRE")
    tibble(event = label, drop_seam = drop_seam, fe = fe_label, outcome = y, outcome_label = outs[[y]],
           rel = as.numeric(sub("year::", "", rownames(ct))), est = ct[, 1], se = ct[, 2],
           att = a_att[1], att_se = a_att[2], att_p = a_att[4],
           late = a_late[1], late_se = a_late[2], late_p = a_late[4],
           pre = a_pre[1], pre_se = a_pre[2], pre_p = a_pre[4],
           n_treated = n_distinct(dy$iso3[dy$cohort != 10000]),
           n_control = n_distinct(dy$iso3[dy$cohort == 10000]))
  }))
}
es <- bind_rows(
  es_run(ev_up,   always_aut, "Democratization (RoW)"),
  es_run(ev_lied, always_aut, "Democratization (LIED)"),
  es_run(ev_dn,   always_dem, "Democratic breakdown (RoW)"),
  es_run(ev_up,   always_aut, "Democratization (RoW)", drop_seam = TRUE),
  es_run(ev_dn,   always_dem, "Democratic breakdown (RoW)", drop_seam = TRUE),
  # comparison-group robustness: compare only within region-year or initial-income-tercile-year cells
  es_run(ev_up,   always_aut, "Democratization (RoW)", fe = "iso3 + region^year", fe_label = "Country + region-by-year FE"),
  es_run(ev_dn,   always_dem, "Democratic breakdown (RoW)", fe = "iso3 + region^year", fe_label = "Country + region-by-year FE"),
  es_run(ev_up,   always_aut, "Democratization (RoW)", fe = "iso3 + inc3^year", fe_label = "Country + income-tercile-by-year FE"),
  es_run(ev_dn,   always_dem, "Democratic breakdown (RoW)", fe = "iso3 + inc3^year", fe_label = "Country + income-tercile-by-year FE")
)
write.csv(es, file.path(out, "levels_event_study.csv"), row.names = FALSE)

# ---- B. ANRR dynamic panel ----------------------------------------------------
pdat <- panel(d, ~ iso3 + year)
lr <- function(m, dvar) {
  b <- coef(m); V <- vcov(m)
  rh <- grep("^l\\(", names(b), value = TRUE)
  s <- sum(b[rh]); est <- unname(b[dvar] / (1 - s))
  g <- setNames(rep(0, length(b)), names(b))                # delta-method gradient
  g[dvar] <- 1 / (1 - s); g[rh] <- b[dvar] / (1 - s)^2
  se <- sqrt(drop(t(g) %*% V %*% g))
  c(lr = unname(est), lr_se = se, lr_p = 2 * pnorm(-abs(est / se)), persistence = s)
}
anrr <- list()
for (y in names(outs)) for (dv in c("row_dem", "lied_dem")) for (L in c(1, 4)) {
  m <- feols(as.formula(sprintf("%s ~ l(%s, 1:%d) + %s | iso3 + year", y, y, L, dv)),
             pdat, vcov = ~iso3, notes = FALSE)
  ct <- coeftable(m)[dv, ]
  anrr[[length(anrr) + 1]] <- tibble(outcome = y, outcome_label = outs[[y]], democracy = dv, lags = L,
                                     short_run = ct[1], sr_se = ct[2], sr_p = ct[4],
                                     !!!as.list(lr(m, dv)), nobs = nobs(m),
                                     n_countries = length(unique(pdat$iso3[obs(m)])))
}
anrr <- bind_rows(anrr)
write.csv(anrr, file.path(out, "levels_anrr_dynamic_panel.csv"), row.names = FALSE)

# correlation of within-country changes: does light track GDP at all here?
ch <- d |> arrange(iso3, year) |> group_by(iso3) |>
  mutate(dl = log_lights - lag(log_lights, 5), dg = log_gdppc - lag(log_gdppc, 5)) |> ungroup()
elastic <- feols(dl ~ dg | year, ch, vcov = ~iso3, notes = FALSE)
write.csv(as.data.frame(coeftable(elastic)), file.path(out, "levels_light_gdp_elasticity.csv"))

options(width = 220)
cat("\n== A. Event studies (log points x100 ~ %) ==\n")
print(es |> distinct(event, drop_seam, fe, outcome, att, att_se, att_p, late, late_se, late_p, pre, pre_p, n_treated, n_control) |>
        mutate(across(where(is.double), ~ round(.x, 2))), n = 40, width = 250)
cat("\n== B. ANRR dynamic panel ==\n")
print(anrr |> mutate(across(where(is.double), ~ round(.x, 3))), n = 30)
cat("\n== 5-yr change in light on 5-yr change in GDP pc (year FE) ==\n"); print(coeftable(elastic))
