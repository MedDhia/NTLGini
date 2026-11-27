# Specification grid: does nighttime-light spatial inequality predict political
# outcomes across many specifications?
suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
})
here  <- file.path("analysis", "spatial-inequality-politics")
panel <- readRDS(file.path(here, "cache", "panel.rds"))
out   <- file.path(here, "output")
dir.create(out, showWarnings = FALSE)

# ---- definitions ------------------------------------------------------------
predictors <- c(
  total_gini        = "Total spatial Gini",
  between_gini_adm1 = "Between-region Gini (ADM1)",
  within_gini_adm1  = "Within-region Gini (ADM1)",
  between_share_adm1 = "Between-region share (ADM1)"
)
adm2_twin <- c(between_gini_adm1 = "between_gini_adm2",
               within_gini_adm1  = "within_gini_adm2",
               between_share_adm1 = "between_share_adm2")

# expected sign if spatial inequality is politically "harmful"
outcomes <- tribble(
  ~var,                 ~label,                                   ~family,                 ~source,         ~binary, ~expect,
  "v2xpe_exlgeo",       "Exclusion by urban-rural location",      "Geographic exclusion",  "V-Dem",         FALSE,  1,
  "v2pepwrgeo",         "Power equal across urban-rural",         "Geographic exclusion",  "V-Dem",         FALSE, -1,
  "v2peapsgeo",         "Public-service access equal urban-rural","Geographic exclusion",  "V-Dem",         FALSE, -1,
  "v2clgeocl",          "Civil liberties equal urban-rural",      "Geographic exclusion",  "V-Dem",         FALSE, -1,
  "v2x_polyarchy",      "Electoral democracy (polyarchy)",        "Regime",                "V-Dem",         FALSE, -1,
  "v2x_libdem",         "Liberal democracy",                      "Regime",                "V-Dem",         FALSE, -1,
  "polity2",            "Polity2",                                "Regime",                "democracyData", FALSE, -1,
  "fh_total_rev",       "Freedom House (higher = freer)",         "Regime",                "democracyData", FALSE, -1,
  "uds_z1",             "Extended UDS",                           "Regime",                "democracyData", FALSE, -1,
  "lexical_index",      "LIED lexical index",                     "Regime",                "democracyData", FALSE, -1,
  "conflict_incidence", "Intrastate conflict (UCDP) incidence",   "Conflict & instability","peacesciencer", TRUE,   1,
  "conflict_onset",     "Intrastate conflict (UCDP) onset",       "Conflict & instability","peacesciencer", TRUE,   1,
  "coup_attempt",       "Coup attempt (Powell & Thyne)",          "Conflict & instability","V-Dem",         TRUE,   1,
  "v2caviol",           "Political violence",                     "Conflict & instability","V-Dem",         FALSE,  1,
  "v2cagenmob",         "Mass mobilization",                      "Conflict & instability","V-Dem",         FALSE,  1,
  "v2svstterr",         "State authority over territory",         "Governance",            "V-Dem",         FALSE, -1,
  "v2x_corr",           "Political corruption",                   "Governance",            "V-Dem",         FALSE,  1,
  "v2x_rule",           "Rule of law",                            "Governance",            "V-Dem",         FALSE, -1,
  "v2stfisccap",        "Fiscal capacity",                        "Governance",            "V-Dem",         FALSE, -1
)

ctrl_tv  <- c("log_gdppc", "log_pop", "urban", "log_mean_rad", "rents")
ctrl_all <- c(ctrl_tv, "log_area", "log_ngroups1", "ethfrac", "rugged")

# ---- prep: standardize predictors & continuous outcomes, build leads/lags ----
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
allx <- c(names(predictors), unname(adm2_twin))
d <- panel |>
  mutate(across(all_of(allx), z),
         across(all_of(outcomes$var[!outcomes$binary]), z))

shift <- function(df, vars, k, suffix) {
  s <- df |> select(iso3, year, all_of(vars)) |> mutate(year = year + k)
  names(s)[-(1:2)] <- paste0(vars, suffix)
  left_join(df, s, by = c("iso3", "year"))
}
d <- shift(d, names(predictors), 3, "_lag3")     # X_{t-3}
d <- shift(d, outcomes$var, -3, "_lead3")        # Y_{t+3}
d <- d |> mutate(country_era = paste(iso3, era))
dp <- panel(d, ~ iso3 + year)

# between-country cross-section (country means)
cs <- d |>
  group_by(iso3, region) |>
  summarise(across(c(all_of(allx), all_of(outcomes$var), all_of(ctrl_all)),
                   ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

# ---- specification grid -----------------------------------------------------
f <- function(y, x, ctrl, fe) {
  rhs <- paste(c(x, ctrl), collapse = " + ")
  as.formula(if (is.null(fe)) paste(y, "~", rhs) else paste(y, "~", rhs, "|", fe))
}
big  <- d$area_km2 >= 20000 & d$n_groups_adm1 >= 5
cc   <- complete.cases(d[, c(ctrl_all, "region")])   # main-spec control sample

specs <- list(
  list(id = "S01", name = "Bivariate + year FE",                  data = "d",  x = "x",    ctrl = NULL,     fe = "year"),
  list(id = "S01b", name = "Bivariate + year FE, main-spec sample", data = "d", x = "x",   ctrl = NULL,     fe = "year", sub = quote(cc)),
  list(id = "S02", name = "Controls + year & region FE (main)",   data = "d",  x = "x",    ctrl = ctrl_all, fe = "year + region"),
  list(id = "S03", name = "S02 without mean-radiance control",    data = "d",  x = "x",    ctrl = setdiff(ctrl_all, "log_mean_rad"), fe = "year + region"),
  list(id = "S04", name = "S02 with predictor lagged 3 yrs",      data = "d",  x = "lag3", ctrl = ctrl_all, fe = "year + region"),
  list(id = "S05", name = "S02, outcome 3 yrs ahead | outcome_t", data = "d",  x = "x",    ctrl = ctrl_all, fe = "year + region", lead = TRUE),
  list(id = "S06", name = "Between-country (country means)",      data = "cs", x = "x",    ctrl = ctrl_all, fe = "region"),
  list(id = "S07", name = "Country + year FE (within)",           data = "d",  x = "x",    ctrl = ctrl_tv,  fe = "iso3 + year"),
  list(id = "S08", name = "Country-by-era + year FE",             data = "d",  x = "x",    ctrl = ctrl_tv,  fe = "country_era + year"),
  list(id = "S09", name = "S02, DMSP era 1992-2012",              data = "d",  x = "x",    ctrl = ctrl_all, fe = "year + region", sub = quote(year <= 2012)),
  list(id = "S10", name = "S02, VIIRS era 2013-2024",             data = "d",  x = "x",    ctrl = ctrl_all, fe = "year + region", sub = quote(year >= 2013)),
  list(id = "S11", name = "S02, excl. small / <5-region states",  data = "d",  x = "x",    ctrl = ctrl_all, fe = "year + region", sub = quote(big)),
  list(id = "S12", name = "S02 with ADM2-level measure",          data = "d",  x = "adm2", ctrl = sub("log_ngroups1", "log_ngroups2", ctrl_all), fe = "year + region"),
  list(id = "S13", name = "ADM2-level measure, country + year FE", data = "d", x = "adm2", ctrl = ctrl_tv, fe = "iso3 + year")
)

run_one <- function(sp, xv, ov) {
  dat <- if (sp$data == "cs") cs else d
  if (!is.null(sp$sub)) dat <- dat[eval(sp$sub, cbind(d, big = big, cc = cc)), ]
  x <- switch(sp$x, x = xv, lag3 = paste0(xv, "_lag3"),
              adm2 = if (xv %in% names(adm2_twin)) adm2_twin[[xv]] else NA)
  if (is.na(x)) return(NULL)
  y <- ov$var
  ctrl <- sp$ctrl
  if (isTRUE(sp$lead)) { ctrl <- c(ctrl, y); y <- paste0(y, "_lead3") }
  m <- tryCatch(
    feols(f(y, x, ctrl, sp$fe), data = dat,
          vcov = if (sp$data == "cs") "hetero" else ~ iso3,
          warn = FALSE, notes = FALSE),
    error = function(e) NULL)
  if (is.null(m) || !x %in% names(coef(m))) return(NULL)
  ct <- coeftable(m)[x, ]
  tibble(spec = sp$id, spec_name = sp$name, predictor = xv, outcome = ov$var,
         est = ct[1], se = ct[2], p = ct[4], nobs = nobs(m),
         nclust = if (sp$data == "cs") nobs(m) else length(unique(dat$iso3[obs(m)])))
}

res <- list()
for (sp in specs) for (xv in names(predictors)) for (i in seq_len(nrow(outcomes))) {
  res[[length(res) + 1]] <- run_one(sp, xv, outcomes[i, ])
}
res <- bind_rows(res) |>
  left_join(outcomes, by = c("outcome" = "var")) |>
  mutate(pred_label = predictors[predictor],
         ci_lo = est - 1.96 * se, ci_hi = est + 1.96 * se,
         sig = p < 0.05,
         harmful_dir = sign(est) == expect)

# Multiple-testing adjustment for the main spec, across all predictor x outcome tests
res <- res |>
  group_by(spec) |>
  mutate(p_bh = p.adjust(p, "BH"), p_holm = p.adjust(p, "holm")) |>
  ungroup()

# Robustness summary: across the 12 specs, does the sign agree with the main
# spec and is p < .05?
main <- res |> filter(spec == "S02") |> select(predictor, outcome, main_sign = est)
robust <- res |>
  filter(!spec %in% c("S01", "S01b")) |>
  left_join(main, by = c("predictor", "outcome")) |>
  group_by(predictor, pred_label, outcome, label, family, source, expect) |>
  summarise(
    main_est = est[spec == "S02"], main_p = p[spec == "S02"],
    main_p_bh = p_bh[spec == "S02"],
    within_est = est[spec == "S07"], within_p = p[spec == "S07"],
    n_specs = n(),
    n_same_sign = sum(sign(est) == sign(main_sign)),
    n_sig_same  = sum(sign(est) == sign(main_sign) & p < 0.05),
    .groups = "drop") |>
  mutate(robust = main_p_bh < 0.05 & n_sig_same >= ceiling(0.75 * n_specs),
         robust_incl_within = robust & sign(within_est) == sign(main_est) & within_p < 0.05)

saveRDS(res, file.path(out, "spec_grid.rds"))
write.csv(res, file.path(out, "spec_grid.csv"), row.names = FALSE)
write.csv(robust, file.path(out, "robustness_summary.csv"), row.names = FALSE)
saveRDS(list(outcomes = outcomes, predictors = predictors, specs = specs), file.path(out, "meta.rds"))

options(width = 200)
print(robust |> arrange(predictor, family) |>
        transmute(predictor, outcome, main_est = round(main_est, 3), main_p_bh = signif(main_p_bh, 2),
                  within_est = round(within_est, 3), within_p = signif(within_p, 2),
                  n_sig_same, n_specs, robust, robust_incl_within), n = 200)
