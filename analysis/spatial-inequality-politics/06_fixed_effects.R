# Fixed-effects battery: what happens when identification comes only from
# within-country change in spatial inequality?
suppressPackageStartupMessages({ library(dplyr); library(fixest) })
here  <- file.path("analysis", "spatial-inequality-politics")
out   <- file.path(here, "output")
panel <- readRDS(file.path(here, "cache", "panel.rds"))
meta  <- readRDS(file.path(out, "meta.rds"))
outcomes <- meta$outcomes

preds <- c(total_gini = "Total Gini", between_gini_adm1 = "Between Gini ADM1",
           within_gini_adm1 = "Within Gini ADM1", between_share_adm1 = "Between share ADM1",
           between_gini_adm2 = "Between Gini ADM2")
ctrl_tv <- c("log_gdppc", "log_pop", "urban", "log_mean_rad", "rents")
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

d <- panel |>
  mutate(across(all_of(names(preds)), z),
         across(all_of(outcomes$var[!outcomes$binary]), z),
         country_era = paste(iso3, era),
         region_year = paste(region, year),
         trend = year - 2008)

# ---- 1. how much within-country variation is there? -------------------------
wv <- bind_rows(lapply(names(preds), function(x) {
  v <- d[[x]]; ok <- !is.na(v)
  dm <- v[ok] - ave(v[ok], d$iso3[ok])                   # country-demeaned
  dm2 <- dm - ave(dm, d$year[ok])                         # and year-demeaned
  tibble(predictor = x,
         sd_total = sd(v[ok]), sd_within_country = sd(dm), sd_within_twfe = sd(dm2),
         share_var_within_twfe = var(dm2) / var(v[ok]))
}))
write.csv(wv, file.path(out, "fe_within_variation.csv"), row.names = FALSE)

# leads / lags of the predictors
shift <- function(df, vars, k, suffix) {
  s <- df |> select(iso3, year, all_of(vars)) |> mutate(year = year + k)
  names(s)[-(1:2)] <- paste0(vars, suffix)
  left_join(df, s, by = c("iso3", "year"))
}
d <- shift(d, names(preds), 3, "_lag3")
d <- shift(d, names(preds), 5, "_lag5")
d <- shift(d, names(preds), -3, "_lead3")   # placebo: future inequality

# 5-year period panel (averages) to damp year-to-year sensor noise
p5 <- d |>
  mutate(period = 1992 + 5 * ((year - 1992) %/% 5)) |>
  group_by(iso3, period) |>
  summarise(across(c(all_of(names(preds)), all_of(outcomes$var), all_of(ctrl_tv)),
                   ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

C <- paste(ctrl_tv, collapse = " + ")
fe_specs <- list(
  F1 = list(name = "Country + year FE, no controls",              x = "",       rhs = NULL, fe = "iso3 + year"),
  F2 = list(name = "Country + year FE + controls",                x = "",       rhs = C,    fe = "iso3 + year"),
  F3 = list(name = "Country + region-by-year FE + controls",      x = "",       rhs = C,    fe = "iso3 + region_year"),
  F4 = list(name = "F2 + country-specific linear trends",         x = "",       rhs = C,    fe = "iso3[trend] + year"),
  F5 = list(name = "Country-by-era + year FE + controls",         x = "",       rhs = C,    fe = "country_era + year"),
  F6 = list(name = "F2, predictor lagged 3 yrs",                  x = "_lag3",  rhs = C,    fe = "iso3 + year"),
  F7 = list(name = "F2, predictor lagged 5 yrs",                  x = "_lag5",  rhs = C,    fe = "iso3 + year"),
  F8 = list(name = "F2, placebo: predictor 3 yrs in the future",  x = "_lead3", rhs = C,    fe = "iso3 + year"),
  F9 = list(name = "5-year periods, country + period FE + controls", x = "",    rhs = C,    fe = "iso3 + period", data = "p5")
)

res <- list()
for (sid in names(fe_specs)) for (xv in names(preds)) for (i in seq_len(nrow(outcomes))) {
  sp <- fe_specs[[sid]]; y <- outcomes$var[i]
  x <- paste0(xv, sp$x)
  dat <- if (identical(sp$data, "p5")) p5 else d
  fml <- as.formula(paste(y, "~", paste(c(x, sp$rhs), collapse = " + "), "|", sp$fe))
  m <- tryCatch(feols(fml, dat, vcov = ~iso3, notes = FALSE, warn = FALSE), error = function(e) NULL)
  if (is.null(m) || !x %in% names(coef(m))) next
  ct <- coeftable(m)[x, ]
  res[[length(res) + 1]] <- tibble(spec = sid, spec_name = sp$name, predictor = xv, outcome = y,
                                   est = ct[1], se = ct[2], p = ct[4], nobs = nobs(m))
}
res <- bind_rows(res) |>
  left_join(outcomes |> select(var, label, family, expect), by = c("outcome" = "var")) |>
  group_by(spec) |> mutate(p_bh = p.adjust(p, "BH")) |> ungroup()
write.csv(res, file.path(out, "fe_battery.csv"), row.names = FALSE)

# "robust within": BH-significant under F2 and same-sign p<.05 in >= 6 of the
# 7 non-placebo alternatives (F1, F3-F7, F9), and NOT significant in the F8 placebo
rob <- res |>
  group_by(predictor, outcome, label, family) |>
  summarise(f2_est = est[spec == "F2"], f2_p = p[spec == "F2"], f2_p_bh = p_bh[spec == "F2"],
            n_alt_sig_same = sum(spec %in% c("F1", "F3", "F4", "F5", "F6", "F7", "F9") &
                                   sign(est) == sign(f2_est) & p < .05),
            placebo_p = p[spec == "F8"], .groups = "drop") |>
  mutate(robust_within = f2_p_bh < .05 & n_alt_sig_same >= 6 & placebo_p > .05)
write.csv(rob, file.path(out, "fe_robustness_summary.csv"), row.names = FALSE)

options(width = 220)
print(wv)
print(res |> group_by(spec, spec_name) |>
        summarise(n = n(), p05 = sum(p < .05), bh05 = sum(p_bh < .05), .groups = "drop"))
print(rob |> arrange(f2_p) |> head(15))
