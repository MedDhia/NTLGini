# Diagnostics:
#  (a) which control absorbs the bivariate association?
#  (b) out-of-sample predictive value: country-blocked cross-validation
suppressPackageStartupMessages({
  library(dplyr)
  library(fixest)
})
here <- file.path("analysis", "spatial-inequality-politics")
out  <- file.path(here, "output")
panel <- readRDS(file.path(here, "cache", "panel.rds"))
meta  <- readRDS(file.path(out, "meta.rds"))
outcomes <- meta$outcomes
preds <- names(meta$predictors)

ctrl_all <- c("log_gdppc", "log_pop", "urban", "log_mean_rad", "rents",
              "log_area", "log_ngroups1", "ethfrac", "rugged")
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
d <- panel |>
  mutate(across(all_of(preds), z),
         across(all_of(outcomes$var[!outcomes$binary]), z)) |>
  filter(complete.cases(across(all_of(c(ctrl_all, "region", preds)))))

# ---- (a) one-control-at-a-time attenuation --------------------------------
key_out  <- c("v2xpe_exlgeo", "v2x_polyarchy", "fh_total_rev", "conflict_incidence", "v2x_corr")
key_pred <- c("total_gini", "between_gini_adm1")
steps <- c("(none)", "region", ctrl_all)
att <- list()
for (x in key_pred) for (y in key_out) for (s in steps) {
  rhs <- if (s %in% c("(none)", "region")) x else paste(x, "+", s)
  fe  <- if (s == "region") "year + region" else "year"
  m <- feols(as.formula(paste(y, "~", rhs, "|", fe)), d, vcov = ~iso3, notes = FALSE)
  ct <- coeftable(m)[x, ]
  att[[length(att) + 1]] <- tibble(predictor = x, outcome = y, added = s,
                                   est = ct[1], se = ct[2], p = ct[4], nobs = nobs(m))
}
att <- bind_rows(att) |>
  group_by(predictor, outcome) |>
  mutate(pct_of_bivariate = 100 * est / est[added == "(none)"]) |>
  ungroup()
write.csv(att, file.path(out, "attenuation.csv"), row.names = FALSE)

# Simple correlations of the predictors with development covariates (one row per country, mean)
cors <- d |>
  group_by(iso3) |>
  summarise(across(c(all_of(preds), log_gdppc, urban, log_mean_rad, log_area, log_ngroups1), mean)) |>
  select(-iso3) |>
  cor() |>
  round(2)
write.csv(cors, file.path(out, "predictor_correlations.csv"))

# ---- (b) country-blocked cross-validation ---------------------------------
auc <- function(y, p) {
  r <- rank(p); n1 <- sum(y == 1); n0 <- sum(y == 0)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
cv_one <- function(y, binary, reps = 10, k = 10) {
  dd <- d[!is.na(d[[y]]), ]
  base_rhs <- paste(c(ctrl_all, "splines::ns(year, df = 6)", "region"), collapse = " + ")
  forms <- list(
    year_only = paste(y, "~ splines::ns(year, df = 6)"),
    ntl_only  = paste(y, "~", paste(c(preds, "splines::ns(year, df = 6)"), collapse = " + ")),
    controls  = paste(y, "~", base_rhs),
    controls_ntl = paste(y, "~", base_rhs, "+", paste(preds, collapse = " + "))
  )
  ctry <- unique(dd$iso3)
  res <- list()
  for (r in seq_len(reps)) {
    set.seed(1000 + r)
    fold <- setNames(sample(rep_len(seq_len(k), length(ctry))), ctry)[dd$iso3]
    pred <- matrix(NA_real_, nrow(dd), length(forms), dimnames = list(NULL, names(forms)))
    for (f in seq_len(k)) {
      tr <- fold != f; te <- fold == f
      for (nm in names(forms)) {
        fm <- as.formula(forms[[nm]])
        m <- if (binary) suppressWarnings(glm(fm, binomial(), dd[tr, ])) else lm(fm, dd[tr, ])
        pred[te, nm] <- suppressWarnings(predict(m, dd[te, ], type = "response"))
      }
    }
    yy <- dd[[y]]
    for (nm in names(forms)) {
      p <- pred[, nm]
      metric <- if (binary) auc(yy, p) else 1 - sum((yy - p)^2) / sum((yy - mean(yy))^2)
      res[[length(res) + 1]] <- tibble(outcome = y, rep = r, model = nm, metric = metric)
    }
  }
  bind_rows(res)
}
cv <- bind_rows(lapply(seq_len(nrow(outcomes)), function(i)
  cv_one(outcomes$var[i], outcomes$binary[i])))
cv_sum <- cv |>
  tidyr::pivot_wider(names_from = model, values_from = metric) |>
  mutate(gain_ntl_over_year = ntl_only - year_only,
         gain_ntl_over_controls = controls_ntl - controls) |>
  group_by(outcome) |>
  summarise(across(c(year_only, ntl_only, controls, controls_ntl,
                     gain_ntl_over_year, gain_ntl_over_controls),
                   list(mean = mean, sd = sd)), .groups = "drop") |>
  left_join(outcomes |> select(var, label, family, binary), by = c("outcome" = "var")) |>
  mutate(metric = ifelse(binary, "AUC", "OOS R2"))
write.csv(cv_sum, file.path(out, "cv_prediction.csv"), row.names = FALSE)

options(width = 220)
print(att |> filter(added %in% c("(none)", "region", "log_gdppc", "urban", "log_mean_rad", "log_area")) |>
        mutate(cell = sprintf("%+.2f (%3.0f%%)", est, pct_of_bivariate)) |>
        select(predictor, outcome, added, cell) |>
        tidyr::pivot_wider(names_from = added, values_from = cell), n = 50)
print(cors)
print(cv_sum |> transmute(outcome, metric, year_only = round(year_only_mean, 3), ntl_only = round(ntl_only_mean, 3),
                          controls = round(controls_mean, 3), controls_ntl = round(controls_ntl_mean, 3),
                          gain = round(gain_ntl_over_controls_mean, 4), gain_sd = round(gain_ntl_over_controls_sd, 4)), n = 50)
