# Follow-up on the one candidate signal from the grid: between-region Gini
# measured at ADM2 vs. regime outcomes. Is it the measure or the sample, and
# does it survive the same robustness checks as the main grid?
suppressPackageStartupMessages({ library(dplyr); library(fixest) })
here <- file.path("analysis", "spatial-inequality-politics")
out  <- file.path(here, "output")
panel <- readRDS(file.path(here, "cache", "panel.rds"))
z <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
regime <- c("v2x_polyarchy", "v2x_libdem", "polity2", "fh_total_rev", "uds_z1", "lexical_index")
ctrl  <- c("log_gdppc", "log_pop", "urban", "log_mean_rad", "rents", "log_area", "ethfrac", "rugged")
d <- panel |>
  mutate(across(c(total_gini, between_gini_adm1, between_gini_adm2, within_gini_adm2), z),
         across(all_of(regime), z)) |>
  filter(!is.na(between_gini_adm2),
         complete.cases(across(all_of(c(ctrl, "log_ngroups1", "log_ngroups2", "region")))))
cs <- d |> group_by(iso3, region) |>
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.x), NA, .x)))

C <- paste(ctrl, collapse = " + ")
variants <- list(
  "ADM1 between Gini, ADM2 sample"            = list(x = "between_gini_adm1", rhs = paste(C, "+ log_ngroups1"), fe = "year + region", data = "d"),
  "ADM2 between Gini (as in S12)"             = list(x = "between_gini_adm2", rhs = paste(C, "+ log_ngroups2"), fe = "year + region", data = "d"),
  "ADM2 between, + ADM1 between & total Gini" = list(x = "between_gini_adm2", rhs = paste(C, "+ log_ngroups2 + between_gini_adm1 + total_gini"), fe = "year + region", data = "d"),
  "ADM2 between, + both unit counts"          = list(x = "between_gini_adm2", rhs = paste(C, "+ log_ngroups1 + log_ngroups2"), fe = "year + region", data = "d"),
  "ADM2 between, country means"               = list(x = "between_gini_adm2", rhs = paste(C, "+ log_ngroups2"), fe = "region", data = "cs"),
  "ADM2 between, no region FE"                = list(x = "between_gini_adm2", rhs = paste(C, "+ log_ngroups2"), fe = "year", data = "d"),
  "ADM2 between, excl. Sub-Saharan Africa"    = list(x = "between_gini_adm2", rhs = paste(C, "+ log_ngroups2"), fe = "year + region", data = "d", sub = quote(region != "4")),
  "ADM2 between, country + year FE"           = list(x = "between_gini_adm2", rhs = "log_gdppc + log_pop + urban + log_mean_rad + rents", fe = "iso3 + year", data = "d")
)
res <- list()
for (nm in names(variants)) for (y in regime) {
  v <- variants[[nm]]
  dat <- if (v$data == "cs") cs else d
  if (!is.null(v$sub)) dat <- dat[eval(v$sub, dat), ]
  m <- feols(as.formula(paste(y, "~", v$x, "+", v$rhs, "|", v$fe)), dat,
             vcov = if (v$data == "cs") "hetero" else ~iso3, notes = FALSE)
  ct <- coeftable(m)[v$x, ]
  res[[length(res) + 1]] <- tibble(variant = nm, outcome = y, est = ct[1], se = ct[2], p = ct[4],
                                   nobs = nobs(m), n_countries = length(unique(dat$iso3[obs(m)])))
}
res <- bind_rows(res)
write.csv(res, file.path(out, "adm2_followup.csv"), row.names = FALSE)
options(width = 200)
print(res |> mutate(cell = sprintf("%+.2f%s", est, ifelse(p < .001, "***", ifelse(p < .01, "**", ifelse(p < .05, "*", ""))))) |>
        select(variant, outcome, cell) |> tidyr::pivot_wider(names_from = outcome, values_from = cell) |> as.data.frame())
print(res |> group_by(variant) |> summarise(n_countries = max(n_countries), nobs = max(nobs)))

# ---- influence checks on the cross-sectional (country-means) version -------
f_cs <- function(dat, y) {
  m <- feols(as.formula(paste(y, "~ between_gini_adm2 +", C, "+ log_ngroups2 | region")), dat,
             vcov = "hetero", notes = FALSE)
  coeftable(m)["between_gini_adm2", c(1, 4)]
}
regions <- c("1" = "E. Europe & C. Asia", "2" = "Latin America", "3" = "MENA",
             "4" = "Sub-Saharan Africa", "5" = "W. Europe & N. America", "6" = "Asia-Pacific")
loro <- bind_rows(lapply(names(regions), function(r) bind_rows(lapply(regime, function(y) {
  e <- f_cs(cs[cs$region != r, ], y); tibble(dropped = regions[[r]], outcome = y, est = e[1], p = e[2])
}))))
jk <- bind_rows(lapply(regime, function(y) {
  ids <- unique(cs$iso3)
  ests <- setNames(vapply(ids, function(i) unname(f_cs(cs[cs$iso3 != i, ], y)[1]), numeric(1)), ids)
  tibble(outcome = y, jk_min = min(ests), jk_max = max(ests),
         most_influential = names(ests)[which.max(abs(ests - f_cs(cs, y)[1]))])
}))
write.csv(loro, file.path(out, "adm2_leave_one_region_out.csv"), row.names = FALSE)
write.csv(jk, file.path(out, "adm2_jackknife.csv"), row.names = FALSE)
print(loro |> mutate(cell = sprintf("%+.2f%s", est, ifelse(p < .01, "**", ifelse(p < .05, "*", ifelse(p < .1, "+", ""))))) |>
        select(dropped, outcome, cell) |> tidyr::pivot_wider(names_from = outcome, values_from = cell) |> as.data.frame())
print(jk)
