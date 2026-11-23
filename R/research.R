#' Add country names and regions to a Gini table
#'
#' Joins [ntl_countries] onto any table with an `iso3` column, keeping the
#' original row order.
#'
#' @param df A data frame with an `iso3` column, e.g. from [ntl_gini_decomp()]
#'   or [ntl_gini_units()].
#' @return `df` as a tibble with the [ntl_countries] columns (`country`,
#'   `continent`, `region`, `un_subregion`, `income_group`, `cown`,
#'   `vdem_id`) added after `iso3`.
#' @examples
#' ntl_add_country_info(ntl_gini_decomp(iso3 = c("TUN", "BRA"), years = 2024))
#' @export
ntl_add_country_info <- function(df) {
  if (!is.data.frame(df) || !"iso3" %in% names(df)) {
    stop("`df` must be a data frame with an `iso3` column.", call. = FALSE)
  }
  meta <- ntlgini::ntl_countries
  info_cols <- setdiff(names(meta), "iso3")
  df <- as.data.frame(df)[, setdiff(names(df), info_cols), drop = FALSE]
  idx <- match(df$iso3, meta$iso3)
  added <- as.data.frame(meta)[idx, info_cols, drop = FALSE]
  pos <- match("iso3", names(df))
  out <- cbind(
    df[, seq_len(pos), drop = FALSE],
    added,
    df[, -seq_len(pos), drop = FALSE]
  )
  rownames(out) <- NULL
  tibble::as_tibble(out)
}

#' Flag observations that need care in analysis
#'
#' Adds a sensor column and logical quality flags to a country-level Gini
#' table, so they can be filtered on or used as controls.
#'
#' Flags:
#' \describe{
#'   \item{`sensor`}{`"DMSP-reconstructed"` for 1992-2011, `"VIIRS"` from 2012.
#'     Cheng et al. (2026) reconstruct the early years from DMSP-OLS; dim rural
#'     pixels drop sharply in 2012, so levels are not fully comparable across
#'     the break.}
#'   \item{`flag_sensor_break`}{`TRUE` for 2012, the first VIIRS year, whose
#'     change from 2011 partly reflects the sensor switch.}
#'   \item{`flag_few_groups`}{`TRUE` when `n_groups < min_groups`. With few
#'     subunits the between/overlap split is unstable.}
#'   \item{`flag_saturated`}{`TRUE` when `total_gini > saturation`. Near 1 the
#'     Gini mostly reflects the share of unlit pixels and has little room to
#'     vary.}
#'   \item{`flag_jump`}{`TRUE` when, from the previous year (same country and
#'     level), `total_gini` changes by more than `jump_threshold` or
#'     `between_share` changes by more than `share_jump_threshold`. Possible
#'     data artifact worth inspecting.}
#'   \item{`flag_missing`}{`TRUE` when `total_gini` is `NA`.}
#'   \item{`any_flag`}{`TRUE` when any flag above (other than `sensor`) is set.}
#' }
#'
#' @param df A country-level table from [ntl_gini_decomp()] (needs `iso3`,
#'   `level`, `year`, `n_groups`, `total_gini`).
#' @param min_groups Minimum number of subunits for a reliable decomposition.
#' @param saturation Threshold above which `total_gini` is flagged as saturated.
#' @param jump_threshold Absolute year-on-year change in `total_gini` that is
#'   flagged. The default (0.05) flags roughly the largest 2 percent of changes.
#' @param share_jump_threshold Absolute year-on-year change in `between_share`
#'   that is flagged (ignored if `df` has no `between_share` column). The
#'   default (0.15) flags roughly the largest 1 percent of changes.
#' @return `df` as a tibble with the columns above added.
#' @examples
#' flags <- ntl_flag_quality(ntl_gini_decomp(iso3 = "GAB", level = "adm1"))
#' flags[flags$flag_jump, c("iso3", "year", "total_gini", "between_share")]
#' @export
ntl_flag_quality <- function(df, min_groups = 5, saturation = 0.99,
                             jump_threshold = 0.05, share_jump_threshold = 0.15) {
  need <- c("iso3", "level", "year", "n_groups", "total_gini")
  miss <- setdiff(need, names(df))
  if (length(miss) > 0L) {
    stop("`df` is missing column(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  df <- as.data.frame(df)
  df$sensor <- ifelse(df$year <= 2011, "DMSP-reconstructed", "VIIRS")
  df$flag_sensor_break <- df$year == 2012
  df$flag_few_groups <- df$n_groups < min_groups
  df$flag_saturated <- !is.na(df$total_gini) & df$total_gini > saturation
  df$flag_missing <- is.na(df$total_gini)

  key <- paste(df$iso3, df$level, sep = "\r")
  ord <- order(key, df$year)
  same <- c(FALSE, key[ord][-1L] == key[ord][-length(ord)])
  lag_of <- function(x) {
    out <- rep(NA, length(x))
    out[ord] <- ifelse(same, c(NA, x[ord][-length(ord)]), NA)
    out
  }
  consecutive <- (df$year - lag_of(df$year)) == 1L
  jumped <- function(x, threshold) {
    j <- consecutive & abs(x - lag_of(x)) > threshold
    !is.na(j) & j
  }
  df$flag_jump <- jumped(df$total_gini, jump_threshold)
  if ("between_share" %in% names(df)) {
    df$flag_jump <- df$flag_jump | jumped(df$between_share, share_jump_threshold)
  }

  flag_cols <- c("flag_sensor_break", "flag_few_groups", "flag_saturated",
                 "flag_jump", "flag_missing")
  df$any_flag <- Reduce(`|`, df[flag_cols])
  tibble::as_tibble(df)
}

#' Summarise Gini trends per country and level
#'
#' For each country and administrative level, reports the first and last
#' values of a measure, the change between them, and an ordinary
#' least-squares slope per year with its standard error.
#'
#' @param df A country-level table from [ntl_gini_decomp()].
#' @param measure Column to summarise, e.g. `"total_gini"`, `"between_gini"`
#'   or `"between_share"`.
#' @param from,to Optional first and last years to include.
#' @param split_sensor If `TRUE`, summarise 1992-2011 (DMSP-reconstructed) and
#'   2012-2024 (VIIRS) separately, so the 2012 sensor switch does not drive
#'   the trend.
#' @return A tibble with one row per country, level (and period): `iso3`,
#'   `level`, `period`, `n_years`, `first_year`, `last_year`, `start`, `end`,
#'   `change`, `pct_change`, `slope`, `slope_se`.
#' @examples
#' d <- ntl_gini_decomp(iso3 = c("TUN", "MAR", "EGY"), level = "adm1")
#' ntl_gini_trend(d)
#' ntl_gini_trend(d, measure = "between_share", split_sensor = TRUE)
#' @export
ntl_gini_trend <- function(df, measure = "total_gini", from = NULL, to = NULL,
                           split_sensor = FALSE) {
  if (!is.character(measure) || length(measure) != 1L || !measure %in% names(df)) {
    stop("`measure` must name a column of `df`.", call. = FALSE)
  }
  if (!is.numeric(df[[measure]])) {
    stop("`measure` must be a numeric column.", call. = FALSE)
  }
  need <- c("iso3", "level", "year")
  miss <- setdiff(need, names(df))
  if (length(miss) > 0L) {
    stop("`df` is missing column(s): ", paste(miss, collapse = ", "), ".", call. = FALSE)
  }
  df <- as.data.frame(df)
  if (!is.null(from)) df <- df[df$year >= from, , drop = FALSE]
  if (!is.null(to)) df <- df[df$year <= to, , drop = FALSE]
  df <- df[!is.na(df[[measure]]), , drop = FALSE]
  df$period <- if (split_sensor) {
    ifelse(df$year <= 2011, "1992-2011", "2012-2024")
  } else {
    rep("all", nrow(df))
  }

  groups <- split(df, list(df$iso3, df$level, df$period), drop = TRUE, sep = "\r")
  rows <- lapply(groups, function(g) {
    g <- g[order(g$year), , drop = FALSE]
    y <- g[[measure]]
    x <- g$year
    n <- length(y)
    slope <- se <- NA_real_
    if (n >= 2L && stats::var(x) > 0) {
      sxx <- sum((x - mean(x))^2)
      slope <- sum((x - mean(x)) * (y - mean(y))) / sxx
      if (n >= 3L) {
        resid <- y - (mean(y) + slope * (x - mean(x)))
        se <- sqrt(sum(resid^2) / (n - 2L) / sxx)
      }
    }
    data.frame(
      iso3 = g$iso3[1L], level = g$level[1L], period = g$period[1L],
      n_years = n, first_year = x[1L], last_year = x[n],
      start = y[1L], end = y[n], change = y[n] - y[1L],
      pct_change = if (y[1L] != 0) 100 * (y[n] - y[1L]) / y[1L] else NA_real_,
      slope = slope, slope_se = se,
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0L) {
    out <- data.frame(
      iso3 = character(), level = character(), period = character(),
      n_years = integer(), first_year = integer(), last_year = integer(),
      start = numeric(), end = numeric(), change = numeric(),
      pct_change = numeric(), slope = numeric(), slope_se = numeric(),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, rows)
    out <- out[order(out$iso3, out$level, out$period), , drop = FALSE]
  }
  rownames(out) <- NULL
  tibble::as_tibble(out)
}
