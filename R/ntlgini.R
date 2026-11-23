#' Filter country-level Gini decompositions (ADM_1 to ADM_4, 1992-2024)
#'
#' @param iso3 Optional character vector of ISO3 country codes to filter.
#' @param level Optional administrative level (`"adm1"`, `"adm2"`, `"adm3"`, `"adm4"`).
#' @param years Optional integer vector of years (`1992:2024`) to filter.
#' @return A tibble of country-year Gini decompositions.
#' @examples
#' ntl_gini_decomp(iso3 = c("TUN", "MAR"), years = 2020:2024)
#' @export
ntl_gini_decomp <- function(iso3 = NULL, level = NULL, years = NULL) {
  df <- ntlgini::ntl_gini_global
  if (!is.null(iso3)) df <- df[df$iso3 %in% iso3, , drop = FALSE]
  if (!is.null(level)) df <- df[df$level %in% level, , drop = FALSE]
  if (!is.null(years)) df <- df[df$year %in% years, , drop = FALSE]
  tibble::as_tibble(df)
}

#' Compute log-linear longitudinal Gini trends (percent per year)
#'
#' Fits OLS log-linear trends (`log(metric) ~ year`) over `"full"` (1992-2024),
#' `"dmsp"` (1992-2011), or `"viirs"` (2012-2024) windows for each
#' country-level pair in `ntl_gini_global`.
#'
#' @param iso3 Optional character vector of ISO3 country codes to filter.
#' @param level Optional administrative level (`"adm1"`..`"adm4"`).
#' @param window Time window: `"full"` (1992-2024), `"dmsp"` (1992-2011), or
#'   `"viirs"` (2012-2024).
#' @param metric Metric column to fit: `"between_gini"`, `"total_gini"`, or
#'   `"within_gini"`.
#' @return A tibble with `iso3`, `level`, `window`, `metric`, `first_year`,
#'   `last_year`, `n_years`, `percent_per_year`, and `r_squared`.
#' @examples
#' ntl_gini_trends(iso3 = c("TUN", "MAR"), window = "viirs")
#' @export
ntl_gini_trends <- function(iso3 = NULL,
                            level = NULL,
                            window = c("full", "dmsp", "viirs"),
                            metric = c("between_gini", "total_gini", "within_gini")) {
  window <- match.arg(window)
  metric <- match.arg(metric)

  yr_range <- switch(
    window,
    full = 1992L:2024L,
    dmsp = 1992L:2011L,
    viirs = 2012L:2024L
  )

  df <- ntl_gini_decomp(iso3 = iso3, level = level, years = yr_range)
  if (nrow(df) == 0) {
    return(tibble::tibble(
      iso3 = character(), level = character(), window = character(),
      metric = character(), first_year = integer(), last_year = integer(),
      n_years = integer(), percent_per_year = double(), r_squared = double()
    ))
  }

  keys <- unique(df[, c("iso3", "level")])
  rows <- vector("list", nrow(keys))
  for (i in seq_len(nrow(keys))) {
    c_iso <- keys$iso3[[i]]
    c_lv <- keys$level[[i]]
    sub <- df[df$iso3 == c_iso & df$level == c_lv, , drop = FALSE]
    y_val <- sub[[metric]]
    ok <- !is.na(y_val) & y_val > 0
    sub <- sub[ok, , drop = FALSE]
    if (nrow(sub) < 3) {
      pct <- NA_real_
      r2 <- NA_real_
    } else {
      fit <- stats::lm(log(sub[[metric]]) ~ sub$year)
      pct <- unname(stats::coef(fit)[2]) * 100
      r2 <- summary(fit)$r.squared
    }
    rows[[i]] <- data.frame(
      iso3 = c_iso,
      level = c_lv,
      window = window,
      metric = metric,
      first_year = if (nrow(sub) > 0) min(sub$year) else NA_integer_,
      last_year = if (nrow(sub) > 0) max(sub$year) else NA_integer_,
      n_years = nrow(sub),
      percent_per_year = pct,
      r_squared = r2,
      stringsAsFactors = FALSE
    )
  }
  tibble::as_tibble(do.call(rbind, rows))
}

#' Load subunit-level Gini series for a given administrative level
#'
#' Reads subunit-level Parquet tables (`gini_by_unit_adm1.parquet` through
#' `gini_by_unit_adm4.parquet`) using `arrow` predicate pushdown when filtering
#' by `iso3` or `years`. If the file is not yet present in `data_dir`, it is
#' downloaded from the `ntlgini` GitHub release (`v0.1.0`) and cached in
#' `tools::R_user_dir("ntlgini", "cache")`.
#'
#' @param level Administrative level (`"adm1"`, `"adm2"`, `"adm3"`, or `"adm4"`).
#' @param iso3 Optional character vector of ISO3 country codes to filter.
#' @param years Optional integer vector of years to filter.
#' @param data_dir Directory where Parquet files are cached.
#' @param download Logical; whether to download missing Parquet files from
#'   GitHub Releases.
#' @return A tibble of subunit-year Gini coefficients and within-contributions.
#' @examples
#' if (interactive() && requireNamespace("arrow", quietly = TRUE)) {
#'   tun_adm4 <- ntl_gini_units(level = "adm4", iso3 = "TUN", years = 2010:2024)
#' }
#' @export
ntl_gini_units <- function(level = c("adm1", "adm2", "adm3", "adm4"),
                           iso3 = NULL,
                           years = NULL,
                           data_dir = tools::R_user_dir("ntlgini", which = "cache"),
                           download = TRUE) {
  level <- match.arg(level)
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "Package 'arrow' is required to read subunit Parquet tables. ",
      "Install with install.packages('arrow').",
      call. = FALSE
    )
  }

  fname <- sprintf("gini_by_unit_%s.parquet", level)
  path <- file.path(data_dir, fname)

  if (!file.exists(path)) {
    if (!download) {
      stop(fname, " not found in ", data_dir, " and download = FALSE.", call. = FALSE)
    }
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    url <- paste0(
      "https://github.com/MedDhia/NTLGini/releases/download/v0.1.0/",
      fname
    )
    tmp <- paste0(path, ".tmp")
    status <- tryCatch(
      suppressWarnings(
        utils::download.file(url, destfile = tmp, mode = "wb", quiet = TRUE)
      ),
      error = function(e) 1L
    )
    if (!identical(status, 0L) || !file.exists(tmp) || isTRUE(file.info(tmp)$size <= 0)) {
      unlink(tmp)
      stop("Failed to download ", fname, " from ", url, call. = FALSE)
    }
    file.rename(tmp, path)
  }

  # Use Arrow Scanner / Expression pushdown to avoid loading full 100 MB tables
  tab <- arrow::read_parquet(path, as_data_frame = FALSE)
  expr <- NULL
  if (!is.null(iso3)) {
    e_iso <- arrow::Expression$field_ref("iso3")$isin(as.character(iso3))
    expr <- e_iso
  }
  if (!is.null(years)) {
    e_yr <- arrow::Expression$field_ref("year")$isin(as.integer(years))
    expr <- if (is.null(expr)) e_yr else (expr & e_yr)
  }
  if (!is.null(expr)) {
    tab <- tab$Filter(expr)
  }
  tibble::as_tibble(as.data.frame(tab))
}

#' Inspect cached ntlgini Parquet files
#'
#' @param data_dir Cache directory (defaults to `tools::R_user_dir("ntlgini", "cache")`).
#' @return A tibble listing cached files and sizes in megabytes.
#' @examples
#' ntl_gini_cache_info()
#' @export
ntl_gini_cache_info <- function(data_dir = tools::R_user_dir("ntlgini", which = "cache")) {
  if (!dir.exists(data_dir)) {
    return(tibble::tibble(file = character(), size_mb = double(), path = character()))
  }
  files <- list.files(data_dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(files) == 0) {
    return(tibble::tibble(file = character(), size_mb = double(), path = character()))
  }
  tibble::tibble(
    file = basename(files),
    size_mb = round(file.info(files)$size / (1024^2), 2),
    path = files
  )
}

#' Clear cached ntlgini Parquet files
#'
#' @param data_dir Cache directory (defaults to `tools::R_user_dir("ntlgini", "cache")`).
#' @return Invisibly returns the number of removed files.
#' @examples
#' ntl_gini_clear_cache(data_dir = tempdir())
#' @export
ntl_gini_clear_cache <- function(data_dir = tools::R_user_dir("ntlgini", which = "cache")) {
  if (!dir.exists(data_dir)) return(invisible(0L))
  files <- list.files(data_dir, pattern = "\\.parquet$", full.names = TRUE)
  if (length(files) > 0) unlink(files)
  invisible(length(files))
}
