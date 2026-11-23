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

#' Load subunit-level Gini series for a given administrative level
#'
#' Reads subunit-level Parquet tables (`gini_by_unit_adm1.parquet` through
#' `gini_by_unit_adm4.parquet`). If the file is not yet present in `data_dir`, it is downloaded from the `ntlgini`
#' GitHub release (`v0.1.0`) and cached in `tools::R_user_dir("ntlgini", "cache")`.
#'
#' @param level Administrative level (`"adm1"`, `"adm2"`, `"adm3"`, or `"adm4"`).
#' @param iso3 Optional character vector of ISO3 country codes to filter.
#' @param years Optional integer vector of years to filter.
#' @param data_dir Directory where Parquet files are cached.
#' @param download Logical; whether to download missing Parquet files from
#'   GitHub Releases.
#' @return A tibble of subunit-year Gini coefficients and within-contributions.
#' @examples
#' \dontrun{
#' tun_adm4 <- ntl_gini_units(level = "adm4", iso3 = "TUN", years = 2010:2024)
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
      "https://github.com/MedDhia/ntlgini/releases/download/v0.1.0/",
      fname
    )
    tmp <- paste0(path, ".tmp")
    status <- tryCatch(
      utils::download.file(url, destfile = tmp, mode = "wb", quiet = TRUE),
      error = function(e) 1L
    )
    if (!identical(status, 0L) || !file.exists(tmp)) {
      unlink(tmp)
      stop("Failed to download ", fname, " from ", url, call. = FALSE)
    }
    file.rename(tmp, path)
  }

  df <- arrow::read_parquet(path)
  if (!is.null(iso3)) df <- df[df$iso3 %in% iso3, , drop = FALSE]
  if (!is.null(years)) df <- df[df$year %in% years, , drop = FALSE]
  tibble::as_tibble(df)
}
