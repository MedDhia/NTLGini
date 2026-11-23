#' Filter country-level Gini decompositions (ADM_1 to ADM_4, 1992-2024)
#'
#' @param iso3 Optional character vector of ISO3 country codes to filter.
#'   Matching is case-insensitive; unknown codes trigger a warning.
#' @param level Optional character vector of administrative levels
#'   (`"adm1"`, `"adm2"`, `"adm3"`, `"adm4"`).
#' @param years Optional integer vector of years (`1992:2024`) to filter.
#'   Years outside the covered range trigger a warning.
#' @return A tibble of country-year Gini decompositions.
#' @examples
#' ntl_gini_decomp(iso3 = c("TUN", "MAR"), years = 2020:2024)
#' ntl_gini_decomp(iso3 = "tun", level = c("adm1", "adm4"), years = 2024)
#' @export
ntl_gini_decomp <- function(iso3 = NULL, level = NULL, years = NULL) {
  iso3 <- check_iso3(iso3)
  level <- check_level(level, several_ok = TRUE)
  years <- check_years(years)

  df <- ntlgini::ntl_gini_global
  if (!is.null(iso3)) df <- df[df$iso3 %in% iso3, , drop = FALSE]
  if (!is.null(level)) df <- df[df$level %in% level, , drop = FALSE]
  if (!is.null(years)) df <- df[df$year %in% years, , drop = FALSE]
  tibble::as_tibble(df)
}

#' Load subunit-level Gini series for a given administrative level
#'
#' Reads subunit-level Parquet tables (`gini_by_unit_adm1.parquet` through
#' `gini_by_unit_adm4.parquet`). If the file is not yet present in `data_dir`,
#' it is downloaded from the `ntlgini` GitHub release matching the package's
#' data version and cached in [ntl_cache_dir()].
#'
#' When 'dplyr' is installed, filters on `iso3` and `years` are pushed down to
#' 'arrow' so only matching rows are read into memory. Without 'dplyr', the
#' whole table is read and then filtered.
#'
#' Large downloads use a timeout of at least 600 seconds (or the current
#' `getOption("timeout")` if larger). Downloaded files are verified against
#' known MD5 checksums when available.
#'
#' @param level Administrative level (`"adm1"`, `"adm2"`, `"adm3"`, or `"adm4"`).
#' @param iso3 Optional character vector of ISO3 country codes to filter
#'   (case-insensitive).
#' @param years Optional integer vector of years to filter.
#' @param data_dir Directory where Parquet files are cached. Defaults to
#'   [ntl_cache_dir()].
#' @param download Logical; whether to download missing Parquet files from
#'   GitHub Releases.
#' @return A tibble of subunit-year Gini coefficients and within-contributions.
#' @seealso [ntl_cache_list()], [ntl_cache_clear()]
#' @examples
#' if (interactive() && requireNamespace("arrow", quietly = TRUE)) {
#'   tun_adm4 <- ntl_gini_units(level = "adm4", iso3 = "TUN", years = 2010:2024)
#' }
#' @export
ntl_gini_units <- function(level = c("adm1", "adm2", "adm3", "adm4"),
                           iso3 = NULL,
                           years = NULL,
                           data_dir = ntl_cache_dir(),
                           download = TRUE) {
  level <- match.arg(level)
  iso3 <- check_iso3(iso3)
  years <- check_years(years)
  if (!is.logical(download) || length(download) != 1L || is.na(download)) {
    stop("`download` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "Package 'arrow' is required to read subunit Parquet tables. ",
      "Install with install.packages('arrow').",
      call. = FALSE
    )
  }

  fname <- unit_file_name(level)
  path <- file.path(data_dir, fname)

  if (!file.exists(path)) {
    if (!download) {
      stop(fname, " not found in ", data_dir, " and download = FALSE.", call. = FALSE)
    }
    ntl_download(fname, path)
  }

  read_units(path, iso3 = iso3, years = years)
}

# Read a Parquet table, pushing filters down to arrow when dplyr is available.
read_units <- function(path, iso3 = NULL, years = NULL) {
  if (requireNamespace("dplyr", quietly = TRUE)) {
    ds <- arrow::open_dataset(path)
    codes <- iso3
    yrs <- years
    if (!is.null(codes)) ds <- dplyr::filter(ds, iso3 %in% codes)
    if (!is.null(yrs)) ds <- dplyr::filter(ds, year %in% yrs)
    df <- dplyr::collect(ds)
  } else {
    df <- arrow::read_parquet(path)
    if (!is.null(iso3)) df <- df[df$iso3 %in% iso3, , drop = FALSE]
    if (!is.null(years)) df <- df[df$year %in% years, , drop = FALSE]
  }
  tibble::as_tibble(as.data.frame(df))
}

utils::globalVariables(c("iso3", "year", ".data"))
