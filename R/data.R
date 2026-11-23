#' Global Nighttime-Light Gini Decomposition (1992-2024)
#'
#' Country-by-year Pyatt/Dagum Gini decompositions of 500 m 'VIIRS'-like
#' nighttime lights across administrative levels `adm1` to `adm4` for 228
#' countries and territories.
#'
#' @format A tibble with 16,203 rows and 13 variables:
#' \describe{
#'   \item{iso3}{ISO 3166-1 alpha-3 country/territory code.}
#'   \item{year}{Calendar year (`1992` to `2024`).}
#'   \item{level}{Administrative level (`"adm1"`, `"adm2"`, `"adm3"`, `"adm4"`).}
#'   \item{n_groups}{Number of administrative subunits with at least one pixel.}
#'   \item{n_pixels}{Total land pixels in the country/territory.}
#'   \item{sum_of_lights}{Total nighttime-light radiance.}
#'   \item{total_gini}{National pixel-level Gini coefficient.}
#'   \item{within_gini}{Weighted within-subunit Gini component.}
#'   \item{between_gini}{Pixel-weighted between-subunit Gini component.}
#'   \item{overlap_gini}{Pyatt/Dagum transvariation overlap residual.}
#'   \item{within_share}{Share of `total_gini` due to `within_gini`.}
#'   \item{between_share}{Share of `total_gini` due to `between_gini`.}
#'   \item{overlap_share}{Share of `total_gini` due to `overlap_gini`.}
#' }
#' @source Derived from Cheng et al. (2026) <doi:10.5194/essd-18-3449-2026>
#'   and GADM 4.1 boundaries.
"ntl_gini_global"

#' Country Coverage Metadata for ntlgini
#'
#' Summary of available administrative levels (`ADM_1` to `ADM_4`), unit counts,
#' pixel counts, and boundary sources for all 228 countries and territories in
#' `ntl_gini_global`.
#'
#' @format A tibble with 228 rows and 9 variables:
#' \describe{
#'   \item{iso3}{ISO 3166-1 alpha-3 country/territory code.}
#'   \item{max_level}{Finest available administrative level (`1` to `4`).}
#'   \item{n_adm1}{Number of `ADM_1` units.}
#'   \item{n_adm2}{Number of `ADM_2` units (`NA` if unavailable).}
#'   \item{n_adm3}{Number of `ADM_3` units (`NA` if unavailable).}
#'   \item{n_adm4}{Number of `ADM_4` units (`NA` if unavailable).}
#'   \item{n_pixels}{Total 500 m land pixels.}
#'   \item{n_years}{Number of calendar years covered.}
#'   \item{boundary_source}{Boundary source (`"GADM 4.1"` or `"MapsTN"`).}
#' }
"ntl_gini_countries"
