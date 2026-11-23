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
#' @section Comparability:
#' \itemize{
#'   \item Years 1992-2011 are reconstructed from DMSP-OLS and 2012 onward are
#'     native VIIRS. Dim rural pixels drop sharply in 2012, so compare levels
#'     across the break with care; see [ntl_flag_quality()] and
#'     `ntl_gini_trend(split_sensor = TRUE)`.
#'   \item `total_gini` does not depend on `level`, but the split between
#'     components does: finer levels move inequality from `within` to
#'     `between`. Compare `between_share` across countries only at levels with
#'     similar numbers of subunits (`n_groups`).
#'   \item With few subunits (small `n_groups`) the overlap term is large and
#'     unstable.
#'   \item Values near 1 are common because most land pixels are unlit; they
#'     mainly reflect how concentrated settlement is.
#' }
#' @source Derived from Cheng et al. (2026) <doi:10.5194/essd-18-3449-2026>
#'   and GADM 4.1 boundaries.
"ntl_gini_global"

#' Country names and regions
#'
#' Lookup table for every country and territory in [ntl_gini_global], used by
#' [ntl_add_country_info()] and the `region` / `continent` filters of
#' [ntl_gini_decomp()].
#'
#' @format A tibble with 228 rows and 5 variables:
#' \describe{
#'   \item{iso3}{ISO 3166-1 alpha-3 code as used by GADM 4.1 (includes the
#'     GADM-specific codes `XAD`, `XKO` and `ZNC`).}
#'   \item{country}{English short name.}
#'   \item{continent}{Continent (`"Africa"`, `"Americas"`, `"Asia"`,
#'     `"Europe"`, `"Oceania"`).}
#'   \item{region}{World Bank region.}
#'   \item{un_subregion}{United Nations geoscheme subregion.}
#' }
#' @source Built with the 'countrycode' package (Arel-Bundock et al. 2018),
#'   with manual entries for GADM-specific codes; see `data-raw/ntl_countries.R`.
"ntl_countries"
