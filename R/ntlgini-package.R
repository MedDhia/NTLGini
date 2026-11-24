#' ntlgini: Global Subnational Nighttime-Light Gini Evolution (1992-2024)
#'
#' Harmonized annual nighttime-light Gini coefficients and Pyatt (1976) /
#' Dagum (1997) spatial decompositions across administrative levels
#' (`ADM_1` to `ADM_4`) for 228 countries and territories from 1992 to 2024,
#' derived from 500 m x 500 m (`250,000 m^2`) VIIRS-like nighttime-light
#' imagery (Cheng et al., 2026) intersected with GADM 4.1 and HDX COD-AB
#' administrative boundaries.
#'
#' @section Spatial Inequality Decomposition:
#' For each country, year, and administrative level, total inequality across
#' `250,000 m^2` (`500 m`) land grid cells is decomposed additively into:
#' \describe{
#'   \item{Overall spatial Gini (`total_gini`)}{Total nighttime-light inequality
#'     across all grid cells nationwide.}
#'   \item{Inter-regional Gini (`between_gini`)}{Inequality across administrative
#'     regions (differences in regional average radiance).}
#'   \item{Intra-regional Gini (`within_gini`)}{Weighted average of internal
#'     grid-cell inequality inside each administrative region.}
#'   \item{Cross-region overlap (`overlap_gini`)}{Transvariation residual
#'     capturing overlapping nighttime-light distributions across regions.}
#' }
#'
#' @section Functions by Category:
#' \strong{Data & Trends}
#' \itemize{
#'   \item [ntl_gini_decomp()]: Filter bundled country-year decompositions.
#'   \item [ntl_gini_trends()]: Fit log-linear `%/yr` trends over `full`, `dmsp`, or `viirs` eras.
#'   \item [ntl_gini_units()]: Query subunit-level Parquet tables (`adm1`-`adm4`) with `arrow`.
#'   \item [ntl_gini_pixels()]: Query 0.125-deg (`30 x 30` pixel) grid-cell Parquet data.
#'   \item [ntl_gini_extract()]: Zonal statistics over ROI polygons (`blackmarblepy::bm_extract` style).
#'   \item [ntl_gini_raster()]: Multi-layer `SpatRaster` over an ROI (`blackmarblepy::bm_raster` style).
#'   \item [ntl_gini_top_units()]: Rank top or bottom subunits by inequality or radiance.
#' }
#'
#' \strong{Plots & Maps (`ggplot2` / `sf`)}
#' \itemize{
#'   \item [ntl_gini_plot_series()]: Time-series of Gini components.
#'   \item [ntl_gini_plot_decomp()]: Stacked area chart of decomposition shares.
#'   \item [ntl_gini_plot_levels()]: Multi-scale inter-regional Gini across `adm1`-`adm4`.
#'   \item [ntl_gini_plot_scatter()]: Cross-country scatter plot of decomposition metrics.
#'   \item [ntl_gini_plot_unit_scatter()]: Subunit scatter plot of radiance vs. internal Gini.
#'   \item [ntl_gini_plot_top_units()]: Horizontal bar chart of top/bottom subunits.
#'   \item [ntl_gini_map_countries()]: Choropleth map of country-level decompositions.
#'   \item [ntl_gini_map_units()]: Choropleth map of subunit-level Gini or radiance.
#'   \item [ntl_gini_map_pixels()]: Tile map of 0.125-deg grid-cell Gini or radiance.
#' }
#'
#' \strong{Cache Management}
#' \itemize{
#'   \item [ntl_gini_cache_info()]: Inspect locally cached `.parquet` files.
#'   \item [ntl_gini_clear_cache()]: Remove cached `.parquet` files.
#' }
#'
#' @section Bundled Datasets:
#' \itemize{
#'   \item [ntl_gini_global]: 17,553 country-year-level rows (228 countries, 1992-2024).
#'   \item [ntl_gini_countries]: Coverage metadata and max administrative level per country.
#' }
#'
#' @seealso
#' Useful links:
#' \itemize{
#'   \item \url{https://github.com/MedDhia/NTLGini}
#'   \item Report bugs at \url{https://github.com/MedDhia/NTLGini/issues}
#' }
#'
#' @keywords internal
"_PACKAGE"
