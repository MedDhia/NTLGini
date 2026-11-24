utils::globalVariables(".data")

.require_ggplot2 <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plotting. ",
      "Install with install.packages('ggplot2').",
      call. = FALSE
    )
  }
}

#' Plot Gini decomposition series over time (1992-2024)
#'
#' Plots either all four Pyatt/Dagum components (`metric = "all"`) or a single
#' Gini component (`"between_gini"`, `"within_gini"`, `"total_gini"`, or
#' `"overlap_gini"`) across one or more countries at a chosen administrative
#' level, marking the 2012 DMSP-simulated to VIIRS transition.
#'
#' @param iso3 Character vector of ISO3 country codes.
#' @param level Single administrative level (`"adm1"`, `"adm2"`, `"adm3"`, or `"adm4"`).
#' @param years Integer vector of years to plot (default `1992:2024`).
#' @param metric `"all"` to facet by country with all four components, or one
#'   of `"between_gini"`, `"within_gini"`, `"total_gini"`, `"overlap_gini"` to
#'   compare countries on a single panel.
#' @return A `ggplot` object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ntl_gini_plot_series(c("EGY", "SYR", "TUN"), level = "adm3", metric = "between_gini")
#' }
#' @export
ntl_gini_plot_series <- function(iso3,
                                 level = c("adm1", "adm2", "adm3", "adm4"),
                                 years = 1992L:2024L,
                                 metric = c("all", "between_gini", "within_gini", "total_gini", "overlap_gini")) {
  .require_ggplot2()
  level <- match.arg(level)
  metric <- match.arg(metric)
  df <- ntl_gini_decomp(iso3 = iso3, level = level, years = years)
  if (nrow(df) == 0L) stop("No rows match the requested iso3/level/years.", call. = FALSE)

  metrics <- c(
    total_gini = "Overall spatial Gini",
    between_gini = "Inter-regional Gini",
    within_gini = "Intra-regional Gini",
    overlap_gini = "Cross-region overlap"
  )
  metric_subtitles <- c(
    total_gini = "Total nighttime-light inequality across 250,000 m\u00b2 (500 m) grid cells nationwide",
    between_gini = sprintf("Nighttime-light inequality across %s regions (differences in regional averages)", toupper(level)),
    within_gini = sprintf("Nighttime-light inequality inside %s regions (weighted average of internal disparities)", toupper(level)),
    overlap_gini = sprintf("Inequality from overlapping nighttime-light distributions across %s regions", toupper(level))
  )

  if (metric != "all") {
    plot_df <- data.frame(
      iso3 = df$iso3,
      year = df$year,
      value = df[[metric]],
      stringsAsFactors = FALSE
    )
    return(
      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = .data[["year"]], y = .data[["value"]], color = .data[["iso3"]])
      ) +
        ggplot2::geom_vline(xintercept = 2012, linetype = "dashed", color = "#64748b", linewidth = 0.5) +
        ggplot2::geom_line(linewidth = 0.95) +
        ggplot2::labs(
          title = sprintf("%s (%s, 1992-2024)", metrics[[metric]], toupper(level)),
          subtitle = metric_subtitles[[metric]],
          x = "Year", y = metrics[[metric]], color = "Country"
        ) +
        ggplot2::theme_minimal(base_size = 11) +
        ggplot2::theme(legend.position = "bottom")
    )
  }

  long_df <- do.call(rbind, lapply(names(metrics), function(col) {
    data.frame(
      iso3 = df$iso3,
      year = df$year,
      component = factor(metrics[[col]], levels = unname(metrics)),
      value = df[[col]],
      stringsAsFactors = FALSE
    )
  }))

  p <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = .data[["year"]], y = .data[["value"]], color = .data[["component"]])
  ) +
    ggplot2::geom_vline(xintercept = 2012, linetype = "dashed", color = "#64748b", linewidth = 0.5) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = sprintf("Nighttime-Light Gini Decomposition (%s)", toupper(level)),
      subtitle = sprintf(
        "Overall spatial Gini split into inter-regional (across %s), intra-regional (inside %s), and overlap",
        toupper(level), toupper(level)
      ),
      x = "Year", y = "Gini Coefficient", color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")

  if (length(unique(long_df$iso3)) > 1L) {
    p <- p + ggplot2::facet_wrap(~iso3)
  }
  p
}

#' Plot Pyatt/Dagum Gini decomposition shares as a stacked area chart
#'
#' Visualizes how `between_share`, `within_share`, and `overlap_share` evolve
#' as proportions of total national nighttime-light Gini.
#'
#' @param iso3 Character vector of ISO3 country codes.
#' @param level Single administrative level (`"adm1"`..`"adm4"`).
#' @param years Integer vector of years to plot (default `1992:2024`).
#' @return A `ggplot` object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ntl_gini_plot_decomp("TUN", level = "adm2")
#' }
#' @export
ntl_gini_plot_decomp <- function(iso3,
                                 level = c("adm1", "adm2", "adm3", "adm4"),
                                 years = 1992L:2024L) {
  .require_ggplot2()
  level <- match.arg(level)
  df <- ntl_gini_decomp(iso3 = iso3, level = level, years = years)
  if (nrow(df) == 0L) stop("No rows match the requested iso3/level/years.", call. = FALSE)

  shares <- c(
    between_share = "Inter-regional share",
    within_share = "Intra-regional share",
    overlap_share = "Cross-region overlap share"
  )
  long_df <- do.call(rbind, lapply(names(shares), function(col) {
    data.frame(
      iso3 = df$iso3,
      year = df$year,
      component = factor(shares[[col]], levels = rev(unname(shares))),
      share = df[[col]] * 100,
      stringsAsFactors = FALSE
    )
  }))

  p <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = .data[["year"]], y = .data[["share"]], fill = .data[["component"]])
  ) +
    ggplot2::geom_area(alpha = 0.88, position = "stack") +
    ggplot2::geom_vline(xintercept = 2012, linetype = "dashed", color = "#0f172a", linewidth = 0.5) +
    ggplot2::labs(
      title = sprintf("Gini Decomposition Shares (%s)", toupper(level)),
      subtitle = sprintf(
        "Share of overall spatial Gini at %s from inter-regional disparities, intra-regional disparities, and overlap",
        toupper(level)
      ),
      x = "Year", y = "Share of Total Gini (%)", fill = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")

  if (length(unique(long_df$iso3)) > 1L) {
    p <- p + ggplot2::facet_wrap(~iso3)
  }
  p
}

#' Plot between-unit Gini across all available administrative levels
#'
#' Compares `between_gini` across `adm1`..`adm4` alongside `total_gini` for a
#' country or set of countries, illustrating multi-scale spatial inequality.
#'
#' @param iso3 Character vector of ISO3 country codes.
#' @param years Integer vector of years to plot (default `1992:2024`).
#' @return A `ggplot` object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ntl_gini_plot_levels(c("TUN", "MAR"))
#' }
#' @export
ntl_gini_plot_levels <- function(iso3, years = 1992L:2024L) {
  .require_ggplot2()
  df <- ntl_gini_decomp(iso3 = iso3, years = years)
  if (nrow(df) == 0L) stop("No rows match the requested iso3/years.", call. = FALSE)

  tot_df <- unique(df[, c("iso3", "year", "total_gini")])
  tot_df$series <- "Overall spatial Gini"
  tot_df$value <- tot_df$total_gini
  tot_df <- tot_df[, c("iso3", "year", "series", "value")]

  lv_df <- data.frame(
    iso3 = df$iso3,
    year = df$year,
    series = paste("Inter-unit", toupper(df$level)),
    value = df$between_gini,
    stringsAsFactors = FALSE
  )
  plot_df <- rbind(tot_df, lv_df)

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(x = .data[["year"]], y = .data[["value"]], color = .data[["series"]])
  ) +
    ggplot2::geom_vline(xintercept = 2012, linetype = "dashed", color = "#64748b", linewidth = 0.5) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::labs(
      title = "Multi-Scale Inter-Regional Gini Across Administrative Levels",
      subtitle = "Overall spatial Gini vs. inter-regional inequality (differences across units) from ADM1 to ADM4",
      x = "Year", y = "Gini Coefficient", color = NULL
    ) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(legend.position = "bottom")

  if (length(unique(plot_df$iso3)) > 1L) {
    p <- p + ggplot2::facet_wrap(~iso3)
  }
  p
}

#' Plot top administrative subunits by inequality contribution or brightness
#'
#' Calls [ntl_gini_top_units()] and renders a horizontal bar chart of the top
#' (or bottom) `n` subunits.
#'
#' @param level Administrative level (`"adm1"`..`"adm4"`).
#' @param iso3 Single ISO3 country code.
#' @param year Single calendar year (default `2024L`).
#' @param n Number of subunits to plot (default `10L`).
#' @param by Ranking column: `"within_contribution"`, `"gini"`,
#'   `"mean_radiance"`, or `"value_share"`.
#' @param decreasing Logical; `TRUE` plots highest values, `FALSE` lowest.
#' @param ... Additional arguments passed to [ntl_gini_top_units()].
#' @return A `ggplot` object.
#' @examples
#' if (interactive() && requireNamespace("arrow", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   ntl_gini_plot_top_units("adm1", iso3 = "TUN", year = 2024, n = 10)
#' }
#' @export
ntl_gini_plot_top_units <- function(level = c("adm1", "adm2", "adm3", "adm4"),
                                    iso3,
                                    year = 2024L,
                                    n = 10L,
                                    by = c("within_contribution", "gini", "mean_radiance", "value_share"),
                                    decreasing = TRUE,
                                    ...) {
  .require_ggplot2()
  level <- match.arg(level)
  by <- match.arg(by)

  top_df <- ntl_gini_top_units(
    level = level, iso3 = iso3, year = year, n = n,
    by = by, decreasing = decreasing, ...
  )
  if (nrow(top_df) == 0L) stop("No non-missing units found.", call. = FALSE)

  top_df$label <- factor(top_df$name, levels = rev(unique(top_df$name)))

  by_labels <- c(
    within_contribution = "Local contribution to inequality",
    gini = "Internal spatial Gini",
    mean_radiance = "Mean nighttime radiance",
    value_share = "Share of national light"
  )
  by_subtitles <- c(
    within_contribution = sprintf("Subunits ranked by their contribution to %s's intra-regional nighttime-light inequality", iso3),
    gini = sprintf("Subunits ranked by internal spatial Gini (nighttime-light inequality across 250,000 m\u00b2 grid cells in each %s unit)", toupper(level)),
    mean_radiance = sprintf("Subunits ranked by average nighttime-light radiance per 250,000 m\u00b2 grid cell in each %s unit", toupper(level)),
    value_share = sprintf("Subunits ranked by percentage share of %s's total national nighttime light", iso3)
  )

  ggplot2::ggplot(
    top_df,
    ggplot2::aes(x = .data[["label"]], y = .data[[by]])
  ) +
    ggplot2::geom_col(fill = "#0f766e", width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = sprintf("Top %d %s Units in %s (%d)", nrow(top_df), toupper(level), iso3, year),
      subtitle = by_subtitles[[by]],
      x = NULL,
      y = by_labels[[by]]
    ) +
    ggplot2::theme_minimal(base_size = 11)
}

.require_sf <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop(
      "Package 'sf' is required for shapefile mapping. ",
      "Install with install.packages('sf').",
      call. = FALSE
    )
  }
}

.read_spatial <- function(sf_or_path, layer = NULL) {
  if (inherits(sf_or_path, "sf")) return(sf_or_path)
  if (!is.character(sf_or_path) || length(sf_or_path) != 1L || !file.exists(sf_or_path)) {
    stop("sf_or_path must be an 'sf' object or an existing shapefile/GeoPackage/GeoJSON path.", call. = FALSE)
  }
  if (is.null(layer)) sf::st_read(sf_or_path, quiet = TRUE) else sf::st_read(sf_or_path, layer = layer, quiet = TRUE)
}

.detect_id_col <- function(cols, candidates) {
  low <- tolower(cols)
  for (cand in tolower(candidates)) {
    hit <- which(low == cand)
    if (length(hit) > 0L) return(cols[hit[1L]])
  }
  NULL
}

#' Map subunit-level nighttime-light Gini or radiance using a shapefile
#'
#' Joins `ntl_gini_units()` output onto user-supplied administrative polygons
#' (an `sf` object or path to a `.shp`, `.gpkg`, or `.geojson` file) and
#' renders a `ggplot2` choropleth map.
#'
#' @param sf_or_path An `sf` polygon object or path to a shapefile/GeoPackage/GeoJSON.
#' @param level Administrative level (`"adm1"`, `"adm2"`, `"adm3"`, or `"adm4"`).
#' @param iso3 Single ISO3 country code (e.g., `"TUN"`).
#' @param year Single calendar year (default `2024L`).
#' @param metric Metric to map: `"gini"`, `"mean_radiance"`,
#'   `"within_contribution"`, `"value_share"`, or `"n_pixels"`.
#' @param id_col Optional column name in `sf_or_path` containing unit IDs matching
#'   `gid` (auto-detected across `GID_*` and `adm*_pcode` if `NULL`).
#' @param layer Optional layer name when reading a multi-layer GeoPackage.
#' @param ... Additional arguments passed to [ntl_gini_units()].
#' @return A `ggplot` object.
#' @examples
#' if (interactive() && requireNamespace("sf", quietly = TRUE) &&
#'     requireNamespace("arrow", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   ntl_gini_map_units("path/to/gadm41_TUN.gpkg", level = "adm2", iso3 = "TUN", layer = "ADM_ADM_2")
#' }
#' @export
ntl_gini_map_units <- function(sf_or_path,
                               level = c("adm1", "adm2", "adm3", "adm4"),
                               iso3,
                               year = 2024L,
                               metric = c("gini", "mean_radiance", "within_contribution", "value_share", "n_pixels"),
                               id_col = NULL,
                               layer = NULL,
                               ...) {
  .require_ggplot2()
  .require_sf()
  level <- match.arg(level)
  metric <- match.arg(metric)
  if (length(iso3) != 1L) stop("iso3 must be a single country code.", call. = FALSE)
  if (length(year) != 1L) stop("year must be a single calendar year.", call. = FALSE)

  shp <- .read_spatial(sf_or_path, layer = layer)
  if (is.null(id_col)) {
    lv_num <- sub("^adm", "", level)
    id_col <- .detect_id_col(
      names(shp),
      c(
        paste0("GID_", lv_num),
        paste0("adm", lv_num, "_pcode"),
        paste0("ADM", lv_num, "_PCODE"),
        "gid", "shapeID", "pcode"
      )
    )
    if (is.null(id_col)) {
      stop("Could not auto-detect join ID column; please specify `id_col`.", call. = FALSE)
    }
  }

  df <- ntl_gini_units(level = level, iso3 = iso3, years = as.integer(year), ...)
  if (nrow(df) == 0L) stop("No subunit data found for the requested iso3/level/year.", call. = FALSE)

  shp$.join_key <- as.character(shp[[id_col]])
  df$.join_key <- as.character(df$gid)
  merged <- merge(shp, df, by = ".join_key", all.x = TRUE, sort = FALSE)

  metric_labels <- c(
    gini = "Internal spatial Gini",
    mean_radiance = "Mean nighttime radiance",
    within_contribution = "Local contribution to inequality",
    value_share = "Share of national light",
    n_pixels = "500 m grid cells"
  )
  metric_subtitles <- c(
    gini = sprintf("Nighttime-light inequality across 250,000 m\u00b2 (500 m) grid cells inside each %s unit", toupper(level)),
    mean_radiance = sprintf("Average nighttime-light radiance per 250,000 m\u00b2 (500 m) grid cell inside each %s unit", toupper(level)),
    within_contribution = sprintf("Each %s unit's contribution to national intra-regional inequality", toupper(level)),
    value_share = sprintf("Percentage of %s's total nighttime light emitted within each %s unit", iso3, toupper(level)),
    n_pixels = sprintf("Count of 250,000 m\u00b2 (500 m x 500 m) grid cells inside each %s unit", toupper(level))
  )

  ggplot2::ggplot(merged) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data[[metric]]),
      color = "#475569",
      linewidth = 0.12
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "inferno",
      direction = -1,
      na.value = "#cbd5e1",
      name = metric_labels[[metric]]
    ) +
    ggplot2::labs(
      title = sprintf("%s (%s, %s %d)", metric_labels[[metric]], iso3, toupper(level), year),
      subtitle = metric_subtitles[[metric]]
    ) +
    ggplot2::theme_void(base_size = 11)
}

#' Map country-level Gini decompositions using a world or regional shapefile
#'
#' Joins bundled `ntl_gini_decomp()` output onto country polygons (`sf` object
#' or shapefile path) and renders a `ggplot2` choropleth map.
#'
#' @param sf_or_path An `sf` country polygon object or path to a shapefile/GeoPackage/GeoJSON.
#' @param level Administrative level (`"adm1"`, `"adm2"`, `"adm3"`, or `"adm4"`).
#' @param year Single calendar year (default `2024L`).
#' @param metric Metric to map: `"between_gini"`, `"total_gini"`,
#'   `"within_gini"`, `"between_share"`, or `"within_share"`.
#' @param id_col Optional ISO3 column name in `sf_or_path` (auto-detected across
#'   `ISO_A3`, `ADM0_A3`, `GID_0`, `iso3`, `ISO3` if `NULL`).
#' @param layer Optional layer name when reading a multi-layer GeoPackage.
#' @return A `ggplot` object.
#' @examples
#' if (interactive() && requireNamespace("sf", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   ntl_gini_map_countries("path/to/world_countries.shp", level = "adm1", year = 2024)
#' }
#' @export
ntl_gini_map_countries <- function(sf_or_path,
                                   level = c("adm1", "adm2", "adm3", "adm4"),
                                   year = 2024L,
                                   metric = c("between_gini", "total_gini", "within_gini", "between_share", "within_share"),
                                   id_col = NULL,
                                   layer = NULL) {
  .require_ggplot2()
  .require_sf()
  level <- match.arg(level)
  metric <- match.arg(metric)
  if (length(year) != 1L) stop("year must be a single calendar year.", call. = FALSE)

  shp <- .read_spatial(sf_or_path, layer = layer)
  if (is.null(id_col)) {
    id_col <- .detect_id_col(
      names(shp),
      c("ISO_A3", "ADM0_A3", "GID_0", "iso3", "ISO3", "SOV_A3", "adm0_pcode")
    )
    if (is.null(id_col)) {
      stop("Could not auto-detect ISO3 column; please specify `id_col`.", call. = FALSE)
    }
  }

  df <- ntl_gini_decomp(level = level, years = as.integer(year))
  key <- as.character(shp[[id_col]])
  if (identical(id_col, "ISO_A3") && "ADM0_A3" %in% names(shp)) {
    bad <- is.na(key) | key == "-99"
    key[bad] <- as.character(shp[["ADM0_A3"]][bad])
  }
  shp$.join_key <- key
  df$.join_key <- as.character(df$iso3)
  merged <- merge(shp, df, by = ".join_key", all.x = TRUE, sort = FALSE)

  metric_labels <- c(
    between_gini = "Inter-regional Gini",
    total_gini = "Overall spatial Gini",
    within_gini = "Intra-regional Gini",
    between_share = "Inter-regional share",
    within_share = "Intra-regional share"
  )
  metric_subtitles <- c(
    between_gini = sprintf("Nighttime-light inequality across %s administrative regions within each country", toupper(level)),
    total_gini = "Total nighttime-light inequality across 250,000 m\u00b2 (500 m) grid cells in each country",
    within_gini = sprintf("Average nighttime-light inequality across 250,000 m\u00b2 grid cells inside %s regions", toupper(level)),
    between_share = sprintf("Share of national spatial inequality driven by disparities across %s regions", toupper(level)),
    within_share = sprintf("Share of national spatial inequality occurring inside %s regions", toupper(level))
  )

  ggplot2::ggplot(merged) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data[[metric]]),
      color = "#475569",
      linewidth = 0.15
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "plasma",
      na.value = "#e2e8f0",
      name = metric_labels[[metric]]
    ) +
    ggplot2::labs(
      title = sprintf("Global %s (%s, %d)", metric_labels[[metric]], toupper(level), year),
      subtitle = metric_subtitles[[metric]]
    ) +
    ggplot2::theme_void(base_size = 11)
}

.require_terra <- function() {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "Package 'terra' is required for 500 m grid-cell raster mapping. ",
      "Install with install.packages('terra').",
      call. = FALSE
    )
  }
}

.read_raster <- function(raster_or_path) {
  if (inherits(raster_or_path, "SpatRaster")) return(raster_or_path)
  if (!is.character(raster_or_path) || length(raster_or_path) != 1L || !file.exists(raster_or_path)) {
    stop("raster_or_path must be a 'SpatRaster' object or an existing GeoTIFF/raster file path.", call. = FALSE)
  }
  terra::rast(raster_or_path)
}

.prepare_grid_df <- function(r, shp = NULL, transform = c("log1p", "linear"), max_cells = 500000L) {
  transform <- match.arg(transform)
  if (!is.null(shp)) {
    shp_proj <- sf::st_transform(shp, terra::crs(r))
    v <- terra::vect(shp_proj)
    r <- terra::crop(r, v)
    r <- terra::mask(r, v)
  }
  nc <- terra::ncell(r)
  if (nc > max_cells) {
    fact <- max(2L, as.integer(ceiling(sqrt(nc / max_cells))))
    r <- terra::aggregate(r, fact = fact, fun = "mean", na.rm = TRUE)
  }
  df <- terra::as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3L] <- "radiance"
  df$fill_val <- if (transform == "log1p") log1p(pmax(df$radiance, 0)) else df$radiance
  df
}

#' Map 500 m (250,000 m2) grid-cell nighttime-light radiance with administrative units
#'
#' Reads a 500 m-resolution nighttime-light raster (`SpatRaster` or GeoTIFF path),
#' optionally crops and masks it to administrative unit polygons (`sf_or_path`),
#' and renders a `ggplot2` grid-cell map with unit borders overlaid.
#'
#' @param raster_or_path A `terra::SpatRaster` object or path to a GeoTIFF/raster file.
#' @param sf_or_path Optional `sf` polygon object or path to a shapefile/GeoPackage/GeoJSON
#'   for cropping, masking, and overlaying administrative unit borders.
#' @param unit_id Optional character vector of unit IDs (matched against `id_col` or
#'   auto-detected `GID_*`/`adm*_pcode` columns) to zoom into specific units.
#' @param id_col Optional column name in `sf_or_path` containing unit IDs.
#' @param layer Optional layer name when reading a multi-layer GeoPackage.
#' @param transform Value scale: `"log1p"` (default) or `"linear"`.
#' @param max_cells Maximum number of grid cells to plot before aggregating (default `500000L`).
#' @param border_color Color for administrative unit borders (default `"#e2e8f0"`).
#' @param border_width Line width for administrative unit borders (default `0.25`).
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#' @return A `ggplot` object.
#' @examples
#' if (interactive() && requireNamespace("terra", quietly = TRUE) &&
#'     requireNamespace("sf", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   # ntl_gini_map_grid("path/to/ntl_500m_2024.tif", sf_or_path = "path/to/adm2.gpkg")
#' }
#' @export
ntl_gini_map_grid <- function(raster_or_path,
                              sf_or_path = NULL,
                              unit_id = NULL,
                              id_col = NULL,
                              layer = NULL,
                              transform = c("log1p", "linear"),
                              max_cells = 500000L,
                              border_color = "#e2e8f0",
                              border_width = 0.25,
                              title = NULL,
                              subtitle = NULL) {
  .require_ggplot2()
  .require_terra()
  transform <- match.arg(transform)

  r <- .read_raster(raster_or_path)
  shp <- NULL
  if (!is.null(sf_or_path)) {
    .require_sf()
    shp <- .read_spatial(sf_or_path, layer = layer)
    if (!is.null(unit_id)) {
      if (is.null(id_col)) {
        id_col <- .detect_id_col(
          names(shp),
          c("GID_4", "GID_3", "GID_2", "GID_1", "adm4_pcode", "adm3_pcode",
            "adm2_pcode", "adm1_pcode", "gid", "shapeID", "pcode")
        )
        if (is.null(id_col)) {
          stop("Could not auto-detect unit ID column; please specify `id_col`.", call. = FALSE)
        }
      }
      shp <- shp[as.character(shp[[id_col]]) %in% as.character(unit_id), , drop = FALSE]
      if (nrow(shp) == 0L) stop("No polygons match the requested `unit_id`.", call. = FALSE)
    }
  }

  grid_df <- .prepare_grid_df(r, shp = shp, transform = transform, max_cells = max_cells)
  fill_name <- if (transform == "log1p") "log(1 + radiance)" else "Radiance"
  if (is.null(title)) title <- "500 m Grid-Cell Nighttime-Light Radiance"
  if (is.null(subtitle)) {
    subtitle <- if (is.null(shp)) {
      "Satellite-observed nighttime-light radiance at 250,000 m\u00b2 (500 m x 500 m) grid-cell resolution"
    } else {
      sprintf("250,000 m\u00b2 (500 m x 500 m) grid-cell radiance overlaid with %d administrative unit(s)", nrow(shp))
    }
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = grid_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["fill_val"]])
    ) +
    ggplot2::scale_fill_viridis_c(option = "inferno", name = fill_name)

  if (!is.null(shp)) {
    shp_plot <- sf::st_transform(shp, terra::crs(r))
    p <- p + ggplot2::geom_sf(
      data = shp_plot,
      fill = NA,
      color = border_color,
      linewidth = border_width
    )
  }

  p +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_void(base_size = 11)
}

#' Map 500 m (250,000 m2) grid-cell radiance with administrative unit Gini labels
#'
#' Combines a 500 m-resolution nighttime-light raster (`raster_or_path`),
#' administrative unit polygons (`sf_or_path`), and `ntl_gini_units()` statistics
#' to map 250,000 m2 grid cells alongside administrative unit boundaries and
#' internal spatial Gini annotations.
#'
#' @param raster_or_path A `terra::SpatRaster` object or path to a GeoTIFF/raster file.
#' @param sf_or_path An `sf` polygon object or path to a shapefile/GeoPackage/GeoJSON.
#' @param level Administrative level (`"adm1"`, `"adm2"`, `"adm3"`, or `"adm4"`).
#' @param iso3 Single ISO3 country code (e.g., `"TUN"`).
#' @param year Single calendar year (default `2024L`).
#' @param unit_id Optional character vector of unit IDs (`gid`) to zoom into specific units.
#' @param id_col Optional column name in `sf_or_path` matching `gid`.
#' @param layer Optional layer name when reading a multi-layer GeoPackage.
#' @param transform Value scale: `"log1p"` (default) or `"linear"`.
#' @param label_units Logical; whether to label units with name and internal Gini
#'   (defaults to `TRUE` when 25 or fewer units are plotted).
#' @param max_cells Maximum number of grid cells to plot before aggregating (default `500000L`).
#' @param ... Additional arguments passed to [ntl_gini_units()].
#' @return A `ggplot` object.
#' @examples
#' if (interactive() && requireNamespace("terra", quietly = TRUE) &&
#'     requireNamespace("sf", quietly = TRUE) &&
#'     requireNamespace("arrow", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   # ntl_gini_map_grid_units("ntl_2024.tif", "adm1.gpkg", level = "adm1", iso3 = "TUN")
#' }
#' @export
ntl_gini_map_grid_units <- function(raster_or_path,
                                    sf_or_path,
                                    level = c("adm1", "adm2", "adm3", "adm4"),
                                    iso3,
                                    year = 2024L,
                                    unit_id = NULL,
                                    id_col = NULL,
                                    layer = NULL,
                                    transform = c("log1p", "linear"),
                                    label_units = NULL,
                                    max_cells = 500000L,
                                    ...) {
  .require_ggplot2()
  .require_sf()
  .require_terra()
  level <- match.arg(level)
  transform <- match.arg(transform)
  if (length(iso3) != 1L) stop("iso3 must be a single country code.", call. = FALSE)
  if (length(year) != 1L) stop("year must be a single calendar year.", call. = FALSE)

  r <- .read_raster(raster_or_path)
  shp <- .read_spatial(sf_or_path, layer = layer)
  if (is.null(id_col)) {
    lv_num <- sub("^adm", "", level)
    id_col <- .detect_id_col(
      names(shp),
      c(
        paste0("GID_", lv_num),
        paste0("adm", lv_num, "_pcode"),
        paste0("ADM", lv_num, "_PCODE"),
        "gid", "shapeID", "pcode"
      )
    )
    if (is.null(id_col)) {
      stop("Could not auto-detect join ID column; please specify `id_col`.", call. = FALSE)
    }
  }

  df <- ntl_gini_units(level = level, iso3 = iso3, years = as.integer(year), ...)
  if (nrow(df) == 0L) stop("No subunit data found for the requested iso3/level/year.", call. = FALSE)

  shp$.join_key <- as.character(shp[[id_col]])
  df$.join_key <- as.character(df$gid)
  if (!is.null(unit_id)) {
    shp <- shp[shp$.join_key %in% as.character(unit_id), , drop = FALSE]
    if (nrow(shp) == 0L) stop("No polygons match the requested `unit_id`.", call. = FALSE)
  }
  merged <- merge(shp, df, by = ".join_key", all.x = TRUE, sort = FALSE)

  grid_df <- .prepare_grid_df(r, shp = merged, transform = transform, max_cells = max_cells)
  fill_name <- if (transform == "log1p") "log(1 + radiance)" else "Radiance"

  merged_proj <- sf::st_transform(merged, terra::crs(r))
  p <- ggplot2::ggplot() +
    ggplot2::geom_raster(
      data = grid_df,
      ggplot2::aes(x = .data[["x"]], y = .data[["y"]], fill = .data[["fill_val"]])
    ) +
    ggplot2::scale_fill_viridis_c(option = "inferno", name = fill_name) +
    ggplot2::geom_sf(
      data = merged_proj,
      fill = NA,
      color = "#e2e8f0",
      linewidth = 0.3
    )

  if (is.null(label_units)) label_units <- nrow(merged_proj) <= 25L
  if (isTRUE(label_units) && "gini" %in% names(merged_proj)) {
    pts <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(merged_proj)))
    coords <- sf::st_coordinates(pts)
    lbl_df <- data.frame(
      x = coords[, 1L],
      y = coords[, 2L],
      label = sprintf("%s\nGini=%.2f", merged_proj$name, merged_proj$gini),
      stringsAsFactors = FALSE
    )
    lbl_df <- lbl_df[!is.na(merged_proj$gini), , drop = FALSE]
    if (nrow(lbl_df) > 0L) {
      p <- p + ggplot2::geom_label(
        data = lbl_df,
        ggplot2::aes(x = .data[["x"]], y = .data[["y"]], label = .data[["label"]]),
        size = 2.6,
        alpha = 0.78
      )
    }
  }

  p +
    ggplot2::labs(
      title = sprintf("500 m Grid-Cell Radiance & %s Units (%s, %d)", toupper(level), iso3, year),
      subtitle = sprintf(
        "250,000 m\u00b2 (500 m x 500 m) satellite grid cells overlaid with %d %s unit(s)",
        nrow(merged_proj), toupper(level)
      )
    ) +
    ggplot2::theme_void(base_size = 11)
}
