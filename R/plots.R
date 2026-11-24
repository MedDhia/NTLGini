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
    total_gini = "Total Gini",
    between_gini = "Between-unit Gini",
    within_gini = "Within-unit Gini",
    overlap_gini = "Overlap residual"
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
    between_share = "Between-unit share",
    within_share = "Within-unit share",
    overlap_share = "Overlap share"
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
  tot_df$series <- "Total pixel Gini"
  tot_df$value <- tot_df$total_gini
  tot_df <- tot_df[, c("iso3", "year", "series", "value")]

  lv_df <- data.frame(
    iso3 = df$iso3,
    year = df$year,
    series = paste("Between", toupper(df$level)),
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
      title = "Multi-Scale Between-Unit Gini Across Administrative Levels",
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

  ggplot2::ggplot(
    top_df,
    ggplot2::aes(x = .data[["label"]], y = .data[[by]])
  ) +
    ggplot2::geom_col(fill = "#0f766e", width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = sprintf("Top %d %s Units in %s (%d)", nrow(top_df), toupper(level), iso3, year),
      x = NULL,
      y = by
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
#'   `"within_contribution"`, or `"value_share"`.
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
                               metric = c("gini", "mean_radiance", "within_contribution", "value_share"),
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
      name = metric
    ) +
    ggplot2::labs(
      title = sprintf("%s (%s, %s %d)", metric, iso3, toupper(level), year)
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

  ggplot2::ggplot(merged) +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data[[metric]]),
      color = "#475569",
      linewidth = 0.15
    ) +
    ggplot2::scale_fill_viridis_c(
      option = "plasma",
      na.value = "#e2e8f0",
      name = metric
    ) +
    ggplot2::labs(
      title = sprintf("Global %s (%s, %d)", metric, toupper(level), year)
    ) +
    ggplot2::theme_void(base_size = 11)
}
