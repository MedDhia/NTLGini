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

.adm_unit_table <- list(
  ARE = c(adm1 = "Emirates", adm2 = "Municipalities", adm3 = "Subdistricts"),
  BRA = c(adm1 = "States", adm2 = "Municipalities"),
  CHN = c(adm1 = "Provinces", adm2 = "Prefectures", adm3 = "Counties"),
  DEU = c(adm1 = "States", adm2 = "Districts", adm3 = "Municipalities"),
  DZA = c(adm1 = "Wilayas", adm2 = "Communes"),
  EGY = c(adm1 = "Governorates", adm2 = "Markazes/Kisms", adm3 = "Shyakhas/Villages"),
  FRA = c(adm1 = "Regions", adm2 = "Departments", adm3 = "Arrondissements", adm4 = "Cantons"),
  GBR = c(adm1 = "Countries", adm2 = "Counties", adm3 = "Districts"),
  IND = c(adm1 = "States/UTs", adm2 = "Districts", adm3 = "Sub-districts"),
  IRQ = c(adm1 = "Governorates", adm2 = "Districts", adm3 = "Subdistricts"),
  LBY = c(adm1 = "Regions", adm2 = "Districts"),
  MAR = c(adm1 = "Regions", adm2 = "Provinces/Prefectures", adm3 = "Circles", adm4 = "Communes"),
  MRT = c(adm1 = "Regions", adm2 = "Departments", adm3 = "Communes"),
  NER = c(adm1 = "Regions", adm2 = "Departments", adm3 = "Communes"),
  SYR = c(adm1 = "Governorates", adm2 = "Districts", adm3 = "Subdistricts"),
  TCD = c(adm1 = "Regions", adm2 = "Departments", adm3 = "Sub-prefectures"),
  TUN = c(adm1 = "Districts", adm2 = "Governorates", adm3 = "Delegations", adm4 = "Sectors"),
  USA = c(adm1 = "States", adm2 = "Counties")
)

.singularize_unit <- function(label) {
  if (grepl("ies$", label)) return(sub("ies$", "y", label))
  if (grepl("es$", label) && grepl("(Markazes|Provinces|Municipalities|Countries)$", label)) {
    if (grepl("Markazes$", label)) return(sub("es$", "", label))
    if (grepl("ies$", label)) return(sub("ies$", "y", label))
    return(sub("s$", "", label))
  }
  if (grepl("s$", label) && !grepl("UTs$", label)) return(sub("s$", "", label))
  label
}

.get_unit_label <- function(iso3, level, unit_label = NULL, plural = TRUE) {
  if (!is.null(unit_label) && nzchar(unit_label)) {
    return(if (plural) unit_label else .singularize_unit(unit_label))
  }
  entry <- .adm_unit_table[[toupper(iso3[1L])]]
  if (!is.null(entry) && level %in% names(entry)) {
    lbl <- entry[[level]]
    return(if (plural) lbl else .singularize_unit(lbl))
  }
  if (plural) paste0(toupper(level), " units") else paste0(toupper(level), " unit")
}

.format_unit_scope <- function(iso3_vec, level, unit_label = NULL) {
  iso3_u <- unique(toupper(iso3_vec))
  if (!is.null(unit_label) && nzchar(unit_label)) {
    return(unit_label)
  }
  if (length(iso3_u) == 1L) {
    return(.get_unit_label(iso3_u, level, plural = TRUE))
  }
  mapped <- vapply(iso3_u, function(code) {
    entry <- .adm_unit_table[[code]]
    if (!is.null(entry) && level %in% names(entry)) entry[[level]] else NA_character_
  }, character(1L))
  known <- mapped[!is.na(mapped)]
  if (length(known) > 0L && length(unique(known)) == 1L && length(known) == length(iso3_u)) {
    return(unique(known))
  }
  paste0(toupper(level), " regions")
}

.format_unit_caption <- function(iso3_vec, level) {
  iso3_u <- unique(toupper(iso3_vec))
  if (length(iso3_u) <= 1L || length(iso3_u) > 8L) return(NULL)
  mapped <- vapply(iso3_u, function(code) {
    entry <- .adm_unit_table[[code]]
    if (!is.null(entry) && level %in% names(entry)) entry[[level]] else NA_character_
  }, character(1L))
  known <- mapped[!is.na(mapped)]
  if (length(known) == 0L || (length(unique(known)) == 1L && length(known) == length(iso3_u))) {
    return(NULL)
  }
  parts <- paste0(names(known), ": ", unname(known))
  paste0(toupper(level), " units \u2014 ", paste(parts, collapse = " | "))
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
#' @param unit_label Optional custom label for the administrative unit (e.g.
#'   `"Governorates"`). If `NULL`, uses country-specific unit names when known.
#' @return A `ggplot` object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ntl_gini_plot_series(c("EGY", "SYR", "TUN"), level = "adm3", metric = "between_gini")
#' }
#' @export
ntl_gini_plot_series <- function(iso3,
                                 level = c("adm1", "adm2", "adm3", "adm4"),
                                 years = 1992L:2024L,
                                 metric = c("all", "between_gini", "within_gini", "total_gini", "overlap_gini"),
                                 unit_label = NULL) {
  .require_ggplot2()
  level <- match.arg(level)
  metric <- match.arg(metric)
  df <- ntl_gini_decomp(iso3 = iso3, level = level, years = years)
  if (nrow(df) == 0L) stop("No rows match the requested iso3/level/years.", call. = FALSE)

  scope_lbl <- .format_unit_scope(df$iso3, level, unit_label)
  unit_caption <- .format_unit_caption(df$iso3, level)
  single_iso <- length(unique(df$iso3)) == 1L

  metrics <- c(
    total_gini = "Overall spatial Gini",
    between_gini = "Inter-regional Gini",
    within_gini = "Intra-regional Gini",
    overlap_gini = "Cross-region overlap"
  )
  metric_subtitles <- c(
    total_gini = "Total nighttime-light inequality across 250,000 m\u00b2 (500 m) grid cells nationwide",
    between_gini = sprintf("Nighttime-light inequality across %s (differences in regional averages)", scope_lbl),
    within_gini = sprintf("Nighttime-light inequality inside %s (weighted average of internal disparities)", scope_lbl),
    overlap_gini = sprintf("Inequality from overlapping nighttime-light distributions across %s", scope_lbl)
  )

  if (metric != "all") {
    plot_df <- data.frame(
      iso3 = df$iso3,
      year = df$year,
      value = df[[metric]],
      stringsAsFactors = FALSE
    )
    title_str <- if (single_iso) {
      sprintf("%s (%s, %s, 1992-2024)", metrics[[metric]], unique(df$iso3), scope_lbl)
    } else {
      sprintf("%s (%s, 1992-2024)", metrics[[metric]], toupper(level))
    }
    return(
      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(x = .data[["year"]], y = .data[["value"]], color = .data[["iso3"]])
      ) +
        ggplot2::geom_vline(xintercept = 2012, linetype = "dashed", color = "#64748b", linewidth = 0.5) +
        ggplot2::geom_line(linewidth = 0.95) +
        ggplot2::labs(
          title = title_str,
          subtitle = metric_subtitles[[metric]],
          caption = unit_caption,
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

  all_title <- if (single_iso) {
    sprintf("Nighttime-Light Gini Decomposition (%s, %s)", unique(df$iso3), scope_lbl)
  } else {
    sprintf("Nighttime-Light Gini Decomposition (%s)", toupper(level))
  }

  p <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = .data[["year"]], y = .data[["value"]], color = .data[["component"]])
  ) +
    ggplot2::geom_vline(xintercept = 2012, linetype = "dashed", color = "#64748b", linewidth = 0.5) +
    ggplot2::geom_line(linewidth = 0.9) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      title = all_title,
      subtitle = sprintf(
        "Overall spatial Gini split into inter-regional (across %s), intra-regional (inside %s), and overlap",
        scope_lbl, scope_lbl
      ),
      caption = unit_caption,
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
#' @param unit_label Optional custom label for the administrative unit.
#' @return A `ggplot` object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ntl_gini_plot_decomp("TUN", level = "adm2")
#' }
#' @export
ntl_gini_plot_decomp <- function(iso3,
                                 level = c("adm1", "adm2", "adm3", "adm4"),
                                 years = 1992L:2024L,
                                 unit_label = NULL) {
  .require_ggplot2()
  level <- match.arg(level)
  df <- ntl_gini_decomp(iso3 = iso3, level = level, years = years)
  if (nrow(df) == 0L) stop("No rows match the requested iso3/level/years.", call. = FALSE)

  scope_lbl <- .format_unit_scope(df$iso3, level, unit_label)
  unit_caption <- .format_unit_caption(df$iso3, level)
  single_iso <- length(unique(df$iso3)) == 1L

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

  decomp_title <- if (single_iso) {
    sprintf("Gini Decomposition Shares (%s, %s)", unique(df$iso3), scope_lbl)
  } else {
    sprintf("Gini Decomposition Shares (%s)", toupper(level))
  }

  p <- ggplot2::ggplot(
    long_df,
    ggplot2::aes(x = .data[["year"]], y = .data[["share"]], fill = .data[["component"]])
  ) +
    ggplot2::geom_area(alpha = 0.88, position = "stack") +
    ggplot2::geom_vline(xintercept = 2012, linetype = "dashed", color = "#0f172a", linewidth = 0.5) +
    ggplot2::labs(
      title = decomp_title,
      subtitle = sprintf(
        "Share of overall spatial Gini across %s from inter-regional disparities, intra-regional disparities, and overlap",
        scope_lbl
      ),
      caption = unit_caption,
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

  single_iso <- length(unique(df$iso3)) == 1L
  lv_series <- if (single_iso) {
    vapply(df$level, function(lv) {
      lbl <- .get_unit_label(unique(df$iso3), lv, plural = TRUE)
      if (grepl("^ADM", lbl)) paste("Inter-unit", toupper(lv)) else paste0("Inter-", lbl, " (", toupper(lv), ")")
    }, character(1L))
  } else {
    paste("Inter-unit", toupper(df$level))
  }

  lv_df <- data.frame(
    iso3 = df$iso3,
    year = df$year,
    series = lv_series,
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
#' @param unit_label Optional custom label for the administrative unit.
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
                                    unit_label = NULL,
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
  unit_pl <- .get_unit_label(iso3, level, unit_label, plural = TRUE)
  unit_sg <- .get_unit_label(iso3, level, unit_label, plural = FALSE)

  by_labels <- c(
    within_contribution = "Local contribution to inequality",
    gini = "Internal spatial Gini",
    mean_radiance = "Mean nighttime radiance",
    value_share = "Share of national light"
  )
  by_subtitles <- c(
    within_contribution = sprintf("%s ranked by their contribution to %s's intra-regional nighttime-light inequality", unit_pl, iso3),
    gini = sprintf("%s ranked by internal spatial Gini (nighttime-light inequality across 250,000 m\u00b2 grid cells in each %s)", unit_pl, unit_sg),
    mean_radiance = sprintf("%s ranked by average nighttime-light radiance per 250,000 m\u00b2 grid cell in each %s", unit_pl, unit_sg),
    value_share = sprintf("%s ranked by percentage share of %s's total national nighttime light", unit_pl, iso3)
  )

  ggplot2::ggplot(
    top_df,
    ggplot2::aes(x = .data[["label"]], y = .data[[by]])
  ) +
    ggplot2::geom_col(fill = "#0f766e", width = 0.7) +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = sprintf("Top %d %s in %s (%d)", nrow(top_df), unit_pl, iso3, year),
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
#'   `"within_contribution"`, or `"value_share"`.
#' @param id_col Optional column name in `sf_or_path` containing unit IDs matching
#'   `gid` (auto-detected across `GID_*` and `adm*_pcode` if `NULL`).
#' @param layer Optional layer name when reading a multi-layer GeoPackage.
#' @param unit_label Optional custom label for the administrative unit (e.g.
#'   `"Sectors"`). If `NULL`, checks `.adm_unit_table` and `ENGTYPE_*` in `sf_or_path`.
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
                               unit_label = NULL,
                               ...) {
  .require_ggplot2()
  .require_sf()
  level <- match.arg(level)
  metric <- match.arg(metric)
  if (length(iso3) != 1L) stop("iso3 must be a single country code.", call. = FALSE)
  if (length(year) != 1L) stop("year must be a single calendar year.", call. = FALSE)

  shp <- .read_spatial(sf_or_path, layer = layer)
  lv_num <- sub("^adm", "", level)
  if (is.null(id_col)) {
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

  if (is.null(unit_label) && is.null(.adm_unit_table[[toupper(iso3)]][[level]])) {
    eng_col <- .detect_id_col(names(shp), c(paste0("ENGTYPE_", lv_num), paste0("TYPE_", lv_num)))
    if (!is.null(eng_col)) {
      vals <- unique(stats::na.omit(as.character(shp[[eng_col]])))
      vals <- vals[nzchar(vals)]
      if (length(vals) > 0L && length(vals) <= 2L) {
        unit_label <- paste(vals, collapse = " / ")
      }
    }
  }

  df <- ntl_gini_units(level = level, iso3 = iso3, years = as.integer(year), ...)
  if (nrow(df) == 0L) stop("No subunit data found for the requested iso3/level/year.", call. = FALSE)

  shp$.join_key <- as.character(shp[[id_col]])
  df$.join_key <- as.character(df$gid)
  merged <- merge(shp, df, by = ".join_key", all.x = TRUE, sort = FALSE)

  unit_pl <- .get_unit_label(iso3, level, unit_label, plural = TRUE)
  unit_sg <- .get_unit_label(iso3, level, unit_label, plural = FALSE)

  metric_labels <- c(
    gini = "Internal spatial Gini",
    mean_radiance = "Mean nighttime radiance",
    within_contribution = "Local contribution to inequality",
    value_share = "Share of national light"
  )
  metric_subtitles <- c(
    gini = sprintf("Nighttime-light inequality across 250,000 m\u00b2 (500 m) grid cells inside each %s", unit_sg),
    mean_radiance = sprintf("Average nighttime-light radiance per 250,000 m\u00b2 (500 m) grid cell inside each %s", unit_sg),
    within_contribution = sprintf("Each %s's contribution to %s's intra-regional inequality", unit_sg, iso3),
    value_share = sprintf("Percentage of %s's total nighttime light emitted within each %s", iso3, unit_sg)
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
      title = sprintf("%s (%s, %s, %d)", metric_labels[[metric]], iso3, unit_pl, year),
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
