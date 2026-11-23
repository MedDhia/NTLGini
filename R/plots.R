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
#' Plots `total_gini`, `between_gini`, `within_gini`, and `overlap_gini` for
#' one or more countries at a chosen administrative level, marking the 2012
#' DMSP-simulated to VIIRS transition.
#'
#' @param iso3 Character vector of ISO3 country codes.
#' @param level Single administrative level (`"adm1"`, `"adm2"`, `"adm3"`, or `"adm4"`).
#' @param years Integer vector of years to plot (default `1992:2024`).
#' @return A `ggplot` object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   ntl_gini_plot_series(c("TUN", "MAR"), level = "adm1")
#' }
#' @export
ntl_gini_plot_series <- function(iso3,
                                 level = c("adm1", "adm2", "adm3", "adm4"),
                                 years = 1992L:2024L) {
  .require_ggplot2()
  level <- match.arg(level)
  df <- ntl_gini_decomp(iso3 = iso3, level = level, years = years)
  if (nrow(df) == 0L) stop("No rows match the requested iso3/level/years.", call. = FALSE)

  metrics <- c(
    total_gini = "Total Gini",
    between_gini = "Between-unit Gini",
    within_gini = "Within-unit Gini",
    overlap_gini = "Overlap residual"
  )
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
      subtitle = "Dashed line marks 2012 DMSP-simulated to VIIRS transition",
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
