#' Plot the Gini decomposition over time
#'
#' Draws a stacked-area chart of the within, between and overlap components
#' for one or more countries, with the total Gini as a line, faceted by
#' country. Requires 'ggplot2'.
#'
#' @param iso3 Character vector of ISO3 country codes (case-insensitive).
#' @param level A single administrative level (`"adm1"` to `"adm4"`).
#' @param years Optional integer vector of years.
#' @param type `"components"` plots Gini components that sum to the total
#'   Gini; `"shares"` plots each component's share of the total (sums to 1).
#' @return A `ggplot` object.
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_ntl_gini(c("TUN", "MAR"), level = "adm1")
#'   plot_ntl_gini("TUN", level = "adm4", type = "shares")
#' }
#' @export
plot_ntl_gini <- function(iso3, level = "adm1", years = NULL,
                          type = c("components", "shares")) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "Package 'ggplot2' is required for plotting. ",
      "Install with install.packages('ggplot2').",
      call. = FALSE
    )
  }
  if (missing(iso3) || is.null(iso3) || length(iso3) == 0L) {
    stop("`iso3` must contain at least one country code.", call. = FALSE)
  }
  type <- match.arg(type)
  level <- check_level(level, several_ok = FALSE)
  df <- ntl_gini_decomp(iso3 = iso3, level = level, years = years)
  if (nrow(df) == 0L) {
    stop("No data for the requested countries, level and years.", call. = FALSE)
  }

  suffix <- if (type == "components") "_gini" else "_share"
  parts <- c(Within = "within", Between = "between", Overlap = "overlap")
  long <- do.call(rbind, lapply(names(parts), function(lab) {
    data.frame(
      iso3 = df$iso3,
      year = df$year,
      component = lab,
      value = df[[paste0(parts[[lab]], suffix)]],
      stringsAsFactors = FALSE
    )
  }))
  long$component <- factor(long$component, levels = c("Overlap", "Between", "Within"))

  p <- ggplot2::ggplot(long, ggplot2::aes(x = .data$year, y = .data$value)) +
    ggplot2::geom_area(ggplot2::aes(fill = .data$component), alpha = 0.9) +
    ggplot2::scale_fill_manual(
      values = c(Within = "#4C78A8", Between = "#F58518", Overlap = "#B0B0B0"),
      breaks = c("Within", "Between", "Overlap"),
      name = NULL
    ) +
    ggplot2::facet_wrap(~iso3) +
    ggplot2::labs(
      x = "Year",
      y = if (type == "components") "Gini" else "Share of total Gini",
      title = sprintf("Nighttime-light Gini decomposition (%s)", toupper(level))
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  if (type == "components") {
    p <- p + ggplot2::geom_line(
      data = as.data.frame(df),
      ggplot2::aes(x = .data$year, y = .data$total_gini),
      colour = "black", linewidth = 0.6, inherit.aes = FALSE
    )
  }
  p
}
