#' Build an analysis-ready country-year panel
#'
#' Combines the decomposition, light intensity, country metadata (names,
#' regions, income group, COW and V-Dem codes) and quality flags into one
#' table, ready for `fixest`, `plm`, or export to Stata with
#' `haven::write_dta()`.
#'
#' Derived columns:
#' \describe{
#'   \item{`mean_light`}{`sum_of_lights / n_pixels`, average radiance per land
#'     pixel. Since pixels have a fixed nominal size (500 m), this is
#'     proportional to light per unit area.}
#'   \item{`log_mean_light`}{`log(mean_light)` (`NA` when `mean_light` is 0).}
#' }
#'
#' @param level A single administrative level. The total Gini does not depend
#'   on level, but the components do, so a panel uses one level at a time
#'   (unless `format = "wide"`).
#' @param iso3,years,region,continent Optional filters, as in
#'   [ntl_gini_decomp()].
#' @param measures Columns of [ntl_gini_global] to keep. Defaults to all
#'   Gini, share, count and light columns.
#' @param flags If `TRUE`, add the columns from [ntl_flag_quality()].
#' @param format `"long"` (one row per country-year) or `"wide"` (one row per
#'   country-year with one set of decomposition columns per level, suffixed
#'   `_adm1` ... `_adm4`). In wide format `level` may name several levels and
#'   `flags` applies to the first one.
#' @return A tibble sorted by `iso3` and `year`.
#' @examples
#' p <- ntl_panel("adm1", continent = "Africa")
#' head(p)
#'
#' w <- ntl_panel(c("adm1", "adm2"), iso3 = "TUN", format = "wide", flags = FALSE)
#' names(w)
#' @export
ntl_panel <- function(level = "adm1", iso3 = NULL, years = NULL, region = NULL,
                      continent = NULL, measures = NULL, flags = TRUE,
                      format = c("long", "wide")) {
  format <- match.arg(format)
  level <- check_level(level, several_ok = format == "wide")
  if (is.null(level)) stop("`level` must be supplied.", call. = FALSE)
  all_measures <- setdiff(names(ntlgini::ntl_gini_global), c("iso3", "year", "level"))
  if (is.null(measures)) measures <- all_measures
  bad <- setdiff(measures, all_measures)
  if (length(bad) > 0L) {
    stop("Unknown `measures`: ", paste(bad, collapse = ", "), ".", call. = FALSE)
  }

  df <- as.data.frame(ntl_gini_decomp(
    iso3 = iso3, level = level, years = years, region = region, continent = continent
  ))
  df$mean_light <- ifelse(df$n_pixels > 0, df$sum_of_lights / df$n_pixels, NA_real_)
  df$log_mean_light <- ifelse(df$mean_light > 0, log(df$mean_light), NA_real_)
  value_cols <- c(measures, "mean_light", "log_mean_light")

  if (format == "long") {
    out <- df[, c("iso3", "year", "level", value_cols), drop = FALSE]
    if (flags) {
      fl <- as.data.frame(ntl_flag_quality(df))
      flag_cols <- c("sensor", grep("^flag_|^any_flag$", names(fl), value = TRUE))
      out <- cbind(out, fl[, flag_cols, drop = FALSE])
    }
  } else {
    pieces <- lapply(level, function(lv) {
      x <- df[df$level == lv, c("iso3", "year", value_cols), drop = FALSE]
      names(x)[-(1:2)] <- paste0(names(x)[-(1:2)], "_", lv)
      x
    })
    out <- Reduce(function(a, b) merge(a, b, by = c("iso3", "year"), all = TRUE), pieces)
    if (flags) {
      fl <- as.data.frame(ntl_flag_quality(df[df$level == level[1L], , drop = FALSE]))
      flag_cols <- c("sensor", grep("^flag_|^any_flag$", names(fl), value = TRUE))
      fl <- fl[, c("iso3", "year", flag_cols), drop = FALSE]
      out <- merge(out, fl, by = c("iso3", "year"), all.x = TRUE)
    }
  }

  out <- out[order(out$iso3, out$year), , drop = FALSE]
  rownames(out) <- NULL
  ntl_add_country_info(out)
}

#' Centre country series on events for event-study designs
#'
#' Selects the years around each event (an election, coup, reform, sanctions,
#' a leader taking office, ...) and adds event time, so series can be
#' averaged or plotted relative to the event.
#'
#' @param df A country-year table with `iso3` and `year` columns, e.g. from
#'   [ntl_panel()] or [ntl_gini_decomp()] (filter to one level first).
#' @param events A data frame with columns `iso3` and `event_year`, and
#'   optionally `event_id` (to distinguish several events in one country).
#'   Any other columns are carried into the result.
#' @param window Integer vector of event times to keep, e.g. `-5:5`.
#' @param measure Optional name of a numeric column. If given, adds
#'   `<measure>_rel`, the value minus its value at `ref_time` for the same
#'   event (`NA` if that year is missing).
#' @param ref_time Event time used as the reference for `measure` (default
#'   `-1`, the year before the event).
#' @return A tibble with the rows of `df` inside each event window, plus
#'   `event_id`, `event_year`, `event_time` and `crosses_sensor_break`
#'   (`TRUE` when the window spans both 2011 and 2012).
#' @examples
#' events <- data.frame(iso3 = c("TUN", "EGY"), event_year = 2011)
#' d <- ntl_panel("adm1", iso3 = c("TUN", "EGY"), flags = FALSE)
#' ev <- ntl_event_window(d, events, window = -3:3, measure = "between_share")
#' ev[, c("iso3", "year", "event_time", "between_share", "between_share_rel")]
#' @export
ntl_event_window <- function(df, events, window = -5:5, measure = NULL,
                             ref_time = -1L) {
  if (!is.data.frame(df) || !all(c("iso3", "year") %in% names(df))) {
    stop("`df` must be a data frame with `iso3` and `year` columns.", call. = FALSE)
  }
  if (!is.data.frame(events) || !all(c("iso3", "event_year") %in% names(events))) {
    stop("`events` must be a data frame with `iso3` and `event_year` columns.", call. = FALSE)
  }
  if (!is.numeric(window) || anyNA(window) || any(window != round(window))) {
    stop("`window` must be a vector of whole numbers.", call. = FALSE)
  }
  if (!is.null(measure) && (!measure %in% names(df) || !is.numeric(df[[measure]]))) {
    stop("`measure` must name a numeric column of `df`.", call. = FALSE)
  }
  if ("level" %in% names(df) && length(unique(df$level)) > 1L) {
    stop("`df` contains several levels; filter to one level first.", call. = FALSE)
  }

  events <- as.data.frame(events)
  events$iso3 <- toupper(events$iso3)
  if (!"event_id" %in% names(events)) {
    events$event_id <- paste(events$iso3, events$event_year, sep = "_")
  }
  if (anyDuplicated(events$event_id)) {
    stop("`event_id` values must be unique.", call. = FALSE)
  }
  df <- as.data.frame(df)
  clash <- setdiff(intersect(names(events), names(df)), "iso3")
  if (length(clash) > 0L) {
    stop("`events` columns clash with `df`: ", paste(clash, collapse = ", "), ".",
         call. = FALSE)
  }

  out <- merge(df, events, by = "iso3")
  out$event_time <- as.integer(out$year - out$event_year)
  out <- out[out$event_time %in% window, , drop = FALSE]
  span <- range(window)
  out$crosses_sensor_break <- (out$event_year + span[1]) <= 2011 &
    (out$event_year + span[2]) >= 2012

  if (!is.null(measure)) {
    ref <- df
    ref <- merge(ref[, c("iso3", "year", measure)], events[, c("iso3", "event_id", "event_year")],
                 by = "iso3")
    ref <- ref[ref$year - ref$event_year == ref_time, c("event_id", measure)]
    out[[paste0(measure, "_rel")]] <- out[[measure]] - ref[[measure]][match(out$event_id, ref$event_id)]
  }

  out <- out[order(out$event_id, out$event_time), , drop = FALSE]
  rownames(out) <- NULL
  tibble::as_tibble(out)
}
