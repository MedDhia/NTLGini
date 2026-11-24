.require_arrow <- function() {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop(
      "Package 'arrow' is required to read Parquet tables. ",
      "Install with install.packages('arrow').",
      call. = FALSE
    )
  }
}

.require_terra <- function() {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop(
      "Package 'terra' is required for raster operations. ",
      "Install with install.packages('terra').",
      call. = FALSE
    )
  }
}

.ensure_parquet_cached <- function(fname, data_dir, download) {
  path <- file.path(data_dir, fname)
  if (!file.exists(path)) {
    if (!download) {
      stop(fname, " not found in ", data_dir, " and download = FALSE.", call. = FALSE)
    }
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
    url <- paste0("https://github.com/MedDhia/NTLGini/releases/download/v0.1.0/", fname)
    tmp <- paste0(path, ".tmp")
    status <- tryCatch(
      suppressWarnings(
        utils::download.file(url, destfile = tmp, mode = "wb", quiet = TRUE)
      ),
      error = function(e) 1L
    )
    if (!identical(status, 0L) || !file.exists(tmp) || isTRUE(file.info(tmp)$size <= 0)) {
      unlink(tmp)
      stop("Failed to download ", fname, " from ", url, call. = FALSE)
    }
    file.rename(tmp, path)
  }
  path
}

#' Load 0.125-degree (30 x 30 pixel) grid-cell Gini and radiance series
#'
#' Reads `gini_by_pixel.parquet` (0.125 x 0.125 degree cells, each aggregating
#' up to 30 x 30 = 900 `500 m` pixels) using `arrow` predicate pushdown when
#' filtering by `iso3`, `years`, or `bbox`. If the file is not yet present in
#' `data_dir`, it is downloaded from the `ntlgini` GitHub release (`v0.1.0`)
#' and cached in `tools::R_user_dir("ntlgini", "cache")`.
#'
#' @param iso3 Optional character vector of ISO3 country codes to filter.
#' @param years Optional integer vector of calendar years (`1992:2024`).
#' @param bbox Optional numeric bounding box `c(xmin, ymin, xmax, ymax)` in
#'   WGS84 (`EPSG:4326`) to filter cell centers (`lon`, `lat`).
#' @param data_dir Directory where Parquet files are cached.
#' @param download Logical; whether to download `gini_by_pixel.parquet` if missing.
#' @return A tibble with `iso3`, `year`, `cell_id`, `lon`, `lat`, `n_pixels`,
#'   `mean_radiance`, and `gini`.
#' @seealso [ntl_gini_raster()], [ntl_gini_extract()], [ntl_gini_map_pixels()],
#'   [ntl_gini_units()]
#' @examples
#' if (interactive() && requireNamespace("arrow", quietly = TRUE)) {
#'   tun_cells <- ntl_gini_pixels(iso3 = "TUN", years = 2024)
#' }
#' @export
ntl_gini_pixels <- function(iso3 = NULL,
                            years = NULL,
                            bbox = NULL,
                            data_dir = tools::R_user_dir("ntlgini", which = "cache"),
                            download = TRUE) {
  .require_arrow()
  path <- .ensure_parquet_cached("gini_by_pixel.parquet", data_dir = data_dir, download = download)

  ds <- arrow::open_dataset(path)
  or_eq <- function(col, vals) {
    Reduce("|", lapply(vals, function(v) arrow::Expression$field_ref(col) == v))
  }
  expr <- NULL
  if (!is.null(iso3)) {
    expr <- or_eq("iso3", as.character(iso3))
  }
  if (!is.null(years)) {
    e_yr <- or_eq("year", as.integer(years))
    expr <- if (is.null(expr)) e_yr else (expr & e_yr)
  }
  if (!is.null(bbox)) {
    if (!is.numeric(bbox) || length(bbox) != 4L) {
      stop("bbox must be a numeric vector of length 4: c(xmin, ymin, xmax, ymax).", call. = FALSE)
    }
    e_bb <- (arrow::Expression$field_ref("lon") >= bbox[1L]) &
      (arrow::Expression$field_ref("lat") >= bbox[2L]) &
      (arrow::Expression$field_ref("lon") <= bbox[3L]) &
      (arrow::Expression$field_ref("lat") <= bbox[4L])
    expr <- if (is.null(expr)) e_bb else (expr & e_bb)
  }

  scan <- ds$NewScan()
  if (!is.null(expr)) {
    scan$Filter(expr)
  }
  tibble::as_tibble(as.data.frame(scan$Finish()$ToTable()))
}

#' Build a multi-layer SpatRaster over a region of interest (blackmarblepy-style)
#'
#' Mimics `blackmarblepy::bm_raster()` using `ntlgini`'s 0.125-degree
#' (`30 x 30` 500 m pixel) grid cells in `gini_by_pixel.parquet`. Queries cells
#' overlapping `roi_sf`, rasterizes each requested year onto a `terra::SpatRaster`
#' (0.125 deg resolution, `EPSG:4326`), and masks to `roi_sf`.
#'
#' @param roi_sf An `sf` polygon object or path to a shapefile/GeoPackage/GeoJSON.
#' @param years Integer vector of years to include as raster layers (default `2024L`).
#' @param metric Grid-cell metric to rasterize: `"mean_radiance"`, `"gini"`, or
#'   `"n_pixels"`.
#' @param iso3 Optional character vector of ISO3 codes to pre-filter cells.
#' @param layer Optional layer name when reading a multi-layer GeoPackage.
#' @param data_dir Directory where Parquet files are cached.
#' @param download Logical; whether to download missing Parquet files.
#' @return A `terra::SpatRaster` with one layer per year (named `y<year>`).
#' @seealso [ntl_gini_extract()], [ntl_gini_pixels()], [ntl_gini_map_pixels()]
#' @examples
#' if (interactive() && requireNamespace("arrow", quietly = TRUE) &&
#'     requireNamespace("sf", quietly = TRUE) &&
#'     requireNamespace("terra", quietly = TRUE)) {
#'   # r <- ntl_gini_raster("path/to/tunisia.shp", years = 2020:2024, metric = "gini")
#' }
#' @export
ntl_gini_raster <- function(roi_sf,
                            years = 2024L,
                            metric = c("mean_radiance", "gini", "n_pixels"),
                            iso3 = NULL,
                            layer = NULL,
                            data_dir = tools::R_user_dir("ntlgini", which = "cache"),
                            download = TRUE) {
  .require_sf()
  .require_terra()
  metric <- match.arg(metric)

  roi <- .read_spatial(roi_sf, layer = layer)
  if (!is.na(sf::st_crs(roi)) && sf::st_crs(roi)$epsg != 4326L) {
    roi <- sf::st_transform(roi, 4326L)
  }
  bb <- as.numeric(sf::st_bbox(roi))
  pad <- 0.125
  df <- ntl_gini_pixels(
    iso3 = iso3,
    years = as.integer(years),
    bbox = c(bb[1L] - pad, bb[2L] - pad, bb[3L] + pad, bb[4L] + pad),
    data_dir = data_dir,
    download = download
  )
  if (nrow(df) == 0L) stop("No grid cells found within the ROI bounding box.", call. = FALSE)

  yrs <- sort(unique(as.integer(df$year)))
  res_deg <- 0.125
  xmin <- min(df$lon) - res_deg / 2
  xmax <- max(df$lon) + res_deg / 2
  ymin <- min(df$lat) - res_deg / 2
  ymax <- max(df$lat) + res_deg / 2

  tmpl <- terra::rast(
    xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
    resolution = res_deg, crs = "EPSG:4326"
  )
  roi_v <- terra::vect(roi)

  layers <- lapply(yrs, function(yr) {
    sub <- df[df$year == yr, , drop = FALSE]
    pts <- terra::vect(
      sub[, c("lon", "lat", metric), drop = FALSE],
      geom = c("lon", "lat"),
      crs = "EPSG:4326"
    )
    r <- terra::rasterize(pts, tmpl, field = metric, fun = mean, na.rm = TRUE)
    terra::mask(r, roi_v)
  })
  out <- terra::rast(layers)
  names(out) <- paste0("y", yrs)
  out
}

#' Extract zonal statistics over ROI polygons (blackmarblepy-style)
#'
#' Mimics `blackmarblepy::bm_extract()` by spatially intersecting `roi_sf`
#' polygons with `0.125`-degree (`30 x 30` 500 m pixel) cells from
#' `gini_by_pixel.parquet` and computing one or more summary functions
#' (`"mean"`, `"sum"`, `"median"`, `"min"`, `"max"`, `"sd"`) per polygon and year.
#'
#' @param roi_sf An `sf` polygon object or path to a shapefile/GeoPackage/GeoJSON.
#' @param years Integer vector of calendar years (default `2024L`).
#' @param metric Grid-cell metric to aggregate: `"mean_radiance"`, `"gini"`, or
#'   `"n_pixels"`.
#' @param aggfunc Character vector of summary functions: any subset of
#'   `"mean"`, `"sum"`, `"median"`, `"min"`, `"max"`, `"sd"`.
#' @param iso3 Optional character vector of ISO3 codes to pre-filter cells.
#' @param layer Optional layer name when reading a multi-layer GeoPackage.
#' @param data_dir Directory where Parquet files are cached.
#' @param download Logical; whether to download missing Parquet files.
#' @return A tibble with `roi_sf` attributes, `year`, `n_cells`, and one column
#'   per requested `aggfunc` (named `<metric>_<aggfunc>`).
#' @seealso [ntl_gini_raster()], [ntl_gini_pixels()], [ntl_gini_units()]
#' @examples
#' if (interactive() && requireNamespace("arrow", quietly = TRUE) &&
#'     requireNamespace("sf", quietly = TRUE)) {
#'   # ntl_gini_extract("path/to/regions.shp", years = 2020:2024, aggfunc = c("mean", "sum"))
#' }
#' @export
ntl_gini_extract <- function(roi_sf,
                             years = 2024L,
                             metric = c("mean_radiance", "gini", "n_pixels"),
                             aggfunc = c("mean", "sum"),
                             iso3 = NULL,
                             layer = NULL,
                             data_dir = tools::R_user_dir("ntlgini", which = "cache"),
                             download = TRUE) {
  .require_sf()
  metric <- match.arg(metric)
  valid_aggs <- c("mean", "sum", "median", "min", "max", "sd")
  aggfunc <- match.arg(aggfunc, choices = valid_aggs, several.ok = TRUE)

  roi <- .read_spatial(roi_sf, layer = layer)
  if (!is.na(sf::st_crs(roi)) && sf::st_crs(roi)$epsg != 4326L) {
    roi <- sf::st_transform(roi, 4326L)
  }
  bb <- as.numeric(sf::st_bbox(roi))
  pad <- 0.125
  df <- ntl_gini_pixels(
    iso3 = iso3,
    years = as.integer(years),
    bbox = c(bb[1L] - pad, bb[2L] - pad, bb[3L] + pad, bb[4L] + pad),
    data_dir = data_dir,
    download = download
  )
  if (nrow(df) == 0L) stop("No grid cells found within the ROI bounding box.", call. = FALSE)

  uniq_cells <- unique(df[, c("cell_id", "lon", "lat")])
  pts_sf <- sf::st_as_sf(uniq_cells, coords = c("lon", "lat"), crs = 4326L)
  hits <- sf::st_intersects(roi, pts_sf)

  roi_attrs <- sf::st_drop_geometry(roi)
  yrs <- sort(unique(as.integer(df$year)))

  agg_fns <- list(
    mean = function(v) if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE),
    sum = function(v) if (all(is.na(v))) NA_real_ else sum(v, na.rm = TRUE),
    median = function(v) if (all(is.na(v))) NA_real_ else stats::median(v, na.rm = TRUE),
    min = function(v) if (all(is.na(v))) NA_real_ else min(v, na.rm = TRUE),
    max = function(v) if (all(is.na(v))) NA_real_ else max(v, na.rm = TRUE),
    sd = function(v) if (sum(!is.na(v)) < 2L) NA_real_ else stats::sd(v, na.rm = TRUE)
  )

  rows <- vector("list", nrow(roi) * length(yrs))
  k <- 0L
  for (i in seq_len(nrow(roi))) {
    cids <- uniq_cells$cell_id[hits[[i]]]
    attr_row <- roi_attrs[i, , drop = FALSE]
    for (yr in yrs) {
      k <- k + 1L
      sub <- df[df$year == yr & df$cell_id %in% cids, , drop = FALSE]
      vals <- sub[[metric]]
      rec <- as.list(attr_row)
      rec$year <- yr
      rec$n_cells <- nrow(sub)
      for (fn_name in aggfunc) {
        rec[[paste0(metric, "_", fn_name)]] <- agg_fns[[fn_name]](vals)
      }
      rows[[k]] <- as.data.frame(rec, stringsAsFactors = FALSE)
    }
  }
  tibble::as_tibble(do.call(rbind, rows))
}

#' Map 0.125-degree grid-cell Gini or radiance over a region of interest
#'
#' Plots `gini_by_pixel.parquet` (`30 x 30` 500 m pixel cells) as a `ggplot2`
#' tile map overlaid with the `roi_sf` boundary.
#'
#' @param roi_sf An `sf` polygon object or path to a shapefile/GeoPackage/GeoJSON.
#' @param year Single calendar year (default `2024L`).
#' @param metric Grid-cell metric to plot: `"gini"`, `"mean_radiance"`, or
#'   `"n_pixels"`.
#' @param iso3 Optional character vector of ISO3 codes to pre-filter cells.
#' @param layer Optional layer name when reading a multi-layer GeoPackage.
#' @param data_dir Directory where Parquet files are cached.
#' @param download Logical; whether to download missing Parquet files.
#' @return A `ggplot` object.
#' @seealso [ntl_gini_pixels()], [ntl_gini_raster()], [ntl_gini_extract()]
#' @examples
#' if (interactive() && requireNamespace("arrow", quietly = TRUE) &&
#'     requireNamespace("sf", quietly = TRUE) &&
#'     requireNamespace("ggplot2", quietly = TRUE)) {
#'   # ntl_gini_map_pixels("path/to/tunisia.shp", year = 2024, metric = "gini")
#' }
#' @export
ntl_gini_map_pixels <- function(roi_sf,
                                year = 2024L,
                                metric = c("gini", "mean_radiance", "n_pixels"),
                                iso3 = NULL,
                                layer = NULL,
                                data_dir = tools::R_user_dir("ntlgini", which = "cache"),
                                download = TRUE) {
  .require_ggplot2()
  .require_sf()
  metric <- match.arg(metric)
  if (length(year) != 1L) stop("year must be a single calendar year.", call. = FALSE)

  roi <- .read_spatial(roi_sf, layer = layer)
  if (!is.na(sf::st_crs(roi)) && sf::st_crs(roi)$epsg != 4326L) {
    roi <- sf::st_transform(roi, 4326L)
  }
  bb <- as.numeric(sf::st_bbox(roi))
  pad <- 0.125
  df <- ntl_gini_pixels(
    iso3 = iso3,
    years = as.integer(year),
    bbox = c(bb[1L] - pad, bb[2L] - pad, bb[3L] + pad, bb[4L] + pad),
    data_dir = data_dir,
    download = download
  )
  if (nrow(df) == 0L) stop("No grid cells found within the ROI bounding box.", call. = FALSE)

  uniq_cells <- unique(df[, c("cell_id", "lon", "lat")])
  pts_sf <- sf::st_as_sf(uniq_cells, coords = c("lon", "lat"), crs = 4326L)
  inside <- lengths(sf::st_intersects(pts_sf, roi)) > 0L
  keep_ids <- uniq_cells$cell_id[inside]
  df <- df[df$cell_id %in% keep_ids, , drop = FALSE]

  metric_labels <- c(
    gini = "Grid-cell spatial Gini",
    mean_radiance = "Mean nighttime radiance",
    n_pixels = "Land grid cells (500 m)"
  )
  metric_subtitles <- c(
    gini = "Nighttime-light inequality across 250,000 m^2 (500 m) pixels inside each 0.125 deg (30 x 30) cell",
    mean_radiance = "Average nighttime-light radiance across 250,000 m^2 (500 m) pixels inside each 0.125 deg cell",
    n_pixels = "Count of 250,000 m^2 (500 m) land pixels inside each 0.125 deg (30 x 30) cell"
  )

  ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = df,
      ggplot2::aes(x = .data[["lon"]], y = .data[["lat"]], fill = .data[[metric]]),
      width = 0.125,
      height = 0.125
    ) +
    ggplot2::geom_sf(data = roi, fill = NA, color = "#0f172a", linewidth = 0.25) +
    ggplot2::scale_fill_viridis_c(
      option = "inferno",
      direction = -1,
      na.value = "#cbd5e1",
      name = metric_labels[[metric]]
    ) +
    ggplot2::labs(
      title = sprintf("%s (0.125 deg grid, %d)", metric_labels[[metric]], year),
      subtitle = metric_subtitles[[metric]]
    ) +
    ggplot2::theme_void(base_size = 11)
}
