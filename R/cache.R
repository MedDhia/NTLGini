#' Manage the local cache of subunit Parquet tables
#'
#' `ntl_cache_dir()` returns the directory where [ntl_gini_units()] stores
#' downloaded files. It is versioned by data release, so files from an older
#' release are never reused by a newer package version.
#' `ntl_cache_list()` lists cached files and their sizes.
#' `ntl_cache_clear()` deletes cached files.
#'
#' @param level Optional character vector of levels (`"adm1"` to `"adm4"`) to
#'   delete. `NULL` (the default) deletes every cached file.
#' @param data_dir Cache directory. Defaults to `ntl_cache_dir()`.
#' @return `ntl_cache_dir()` returns a path. `ntl_cache_list()` returns a
#'   data frame with columns `file`, `level`, `size_mb` and `modified`.
#'   `ntl_cache_clear()` invisibly returns the paths it deleted.
#' @examples
#' ntl_cache_dir()
#' ntl_cache_list()
#' @export
ntl_cache_dir <- function() {
  file.path(tools::R_user_dir("ntlgini", which = "cache"), ntl_data_version)
}

#' @rdname ntl_cache_dir
#' @export
ntl_cache_list <- function(data_dir = ntl_cache_dir()) {
  files <- list.files(data_dir, pattern = "^gini_by_unit_adm[1-4]\\.parquet$",
                      full.names = TRUE)
  info <- file.info(files)
  data.frame(
    file = basename(files),
    level = sub("^gini_by_unit_(adm[1-4])\\.parquet$", "\\1", basename(files)),
    size_mb = round(info$size / 1024^2, 2),
    modified = info$mtime,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' @rdname ntl_cache_dir
#' @export
ntl_cache_clear <- function(level = NULL, data_dir = ntl_cache_dir()) {
  level <- check_level(level, several_ok = TRUE)
  if (is.null(level)) level <- ntl_levels
  paths <- file.path(data_dir, unit_file_name(level))
  paths <- paths[file.exists(paths)]
  unlink(paths)
  if (length(paths) == 0L) {
    message("No cached files to delete.")
  } else {
    message("Deleted ", length(paths), " cached file(s).")
  }
  invisible(paths)
}
