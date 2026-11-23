# Internal helpers: input validation, data version, and download logic.

# GitHub release tag that hosts the subunit Parquet tables for this version.
ntl_data_version <- "v0.1.0"

ntl_release_url <- "https://github.com/MedDhia/NTLGini/releases/download"

ntl_levels <- c("adm1", "adm2", "adm3", "adm4")

# Known MD5 checksums of release assets. NA means "not yet recorded": the file
# is accepted without verification. Fill these in when assets are published:
#   tools::md5sum("gini_by_unit_adm1.parquet")
ntl_checksums <- c(
  gini_by_unit_adm1.parquet = NA_character_,
  gini_by_unit_adm2.parquet = NA_character_,
  gini_by_unit_adm3.parquet = NA_character_,
  gini_by_unit_adm4.parquet = NA_character_
)

unit_file_name <- function(level) sprintf("gini_by_unit_%s.parquet", level)

check_iso3 <- function(iso3) {
  if (is.null(iso3)) return(NULL)
  if (!is.character(iso3) || anyNA(iso3)) {
    stop("`iso3` must be a character vector of ISO3 codes without NA.", call. = FALSE)
  }
  iso3 <- unique(toupper(trimws(iso3)))
  unknown <- setdiff(iso3, unique(ntlgini::ntl_gini_global$iso3))
  if (length(unknown) > 0L) {
    warning(
      "Unknown ISO3 code(s) ignored: ", paste(unknown, collapse = ", "), ".",
      call. = FALSE
    )
  }
  iso3
}

check_level <- function(level, several_ok = FALSE) {
  if (is.null(level)) return(NULL)
  if (!is.character(level) || anyNA(level)) {
    stop("`level` must be a character vector.", call. = FALSE)
  }
  level <- unique(tolower(level))
  if (!several_ok && length(level) != 1L) {
    stop("`level` must be a single administrative level.", call. = FALSE)
  }
  bad <- setdiff(level, ntl_levels)
  if (length(bad) > 0L) {
    stop(
      "Invalid `level`: ", paste(bad, collapse = ", "),
      ". Must be one of ", paste(ntl_levels, collapse = ", "), ".",
      call. = FALSE
    )
  }
  level
}

check_years <- function(years) {
  if (is.null(years)) return(NULL)
  if (!is.numeric(years) || anyNA(years) || any(years != round(years))) {
    stop("`years` must be a vector of whole numbers without NA.", call. = FALSE)
  }
  years <- as.integer(unique(years))
  outside <- setdiff(years, 1992:2024)
  if (length(outside) > 0L) {
    warning(
      "Year(s) outside 1992-2024 have no data: ",
      paste(sort(outside), collapse = ", "), ".",
      call. = FALSE
    )
  }
  years
}

# Thin wrapper so tests can mock the network call.
download_file <- function(url, destfile) {
  utils::download.file(url, destfile = destfile, mode = "wb", quiet = TRUE)
}

ntl_download <- function(fname, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  url <- paste(ntl_release_url, ntl_data_version, fname, sep = "/")
  tmp <- paste0(path, ".tmp")
  on.exit(unlink(tmp), add = TRUE)

  old <- options(timeout = max(600, getOption("timeout", 60)))
  on.exit(options(old), add = TRUE)

  message("Downloading ", fname, " (", ntl_data_version, ") to ", dirname(path), " ...")
  status <- tryCatch(
    suppressWarnings(download_file(url, tmp)),
    error = function(e) 1L
  )
  if (!identical(as.integer(status), 0L) || !file.exists(tmp) ||
      isTRUE(file.info(tmp)$size <= 0)) {
    stop("Failed to download ", fname, " from ", url, call. = FALSE)
  }

  expected <- unname(ntl_checksums[fname])
  if (length(expected) == 1L && !is.na(expected)) {
    actual <- unname(tools::md5sum(tmp))
    if (!identical(actual, expected)) {
      stop(
        "Checksum mismatch for ", fname, " (expected ", expected,
        ", got ", actual, "). The download may be corrupted; please retry.",
        call. = FALSE
      )
    }
  }

  if (!file.rename(tmp, path)) {
    stop("Could not move downloaded file into ", dirname(path), call. = FALSE)
  }
  invisible(path)
}

# Validate a region/continent filter against ntl_countries (case-insensitive).
check_meta <- function(x, column) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || anyNA(x)) {
    stop("`", column, "` must be a character vector.", call. = FALSE)
  }
  valid <- sort(unique(ntlgini::ntl_countries[[column]]))
  hit <- valid[match(tolower(x), tolower(valid))]
  if (anyNA(hit)) {
    stop(
      "Invalid `", column, "`: ", paste(x[is.na(hit)], collapse = ", "),
      ". Must be one of: ", paste(valid, collapse = "; "), ".",
      call. = FALSE
    )
  }
  unique(hit)
}
