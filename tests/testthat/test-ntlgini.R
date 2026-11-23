test_that("bundled ntl_gini_global dataset schema and completeness", {
  data("ntl_gini_global", package = "ntlgini")
  expect_s3_class(ntl_gini_global, "tbl_df")
  expect_equal(nrow(ntl_gini_global), 16203)
  expect_equal(ncol(ntl_gini_global), 13)
  expect_equal(length(unique(ntl_gini_global$iso3)), 228)
  expect_true(all(c("TUN", "MAR", "USA", "RUS") %in% ntl_gini_global$iso3))
  expect_true(all(c("adm1", "adm2", "adm3", "adm4") %in% ntl_gini_global$level))
  expect_false(anyNA(ntl_gini_global$iso3))
  expect_false(anyNA(ntl_gini_global$year))
  expect_false(anyNA(ntl_gini_global$level))
  expect_true(all(ntl_gini_global$year %in% 1992:2024))
})

test_that("Pyatt/Dagum identities hold across all 16,203 rows", {
  data("ntl_gini_global", package = "ntlgini")
  sub <- ntl_gini_global[!is.na(ntl_gini_global$total_gini) & ntl_gini_global$total_gini > 0, ]
  diff_g <- abs(sub$total_gini - (sub$within_gini + sub$between_gini + sub$overlap_gini))
  expect_lt(max(diff_g), 1e-6)

  diff_s <- abs(1.0 - (sub$within_share + sub$between_share + sub$overlap_share))
  expect_lt(max(diff_s), 1e-6)

  expect_true(all(sub$total_gini >= 0 & sub$total_gini <= 1))
  expect_true(all(sub$between_gini >= 0 & sub$between_gini <= 1))
  expect_true(all(sub$within_gini >= 0 & sub$within_gini <= 1))
})

test_that("between_gini increases monotonically with finer admin levels", {
  tun24 <- ntl_gini_decomp(iso3 = "TUN", years = 2024)
  tun24 <- tun24[order(tun24$level), ]
  expect_equal(tun24$level, c("adm1", "adm2", "adm3", "adm4"))
  expect_true(all(diff(tun24$between_gini) > 0))
})

test_that("ntl_gini_decomp handles full, filtered, and empty queries", {
  all_rows <- ntl_gini_decomp()
  expect_equal(nrow(all_rows), 16203)

  res <- ntl_gini_decomp(iso3 = c("TUN", "MAR"), level = "adm1", years = 2020:2024)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 10)
  expect_setequal(unique(res$iso3), c("TUN", "MAR"))
  expect_equal(unique(res$level), "adm1")
  expect_setequal(unique(res$year), 2020:2024)

  expect_warning(empty <- ntl_gini_decomp(iso3 = "ZZZ"), "Unknown ISO3")
  expect_s3_class(empty, "tbl_df")
  expect_equal(nrow(empty), 0)
  expect_equal(ncol(empty), 13)
})

test_that("ntl_gini_units validates arguments and fails gracefully offline", {
  expect_error(ntl_gini_units(level = "adm5"))
  skip_if_not_installed("arrow")
  expect_error(
    ntl_gini_units(level = "adm1", data_dir = tempdir(), download = FALSE),
    "not found"
  )
})

test_that("ntl_gini_decomp validates and normalises inputs", {
  lower <- ntl_gini_decomp(iso3 = "tun", level = "ADM1", years = 2024)
  expect_equal(nrow(lower), 1)
  expect_equal(lower$iso3, "TUN")

  expect_warning(
    res <- ntl_gini_decomp(iso3 = c("TUN", "XXX"), level = "adm1", years = 2024),
    "XXX"
  )
  expect_equal(nrow(res), 1)

  expect_error(ntl_gini_decomp(level = "adm5"), "Invalid `level`")
  expect_error(ntl_gini_decomp(iso3 = 1), "character")
  expect_error(ntl_gini_decomp(years = "2020"), "whole numbers")
  expect_error(ntl_gini_decomp(years = 2020.5), "whole numbers")
  expect_warning(ntl_gini_decomp(iso3 = "TUN", years = 1980:1992), "1980")
})

test_that("cache helpers list and clear files in a versioned directory", {
  expect_match(ntl_cache_dir(), "v0\\.1\\.0$")

  dir <- withr::local_tempdir()
  expect_equal(nrow(ntl_cache_list(dir)), 0)

  writeBin(as.raw(1:10), file.path(dir, "gini_by_unit_adm1.parquet"))
  writeBin(as.raw(1:10), file.path(dir, "gini_by_unit_adm3.parquet"))
  writeBin(as.raw(1:10), file.path(dir, "unrelated.txt"))
  lst <- ntl_cache_list(dir)
  expect_setequal(lst$level, c("adm1", "adm3"))

  expect_message(deleted <- ntl_cache_clear("adm1", data_dir = dir), "Deleted 1")
  expect_equal(basename(deleted), "gini_by_unit_adm1.parquet")
  expect_equal(ntl_cache_list(dir)$level, "adm3")

  expect_message(ntl_cache_clear(data_dir = dir), "Deleted 1")
  expect_message(ntl_cache_clear(data_dir = dir), "No cached files")
  expect_true(file.exists(file.path(dir, "unrelated.txt")))
  expect_error(ntl_cache_clear("adm9", data_dir = dir), "Invalid")
})

test_that("ntl_download writes the file on success (mocked network)", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "gini_by_unit_adm1.parquet")
  seen <- NULL
  local_mocked_bindings(download_file = function(url, destfile) {
    seen <<- url
    writeBin(as.raw(1:100), destfile)
    0L
  })
  expect_message(ntlgini:::ntl_download("gini_by_unit_adm1.parquet", dest), "Downloading")
  expect_true(file.exists(dest))
  expect_false(file.exists(paste0(dest, ".tmp")))
  expect_match(seen, "releases/download/v0\\.1\\.0/gini_by_unit_adm1\\.parquet$")
})

test_that("ntl_download fails cleanly and leaves no partial file", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "gini_by_unit_adm1.parquet")

  local_mocked_bindings(download_file = function(url, destfile) stop("offline"))
  expect_error(
    suppressMessages(ntlgini:::ntl_download("gini_by_unit_adm1.parquet", dest)),
    "Failed to download"
  )
  expect_false(file.exists(dest))
  expect_false(file.exists(paste0(dest, ".tmp")))
})

test_that("ntl_download rejects files with a wrong checksum", {
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "gini_by_unit_adm1.parquet")
  local_mocked_bindings(
    download_file = function(url, destfile) {
      writeBin(as.raw(1:100), destfile)
      0L
    },
    ntl_checksums = c(gini_by_unit_adm1.parquet = "00000000000000000000000000000000")
  )
  expect_error(
    suppressMessages(ntlgini:::ntl_download("gini_by_unit_adm1.parquet", dest)),
    "Checksum mismatch"
  )
  expect_false(file.exists(dest))
})

test_that("ntl_gini_units reads and filters a local Parquet file", {
  skip_if_not_installed("arrow")
  dir <- withr::local_tempdir()
  fake <- data.frame(
    iso3 = c("TUN", "TUN", "MAR"),
    year = c(2020L, 2021L, 2020L),
    gini = c(0.5, 0.6, 0.7)
  )
  arrow::write_parquet(fake, file.path(dir, "gini_by_unit_adm1.parquet"))
  res <- ntl_gini_units("adm1", iso3 = "tun", years = 2020, data_dir = dir, download = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_equal(res$gini, 0.5)
})

test_that("plot_ntl_gini returns a ggplot for components and shares", {
  skip_if_not_installed("ggplot2")
  p <- plot_ntl_gini(c("TUN", "MAR"), level = "adm1")
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))

  s <- plot_ntl_gini("TUN", level = "adm4", years = 2000:2024, type = "shares")
  expect_s3_class(s, "ggplot")
  expect_equal(s$labels$y, "Share of total Gini")

  expect_error(plot_ntl_gini("TUN", level = c("adm1", "adm2")), "single")
  expect_error(plot_ntl_gini(character()), "at least one")
  expect_error(suppressWarnings(plot_ntl_gini("ZZZ")), "No data")
})
