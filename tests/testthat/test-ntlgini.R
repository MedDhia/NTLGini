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

test_that("bundled ntl_gini_countries metadata matches ntl_gini_global", {
  data("ntl_gini_countries", package = "ntlgini")
  expect_s3_class(ntl_gini_countries, "tbl_df")
  expect_equal(nrow(ntl_gini_countries), 228)
  expect_equal(ncol(ntl_gini_countries), 9)
  tun <- ntl_gini_countries[ntl_gini_countries$iso3 == "TUN", ]
  expect_equal(tun$max_level, 4L)
  expect_equal(tun$boundary_source, "MapsTN")
  expect_equal(tun$n_adm4, 2075L)
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

  empty <- ntl_gini_decomp(iso3 = "ZZZ")
  expect_s3_class(empty, "tbl_df")
  expect_equal(nrow(empty), 0)
  expect_equal(ncol(empty), 13)
})

test_that("ntl_gini_trends computes log-linear trends cleanly", {
  tr <- ntl_gini_trends(iso3 = c("TUN", "MAR"), level = "adm1", window = "viirs")
  expect_s3_class(tr, "tbl_df")
  expect_equal(nrow(tr), 2)
  expect_equal(unique(tr$window), "viirs")
  expect_true(all(tr$n_years == 13L))
  expect_true(all(!is.na(tr$percent_per_year)))
  expect_true(all(tr$r_squared >= 0 & tr$r_squared <= 1))

  empty_tr <- ntl_gini_trends(iso3 = "ZZZ")
  expect_equal(nrow(empty_tr), 0)
})

test_that("cache helpers work in an isolated tempdir", {
  tmp_dir <- file.path(tempdir(), "ntlgini_cache_test")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines("test", file.path(tmp_dir, "gini_by_unit_adm1.parquet"))

  info <- ntl_gini_cache_info(data_dir = tmp_dir)
  expect_equal(nrow(info), 1)
  expect_equal(info$file, "gini_by_unit_adm1.parquet")

  n_del <- ntl_gini_clear_cache(data_dir = tmp_dir)
  expect_equal(n_del, 1L)
  expect_equal(nrow(ntl_gini_cache_info(data_dir = tmp_dir)), 0)
  unlink(tmp_dir, recursive = TRUE)
})

test_that("ntl_gini_units validates arguments and fails gracefully offline", {
  expect_error(ntl_gini_units(level = "adm5"))
  skip_if_not_installed("arrow")
  expect_error(
    ntl_gini_units(level = "adm1", data_dir = tempdir(), download = FALSE),
    "not found"
  )
})
