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

  empty <- ntl_gini_decomp(iso3 = "ZZZ")
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
