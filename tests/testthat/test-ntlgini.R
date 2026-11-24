test_that("bundled ntl_gini_global dataset schema and completeness", {
  data("ntl_gini_global", package = "ntlgini")
  expect_s3_class(ntl_gini_global, "tbl_df")
  expect_equal(nrow(ntl_gini_global), 17553)
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
  expect_equal(tun$boundary_source, "HDX COD-AB")
  expect_equal(tun$n_adm4, 2075L)
  egy <- ntl_gini_countries[ntl_gini_countries$iso3 == "EGY", ]
  expect_equal(egy$max_level, 3L)
  expect_equal(egy$boundary_source, "HDX COD-AB")
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
  expect_equal(nrow(all_rows), 17553)

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

test_that("ntl_gini_units validates arguments and filters Parquet with arrow", {
  expect_error(ntl_gini_units(level = "adm5"))
  skip_if_not_installed("arrow")
  expect_error(
    ntl_gini_units(level = "adm1", data_dir = tempdir(), download = FALSE),
    "not found"
  )

  tmp_dir <- file.path(tempdir(), "ntlgini_arrow_test")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  sample_df <- data.frame(
    iso3 = c("TUN", "TUN", "MAR"),
    year = c(2023L, 2024L, 2024L),
    level = c("adm1", "adm1", "adm1"),
    gid = c("G1", "G1", "G2"),
    name = c("A", "A", "B"),
    n_pixels = c(10L, 10L, 20L),
    mean_radiance = c(1.0, 2.0, 3.0),
    pop_share = c(0.5, 0.5, 1.0),
    value_share = c(0.5, 0.5, 1.0),
    gini = c(0.4, 0.3, 0.5),
    within_contribution = c(0.1, 0.1, 0.2),
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(sample_df, file.path(tmp_dir, "gini_by_unit_adm1.parquet"))

  res <- ntl_gini_units(
    level = "adm1", iso3 = "TUN", years = 2024L,
    data_dir = tmp_dir, download = FALSE
  )
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 1)
  expect_equal(res$iso3, "TUN")
  expect_equal(res$year, 2024L)

  top <- ntl_gini_top_units(
    level = "adm1", iso3 = "TUN", year = 2024L, n = 1L,
    data_dir = tmp_dir, download = FALSE
  )
  expect_equal(nrow(top), 1)
  expect_equal(top$name, "A")

  if (requireNamespace("ggplot2", quietly = TRUE)) {
    p_top <- ntl_gini_plot_top_units(
      level = "adm1", iso3 = "TUN", year = 2024L, n = 1L,
      data_dir = tmp_dir, download = FALSE
    )
    expect_s3_class(p_top, "ggplot")

    p_uscat <- ntl_gini_plot_unit_scatter(
      level = "adm1", iso3 = "TUN", year = 2024L, label_top = 1L,
      data_dir = tmp_dir, download = FALSE
    )
    expect_s3_class(p_uscat, "ggplot")
  }
  unlink(tmp_dir, recursive = TRUE)
})

test_that("plotting functions return ggplot objects", {
  skip_if_not_installed("ggplot2")
  expect_s3_class(ntl_gini_plot_series(c("TUN", "MAR"), level = "adm1"), "ggplot")
  expect_s3_class(ntl_gini_plot_decomp("TUN", level = "adm2"), "ggplot")
  expect_s3_class(ntl_gini_plot_levels(c("TUN", "MAR")), "ggplot")
  expect_s3_class(ntl_gini_plot_scatter(level = "adm1", year = 2024L, label_iso3 = c("TUN", "MAR")), "ggplot")

  p_series <- ntl_gini_plot_series(c("MDG", "SVK", "BLR", "BGD", "AGO"), level = "adm1")
  expect_equal(levels(p_series$data$country), c("Madagascar", "Slovakia", "Belarus", "Bangladesh", "Angola"))
  expect_true(grepl("Divisions \\(Bangladesh\\)", p_series$labels$caption))
  expect_true(grepl("Oblasts \\(Belarus\\)", p_series$labels$caption))
  expect_true(grepl("Provinces \\(Angola\\)", p_series$labels$caption))
  expect_true(grepl("Regions \\(Madagascar, Slovakia\\)", p_series$labels$caption))
})

test_that("global mathematical consistency and Dagum decomposition invariants", {
  data("ntl_gini_global", package = "ntlgini")
  # Subsetting valid non-zero rows
  valid <- ntl_gini_global[!is.na(ntl_gini_global$total_gini) & ntl_gini_global$total_gini > 0, ]

  # 1. Dagum additive components
  dagum_diff <- abs(valid$total_gini - (valid$within_gini + valid$between_gini + valid$overlap_gini))
  expect_lt(max(dagum_diff), 1e-5)

  # 2. Shares sum to unity
  share_diff <- abs(1.0 - (valid$within_share + valid$between_share + valid$overlap_share))
  expect_lt(max(share_diff), 1e-5)

  # 3. Component bounds [0, 1] (with epsilon tolerance for floating-point precision)
  expect_true(all(valid$total_gini >= 0 & valid$total_gini <= 1))
  expect_true(all(valid$between_gini >= 0 & valid$between_gini <= 1))
  expect_true(all(valid$within_gini >= 0 & valid$within_gini <= 1))
  expect_true(all(valid$overlap_gini >= -1e-12 & valid$overlap_gini <= 1))
  expect_true(all(valid$between_share >= 0 & valid$between_share <= 1))
  expect_true(all(valid$within_share >= 0 & valid$within_share <= 1))
  expect_true(all(valid$overlap_share >= -1e-12 & valid$overlap_share <= 1))

  # 4. Within and between cannot exceed total
  expect_true(all(valid$between_gini <= valid$total_gini + 1e-6))
  expect_true(all(valid$within_gini <= valid$total_gini + 1e-6))

  # 5. Metadata integrity
  data("ntl_gini_countries", package = "ntlgini")
  expect_true(all(ntl_gini_countries$max_level %in% 1:4))
  expect_true(all(ntl_gini_countries$n_pixels > 0))
  expect_true(all(ntl_gini_countries$n_years >= 32L))
})

test_that("input validation, case insensitivity, and country name resolvers", {
  # Lowercase and mixed case
  res_lower <- ntl_gini_decomp(iso3 = c("tun", "Mar"), years = 2024L)
  expect_equal(sort(unique(res_lower$iso3)), c("MAR", "TUN"))

  # Resolving full country names
  res_name <- ntl_gini_decomp(iso3 = c("Tunisia", "Morocco"), years = 2024L)
  expect_equal(sort(unique(res_name$iso3)), c("MAR", "TUN"))

  # Redundant duplicates handled cleanly
  res_dups <- ntl_gini_decomp(iso3 = c("TUN", "tun", "Tunisia"), years = 2024L)
  expect_equal(unique(res_dups$iso3), "TUN")

  # Non-existent country returns empty tibble without error
  res_none <- ntl_gini_decomp(iso3 = "Atlantis")
  expect_s3_class(res_none, "tbl_df")
  expect_equal(nrow(res_none), 0)

  # Out of range years return empty tibble
  res_year <- ntl_gini_decomp(iso3 = "TUN", years = c(1850L, 2100L))
  expect_equal(nrow(res_year), 0)
})

test_that("ntl_gini_trends resilience under edge cases and window selections", {
  # All three window modes
  tr_full <- ntl_gini_trends(iso3 = "TUN", level = "adm1", window = "full")
  expect_equal(tr_full$n_years, 33L)
  expect_equal(tr_full$first_year, 1992L)
  expect_equal(tr_full$last_year, 2024L)

  tr_dmsp <- ntl_gini_trends(iso3 = "TUN", level = "adm1", window = "dmsp")
  expect_equal(tr_dmsp$n_years, 20L)
  expect_equal(tr_dmsp$first_year, 1992L)
  expect_equal(tr_dmsp$last_year, 2011L)

  tr_viirs <- ntl_gini_trends(iso3 = "TUN", level = "adm1", window = "viirs")
  expect_equal(tr_viirs$n_years, 13L)
  expect_equal(tr_viirs$first_year, 2012L)
  expect_equal(tr_viirs$last_year, 2024L)

  # Custom metric trends
  tr_within <- ntl_gini_trends(iso3 = "TUN", level = "adm1", metric = "within_gini")
  expect_equal(unique(tr_within$metric), "within_gini")

  tr_total <- ntl_gini_trends(iso3 = "TUN", level = "adm1", metric = "total_gini")
  expect_equal(unique(tr_total$metric), "total_gini")

  # Non-existent country returns clean empty schema
  tr_none <- ntl_gini_trends(iso3 = "Atlantis")
  expect_equal(nrow(tr_none), 0)

  # Invalid window throws error
  expect_error(ntl_gini_trends(window = "invalid_window"))
})

test_that("plotting functions handle varied inputs and options cleanly", {
  skip_if_not_installed("ggplot2")

  # Single country plot
  p_single <- ntl_gini_plot_series("TUN", level = "adm2", metric = "between_gini")
  expect_s3_class(p_single, "ggplot")
  expect_true(grepl("Tunisia", p_single$labels$title))

  # Plot with metric = 'all' and single country
  p_all_single <- ntl_gini_plot_series("TUN", level = "adm1", metric = "all")
  expect_s3_class(p_all_single, "ggplot")

  # Custom unit_label override
  p_custom <- ntl_gini_plot_series(c("TUN", "DZA"), level = "adm1", unit_label = "Provinces")
  expect_s3_class(p_custom, "ggplot")
  expect_true(grepl("Provinces", p_custom$labels$subtitle))

  # ntl_gini_plot_decomp
  p_decomp_multi <- ntl_gini_plot_decomp(c("TUN", "MAR"), level = "adm1")
  expect_s3_class(p_decomp_multi, "ggplot")

  # ntl_gini_plot_scatter variations
  p_scat_nodag <- ntl_gini_plot_scatter(level = "adm1", year = 2024L, diag = FALSE, color = "none")
  expect_s3_class(p_scat_nodag, "ggplot")

  p_scat_shares <- ntl_gini_plot_scatter(
    level = "adm1", year = 2024L,
    x = "between_share", y = "within_share",
    color = "total_gini"
  )
  expect_s3_class(p_scat_shares, "ggplot")

  # Error handling on missing iso3 in plot
  expect_error(ntl_gini_plot_series("NON_EXISTENT_COUNTRY"), "No rows match")
})

test_that("shapefile mapping functions return ggplot objects", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("sf")

  poly1 <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  poly2 <- sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0))))
  world_sf <- sf::st_sf(
    ISO_A3 = c("TUN", "MAR"),
    geometry = sf::st_sfc(poly1, poly2, crs = 4326)
  )
  p_world <- ntl_gini_map_countries(world_sf, level = "adm1", year = 2024L)
  expect_s3_class(p_world, "ggplot")

  p_globe <- ntl_gini_map_countries(world_sf, level = "adm1", year = 2024L, globe = TRUE)
  expect_s3_class(p_globe, "ggplot")

  p_bg <- ntl_gini_map_countries(world_sf[1, ], level = "adm1", year = 2024L, bg_sf = world_sf)
  expect_s3_class(p_bg, "ggplot")

  skip_if_not_installed("arrow")
  tmp_dir <- file.path(tempdir(), "ntlgini_map_test")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)
  sample_df <- data.frame(
    iso3 = c("TUN", "TUN"),
    year = c(2024L, 2024L),
    level = c("adm1", "adm1"),
    gid = c("U1", "U2"),
    name = c("Unit 1", "Unit 2"),
    n_pixels = c(10L, 20L),
    mean_radiance = c(1.0, 2.0),
    pop_share = c(0.4, 0.6),
    value_share = c(0.3, 0.7),
    gini = c(0.25, 0.65),
    within_contribution = c(0.05, 0.15),
    stringsAsFactors = FALSE
  )
  arrow::write_parquet(sample_df, file.path(tmp_dir, "gini_by_unit_adm1.parquet"))
  unit_sf <- sf::st_sf(
    GID_1 = c("U1", "U2"),
    geometry = sf::st_sfc(poly1, poly2, crs = 4326)
  )
  p_units <- ntl_gini_map_units(
    unit_sf, level = "adm1", iso3 = "TUN", year = 2024L,
    overlay_sf = unit_sf,
    data_dir = tmp_dir, download = FALSE
  )
  expect_s3_class(p_units, "ggplot")
  unlink(tmp_dir, recursive = TRUE)
})

test_that(".fast_gini computes exact Gini inequality accurately", {
  # Empty and single-element edge cases
  expect_equal(.fast_gini(numeric(0)), 0.0)
  expect_equal(.fast_gini(c(10)), 0.0)
  expect_equal(.fast_gini(c(0, 0, 0)), 0.0)
  expect_equal(.fast_gini(c(5, 5, 5, 5)), 0.0)

  # Known standard cases
  # Vector 1..5: Gini is exactly 4/15 = 0.2666667
  expect_equal(round(.fast_gini(1:5), 6), round(4 / 15, 6))

  # Vector with zeros
  v_z <- c(0, 0, 10, 20)
  # Sorted: 0, 0, 10, 20; n=4, sum=30
  # 2*(3*10 + 4*20)/(4*30) - 5/4 = 2*(110)/120 - 1.25 = 220/120 - 1.25 = 1.833333 - 1.25 = 0.5833333
  expect_equal(round(.fast_gini(v_z), 6), round(7 / 12, 6))

  # Handles NA values
  expect_equal(.fast_gini(c(1, 2, NA, 3, 4, 5)), .fast_gini(1:5))
})

test_that("ntl_gini_monthly retrieves and filters monthly data accurately", {
  skip_if_not_installed("arrow")

  # National query by code
  m_tun <- ntl_gini_monthly("TUN", level = "national", years = 2024L)
  expect_s3_class(m_tun, "tbl_df")
  expect_equal(nrow(m_tun), 12L)
  expect_true(all(m_tun$iso3 == "TUN"))
  expect_true(all(m_tun$country == "Tunisia"))
  expect_true(all(m_tun$month %in% 1:12))
  expect_true(all(m_tun$gini >= 0 & m_tun$gini <= 1))

  # National query by full name
  m_mar <- ntl_gini_monthly("Morocco", level = "national", years = 2024L, months = 1:3)
  expect_equal(nrow(m_mar), 3L)
  expect_true(all(m_mar$iso3 == "MAR"))

  # ADM1 query
  m_adm <- ntl_gini_monthly("TUN", level = "adm1", years = 2024L, months = 1L)
  expect_equal(nrow(m_adm), 24L) # 24 Tunisian governorates
  expect_true("mean_radiance" %in% names(m_adm))
  expect_true("lit_share" %in% names(m_adm))
  expect_true("gini" %in% names(m_adm))
  expect_true(all(m_adm$lit_share >= 0 & m_adm$lit_share <= 1))

  # lit_only vs zeros_included
  m_lit <- ntl_gini_monthly("TUN", level = "national", years = 2024L, zeros = "lit_only")
  expect_equal(nrow(m_lit), 12L)
  expect_true(all(m_lit$gini < m_tun$gini)) # lit-only Gini is strictly less than zeros-included Gini

  # Full multi-year query across the complete satellite era (1992-2024)
  m_33yr <- ntl_gini_monthly("TUN", level = "national", years = 1992:2024)
  expect_equal(nrow(m_33yr), 393L) # 33 years (1992 has 9 months, 1993-2024 have 12 months each)
  expect_equal(min(m_33yr$year), 1992L)
  expect_equal(max(m_33yr$year), 2024L)
  expect_equal(min(m_33yr$date), "1992-04-01")
  expect_equal(max(m_33yr$date), "2024-12-01")

  # Validation errors
  expect_error(ntl_gini_monthly("TUN", level = "adm2"), "should be one of")
  expect_error(ntl_gini_monthly("TUN", zeros = "invalid"), "should be one of")
  expect_error(
    ntl_gini_monthly("TUN", data_dir = "/nonexistent_dir_12345", download = FALSE),
    "not found"
  )
})

test_that("ntl_raster and ntl_extract validate parameters and work with local data", {
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")

  # Validation errors
  expect_error(ntl_raster(year = 1980L), "between 1992 and 2024")
  expect_error(ntl_raster(year = 2030L), "between 1992 and 2024")
  expect_error(ntl_raster(year = 2024L, month = 13L), "between 1 and 12")
  expect_error(ntl_raster(year = 2024L, month = 0L), "between 1 and 12")
  expect_error(ntl_raster(), "Either 'country' or 'roi' must be specified")
  expect_error(ntl_raster(country = "XYZ999"), "Unknown country")
  expect_error(ntl_raster(roi = "invalid_roi"), "Unsupported roi class")

  expect_error(ntl_extract(roi = "not_sf"), "must be an 'sf' object")

  # Test ntl_extract on a synthetic local raster
  tmp_dir <- file.path(tempdir(), "ntlgini_extract_test")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  poly1 <- sf::st_polygon(list(matrix(c(10, 36, 11, 36, 11, 37, 10, 37, 10, 36), ncol = 2, byrow = TRUE)))
  poly2 <- sf::st_polygon(list(matrix(c(11, 36, 12, 36, 12, 37, 11, 37, 11, 36), ncol = 2, byrow = TRUE)))
  test_sf <- sf::st_sf(district = c("D1", "D2"), geometry = sf::st_sfc(poly1, poly2, crs = 4326))

  # Mock a cached raster file
  # Tag for test_sf bbox is roi_10p00_36p00_12p00_37p00
  mock_tag <- sprintf("ntl_roi_%s_%s_%s_%s_2024_01.tif", "10p00", "36p00", "12p00", "37p00")
  mock_path <- file.path(tmp_dir, mock_tag)
  r_mock <- terra::rast(terra::ext(10, 12, 36, 37), resolution = 0.1, crs = "EPSG:4326")
  terra::values(r_mock) <- rep(c(0, 5, 10, 20), length.out = terra::ncell(r_mock))
  terra::writeRaster(r_mock, mock_path, overwrite = TRUE)

  # Extract using cached mock raster
  res <- ntl_extract(test_sf, year = 2024L, month = 1L, output_dir = tmp_dir, drop_geom = TRUE)
  expect_s3_class(res, "tbl_df")
  expect_equal(nrow(res), 2L)
  expect_true(all(c("sum_of_lights", "mean_radiance", "pixels", "lit_share", "gini") %in% names(res)))
  expect_equal(res$pixels, c(100L, 100L))
  expect_true(all(res$gini > 0 & res$gini < 1))
  expect_equal(res$month, c(1L, 1L))
  expect_equal(res$date, c("2024-01-01", "2024-01-01"))

  unlink(tmp_dir, recursive = TRUE)
})

