test_that("ntl_countries covers every country with no missing metadata", {
  expect_s3_class(ntl_countries, "tbl_df")
  expect_setequal(ntl_countries$iso3, unique(ntl_gini_global$iso3))
  expect_false(anyNA(ntl_countries[, c("iso3", "country", "continent", "region",
                                        "un_subregion")]))
  expect_equal(ntl_countries$cown[ntl_countries$iso3 == "TUN"], 616L)
  expect_true(all(stats::na.omit(ntl_countries$income_group) %in%
                    c("Low income", "Lower middle income", "Upper middle income", "High income")))
  expect_false(anyDuplicated(ntl_countries$iso3) > 0)
  expect_equal(ntl_countries$country[ntl_countries$iso3 == "XKO"], "Kosovo")
})

test_that("ntl_gini_decomp filters by region and continent", {
  af <- ntl_gini_decomp(continent = "africa", level = "adm1", years = 2024)
  expect_true("TUN" %in% af$iso3)
  expect_false("FRA" %in% af$iso3)
  expect_true(all(af$iso3 %in% ntl_countries$iso3[ntl_countries$continent == "Africa"]))

  ssa <- ntl_gini_decomp(region = "Sub-Saharan Africa", iso3 = c("GAB", "TUN"), years = 2024)
  expect_equal(unique(ssa$iso3), "GAB")

  expect_error(ntl_gini_decomp(region = "Atlantis"), "Invalid `region`")
  expect_error(ntl_gini_decomp(continent = 1), "character")
})

test_that("ntl_add_country_info adds metadata and keeps order", {
  d <- ntl_gini_decomp(iso3 = c("TUN", "BRA"), level = "adm1", years = 2023:2024)
  d <- d[c(4, 1, 3, 2), ]
  out <- ntl_add_country_info(d)
  expect_equal(out$iso3, d$iso3)
  expect_equal(names(out)[1:5], c("iso3", "country", "continent", "region", "un_subregion"))
  expect_equal(unique(out$country[out$iso3 == "TUN"]), "Tunisia")
  expect_equal(ncol(out), ncol(d) + 7)

  again <- ntl_add_country_info(out)
  expect_equal(ncol(again), ncol(out))
  expect_error(ntl_add_country_info(data.frame(x = 1)), "iso3")
})

test_that("ntl_flag_quality sets each flag correctly", {
  df <- data.frame(
    iso3 = "AAA", level = "adm1",
    year = c(2010L, 2011L, 2012L, 2013L, 2015L),
    n_groups = c(3L, 10L, 10L, 10L, 10L),
    total_gini = c(0.80, 0.81, 0.90, 0.995, NA)
  )
  q <- ntl_flag_quality(df[c(3, 1, 5, 2, 4), ])
  q <- q[order(q$year), ]
  expect_equal(q$sensor, c(rep("DMSP-reconstructed", 2), rep("VIIRS", 3)))
  expect_equal(q$flag_sensor_break, c(FALSE, FALSE, TRUE, FALSE, FALSE))
  expect_equal(q$flag_few_groups, c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(q$flag_saturated, c(FALSE, FALSE, FALSE, TRUE, FALSE))
  expect_equal(q$flag_jump, c(FALSE, FALSE, TRUE, TRUE, FALSE))
  expect_equal(q$flag_missing, c(FALSE, FALSE, FALSE, FALSE, TRUE))
  expect_equal(q$any_flag, c(TRUE, FALSE, TRUE, TRUE, TRUE))

  # jumps are not computed across countries or levels
  two <- rbind(df[1, ], transform(df[2, ], iso3 = "BBB", total_gini = 0.1))
  expect_false(any(ntl_flag_quality(two)$flag_jump))

  expect_error(ntl_flag_quality(data.frame(iso3 = "A")), "missing column")
})

test_that("ntl_flag_quality works on the full bundled data", {
  q <- ntl_flag_quality(ntl_gini_decomp())
  expect_equal(nrow(q), nrow(ntl_gini_global))
  expect_false(anyNA(q$any_flag))
  expect_true(mean(q$flag_jump) < 0.05)
  gab <- q[q$iso3 == "GAB" & q$level == "adm1" & q$flag_jump, ]
  expect_true(1997 %in% gab$year)
})

test_that("ntl_flag_quality flags between_share jumps", {
  df <- data.frame(
    iso3 = "AAA", level = "adm1", year = 2000:2002, n_groups = 10L,
    total_gini = c(0.9, 0.9, 0.9), between_share = c(0.5, 0.2, 0.25)
  )
  expect_equal(ntl_flag_quality(df)$flag_jump, c(FALSE, TRUE, FALSE))
  expect_equal(ntl_flag_quality(df, share_jump_threshold = 0.5)$flag_jump, rep(FALSE, 3))
})

test_that("ntl_gini_trend computes OLS slope, change and sensor periods", {
  df <- data.frame(
    iso3 = "AAA", level = "adm1", year = 2008:2015,
    total_gini = 0.5 + 0.01 * (0:7)
  )
  tr <- ntl_gini_trend(df)
  expect_equal(nrow(tr), 1)
  expect_equal(tr$slope, 0.01, tolerance = 1e-10)
  expect_equal(tr$slope_se, 0, tolerance = 1e-10)
  expect_equal(tr$change, 0.07, tolerance = 1e-10)
  expect_equal(tr$pct_change, 14, tolerance = 1e-8)
  expect_equal(c(tr$first_year, tr$last_year), c(2008L, 2015L))

  sp <- ntl_gini_trend(df, split_sensor = TRUE)
  expect_equal(sp$period, c("1992-2011", "2012-2024"))
  expect_equal(sp$n_years, c(4L, 4L))

  win <- ntl_gini_trend(df, from = 2010, to = 2012)
  expect_equal(win$n_years, 3L)

  real <- ntl_gini_trend(ntl_gini_decomp(iso3 = c("TUN", "MAR"), level = c("adm1", "adm2")),
                         measure = "between_share")
  expect_equal(nrow(real), 4)
  expect_true(all(real$n_years == 33))

  expect_equal(nrow(ntl_gini_trend(df, from = 2030)), 0)
  expect_error(ntl_gini_trend(df, measure = "nope"), "must name a column")
  expect_error(ntl_gini_trend(df, measure = "iso3"), "numeric")
})
