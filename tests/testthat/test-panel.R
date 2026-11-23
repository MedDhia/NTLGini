test_that("ntl_panel builds a long panel with metadata, intensity and flags", {
  p <- ntl_panel("adm1", iso3 = c("TUN", "MAR"))
  expect_equal(nrow(p), 2 * 33)
  expect_false(anyDuplicated(p[, c("iso3", "year")]) > 0)
  expect_true(all(c("country", "income_group", "cown", "vdem_id", "mean_light",
                    "log_mean_light", "sensor", "any_flag") %in% names(p)))
  expect_equal(p$mean_light, p$sum_of_lights / p$n_pixels)
  expect_equal(p, p[order(p$iso3, p$year), ])

  slim <- ntl_panel("adm2", iso3 = "TUN", measures = "total_gini", flags = FALSE)
  expect_false("between_gini" %in% names(slim))
  expect_false("sensor" %in% names(slim))
  expect_true("mean_light" %in% names(slim))

  expect_error(ntl_panel(c("adm1", "adm2")), "single")
  expect_error(ntl_panel("adm1", measures = "nope"), "Unknown")
})

test_that("ntl_panel wide format has one row per country-year", {
  w <- ntl_panel(c("adm1", "adm2"), iso3 = "TUN", format = "wide")
  expect_equal(nrow(w), 33)
  expect_true(all(c("between_share_adm1", "between_share_adm2", "flag_jump") %in% names(w)))
  expect_equal(w$total_gini_adm1, w$total_gini_adm2, tolerance = 1e-3)
  long <- ntl_panel("adm2", iso3 = "TUN", flags = FALSE)
  expect_equal(w$between_share_adm2, long$between_share)
})

test_that("ntl_event_window centres series on events", {
  d <- ntl_panel("adm1", iso3 = c("TUN", "EGY", "MAR"), flags = FALSE)
  ev <- data.frame(iso3 = c("tun", "EGY"), event_year = c(2011, 2013))
  out <- ntl_event_window(d, ev, window = -2:2, measure = "between_share")

  expect_equal(nrow(out), 10)
  expect_setequal(unique(out$iso3), c("TUN", "EGY"))
  expect_equal(out$event_time[out$iso3 == "TUN"], -2:2)
  expect_equal(out$year[out$iso3 == "EGY"], 2011:2015)
  expect_equal(out$between_share_rel[out$event_time == -1], c(0, 0))
  tun <- out[out$iso3 == "TUN", ]
  expect_equal(tun$between_share_rel, tun$between_share - tun$between_share[2])
  expect_true(all(out$crosses_sensor_break))
  expect_false(any(ntl_event_window(d, data.frame(iso3 = "TUN", event_year = 2018),
                                    window = 0:3)$crosses_sensor_break))

  # several events per country with explicit ids
  multi <- data.frame(iso3 = "TUN", event_year = c(2000, 2014), event_id = c("a", "b"),
                      type = c("x", "y"))
  m <- ntl_event_window(d, multi, window = 0:1)
  expect_equal(m$event_id, c("a", "a", "b", "b"))
  expect_equal(m$type, c("x", "x", "y", "y"))

  expect_error(ntl_event_window(d, data.frame(iso3 = "TUN")), "event_year")
  expect_error(ntl_event_window(d, ev, measure = "country"), "numeric")
  expect_error(ntl_event_window(ntl_gini_decomp(iso3 = "TUN"), ev), "several levels")
  expect_error(
    ntl_event_window(d, data.frame(iso3 = c("TUN", "TUN"), event_year = 2011)),
    "unique"
  )
})
