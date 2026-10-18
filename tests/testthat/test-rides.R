fit <- list.files(file.path("..", "..", "done_rides"), pattern = "\\.fit$", full.names = TRUE)

test_that("a Wahoo .fit file reads into lon/lat/time/ele", {
  skip_if(length(fit) == 0, "no .fit ride in done_rides/")
  d <- read_points(fit[1])
  expect_true(all(c("lon", "lat", "time", "ele") %in% names(d)))
  expect_gt(nrow(d), 100)
  expect_true(all(d$lat > 59 & d$lat < 61 & d$lon > 10 & d$lon < 12))
  s <- ride_summary(d, gap_m = 150)
  expect_gt(s$km, 1)
  expect_gt(s$moving_h, 0)
})

test_that("ascent ignores noise and counts real climbs", {
  expect_equal(ascent_m(c(rep(100, 20), seq(100, 150, length.out = 20), rep(150, 20))), 50, tolerance = 1)
  expect_true(is.na(ascent_m(c(1, 2))))
})

test_that("a broken file gives no points instead of crashing", {
  bad <- tempfile(fileext = ".gpx"); writeLines("not a gpx", bad)
  expect_null(tryCatch(read_points(bad), error = function(e) NULL))
  expect_null(points_to_track(data.frame(lon = 10.7, lat = 59.9), gap_m = 150))
})
