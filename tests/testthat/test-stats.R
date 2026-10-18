mk_rides <- function(days, km, new_km, moving_h = km / 20) {
  data.frame(ride = seq_along(days), file = paste0("r", seq_along(days), ".fit"),
             start = as.POSIXct("2026-10-01 10:00", tz = "UTC") + days * 86400,
             km = km, moving_h = moving_h, elapsed_h = moving_h * 1.2, ascent_m = 100,
             avg_kmh = 20, new_km = new_km)
}
today <- as.POSIXct("2026-10-29 12:00", tz = "UTC")
remaining <- data.frame(len_km = c(50, 50), dist_km = c(1, 3))

test_that("one ride: pace per week and finish date are NA, per-ride numbers work", {
  r <- mk_rides(0, 20, 10)
  p <- predict_finish(r, remaining_km = 100, remaining, ride_dist_km = 1, today = today)
  expect_true(is.na(p$all$rides_per_week))
  expect_true(is.na(p$simple$finish))
  expect_equal(p$simple$rides_left, 10)
  expect_equal(p$simple$ride_km_left, 200)
  expect_equal(p$simple$hours_left, 10)
})

test_that("two rides a week apart: 1 ride/week, finish date follows", {
  r <- mk_rides(c(0, 7), c(20, 20), c(10, 10))
  p <- predict_finish(r, remaining_km = 100, remaining, ride_dist_km = c(1, 1), today = today)
  expect_equal(p$all$rides_per_week, 1)
  expect_equal(p$all$new_per_week, 10)
  expect_equal(p$simple$finish, as.Date(today) + 70)
})

test_that("distance-aware estimate needs more rides than the simple one when streets are far", {
  r <- mk_rides(c(0, 7), c(20, 20), c(10, 10))
  far <- data.frame(len_km = c(50, 50), dist_km = c(4, 6))
  p <- predict_finish(r, remaining_km = 100, far, ride_dist_km = c(1, 1), today = today)
  expect_gt(p$distance$rides_left, p$simple$rides_left)
})

test_that("no rides: nothing errors, everything NA", {
  r <- mk_rides(0, 20, 10)[0, ]
  p <- predict_finish(r, remaining_km = 100, remaining, ride_dist_km = numeric(0), today = today)
  expect_true(is.na(p$simple$rides_left))
  expect_true(is.na(p$distance$rides_left))
  expect_equal(nrow(projection_series(r, p, goal_km = 100, today = today)), 0)
})

test_that("weekly_rides fills empty weeks", {
  r <- mk_rides(c(0, 15), c(20, 30), c(5, 6))
  w <- weekly_rides(r)
  expect_equal(w$rides, c(1L, 0L, 1L))
  expect_equal(sum(w$km), 50)
})

test_that("projection reaches the goal", {
  r <- mk_rides(c(0, 7), c(20, 20), c(10, 10))
  p <- predict_finish(r, remaining_km = 100, remaining, ride_dist_km = c(1, 1), today = today)
  s <- projection_series(r, p, goal_km = 120, today = today)
  expect_equal(max(s$ridden_km[s$series == "At current pace"]), 120)
  expect_equal(max(s$ridden_km[s$series == "Distance-aware"]), 120)
})

test_that("rolling_mean averages the last k values, with a short start", {
  expect_equal(rolling_mean(c(10, 20, 30, 40, 50), 3), c(10, 15, 20, 30, 40))
  expect_equal(rolling_mean(numeric(0), 4), numeric(0))
})
