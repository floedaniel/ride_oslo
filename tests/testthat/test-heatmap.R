# A tiny fake network: 4 Oslo streets, two named
fake_net <- list(edges = data.frame(len = c(300, 300, 100, 500), in_net = TRUE, conn = FALSE, in_goal = c(TRUE, TRUE, TRUE, FALSE),
                                    name = c("Storgata", "Storgata", "Smalgata", "Utenfor"), stringsAsFactors = FALSE))
act <- function(date, eids, source = "Strava history", km = 10)
  list(date = as.Date(date), eids = eids, source = source, km_oslo = km, km_total = km + 5,
       district_km = c(Sagene = km))

test_that("street counts need enough of the street, and count rides not passes", {
  a <- list(act("2016-05-01", c(1, 2)), act("2016-06-01", 1), act("2026-10-04", c(1, 3), "Ride Oslo project"))
  s <- heat_summary(a, fake_net)
  expect_equal(s$top_streets$street, "Storgata")         # Smalgata (100 m) is too short to list, like a junction name
  expect_equal(s$top_streets$rides, 3L)                  # 300 m of Storgata >= 200 m in every ride
  expect_equal(s$touched_km, 0.7)                       # the street outside Oslo never counts
  expect_equal(s$rides, 3)
})

test_that("filters by year and source", {
  a <- list(act("2016-05-01", 1), act("2026-10-04", 1, "Ride Oslo project"))
  expect_length(heat_filter(a, years = c(2020, 2030)), 1)
  expect_length(heat_filter(a, sources = "Strava history"), 1)
  expect_length(heat_filter(a, years = c(2017, 2025)), 0)
})

test_that("per year and month tables add up", {
  a <- list(act("2016-05-01", 1), act("2016-05-20", 1), act("2018-01-02", 2))
  s <- heat_summary(a, fake_net)
  expect_equal(sum(s$per_year$rides), 3)
  expect_equal(s$month_year["2016", "5"], 2)
  expect_equal(dim(s$month_year), c(3L, 12L))           # 2016, 2017 (empty), 2018
})

test_that("no rides: summary is empty, not an error", {
  s <- heat_summary(list(), fake_net)
  expect_equal(s$rides, 0)
  expect_equal(nrow(s$top_streets), 0)
})
