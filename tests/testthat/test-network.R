ways <- function(highway, tags) data.frame(highway = highway, other_tags = tags, stringsAsFactors = FALSE)

test_that("roads count, motorways and primary roads do not", {
  w <- ways(c("residential", "tertiary", "primary", "motorway", "cycleway"), NA)
  expect_equal(way_filter(w), c(TRUE, TRUE, FALSE, FALSE, TRUE))
})

test_that("sidewalks and crossings do not count, a park footway does", {
  w <- ways(c("footway", "footway", "footway"),
            c('"footway"=>"sidewalk"', '"footway"=>"crossing"', NA))
  expect_equal(way_filter(w), c(FALSE, FALSE, TRUE))
})

test_that("paths need a good surface or bike signs, trails are out", {
  w <- ways(rep("path", 5),
            c('"surface"=>"gravel"', '"bicycle"=>"designated"', NA, '"surface"=>"ground"',
              '"surface"=>"gravel","mtb:scale"=>"2"'))
  expect_equal(way_filter(w, in_marka = rep(TRUE, 5)), c(TRUE, TRUE, FALSE, FALSE, FALSE))
})

test_that("Marka gravel roads count, rough tracks do not", {
  w <- ways(rep("track", 5),
            c('"tracktype"=>"grade2"', '"surface"=>"compacted"', '"tracktype"=>"grade4"', NA,
              '"tracktype"=>"grade1","surface"=>"dirt"'))
  expect_equal(way_filter(w, in_marka = rep(TRUE, 5)), c(TRUE, TRUE, FALSE, FALSE, FALSE))
})

test_that("private, no-bike, steps and driveways are out", {
  w <- ways(c("service", "residential", "steps", "service", "service"),
            c('"access"=>"private"', '"bicycle"=>"no"', NA, '"service"=>"driveway"', '"service"=>"alley"'))
  expect_equal(way_filter(w), c(FALSE, FALSE, FALSE, FALSE, TRUE))
})

test_that("untagged paths and tracks count in the city but not in Marka", {
  w <- ways(c("path", "path", "track", "track"), NA)
  expect_equal(way_filter(w, in_marka = c(FALSE, TRUE, FALSE, TRUE)), c(TRUE, FALSE, TRUE, FALSE))
})

test_that("way_class groups highway types", {
  expect_equal(way_class(c("residential", "cycleway", "footway", "track")), c("road", "road", "path", "gravel"))
})
