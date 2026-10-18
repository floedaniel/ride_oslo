# ------------------------------------------------------------
# Historic heatmap of Oslo: every bike ride from the Strava history (data/strava/) plus the Ride
# Oslo project rides. Display only - Strava never counts toward the project goal or the planner.
# Each ride is reduced once (cached) to: its track clipped to Oslo, the grid cells it passes
# (for the glow) and the streets it covered (for the street stats).
# ------------------------------------------------------------

heat_res_m   <- 20       # glow grid cell size
bike_sports  <- c("Ride", "GravelRide", "MountainBikeRide", "EBikeRide", "EMountainBikeRide")

# Fixed grid over Oslo (EPSG:25832) that every ride's cells refer to
heat_template <- function(net) {
  b <- st_transform(net$boundary, 25832)
  terra::rast(terra::ext(terra::vect(b)), resolution = heat_res_m, crs = "EPSG:25832")
}

# One ride -> list(id, date, source, km_total, km_oslo, track (Oslo part, EPSG:25832), cells, eids,
# district_km). NULL when the ride never enters Oslo.
heat_activity <- function(trk, id, date, source, km_total, net, tmpl, b_utm, d_utm) {
  if (is.null(trk)) return(NULL)
  g <- st_sfc(trk, crs = 25832)
  clip <- suppressWarnings(st_intersection(g, b_utm))
  if (length(clip) == 0) return(NULL)
  clip <- suppressWarnings(st_collection_extract(clip, "LINESTRING"))   # a border touch can add stray points
  if (length(clip) == 0 || as.numeric(sum(st_length(clip))) < 50) return(NULL)
  clip <- st_cast(st_combine(clip), "MULTILINESTRING")
  # streets: a street counts as passed when half its sample points lie within 20 m of the track
  # (looser than the project rule, because old Strava outlines cut corners)
  s <- covered_samples(trk, net, 20)
  e <- net$samp_eid[s]
  frac <- table(e) / net$samp_n[names(table(e))]
  dk <- suppressWarnings(st_intersection(st_sf(geometry = clip), d_utm))
  list(id = as.character(id), date = as.Date(date), source = source, km_total = km_total,
       km_oslo = as.numeric(sum(st_length(clip))) / 1000,
       track = clip,
       cells = unique(terra::cells(tmpl, terra::vect(st_sf(geometry = clip)), touches = TRUE)[, "cell"]),
       eids = as.integer(names(frac)[frac >= 0.5]),
       district_km = if (nrow(dk)) tapply(as.numeric(st_length(dk)) / 1000, dk$district, sum) else numeric(0))
}

# All Strava bike rides that touch Oslo (cached; rebuilt when index.csv or the network changes)
strava_activities <- function(cfg, net) {
  dir <- file.path(cfg$data_dir, "strava"); ixf <- file.path(dir, "index.csv")
  if (!file.exists(ixf)) return(list())
  cf <- file.path(cfg$cache_dir, sprintf("heat_strava_%s_%d.rds", net$version, as.integer(file.mtime(ixf))))
  if (file.exists(cf)) return(read_cache(cf))
  ix <- read.csv(ixf, stringsAsFactors = FALSE)
  ix <- ix[ix$sport %in% bike_sports, ]
  tmpl <- heat_template(net)
  b_utm <- st_geometry(st_transform(net$boundary, 25832)); d_utm <- st_transform(net$districts, 25832)
  acts <- lapply(seq_len(nrow(ix)), function(i) {
    is_poly <- ix$source[i] != "stream"
    f <- file.path(dir, if (is_poly) "poly" else "", paste0(ix$id[i], ".csv"))
    if (!file.exists(f)) return(NULL)
    d <- tryCatch(read.csv(f), error = function(e) NULL)
    if (is.null(d) || nrow(d) < 2) return(NULL)
    # outlines are sparse points of one continuous ride: never split them at "gaps"
    trk <- points_to_track(d, if (is_poly) Inf else cfg$track_gap_m)
    heat_activity(trk, ix$id[i], ix$date[i], "Strava history", ix$km[i], net, tmpl, b_utm, d_utm)
  })
  acts <- Filter(Negate(is.null), acts)
  saveRDS(acts, cf)
  acts
}

# Project rides (from load_rides) in the same form
project_activities <- function(prog, net) {
  if (nrow(prog$rides) == 0) return(list())
  tmpl <- heat_template(net)
  b_utm <- st_geometry(st_transform(net$boundary, 25832)); d_utm <- st_transform(net$districts, 25832)
  trk <- st_transform(prog$tracks, 25832)
  Filter(Negate(is.null), lapply(seq_len(nrow(prog$rides)), function(i)
    heat_activity(trk[[i]], prog$rides$file[i], as.Date(prog$rides$start[i]), "Ride Oslo project",
                  prog$rides$km[i], net, tmpl, b_utm, d_utm)))
}

heat_filter <- function(acts, years = c(1900, 3000), sources = c("Strava history", "Ride Oslo project")) {
  Filter(function(a) a$source %in% sources && as.integer(format(a$date, "%Y")) >= years[1] &&
           as.integer(format(a$date, "%Y")) <= years[2], acts)
}

# Glow raster (values 0..1, NA where nothing) from the rides' cells: rides per cell on a log
# scale, plus two blurred copies of itself for the halo.
heat_glow <- function(acts, net, glow = 3) {
  tmpl <- heat_template(net)
  if (length(acts) == 0) return(NULL)
  cnt <- tabulate(as.integer(unlist(lapply(acts, `[[`, "cells"))), nbins = terra::ncell(tmpl))
  v <- terra::setValues(tmpl, log1p(cnt) / log1p(max(cnt)))
  halo <- function(sigma_m) terra::focal(v, w = terra::focalMat(v, sigma_m, "Gauss"), fun = "sum", na.rm = TRUE, fillvalue = 0)
  k <- glow / 3
  out <- 0.85 * v + k * 1.3 * halo(2 * heat_res_m) + k * 2.5 * halo(7 * heat_res_m)
  out <- terra::clamp(out, 0, 1)
  out <- terra::mask(out, terra::vect(st_transform(net$boundary, 25832)))
  out[out < 0.03] <- NA
  out
}

# Rides passing each street, and statistics for the filtered rides
heat_summary <- function(acts, net, min_street_m = 200) {
  e <- net$edges
  goal <- e$in_net & !e$conn & e$in_goal
  n <- length(acts)
  passes <- tabulate(as.integer(unlist(lapply(acts, `[[`, "eids"))), nbins = nrow(e))
  touched_km <- sum(e$len[goal & passes > 0]) / 1000

  yr <- vapply(acts, function(a) as.integer(format(a$date, "%Y")), 1L)
  mo <- vapply(acts, function(a) as.integer(format(a$date, "%m")), 1L)
  src <- vapply(acts, `[[`, "", "source")
  km  <- vapply(acts, `[[`, 0, "km_oslo")
  per_year <- if (n) aggregate(list(rides = rep(1L, n), km = km), list(year = yr, source = src), sum) else
    data.frame(year = integer(0), source = character(0), rides = integer(0), km = numeric(0))

  # A street counts for a ride when the ride covered >= min_street_m of it (or half of a shorter one)
  name_len <- tapply(e$len[goal], e$name[goal], sum)
  street_hits <- unlist(lapply(acts, function(a) {
    ee <- a$eids[goal[a$eids] & !is.na(e$name[a$eids])]
    if (!length(ee)) return(character(0))
    l <- tapply(e$len[ee], e$name[ee], sum)
    names(l)[l >= pmin(min_street_m, 0.5 * name_len[names(l)])]
  }))
  street_hits <- street_hits[name_len[street_hits] >= 250]     # skip junction names and tiny stubs
  top <- sort(table(street_hits), decreasing = TRUE)
  top_streets <- data.frame(street = names(top), rides = as.integer(top), stringsAsFactors = FALSE)
  if (nrow(top_streets)) top_streets$km <- as.numeric(name_len[top_streets$street]) / 1000

  dk <- unlist(lapply(acts, `[[`, "district_km"))
  district <- if (length(dk)) sort(tapply(dk, names(dk), sum), decreasing = TRUE) else numeric(0)

  list(rides = n, km_oslo = sum(km), km_total = sum(vapply(acts, `[[`, 0, "km_total")),
       touched_km = touched_km, goal_km = sum(e$len[goal]) / 1000,
       first = if (n) min(do.call(c, lapply(acts, `[[`, "date"))) else NA,
       last  = if (n) max(do.call(c, lapply(acts, `[[`, "date"))) else NA,
       longest_km = if (n) max(km) else NA_real_,
       per_year = per_year,
       month_year = if (n) table(factor(yr, levels = min(yr):max(yr)), factor(mo, levels = 1:12)) else NULL,
       top_streets = top_streets,
       district_km = data.frame(district = names(district), km = as.numeric(district), stringsAsFactors = FALSE),
       passes = passes)
}
