# ------------------------------------------------------------
# Rides: read the files in done_rides/, per-ride stats, which streets each ride covered.
# A street counts as ridden once ridden_min_cover of its sample points lie within
# ridden_buffer_m of a track. Coverage per file is cached, so only new rides cost time.
# ------------------------------------------------------------

# Every recorded point of a ride: lon, lat, and ele / time when the file has them.
read_points <- function(f) {
  if (grepl("\\.fit$", f, ignore.case = TRUE)) {
    r <- FITfileR::records(FITfileR::readFitFile(f))
    if (!is.data.frame(r)) r <- dplyr::bind_rows(r)    # several record types -> one table
    if (!all(c("position_lat", "position_long") %in% names(r))) return(data.frame(lon = numeric(0), lat = numeric(0)))
    r <- r[!is.na(r$position_lat) & !is.na(r$position_long), ]
    r <- r[order(r$timestamp), ]
    d <- data.frame(lon = r$position_long, lat = r$position_lat, time = as.POSIXct(r$timestamp, tz = "UTC"))
    if ("altitude" %in% names(r)) d$ele <- as.numeric(r$altitude)
  } else if (grepl("\\.gpx$", f, ignore.case = TRUE)) {
    p <- suppressWarnings(st_read(f, layer = "track_points", quiet = TRUE))
    d <- data.frame(lon = st_coordinates(p)[, 1], lat = st_coordinates(p)[, 2])
    if ("ele"  %in% names(p)) d$ele  <- suppressWarnings(as.numeric(p$ele))
    if ("time" %in% names(p)) d$time <- as.POSIXct(p$time, tz = "UTC")
  } else {
    d <- read.csv(f)
    if ("time" %in% names(d)) d$time <- as.POSIXct(d$time, tz = "UTC")
  }
  d
}

# Track as a MULTILINESTRING in EPSG:25832, split at GPS dropouts. NULL if too short.
points_to_track <- function(d, gap_m) {
  if (nrow(d) < 2) return(NULL)
  d <- d[c(TRUE, diff(d$lat) != 0 | diff(d$lon) != 0), , drop = FALSE]
  if (nrow(d) < 2) return(NULL)
  xy <- st_coordinates(st_transform(st_as_sf(d, coords = c("lon", "lat"), crs = 4326), 25832))
  grp <- c(1L, 1L + cumsum(sqrt(rowSums(diff(xy)^2)) > gap_m))
  parts <- split(seq_len(nrow(xy)), grp)
  parts <- parts[lengths(parts) >= 2]
  if (length(parts) == 0) return(NULL)
  st_multilinestring(lapply(parts, function(ix) xy[ix, , drop = FALSE]))
}

# Climbing from an elevation series, smoothed so GPS/barometer noise does not add up.
ascent_m <- function(e) {
  e <- e[!is.na(e)]
  if (length(e) < 5) return(NA_real_)
  sm <- as.numeric(stats::filter(e, rep(1 / 9, 9), sides = 2))
  sm[is.na(sm)] <- e[is.na(sm)]
  d <- diff(sm); sum(d[d > 0])
}

# Distance, time and climbing of one ride. Time needs timestamps, climbing needs elevation.
ride_summary <- function(d, gap_m, fallback_time = Sys.time()) {
  xy <- st_coordinates(st_transform(st_as_sf(d, coords = c("lon", "lat"), crs = 4326), 25832))
  step <- sqrt(rowSums(diff(xy)^2))
  ok <- step <= gap_m                                 # a long jump is a GPS dropout, not riding
  has_t <- "time" %in% names(d) && sum(!is.na(d$time)) > 1
  dt <- if (has_t) as.numeric(diff(as.numeric(d$time))) else rep(NA_real_, length(step))
  moving <- ok & !is.na(dt) & dt > 0 & dt <= 30 & step / dt > 0.5     # above 1.8 km/h, no pauses
  km <- sum(step[ok]) / 1000
  moving_h <- if (has_t) sum(dt[moving]) / 3600 else NA_real_
  data.frame(start = if (has_t) min(d$time, na.rm = TRUE) else as.POSIXct(fallback_time, tz = "UTC"),
             km = km, moving_h = moving_h,
             elapsed_h = if (has_t) as.numeric(difftime(max(d$time, na.rm = TRUE), min(d$time, na.rm = TRUE), units = "hours")) else NA_real_,
             ascent_m = if ("ele" %in% names(d)) ascent_m(d$ele) else NA_real_,
             avg_kmh = if (has_t && moving_h > 0) km / moving_h else NA_real_)
}

# Indices of the network's sample points that lie within buffer_m of a track.
covered_samples <- function(trk, net, buffer_m) {
  buf <- st_buffer(st_simplify(st_sfc(trk, crs = 25832), dTolerance = 2), buffer_m)
  bb <- st_bbox(buf); s <- net$samp_xy
  cand <- which(s[, 1] >= bb["xmin"] & s[, 1] <= bb["xmax"] & s[, 2] >= bb["ymin"] & s[, 2] <= bb["ymax"])
  if (length(cand) == 0) return(integer(0))
  pts <- st_as_sf(as.data.frame(s[cand, , drop = FALSE]), coords = c("X", "Y"), crs = 25832)
  cand[lengths(st_intersects(pts, buf)) > 0]
}

ride_files <- function(cfg) {
  list.files(cfg$rides_dir, pattern = "\\.(csv|gpx|fit)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
}

# One ride file -> summary, covered sample indices and a simplified WGS84 track. Cached by file
# name, size and time stamp. NULL (with a reason) when the file cannot be used.
process_ride <- function(f, cfg, net) {
  info <- file.info(f)
  cdir <- file.path(cfg$cache_dir, paste0("rides_", net$version, "_b", cfg$ridden_buffer_m))
  dir.create(cdir, showWarnings = FALSE, recursive = TRUE)
  cf <- file.path(cdir, sprintf("%s_%d_%d.rds", gsub("[^A-Za-z0-9._-]", "_", basename(f)),
                                as.integer(info$size), as.integer(info$mtime)))
  if (file.exists(cf)) return(read_cache(cf))
  d <- tryCatch(read_points(f), error = function(e) NULL)
  if (is.null(d) || nrow(d) < 2) return(structure(list(file = f), reason = "no GPS points could be read"))
  trk <- points_to_track(d, cfg$track_gap_m)
  if (is.null(trk)) return(structure(list(file = f), reason = "track too short"))
  out <- list(file = f, summary = ride_summary(d, cfg$track_gap_m, info$mtime),
              samples = covered_samples(trk, net, cfg$ridden_buffer_m),
              track_wgs = st_transform(st_simplify(st_sfc(trk, crs = 25832), dTolerance = 3), 4326))
  saveRDS(out, cf)
  out
}

# All rides, in time order, and what they covered (cumulatively, so per-ride "new street" adds
# up to the total). Returns:
#   rides      data frame, one row per ride
#   cover      per edge id: share of the street within the buffer of any ride (NA = no samples)
#   ridden     per edge id: TRUE once the street counts as ridden
#   first_ride per edge id: the ride number that first covered it (NA = not ridden)
#   tracks     sfc of ride tracks (WGS84), in ride order
#   skipped    data frame of files that could not be used, with the reason
load_rides <- function(cfg, net, files = ride_files(cfg)) {
  res <- lapply(files, process_ride, cfg = cfg, net = net)
  bad <- vapply(res, function(r) is.null(r$summary), logical(1))
  skipped <- data.frame(file = basename(files[bad]),
                        reason = vapply(res[bad], function(r) attr(r, "reason") %||% "unreadable", ""))
  res <- res[!bad]
  n_eid <- nrow(net$edges)
  n_s <- net$samp_n; sid <- as.integer(names(n_s))
  samp_edge <- rep(seq_along(sid), n_s)
  goal <- net$edges$in_goal[sid]
  first_ride <- rep(NA_integer_, n_eid); cover <- rep(NA_real_, n_eid); cover[sid] <- 0

  if (length(res) == 0) {
    rides <- data.frame(ride = integer(0), file = character(0), start = as.POSIXct(character(0)),
                        km = numeric(0), moving_h = numeric(0), elapsed_h = numeric(0),
                        ascent_m = numeric(0), avg_kmh = numeric(0), new_km = numeric(0))
    return(list(rides = rides, cover = cover, ridden = rep(FALSE, n_eid), first_ride = first_ride,
                tracks = st_sfc(crs = 4326), skipped = skipped))
  }
  res <- res[order(vapply(res, function(r) as.numeric(r$summary$start), 0))]
  inside <- rep(FALSE, nrow(net$samp_xy))
  done <- rep(FALSE, length(sid)); new_km <- numeric(length(res))
  for (i in seq_along(res)) {
    inside[res[[i]]$samples] <- TRUE
    frac <- as.numeric(rowsum(as.integer(inside), samp_edge)) / n_s
    now <- frac >= cfg$ridden_min_cover
    fresh <- now & !done
    first_ride[sid[fresh]] <- i
    new_km[i] <- sum(net$edges$len[sid][fresh & goal]) / 1000
    done <- now
  }
  cover[sid] <- frac
  rides <- cbind(data.frame(ride = seq_along(res), file = basename(vapply(res, `[[`, "", "file"))),
                 do.call(rbind, lapply(res, `[[`, "summary")), new_km = new_km)
  ridden <- !is.na(first_ride)
  list(rides = rides, cover = cover, ridden = ridden, first_ride = first_ride,
       tracks = do.call(c, lapply(res, `[[`, "track_wgs")), skipped = skipped)
}
