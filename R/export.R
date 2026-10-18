# ------------------------------------------------------------
# Elevation lookup and writing the next ride.
# ------------------------------------------------------------

# Elevation (m) for route points (sf, WGS84) from AWS terrain tiles. NA when offline.
route_elevation <- function(pts_wgs, zoom = 13) {
  tryCatch({
    dem <- suppressMessages(elevatr::get_elev_raster(locations = pts_wgs, z = zoom, clip = "bbox", src = "aws", verbose = FALSE))
    as.numeric(terra::extract(terra::rast(dem), terra::vect(pts_wgs))[, 2])
  }, error = function(e) {
    message("Elevation lookup failed (", conditionMessage(e), ") - continuing without it.")
    rep(NA_real_, nrow(pts_wgs))
  })
}

write_gpx_track <- function(df, filename, track_name) {
  pts <- ifelse(is.na(df$ele),
                sprintf('      <trkpt lat="%f" lon="%f"></trkpt>', df$lat, df$lon),
                sprintf('      <trkpt lat="%f" lon="%f"><ele>%.1f</ele></trkpt>', df$lat, df$lon, df$ele))
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
               '<gpx version="1.1" creator="ride_oslo" xmlns="http://www.topografix.com/GPX/1/1">',
               sprintf('  <trk><name>%s</name>', track_name), '    <trkseg>', pts, '    </trkseg>',
               '  </trk>', '</gpx>'), filename)
}

# next_ride/ always holds exactly one file. The full plan is kept in the cache for the app.
save_next_ride <- function(plan, cfg) {
  dir.create(cfg$next_dir, showWarnings = FALSE)
  unlink(list.files(cfg$next_dir, full.names = TRUE, all.files = TRUE, no.. = TRUE), recursive = TRUE)
  f <- file.path(cfg$next_dir, "next_ride.gpx")
  write_gpx_track(plan$route, f, sprintf("Oslo next ride %.0f km (%.0f%% new)", plan$stats$km,
                                         100 * plan$stats$new_km / plan$stats$km))
  saveRDS(plan, file.path(cfg$cache_dir, "last_plan.rds"))
  invisible(f)
}

load_last_plan <- function(cfg) {
  f <- file.path(cfg$cache_dir, "last_plan.rds")
  if (file.exists(f)) read_cache(f) else NULL
}
