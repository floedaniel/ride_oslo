# ------------------------------------------------------------
# One-off extract from the Norway OSM file: the Oslo boundary, its districts (the 15 bydeler +
# Sentrum, and "Marka" for everything in Oslo outside them) and every highway line within 1 km
# of Oslo (the margin lets a route use a street just over the border to get around).
# Run again only to refresh the map data: extract_oslo("path/to/norway-latest.osm.pbf").
# ------------------------------------------------------------

extract_oslo <- function(norway_pbf, out_dir = "data", margin_m = 1000) {
  stopifnot(file.exists(norway_pbf))
  dir.create(out_dir, showWarnings = FALSE)
  utf8 <- function(x) { Encoding(x) <- "UTF-8"; x }
  read_q <- function(sql, wkt = character(0))
    suppressWarnings(st_read(norway_pbf, query = sql, wkt_filter = wkt, quiet = TRUE))

  adm <- read_q("SELECT osm_id, osm_way_id, name, admin_level FROM multipolygons
                 WHERE boundary = 'administrative' AND admin_level IN ('7', '9')")
  adm$name <- utf8(adm$name)
  oslo <- adm[adm$admin_level == "7" & adm$name %in% "Oslo", ]
  if (nrow(oslo) != 1) stop("Expected one Oslo municipality polygon, found ", nrow(oslo))
  oslo <- st_make_valid(st_transform(oslo, 25832))
  boundary <- st_sf(name = "Oslo", geometry = st_geometry(oslo))

  bydel <- st_make_valid(st_transform(adm[adm$admin_level == "9" & !is.na(adm$name), ], 25832))
  inside <- lengths(st_intersects(st_point_on_surface(bydel), boundary)) > 0
  bydel <- st_sf(district = bydel$name[inside], geometry = st_geometry(bydel)[inside])
  marka <- st_difference(st_geometry(boundary), st_union(bydel))
  districts <- rbind(bydel, st_sf(district = "Marka", geometry = marka))
  districts$area_km2 <- round(as.numeric(st_area(districts)) / 1e6, 1)

  area_wkt <- st_as_text(st_geometry(st_transform(st_simplify(st_buffer(boundary, margin_m), dTolerance = 50), 4326)))
  ways <- read_q("SELECT osm_id, name, highway, other_tags FROM lines WHERE highway IS NOT NULL", area_wkt)
  ways$name <- utf8(ways$name); ways$other_tags <- utf8(ways$other_tags)

  st_write(boundary,  file.path(out_dir, "oslo_boundary.gpkg"),  delete_dsn = TRUE, quiet = TRUE)
  st_write(districts, file.path(out_dir, "oslo_districts.gpkg"), delete_dsn = TRUE, quiet = TRUE)
  st_write(ways,      file.path(out_dir, "oslo_ways.gpkg"),      delete_dsn = TRUE, quiet = TRUE)
  message(sprintf("Oslo %.0f km2, %d districts, %d highway lines written to %s/",
                  as.numeric(st_area(boundary)) / 1e6, nrow(districts), nrow(ways), out_dir))
  invisible(list(boundary = boundary, districts = districts, ways = ways))
}
