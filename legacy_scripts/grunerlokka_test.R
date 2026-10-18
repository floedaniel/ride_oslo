# ============================================================
# Ride Oslo - Chinese/Rural Postman route for one bydel
# - Reads a local OSM extract (no live Overpass dependency)
# - Keeps only bikeable public streets
# - Solves the Rural Postman Problem: every street INSIDE the district must be
#   ridden, streets in a surrounding buffer may be used as transit
# - Exports GPX (with elevation), chunked into ride-sized segments
# - ggplots for inspection at each stage, plus an interactive leaflet map
# ============================================================

# Ensure the UTF-8 accented characters in this file (Grünerløkka) are read
# and rendered correctly regardless of the OS/session default encoding.
options(encoding = "UTF-8")

required_pkgs <- c("osmdata", "sf", "sfnetworks", "igraph", "nbpMatching",
                   "dplyr", "ggplot2", "units", "leaflet", "elevatr", "terra")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ------------------------------------------------------------
# 0. Configuration - the only block to touch when doing the next district
# ------------------------------------------------------------
district_name <- "Grunerlokka, Oslo, Norway"
district_slug <- "grunerlokka"
start_lonlat  <- c(lon = 10.780919, lat = 59.932804)  # home; route starts and ends here

buffer_m      <- 300   # streets this far outside the district may be used as transit
snap_m        <- 2     # dead-end paths ending this close to another street are joined to it
min_spur_m    <- 30    # dead-end stubs up to this long are optional, not ridden out and back
strava_dir       <- "strava"  # one lat,lon CSV per ride; streets already ridden stop being required
ridden_buffer_m  <- 12        # a track this close to a street counts as riding it
ridden_min_cover <- 0.8       # share of a street's length that must be covered to count as ridden
track_gap_m      <- 150       # a jump this long between GPS points splits a track (dropout)
max_bridge_m  <- 60    # longest straight-line dismount connector we'll invent
n_chunks      <- 5     # ride-sized pieces to split the finished route into
dem_zoom      <- 13    # elevatr tile zoom (~19 m/px at this latitude)

osm_pbf     <- "osm_data/Oslo.osm.pbf"
osm_pbf_url <- "https://download.bbbike.org/osm/bbbike/Oslo/Oslo.osm.pbf"

# ------------------------------------------------------------
# 1. District boundary + local OSM extract, filtered to bikeable public streets
# ------------------------------------------------------------
boundary_sf <- getbb(district_name, format_out = "sf_polygon")
if (inherits(boundary_sf, "list")) boundary_sf <- boundary_sf[[1]]  # first match if ambiguous
boundary_sf <- st_make_valid(boundary_sf)

# Project to a planar CRS (UTM 32N) before any geometric ops below. sf defaults to the s2
# spherical engine for lon/lat data, which crashes (native heap corruption) on Windows when
# fed messy OSM line topology (self-touching ways, near-duplicate vertices). Planar ops via
# GEOS are robust to this, and give correct metric distances for route weights besides.
district_sf <- st_transform(boundary_sf, 25832)
study_sf    <- st_buffer(district_sf, buffer_m)

# A local extract instead of the live Overpass API. The public mirrors are shared instances
# that returned 504s under load and needed retry/backoff, and this project re-reads the data
# constantly while tuning filters and works through 15 districts. The whole of Oslo is a 44 MB
# download that reads in a few seconds, offline and reproducibly. Downloaded once, then cached.
if (!file.exists(osm_pbf)) {
  dir.create(dirname(osm_pbf), showWarnings = FALSE, recursive = TRUE)
  message("Downloading OSM extract (~44 MB) to ", osm_pbf, " ...")
  download.file(osm_pbf_url, osm_pbf, mode = "wb", quiet = FALSE)
}

# GDAL's OSM driver promotes only a handful of tags to columns; everything else lands in a
# single hstore-style other_tags string, so the tags we filter on have to be pulled back out.
tag_value <- function(tags, key) {
  out <- rep(NA_character_, length(tags))
  ok <- !is.na(tags)                       # ways carrying no extra tags at all
  if (!any(ok)) return(out)
  pat <- paste0('"', key, '"=>"')
  r <- regexpr(paste0(pat, '[^"]*"'), tags[ok])
  hit <- rep(NA_character_, sum(ok))
  hit[r > 0] <- sub('"$', "", sub(pat, "", regmatches(tags[ok], r), fixed = TRUE))
  out[ok] <- hit
  out
}

study_wkt <- st_as_text(st_geometry(st_transform(study_sf, 4326)))
raw_lines <- st_read(osm_pbf, quiet = TRUE, wkt_filter = study_wkt,
                     query = "SELECT osm_id, highway, other_tags FROM lines WHERE highway IS NOT NULL")
cat("Read", nrow(raw_lines), "highway ways from local extract.\n")

# None of these exist in Grünerløkka, but they do elsewhere in Oslo (35 km of "proposed"
# alone), so they have to go before this runs on another district.
exclude_classes <- c("motorway", "motorway_link",
                     "trunk", "trunk_link",
                     "primary", "primary_link",
                     "construction", "proposed",     # not open / not built (OSM wiki guidance)
                     "raceway", "bus_guideway", "busway", "escape",
                     "platform", "elevator", "services", "rest_area", "street_lamp")

# highway=footway/path/pedestrian/corridor are sidewalks and foot paths, not streets - by
# far the largest class in a raw "highway" pull (footway alone was 60% of all ways here),
# and the vast majority carry no bicycle tag at all (no indication cycling is even allowed).
# Left in, they run parallel to the real adjacent street and get treated as separately
# rideable, which both misroutes onto sidewalks and produces near-duplicate parallel edges
# next to the road. Only include them where cycling is explicitly signed.
# bridleway gets the same treatment: a horse path is only ours where cycling is signed.
pedestrian_classes <- c("footway", "path", "pedestrian", "corridor", "bridleway")

# access=private/no/customers/permit means the general public isn't allowed on that way at
# all (a gated courtyard, a restricted lot) - routing a ride through it is both wrong and
# often physically impossible. service=driveway/parking_aisle are private building access and
# parking-lot circulation, not through-streets; left in, they're the source of a lot of the
# short dead-end spurs poking into courtyards/lots, each forcing a needless out-and-back
# detour just to satisfy "every edge must be ridden."
private_access     <- c("private", "no", "customers", "permit")
non_street_service <- c("driveway", "parking_aisle")

# Gravel roads are in: highway=track is a road (forestry/farm/gravel lane), unlike footway/path,
# and this is a blacklist filter so tracks pass without being named. The exception is a track
# that is really a trail - tracktype=grade5 (soft/overgrown, no firm surface) or an
# mtb:scale of 2 or more (technical singletrack) - which is smaller than a road and excluded.
edges_sf <- raw_lines |>
  mutate(bicycle   = tag_value(other_tags, "bicycle"),
         access    = tag_value(other_tags, "access"),
         service   = tag_value(other_tags, "service"),
         tracktype = tag_value(other_tags, "tracktype"),
         mtb_scale = tag_value(other_tags, "mtb:scale")) |>
  filter(
    !highway %in% exclude_classes,
    highway != "steps",
    is.na(bicycle) | !bicycle %in% c("no", "dismount"),
    !highway %in% pedestrian_classes | bicycle %in% c("yes", "designated"),
    is.na(access) | !access %in% private_access,
    highway != "service" | is.na(service) | !service %in% non_street_service,
    !(highway == "track" & (tracktype %in% "grade5" | grepl("^[2-9]", mtb_scale)))
  ) |>
  st_transform(25832) |>
  st_intersection(st_geometry(study_sf)) |>   # clip to district + transit buffer
  st_collection_extract("LINESTRING") |>
  st_cast("MULTILINESTRING") |>  # normalize type before exploding
  st_cast("LINESTRING")          # split any MULTILINESTRING rows sfnetworks can't accept

district_km <- as.numeric(sum(st_length(st_intersection(st_geometry(edges_sf),
                                                        st_geometry(district_sf))))) / 1000
cat("Bikeable street inside the district:", round(district_km, 2), "km",
    "(plus a", buffer_m, "m transit buffer)\n")

# --- PLOT 1: filtered street network ---
p1 <- ggplot() +
  geom_sf(data = edges_sf, color = "grey75", linewidth = 0.3) +
  geom_sf(data = district_sf, fill = NA, color = "black", linewidth = 0.5, linetype = "dashed") +
  labs(title = paste0(district_slug, " bikeable street network"),
       subtitle = paste0(round(district_km, 1), " km inside the district (dashed), grey includes buffer")) +
  theme_minimal()
p1

# ------------------------------------------------------------
# 2. Build the weighted graph; mark which edges MUST be ridden
# ------------------------------------------------------------
# as_sfnetwork() only creates nodes at line ENDPOINTS, not at every place two lines
# geometrically cross - OSM ways routinely cross mid-block without sharing a node
# (especially after boundary clipping), so without explicit noding the graph fragments
# into ~2000 disconnected pieces and most of the study area silently gets dropped below.
# st_union() + st_cast("LINESTRING") fully nodes the geometry at every real 2D crossing.
edges_noded <- edges_sf |>
  st_geometry() |>
  st_union() |>
  st_cast("LINESTRING")

# Plain noding only joins lines that actually touch or cross. OSM is full of paths that stop a
# metre or two short of the street they belong to (a footway ending beside a road, a service
# lane that nearly meets another), so each one shows up as a dead end: an odd vertex that forces
# an out-and-back. After noding, any endpoint still touching only one line is a dead end; if
# another street lies within snap_m, extend the dead-end line to the nearest point on it and
# node again, which turns the near-miss into a real junction. Only dead-end endpoints move.
snap_dead_ends <- function(lines, tol) {
  n <- length(lines)
  xy <- lapply(lines, function(l) st_coordinates(l)[, 1:2, drop = FALSE])
  key <- function(p) paste(round(p[1], 2), round(p[2], 2))
  ends_key <- unlist(lapply(xy, function(m) c(key(m[1, ]), key(m[nrow(m), ]))))
  cnt <- table(ends_key)

  snapped <- 0L
  for (i in seq_len(n)) {
    m <- xy[[i]]
    for (side in c("first", "last")) {
      p <- if (side == "first") m[1, ] else m[nrow(m), ]
      if (cnt[[key(p)]] != 1L) next                      # already shares a node with another line
      pt <- st_sfc(st_point(p), crs = st_crs(lines))
      cand <- setdiff(st_is_within_distance(pt, lines, tol)[[1]], i)
      if (length(cand) == 0L) next
      d <- as.numeric(st_distance(pt, lines[cand]))
      j <- cand[which.min(d)]
      q <- st_coordinates(st_nearest_points(pt, lines[j]))[2, 1:2]
      m <- if (side == "first") rbind(q, m) else rbind(m, q)
      snapped <- snapped + 1L
    }
    xy[[i]] <- m
  }
  message(snapped, " dead-end endpoint(s) snapped onto a street within ", tol, " m.")
  st_sfc(lapply(xy, st_linestring), crs = st_crs(lines))
}

edges_noded_sf <- snap_dead_ends(edges_noded, snap_m) |>
  st_union() |>
  st_cast("LINESTRING") |>
  st_sf(geometry = _)

net <- as_sfnetwork(edges_noded_sf, directed = FALSE) |>
  activate("edges") |>
  mutate(weight = st_length(geometry))

g <- as.igraph(net)
node_sf <- net |> activate("nodes") |> st_as_sf()
edge_sf <- net |> activate("edges") |> st_as_sf()

edge_geoms <- lapply(st_geometry(edge_sf), function(gm) st_coordinates(gm)[, 1:2, drop = FALSE])

# Tag every edge with the index of its geometry. A plain integer attribute is immune to the
# c() crash described below (unlike the sfc/units ones), survives induced_subgraph(), and can
# be carried onto duplicate copies - so any edge, at any later point, can still be drawn along
# the street it actually represents. Keying by endpoint coordinates instead would silently
# merge the pairs of parallel edges that run between the same two crossings by different
# physical routes.
E(g)$geom_id <- seq_len(ecount(g))

# Noding dissolves per-way attributes, so "is this edge inside the district" is decided
# geometrically afterwards, from each edge's midpoint.
mid <- st_line_sample(st_geometry(edge_sf), sample = 0.5) |> st_cast("POINT")
E(g)$required <- lengths(st_intersects(mid, st_geometry(district_sf))) > 0
cat(sum(E(g)$required), "of", ecount(g), "edges are inside the district (must be ridden);",
    "the rest are transit-only buffer.\n")

# Progress from real rides. Each CSV (lat,lon) under strava_dir is one ride track: full GPS
# streams, or - for rides where only Strava's simplified polyline was available - files in
# strava_dir/poly (a polyline's chords stay within a few metres of the real track on recent
# rides, so it is good enough at this buffer). A street counts as ridden once ridden_min_cover
# of its length lies within ridden_buffer_m of a track. Ridden streets stop being required but
# stay usable as transit, so the route that is left covers only what is still to do - and the
# same step is what a "next 30 km" generator would read. Streaming tracks are broken wherever
# consecutive points are more than track_gap_m apart (GPS dropouts), so a gap is not bridged.
read_track <- function(f, is_poly) {
  d <- read.csv(f)
  d <- d[c(TRUE, diff(d$lat) != 0 | diff(d$lon) != 0), , drop = FALSE]  # drop standing-still repeats
  if (nrow(d) < 2) return(NULL)
  xy <- st_coordinates(st_transform(st_as_sf(d, coords = c("lon", "lat"), crs = 4326), 25832))
  grp <- if (is_poly) rep(1L, nrow(xy)) else
    c(1L, 1L + cumsum(sqrt(rowSums(diff(xy)^2)) > track_gap_m))
  parts <- split(seq_len(nrow(xy)), grp)
  parts <- parts[lengths(parts) >= 2]
  if (length(parts) == 0) return(NULL)
  st_multilinestring(lapply(parts, function(ix) xy[ix, , drop = FALSE]))
}

ridden <- rep(FALSE, ecount(g))
track_files <- list.files(strava_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
track_files <- track_files[basename(track_files) != "index.csv"]
if (length(track_files) > 0) {
  track_id   <- sub("\\.csv$", "", basename(track_files))
  track_poly <- basename(dirname(track_files)) == "poly"
  drop <- track_poly & track_id %in% track_id[!track_poly]    # full stream beats its own polyline
  track_files <- track_files[!drop]; track_poly <- track_poly[!drop]
  trk <- Filter(Negate(is.null), Map(read_track, track_files, track_poly))
  trk_buf <- st_union(st_buffer(st_sfc(trk, crs = 25832), ridden_buffer_m))

  eg   <- st_geometry(edge_sf)
  hit  <- which(lengths(st_intersects(eg, trk_buf)) > 0)
  part <- st_intersection(st_sf(id = hit, geometry = eg[hit]), st_sf(geometry = trk_buf))
  inside_len <- tapply(as.numeric(st_length(part)), part$id, sum)
  cover <- rep(0, length(eg))
  cover[as.integer(names(inside_len))] <- inside_len / as.numeric(st_length(eg))[as.integer(names(inside_len))]
  ridden <- cover >= ridden_min_cover
}
ridden_req <- E(g)$required & ridden
req_km_all <- sum(as.numeric(E(g)$weight)[E(g)$required]) / 1000
cat(length(track_files), "ride track(s) read;", sum(ridden_req), "required edges already ridden (",
    round(sum(as.numeric(E(g)$weight)[ridden_req]) / 1000, 2), "of", round(req_km_all, 2),
    "km =", round(100 * sum(as.numeric(E(g)$weight)[ridden_req]) / 1000 / req_km_all, 1), "%).\n")
ridden_sf <- edge_sf[ridden_req, ]
E(g)$required[ridden_req] <- FALSE

# --- PLOT 4: progress - ridden vs still to ride, inside the district ---
in_district <- E(g)$required | ridden_req
p4 <- ggplot() +
  geom_sf(data = edge_sf[!in_district, ], color = "grey88", linewidth = 0.25) +
  geom_sf(data = edge_sf[E(g)$required, ], color = "#c62828", linewidth = 0.4) +
  geom_sf(data = ridden_sf, color = "#2e7d32", linewidth = 0.6) +
  geom_sf(data = district_sf, fill = NA, color = "black", linewidth = 0.5, linetype = "dashed") +
  labs(title = paste0(district_slug, " progress: ridden (green) vs still to ride (red)"),
       subtitle = paste0(length(track_files), " rides; ",
                         round(100 * sum(as.numeric(E(g)$weight)[ridden_req]) / 1000 / req_km_all, 1),
                         "% of the district's street ridden")) +
  theme_minimal()
ggsave(paste0("progress_", district_slug, ".png"), p4, width = 10, height = 10, dpi = 110)

# A short dead-end stub (a courtyard mouth, a few metres of lane) costs a full out-and-back to
# ride and is rarely what anyone means by "a street". Required edges that dead-end in the
# required network and are no longer than min_spur_m become optional: still usable as transit
# if a detour happens to pass that way, but no longer forced. One pass only, so a chain of
# short stubs doesn't get eaten from the end. Set min_spur_m <- 0 to ride every stub.
req_deg <- degree(subgraph_from_edges(g, which(E(g)$required), delete.vertices = FALSE))
el_all  <- ends(g, E(g), names = FALSE)
spur    <- E(g)$required & as.numeric(E(g)$weight) <= min_spur_m &
           (req_deg[el_all[, 1]] == 1 | req_deg[el_all[, 2]] == 1)
cat(sum(spur), "dead-end stub(s) of <=", min_spur_m, "m made optional (",
    round(sum(as.numeric(E(g)$weight)[spur]) / 1000, 2), "km not forced).\n")
E(g)$required[spur] <- FALSE

# Draw one traversed edge: its real geometry, flipped if we ride it against the direction the
# linestring is stored in, or a straight line for synthetic connectors (geom_id NA), which are
# meant to be straight. nc is a plain coordinate matrix - this runs once per traversed edge
# (thousands of times) and re-running st_coordinates() inside would dominate the runtime.
edge_draw_coords <- function(nc, gid, v_from, v_to) {
  if (is.na(gid)) return(rbind(nc[v_from, ], nc[v_to, ]))
  cm <- edge_geoms[[gid]]
  if (max(abs(cm[1, ] - nc[v_from, ])) < 0.01) cm else cm[nrow(cm):1, , drop = FALSE]
}

# g also carries an sfc geometry column and a units-classed weight, inherited from
# as.igraph(net). add_edges() below (for bridging, for connecting required components, and for
# the matching in step 3) must NA-pad every existing edge attribute for each newly added edge
# via a generic c(), and the strict S3 c() methods for "units" and "sfc" both reject combining
# with a plain NA (c.units: "no applicable method for 'units'"; c.sfc: "arguments have
# different crs"). Keep only the plain-typed attributes we actually need.
w <- as.numeric(E(g)$weight); gid <- E(g)$geom_id; req <- E(g)$required
for (a in edge_attr_names(g)) g <- delete_edge_attr(g, a)
E(g)$weight <- w; E(g)$geom_id <- gid; E(g)$required <- req

# Keep the largest connected piece of the study-area network, then invent short straight-line
# dismount connectors for any required fragment still cut off. Excluding sidewalks (step 1)
# means the bikeable network alone doesn't always stay in one piece - a cul-de-sac, courtyard
# or roundabout slip lane can be linked to the rest of the grid only via a short pedestrian
# stretch. A fragment further than max_bridge_m from anything else is left out as a genuine
# island rather than bridged with an unrealistically long "walk".
comp <- components(g)
coords <- st_coordinates(node_sf)
bridge_from <- integer(0); bridge_to <- integer(0); bridge_dist <- numeric(0)
# Only worth bridging a fragment that actually contains street we have to ride; a stranded
# scrap of buffer is never needed.
frag_has_required <- tapply(E(g)$required, comp$membership[ends(g, E(g))[, 1]], any)
if (comp$no > 1) {
  giant_id <- which.max(comp$csize)
  connected_ids <- which(comp$membership == giant_id)
  n_unreachable <- 0
  for (cid in setdiff(seq_len(comp$no), giant_id)) {
    if (!isTRUE(frag_has_required[as.character(cid)])) next
    frag_nodes <- which(comp$membership == cid)
    d <- sqrt(outer(coords[frag_nodes, 1], coords[connected_ids, 1], "-")^2 +
              outer(coords[frag_nodes, 2], coords[connected_ids, 2], "-")^2)
    if (min(d) > max_bridge_m) { n_unreachable <- n_unreachable + 1; next }
    idx <- which(d == min(d), arr.ind = TRUE)[1, ]
    bridge_from <- c(bridge_from, frag_nodes[idx[1]])
    bridge_to   <- c(bridge_to,   connected_ids[idx[2]])
    bridge_dist <- c(bridge_dist, min(d))
    connected_ids <- c(connected_ids, frag_nodes)
  }
  message(length(bridge_from), " fragment(s) bridged with a direct dismount connector (<= ",
          max_bridge_m, "m); ", n_unreachable, " left as unreachable island(s).")
  for (i in seq_along(bridge_from)) {
    # Transit, not required: a connector is a way to REACH stranded street, never a thing to
    # ride for its own sake. The linking step below will use it only where it's the cheapest
    # way in.
    g <- add_edges(g, c(bridge_from[i], bridge_to[i]),
                   attr = list(weight = bridge_dist[i], geom_id = NA_integer_, required = FALSE))
  }
}

comp <- components(g)
keep <- which(comp$membership == which.max(comp$csize))
if (comp$no > 1) message("Keeping largest component (", length(keep), " of ", vcount(g), " nodes).")

kept_bridges <- which(bridge_from %in% keep & bridge_to %in% keep)
bridge_edge_coords <- lapply(kept_bridges, function(i) {
  st_sfc(st_linestring(rbind(coords[bridge_from[i], ], coords[bridge_to[i], ])), crs = st_crs(node_sf))
})

g <- induced_subgraph(g, keep)
node_sf <- node_sf[keep, ]
node_coords <- st_coordinates(node_sf)  # reused per-edge below; recomputing it is far too slow

# ------------------------------------------------------------
# 3. Rural Postman: connect the required streets, then fix parity
# ------------------------------------------------------------
# Clipping hard at the district boundary makes every street that crosses it dead-end at the
# cut, forcing an out-and-back on each one. Instead only the streets INSIDE the district have
# to be ridden; the buffer around it may be used to ride through and come back another way.
# That is a Rural Postman Problem, and the standard construction is: make the required edges
# connected (via shortest paths through the whole network), then even out the degrees of that
# subgraph by matching its odd vertices along shortest paths - again through the whole network,
# so a detour may leave the district and return.
must <- which(E(g)$required)
g_req <- subgraph_from_edges(g, must, delete.vertices = FALSE)
creq <- components(g_req)
# Components with no required edge at all are just buffer vertices, not real fragments.
req_touch <- unique(as.vector(ends(g, E(g)[must])))
live <- unique(creq$membership[req_touch])

link_edges <- integer(0)
if (length(live) > 1) {
  sizes <- table(creq$membership[req_touch])
  hub <- as.integer(names(sizes)[which.max(sizes)])
  others <- setdiff(live, hub)
  hub_nodes <- intersect(which(creq$membership == hub), req_touch)
  for (cid in others) {
    fn <- intersect(which(creq$membership == cid), req_touch)
    d <- distances(g, v = fn, to = hub_nodes, weights = E(g)$weight)
    if (!any(is.finite(d))) next
    idx <- which(d == min(d), arr.ind = TRUE)[1, ]
    sp <- shortest_paths(g, fn[idx[1]], hub_nodes[idx[2]],
                         weights = E(g)$weight, output = "epath")$epath[[1]]
    link_edges <- c(link_edges, as.integer(sp))
  }
  link_edges <- setdiff(unique(link_edges), must)
  message(length(others), " required fragment(s) linked through the network using ",
          round(sum(E(g)$weight[link_edges]) / 1000, 2), " km of transit street.")
}

ride <- sort(union(must, link_edges))   # the edge set the ride must cover
g_ride <- subgraph_from_edges(g, ride, delete.vertices = FALSE)
deg <- degree(g_ride)
odd_v <- V(g)[deg %% 2 == 1]

# --- PLOT 2: node degree parity of the must-ride subgraph ---
deg_df <- node_sf |> mutate(degree = deg, odd = deg %% 2 == 1) |> filter(degree > 0)
p2 <- ggplot() +
  geom_sf(data = edges_sf, color = "grey85", linewidth = 0.25) +
  geom_sf(data = deg_df, aes(color = odd, size = odd)) +
  scale_color_manual(values = c(`FALSE` = "steelblue", `TRUE` = "red"),
                     labels = c("even degree", "odd degree"), name = NULL) +
  scale_size_manual(values = c(`FALSE` = 0.4, `TRUE` = 1.2), guide = "none") +
  labs(title = "Intersections by degree parity",
       subtitle = paste0(length(odd_v), " odd-degree vertices need matching")) +
  theme_minimal()
p2

g_aug <- g_ride
dup_edge_coords <- list()

if (length(odd_v) > 0) {
  # Shortest paths through the FULL graph, so a parity-fixing detour is free to use the
  # transit buffer rather than being forced back along a required street.
  D  <- distances(g, v = odd_v, to = odd_v, weights = E(g)$weight)
  m  <- nonbimatch(distancematrix(D))
  pairs <- m$halves[m$halves$Group1.Row != m$halves$Group2.Row, ]

  for (i in seq_len(nrow(pairs))) {
    v1 <- odd_v[pairs$Group1.Row[i]]
    v2 <- odd_v[pairs$Group2.Row[i]]
    sp <- shortest_paths(g, v1, v2, weights = E(g)$weight, output = "epath")$epath[[1]]
    ends_mat <- ends(g, sp)
    # add_edges() flattens a matrix COLUMN-major, so passing ends_mat directly pairs up
    # vertices from different rows: it silently creates edges between vertices that aren't
    # adjacent at all (spans of hundreds of metres, drawn as straight lines through
    # buildings) plus self-loops, instead of duplicating the shortest path. t() gives the
    # interleaved from,to,from,to vector it actually expects.
    g_aug <- add_edges(g_aug, as.vector(t(ends_mat)),
                       attr = list(weight = E(g)$weight[sp], geom_id = E(g)$geom_id[sp],
                                   required = E(g)$required[sp]))
    for (j in seq_along(sp)) {
      dup_edge_coords[[length(dup_edge_coords) + 1]] <- st_sfc(
        st_linestring(edge_draw_coords(node_coords, E(g)$geom_id[sp[j]],
                                       ends_mat[j, 1], ends_mat[j, 2])),
        crs = st_crs(node_sf))
    }
  }
}

# Buffer vertices that ended up on no ridden edge are simply never visited. Leave them in
# place rather than dropping them, so vertex ids stay aligned with node_sf/node_coords.
active <- which(degree(g_aug) > 0)
stopifnot(all(degree(g_aug) %% 2 == 0),
          components(induced_subgraph(g_aug, active))$no == 1)

route_km <- sum(E(g_aug)$weight) / 1000
# Overhead is measured against the street still left to ride, not the whole district: ridden
# streets and skipped stubs are no longer required, so the district total would understate it.
req_km_left <- sum(E(g)$weight[must]) / 1000
cat("\nRoute length:", round(route_km, 2), "km for", round(req_km_left, 2),
    "km of street still to ride (district total", round(district_km, 2), "km; +",
    round((route_km / req_km_left - 1) * 100, 1), "% )\n")

# --- PLOT 3: segments ridden twice ---
if (length(dup_edge_coords) > 0) {
  dup_sf <- st_sf(geometry = do.call(c, dup_edge_coords))
  p3 <- ggplot() +
    geom_sf(data = edges_sf, color = "grey85", linewidth = 0.25) +
    geom_sf(data = dup_sf, color = "darkorange", linewidth = 0.8) +
    labs(title = "Segments ridden twice",
         subtitle = paste0(nrow(dup_sf), " segment(s) repeated to make the route Eulerian")) +
    theme_minimal()
  print(p3)
}

# ------------------------------------------------------------
# 4. Eulerian circuit = the ride
# ------------------------------------------------------------
start_pt <- st_sfc(st_point(start_lonlat), crs = 4326) |> st_transform(st_crs(node_sf))
start_node <- active[st_nearest_feature(start_pt, node_sf[active, ])]  # must be a vertex on the route
cat("Start point snapped to nearest node on the route,",
    round(as.numeric(st_distance(start_pt, node_sf[start_node, ])), 1), "m away.\n")

# Every vertex in g_aug has even degree (that's what step 3's matching buys), so an Eulerian
# CIRCUIT exists and every valid circuit rides exactly the same edges for exactly the same
# total distance - only the ORDER differs. igraph's eulerian_path() picks that order from
# internal edge numbering, which has nothing to do with geography, so it turns at random and
# the ride keeps doubling back on itself for no reason.
#
# Hierholzer's algorithm works with any choice of next edge, so choose the one continuing
# closest to straight ahead. Same edges, same length, but the route flows down a street
# instead of zig-zagging, and dead ends resolve naturally (the only way out is back).
# Starting the walk at start_node also makes the circuit begin and end there directly.
euler_smart <- function(g, nc, start_v) {
  ne <- ecount(g)
  el <- ends(g, E(g), names = FALSE)
  f <- factor(c(el[, 1], el[, 2]), levels = seq_len(vcount(g)))
  inc_e <- split(rep(seq_len(ne), 2), f)   # edge ids incident to each vertex
  inc_v <- split(c(el[, 2], el[, 1]), f)   # the opposite endpoint of each

  used <- logical(ne)
  sv <- integer(ne + 2L); se <- integer(ne + 2L); sp <- 1L; sv[1] <- start_v; se[1] <- 0L
  out_v <- integer(ne + 2L); out_e <- integer(ne + 2L); op <- 0L

  while (sp > 0L) {
    v <- sv[sp]
    ie <- inc_e[[v]]
    avail <- which(!used[ie])
    if (length(avail) == 0L) {
      op <- op + 1L; out_v[op] <- v; out_e[op] <- se[sp]; sp <- sp - 1L
      next
    }
    pick <- avail[1]
    if (length(avail) > 1L && sp > 1L) {
      d0 <- nc[v, ] - nc[sv[sp - 1L], ]      # direction we arrived from
      n0 <- sqrt(sum(d0^2))
      if (n0 > 0) {
        nxts <- inc_v[[v]][avail]
        dx <- nc[nxts, 1] - nc[v, 1]; dy <- nc[nxts, 2] - nc[v, 2]
        nn <- sqrt(dx * dx + dy * dy); nn[nn == 0] <- 1
        pick <- avail[which.max((dx * d0[1] + dy * d0[2]) / (nn * n0))]  # max cos = straightest
      }
    }
    used[ie[pick]] <- TRUE
    sp <- sp + 1L; sv[sp] <- inc_v[[v]][pick]; se[sp] <- ie[pick]
  }
  # out_e[i] is the edge used to ARRIVE at out_v[i], so after reversing, the edge ridden
  # between vertex i and i+1 is element i+1 - which edge matters, not just which endpoints,
  # because parallel edges between the same two crossings are different physical streets.
  list(vpath = rev(out_v[seq_len(op)]), epath = rev(out_e[seq_len(op)])[-1])
}

eul <- euler_smart(g_aug, node_coords, start_node)
vpath <- eul$vpath; epath <- eul$epath
stopifnot(length(vpath) == ecount(g_aug) + 1, length(epath) == ecount(g_aug),
          vpath[1] == start_node, vpath[length(vpath)] == start_node,
          !anyDuplicated(epath))

# Assemble the route from each ridden edge's REAL geometry rather than a straight chord
# between its two end nodes: a node only exists at a crossing, so a street that curves for a
# stretch without one is a single edge, and the chord across it cuts through whatever is
# inside the bend.
route_gid <- E(g_aug)$geom_id[epath]
route_segs <- vector("list", length(epath))
for (i in seq_along(epath)) {
  seg <- edge_draw_coords(node_coords, route_gid[i], vpath[i], vpath[i + 1])
  route_segs[[i]] <- if (i > 1) seg[-1, , drop = FALSE] else seg
}
route_coords <- do.call(rbind, route_segs)  # UTM metres - used for accurate metric distance

route_pts <- st_as_sf(as.data.frame(route_coords), coords = c("X", "Y"), crs = st_crs(node_sf))
route_wgs <- st_transform(route_pts, 4326)
route_coords_wgs84 <- st_coordinates(route_wgs)  # GPX requires WGS84, not UTM

seg_dist <- c(0, sqrt(diff(route_coords[, 1])^2 + diff(route_coords[, 2])^2))
cum_dist <- cumsum(seg_dist)

# ------------------------------------------------------------
# 5. Elevation, then split into ride-sized chunks
# ------------------------------------------------------------
# Flat here, but the outer Oslo districts this project moves on to are not - climb per chunk
# is what makes a 37 km piece plannable. Non-fatal: the route is still valid without it.
route_ele <- tryCatch({
  dem <- elevatr::get_elev_raster(locations = route_wgs, z = dem_zoom, clip = "bbox", src = "aws")
  as.numeric(terra::extract(terra::rast(dem), terra::vect(route_wgs))[, 2])
}, error = function(e) {
  message("Elevation lookup failed (", conditionMessage(e), ") - continuing without it.")
  rep(NA_real_, nrow(route_coords))
})

# Raw DEM samples are noisy enough to invent tens of metres of phantom climb over a long
# route, so smooth before summing the positive steps. Still only an estimate.
ascent_m <- function(e) {
  e <- e[!is.na(e)]
  if (length(e) < 5) return(NA_real_)
  sm <- as.numeric(stats::filter(e, rep(1 / 9, 9), sides = 2))
  sm[is.na(sm)] <- e[is.na(sm)]
  d <- diff(sm)
  sum(d[d > 0])
}

chunk_km <- (max(cum_dist) / 1000) / n_chunks   # even split of the total route
chunk_id <- pmin(floor(cum_dist / (chunk_km * 1000)) + 1, n_chunks)

route_df <- data.frame(
  lon = route_coords_wgs84[, 1],
  lat = route_coords_wgs84[, 2],
  ele = route_ele,
  cum_dist_km = cum_dist / 1000,
  chunk = factor(chunk_id)
)

# Chunk i ends where chunk i+1 begins. Splitting by point leaves the stretch between the last
# point of one chunk and the first of the next in neither, so each chunk (but the last) also
# takes the next chunk's first point. Used for the map, the stats and the GPX files; the full
# route needs no such fix.
route_chunks <- do.call(rbind, lapply(levels(route_df$chunk), function(i) {
  rows <- which(route_df$chunk == i)
  if (i != tail(levels(route_df$chunk), 1)) rows <- c(rows, max(rows) + 1L)
  d <- route_df[rows, ]
  d$chunk <- factor(i, levels = levels(route_df$chunk))
  d
}))

route_sf <- st_as_sf(route_chunks, coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  group_by(chunk) |>
  summarise(do_union = FALSE) |>
  st_cast("LINESTRING")

chunk_stats <- route_chunks |>
  group_by(chunk) |>
  summarise(km = round(max(cum_dist_km) - min(cum_dist_km), 2),
            start_km = round(min(cum_dist_km), 2),
            end_km = round(max(cum_dist_km), 2),
            ascent_m = round(ascent_m(ele)),
            .groups = "drop")

cat("\nRoute statistics by chunk:\n")
print(as.data.frame(chunk_stats), row.names = FALSE)
cat("\nTotal:", nrow(chunk_stats), "chunks,", round(sum(chunk_stats$km), 1), "km,",
    sum(chunk_stats$ascent_m, na.rm = TRUE), "m ascent\n")

# --- MAP: interactive route coloured by chunk, zoomable, on a real basemap ---
route_sf_map <- route_sf |> left_join(chunk_stats, by = "chunk")
chunk_pal <- colorFactor(hcl.colors(nlevels(route_df$chunk), "Dark 3"), domain = levels(route_df$chunk))

# CartoDB.Positron now requires an authenticated account for its tiles - anonymous requests
# get rate-limited/blocked, leaving blank patches. Leaflet's default lineJoin is "round",
# which at a dead-end - where the path reverses ~180 degrees at an interior vertex to ride the
# same street back out - draws a smooth little curve instead of a sharp point; "miter" renders
# that turnaround as a point.
# tile.openstreetmap.org now answers this project with a 418 "Access blocked" tile (their usage
# policy), so the standard OSM layer is not offered. CyclOSM is the default - it shows cycle
# infrastructure, which suits a ride plan - with a few alternatives selectable in the layer box.
route_map <- leaflet(route_sf_map) |>
  addProviderTiles(providers$CyclOSM, group = "CyclOSM") |>
  addProviderTiles(providers$OpenStreetMap.DE, group = "OSM (DE)") |>
  addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri streets") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addPolylines(
    color = ~chunk_pal(chunk), weight = 4, opacity = 0.9,
    options = pathOptions(lineJoin = "miter", lineCap = "butt"),
    label = ~paste0("Chunk ", chunk, ": ", km, " km"),
    popup = ~paste0("<b>Chunk ", chunk, "</b><br>", km, " km, ", ascent_m, " m up<br>",
                    start_km, "-", end_km, " km along route"),
    highlightOptions = highlightOptions(weight = 7, color = "#000000", bringToFront = TRUE),
    group = "Route"
  ) |>
  addLegend("bottomright", pal = chunk_pal, values = levels(route_df$chunk), title = "Chunk")

# Segments ridden TWICE, as their own toggleable layer, so a spot that looks like several
# overlapping strokes can be checked: genuine repeat ride, or just distinct streets meeting.
if (length(dup_edge_coords) > 0) {
  dup_sf_map <- st_sf(geometry = do.call(c, dup_edge_coords)) |> st_transform(4326)
  route_map <- route_map |>
    addPolylines(data = dup_sf_map, color = "black", weight = 3, opacity = 0.9, dashArray = "4,4",
                 label = "Ridden twice (postman duplication)", group = "Ridden twice")
}

# Dismount connectors: straight-line "walk here" gaps, not real paths - marking them
# distinctly keeps the route honest about where you'll get off the bike.
if (length(bridge_edge_coords) > 0) {
  bridge_sf_map <- st_sf(geometry = do.call(c, bridge_edge_coords)) |> st_transform(4326)
  route_map <- route_map |>
    addPolylines(data = bridge_sf_map, color = "red", weight = 3, opacity = 0.9, dashArray = "2,6",
                 label = "Connector (dismount - not a mapped path)", group = "Connector (dismount)")
}

# Streets already covered by past rides (drawn in green), hidden by default - the route above
# is what is left. Toggle it to check the matching: a wrongly "ridden" street shows up here.
if (nrow(ridden_sf) > 0) {
  route_map <- route_map |>
    addPolylines(data = st_transform(ridden_sf, 4326), color = "#2e7d32", weight = 4, opacity = 0.8,
                 label = "Already ridden (from Strava tracks)", group = "Already ridden") |>
    hideGroup("Already ridden")
}

route_map <- route_map |>
  addLayersControl(baseGroups = c("CyclOSM", "OSM (DE)", "Esri streets", "Satellite"),
                   overlayGroups = c("Route", "Already ridden", "Ridden twice", "Connector (dismount)"),
                   options = layersControlOptions(collapsed = FALSE))

map_file <- paste0("route_map_", district_slug, ".html")
htmlwidgets::saveWidget(route_map, normalizePath(map_file, mustWork = FALSE), selfcontained = TRUE)
cat("Interactive route map saved to", map_file, "\n")
route_map

# ------------------------------------------------------------
# 6. GPX export
# ------------------------------------------------------------
write_gpx_track <- function(df, filename, track_name) {
  pts <- ifelse(is.na(df$ele),
                sprintf('      <trkpt lat="%f" lon="%f"></trkpt>', df$lat, df$lon),
                sprintf('      <trkpt lat="%f" lon="%f"><ele>%.1f</ele></trkpt>', df$lat, df$lon, df$ele))
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<gpx version="1.1" creator="R" xmlns="http://www.topografix.com/GPX/1/1">',
    sprintf('  <trk><name>%s</name>', track_name),
    '    <trkseg>', pts, '    </trkseg>',
    '  </trk>', '</gpx>'), filename)
}

dir.create("gpx_out", showWarnings = FALSE)
for (i in levels(route_df$chunk)) {
  sub <- route_chunks[route_chunks$chunk == i, ]
  fname <- sprintf("gpx_out/%s_chunk_%02d.gpx", district_slug, as.integer(i))
  write_gpx_track(sub, fname, sprintf("%s chunk %s", district_slug, i))
  cat("Wrote", fname, "-", chunk_stats$km[chunk_stats$chunk == i], "km\n")
}

# The route is a closed loop starting and ending at the same (home) node - step 4 starts the
# Eulerian walk there - so the full, unchunked route is one continuous ride back to the door.
full_fname <- sprintf("gpx_out/%s_full_route.gpx", district_slug)
write_gpx_track(route_df, full_fname, sprintf("%s full route", district_slug))
cat("Wrote", full_fname, "-", round(sum(chunk_stats$km), 2), "km (starts and ends at home)\n")
