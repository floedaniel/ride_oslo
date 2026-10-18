# ============================================================
# Explore Oslo - the next 30 km, starting from scratch
# - Nothing is assumed ridden. The "database" is the folder rides/: every GPX or lat,lon CSV in
#   it is a ride you have done FOR THIS PROJECT. It starts empty, so the first route is simply
#   the nearest streets to home. After each ride, drop the recorded track into rides/, re-run,
#   and the next route is planned from the updated history. Nothing else is stored.
# - A street counts as ridden once most of its length lies within a few metres of a track.
# - The route is a closed loop from home of about target_km that maximises the length of
#   street you have NOT ridden, while wasting little on streets already ridden and staying
#   near home. Built per ride as a small Rural Postman problem (see solve_route()).
# - Exports GPX (with elevation), an interactive map and a plan picture.
# ============================================================

options(encoding = "UTF-8")

required_pkgs <- c("sf", "igraph", "nbpMatching", "dplyr", "ggplot2", "leaflet", "elevatr", "terra")
missing_pkgs <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
invisible(lapply(required_pkgs, library, character.only = TRUE))

# ------------------------------------------------------------
# 0. Configuration
# ------------------------------------------------------------
home_lonlat <- c(lon = 10.780919, lat = 59.932804)   # every route starts and ends here

target_km    <- 5     # wanted length of the ride
length_tol   <- 0.08   # a route within +/- this share of target_km is accepted
radius_km    <- 8      # street network kept around home (straight-line radius)
max_reach_km <- 7      # only streets this close to home (along the network) are candidates

# What the planner optimises (per route, in km, so they are directly comparable):
#   score = new_km - w_repeat * repeated_km - w_home * (mean distance from home of the new km)
w_repeat <- 0.5        # cost of riding street you have already ridden
w_home   <- 0.4        # cost of the new street being far from home
w_cluster_home <- 0.5  # while growing a cluster, how strongly nearness to home beats nearness to the seed
rho          <- 1.35   # first guess of ride length / street length (parity + links); refined by solving
detour_bonus <- 0.35   # a link or parity detour through an unridden street is this much cheaper than
                       # the same length of ridden street, so detours pick up new streets on the way
conn_penalty <- 4      # a straight-line dismount connector costs this many times its length in a
                       # link or detour, so the planner only walks across one when it has to

# Ridden detection: a street counts as ridden once ridden_min_cover of its length lies within
# ridden_buffer_m of a recorded track
rides_dir        <- "plan/ridden"   # recorded rides (GPX or lat,lon CSV); everything in here counts as ridden
ridden_buffer_m  <- 12
ridden_min_cover <- 0.8
track_gap_m      <- 150
min_spur_m       <- 0    # dead-end stubs up to this long are optional, not forced. 0 = every street
                         # is required, including courtyard mouths and other tiny stubs
snap_m           <- 2    # dead-end paths ending this close to another street are joined to it
max_bridge_m     <- 60   # longest straight-line dismount connector we'll invent

# Which ways exist for the planner. TRUE: footways, paths, pedestrian streets and every track
# (even soft/technical ones) count as streets to explore; pavements beside roads, steps and
# anything signed bicycle=no/dismount stay out. FALSE: roads and bike-signed paths only.
include_small_ways <- TRUE
path_link_mult     <- 1.25   # a footway/path costs this many times its length when it is only a
                             # link or detour (roads are preferred for getting around)

n_sectors  <- 12       # compass sectors that each propose a candidate route
n_refine   <- 3        # best candidates that get length-tuned
rebuild_network <- FALSE   # TRUE after changing the street filters or radius

osm_pbf   <- "osm_data/Oslo.osm.pbf"
out_dir   <- "plan"
cache_dir <- "cache"
dem_zoom  <- 13

# ------------------------------------------------------------
# 1. Street network around home (built once, then cached)
# ------------------------------------------------------------
home_utm <- st_sfc(st_point(home_lonlat), crs = 4326) |> st_transform(25832)

tag_value <- function(tags, key) {
  out <- rep(NA_character_, length(tags))
  ok <- !is.na(tags)
  if (!any(ok)) return(out)
  pat <- paste0('"', key, '"=>"')
  r <- regexpr(paste0(pat, '[^"]*"'), tags[ok])
  hit <- rep(NA_character_, sum(ok))
  hit[r > 0] <- sub('"$', "", sub(pat, "", regmatches(tags[ok], r), fixed = TRUE))
  out[ok] <- hit
  out
}

# Dead-end near-misses: after noding, an endpoint that still touches only one line is a dead
# end; if another street lies within tol, extend it onto that street (then re-node).
snap_dead_ends <- function(lines, tol) {
  cc <- st_coordinates(lines)
  l1 <- cc[, "L1"]
  fi <- which(!duplicated(l1)); la <- which(!duplicated(l1, fromLast = TRUE))
  key <- function(m) paste(round(m[, 1], 2), round(m[, 2], 2))
  k_all <- c(key(cc[fi, 1:2, drop = FALSE]), key(cc[la, 1:2, drop = FALSE]))
  cnt <- table(k_all)
  n <- length(lines)
  p_all <- rbind(cc[fi, 1:2], cc[la, 1:2])
  line_of <- c(l1[fi], l1[la]); side <- rep(c("first", "last"), each = n)
  dead <- which(cnt[k_all] == 1L)
  if (length(dead) == 0L) return(lines)

  pts  <- st_sfc(lapply(dead, function(i) st_point(p_all[i, ])), crs = st_crs(lines))
  # buffer + intersects uses the spatial index; st_is_within_distance scans every line per point
  # (13 ms each - over an hour for a city-sized network) and gives the same answer.
  cand <- st_intersects(st_buffer(pts, tol), lines)
  idx_of_line <- split(seq_len(nrow(cc)), l1)
  mods <- list(); snapped <- 0L
  for (d in seq_along(dead)) {
    i <- line_of[dead[d]]
    cc_i <- setdiff(cand[[d]], i)
    if (length(cc_i) == 0L) next
    dd <- as.numeric(st_distance(pts[d], lines[cc_i]))
    j  <- cc_i[which.min(dd)]
    q  <- st_coordinates(st_nearest_points(pts[d], lines[j]))[2, 1:2]
    m  <- if (is.null(mods[[as.character(i)]])) cc[idx_of_line[[i]], 1:2, drop = FALSE] else mods[[as.character(i)]]
    mods[[as.character(i)]] <- if (side[dead[d]] == "first") rbind(q, m) else rbind(m, q)
    snapped <- snapped + 1L
  }
  message(snapped, " dead-end endpoint(s) snapped onto a street within ", tol, " m.")
  if (length(mods) > 0L)
    lines[as.integer(names(mods))] <- st_sfc(lapply(mods, st_linestring), crs = st_crs(lines))
  lines
}

build_network <- function() {
  study <- st_buffer(home_utm, radius_km * 1000)
  study_wkt <- st_as_text(st_geometry(st_transform(study, 4326)))
  if (!file.exists(osm_pbf)) stop("OSM extract not found: ", osm_pbf)
  raw <- st_read(osm_pbf, quiet = TRUE, wkt_filter = study_wkt,
                 query = "SELECT osm_id, highway, other_tags FROM lines WHERE highway IS NOT NULL")
  cat("Read", nrow(raw), "highway ways within", radius_km, "km of home.\n")

  # Street rules. Public, rideable ways only: no motorways/trunk/primary, no steps, nothing
  # signed bicycle=no/dismount, nothing private, no driveways or parking aisles. With
  # include_small_ways (the default here, because this planner stays near home and aims to
  # explore everything) footways, paths, pedestrian streets, bridleways and every track are
  # in too - except the pavement running beside a road (footway=sidewalk/crossing), which is
  # the same street again, not a new one. Without it: only paths signed for bikes, and no
  # grade5 / technical tracks.
  exclude_classes <- c("motorway", "motorway_link", "trunk", "trunk_link", "primary", "primary_link",
                       "construction", "proposed", "raceway", "bus_guideway", "busway", "escape",
                       "platform", "elevator", "services", "rest_area", "street_lamp", "corridor")
  pedestrian_classes <- c("footway", "path", "pedestrian", "corridor", "bridleway")
  private_access     <- c("private", "no", "customers", "permit")
  non_street_service <- c("driveway", "parking_aisle")
  pavement_types     <- c("sidewalk", "crossing", "traffic_island", "access_aisle")

  ways <- raw |>
    mutate(bicycle   = tag_value(other_tags, "bicycle"),
           access    = tag_value(other_tags, "access"),
           service   = tag_value(other_tags, "service"),
           tracktype = tag_value(other_tags, "tracktype"),
           mtb_scale = tag_value(other_tags, "mtb:scale"),
           footway   = tag_value(other_tags, "footway"),
           bridge    = tag_value(other_tags, "bridge"),
           tunnel    = tag_value(other_tags, "tunnel"),
           layer     = tag_value(other_tags, "layer")) |>
    filter(
      !highway %in% exclude_classes,
      highway != "steps",
      is.na(bicycle) | !bicycle %in% c("no", "dismount"),
      include_small_ways | !highway %in% pedestrian_classes | bicycle %in% c("yes", "designated"),
      !(include_small_ways & highway %in% pedestrian_classes & footway %in% pavement_types),
      is.na(access) | !access %in% private_access,
      highway != "service" | is.na(service) | !service %in% non_street_service,
      include_small_ways | !(highway == "track" & (tracktype %in% "grade5" | grepl("^[2-9]", mtb_scale)))
    ) |>
    st_transform(25832)
  ways_ls <- suppressWarnings(ways |> st_cast("MULTILINESTRING") |> st_cast("LINESTRING"))
  lines <- st_geometry(ways_ls)
  elevated    <- (!is.na(ways_ls$bridge) & ways_ls$bridge != "no") | (!is.na(ways_ls$tunnel) & ways_ls$tunnel != "no") |
                 (!is.na(ways_ls$layer) & ways_ls$layer != "0")
  is_path_way <- ways_ls$highway %in% pedestrian_classes
  cat(length(lines), "bikeable ways (", sum(is_path_way), "of them footways/paths); noding...\n")

  # OSM ways cross without sharing a node, so node them with a union (planar GEOS, not s2,
  # which crashes on messy OSM line topology on Windows), snap dead-end near-misses, node again.
  noded <- lines |> st_union() |> st_cast("LINESTRING")
  noded <- snap_dead_ends(noded, snap_m) |> st_union() |> st_cast("LINESTRING")
  noded <- noded[as.numeric(st_length(noded)) > 0]

  # Which original ways does each noded edge lie on? (Noding dissolves attributes; the edge
  # midpoint sits exactly on its source way.)
  mid <- st_cast(st_line_sample(noded, sample = 0.5), "POINT")
  mid_buf <- st_buffer(mid, 0.5)
  near <- function(x) if (length(x) == 0) rep(FALSE, length(mid)) else lengths(st_intersects(mid_buf, x)) > 0
  on_elev <- near(lines[elevated])    & !near(lines[!elevated])       # bridge / tunnel / other layer
  on_path <- near(lines[is_path_way]) & !near(lines[!is_path_way])    # footway / path (not a road)

  cc <- st_coordinates(noded)
  l1 <- cc[, "L1"]
  fi <- which(!duplicated(l1)); la <- which(!duplicated(l1, fromLast = TRUE))
  key <- function(m) paste(round(m[, 1], 2), round(m[, 2], 2))
  k1 <- key(cc[fi, 1:2, drop = FALSE]); k2 <- key(cc[la, 1:2, drop = FALSE])
  all_key <- c(k1, k2); all_xy <- rbind(cc[fi, 1:2], cc[la, 1:2])
  keys <- unique(all_key)
  node_xy <- all_xy[match(keys, all_key), , drop = FALSE]
  u <- match(k1, keys); v <- match(k2, keys)
  len <- as.numeric(st_length(noded))
  n_line <- length(noded)

  # Bridges, tunnels and underpasses. Union-noding joins any two lines that cross, including an
  # underpass and the road above it. For an edge on an elevated/underground way, an end node
  # that ground edges also touch and that is not one of the way's real end points is a false
  # crossing: that edge gets its own copy of the node, so you cannot "turn" from the bridge
  # onto the road below.
  ends_of <- function(g) {
    cg <- st_coordinates(g); l <- cg[, "L1"]
    rbind(cg[!duplicated(l), 1:2, drop = FALSE], cg[!duplicated(l, fromLast = TRUE), 1:2, drop = FALSE])
  }
  elev_end_keys <- if (any(elevated)) key(ends_of(lines[elevated])) else character(0)
  touch_ground  <- unique(c(u[!on_elev], v[!on_elev]))
  fu <- on_elev & u %in% touch_ground & !(k1 %in% elev_end_keys)
  fv <- on_elev & v %in% touch_ground & !(k2 %in% elev_end_keys)
  to_split <- unique(c(u[fu], v[fv]))
  if (length(to_split) > 0) {
    new_id <- nrow(node_xy) + seq_along(to_split)
    u[fu] <- new_id[match(u[fu], to_split)]; v[fv] <- new_id[match(v[fv], to_split)]
    node_xy <- rbind(node_xy, node_xy[to_split, , drop = FALSE])
  }
  cat(length(to_split), "false crossings between bridges/tunnels and the streets below removed.\n")

  # Bridge small fragments to the main network with straight dismount connectors; drop the rest.
  g0 <- make_graph(as.vector(rbind(u, v)), n = nrow(node_xy), directed = FALSE)
  cm <- components(g0)
  giant <- which.max(cm$csize)
  in_giant <- cm$membership == giant
  conn_geom <- list(); cu <- integer(0); cv <- integer(0); cl <- numeric(0)
  if (cm$no > 1) {
    far <- which(!in_giant); gi <- which(in_giant)
    pts_far <- st_as_sf(data.frame(x = node_xy[far, 1], y = node_xy[far, 2]), coords = c("x", "y"), crs = 25832)
    pts_gi  <- st_as_sf(data.frame(x = node_xy[gi, 1],  y = node_xy[gi, 2]),  coords = c("x", "y"), crs = 25832)
    nn <- gi[st_nearest_feature(pts_far, pts_gi)]
    d  <- sqrt(rowSums((node_xy[far, , drop = FALSE] - node_xy[nn, , drop = FALSE])^2))
    for (k in split(seq_along(far), cm$membership[far])) {
      b <- k[which.min(d[k])]
      if (d[b] > max_bridge_m) next
      cu <- c(cu, far[b]); cv <- c(cv, nn[b]); cl <- c(cl, d[b])
      conn_geom[[length(conn_geom) + 1]] <- st_linestring(rbind(node_xy[far[b], ], node_xy[nn[b], ]))
    }
    cat(length(cu), "fragment(s) bridged with a dismount connector (<=", max_bridge_m, "m).\n")
  }
  edge_geom <- c(noded, st_sfc(conn_geom, crs = 25832))
  eu_all <- c(u, cu); ev_all <- c(v, cv); elen_all <- c(len, cl)
  conn_all <- c(rep(FALSE, n_line), rep(TRUE, length(cu)))
  path_all <- c(on_path, rep(FALSE, length(cu)))

  G <- make_graph(as.vector(rbind(eu_all, ev_all)), n = nrow(node_xy), directed = FALSE)
  E(G)$eid <- seq_along(eu_all); E(G)$len <- elen_all; E(G)$conn <- conn_all; E(G)$path <- path_all
  cm <- components(G)
  keep <- which(cm$membership == which.max(cm$csize))
  cat("Keeping the largest component:", length(keep), "of", vcount(G), "nodes.\n")
  G <- induced_subgraph(G, keep)          # edge attribute eid survives; vertex ids are renumbered
  node_xy <- node_xy[keep, , drop = FALSE]
  # Streets cut off from the main network by more than max_bridge_m can never be planned;
  # keep them so they can be listed instead of silently disappearing.
  dropped <- noded[setdiff(seq_len(n_line), E(G)$eid)]
  list(G = G, node_xy = node_xy, edge_geom = edge_geom, dropped = dropped)
}

dir.create(cache_dir, showWarnings = FALSE)
cache_file <- file.path(cache_dir, sprintf("plan_network_r%d_%s.rds", radius_km, if (include_small_ways) "all" else "roads"))
if (file.exists(cache_file) && !rebuild_network) {
  net <- readRDS(cache_file)
  cat("Loaded cached network:", vcount(net$G), "nodes,", ecount(net$G), "edges.\n")
} else {
  t0 <- Sys.time()
  net <- build_network()
  saveRDS(net, cache_file)
  cat("Network built in", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "min and cached.\n")
}
G <- net$G; node_xy <- net$node_xy; edge_geom <- net$edge_geom

# Coverage check: every street that passed the filters is either in the network (and so will
# be planned) or listed here as cut off from it.
dir.create(out_dir, showWarnings = FALSE)
dropped_km <- sum(as.numeric(st_length(net$dropped))) / 1000
net_km <- sum(E(G)$len[!E(G)$conn]) / 1000
cat(sprintf("Network: %.1f km of street will be planned. %.1f km (%.1f%%) is cut off from it by gaps over %d m and cannot be reached%s\n",
            net_km, dropped_km, 100 * dropped_km / (net_km + dropped_km), max_bridge_m,
            if (dropped_km > 0) " - listed in next_route/unreachable_streets.geojson." else "."))
if (length(net$dropped) > 0)
  suppressWarnings(st_write(st_sf(length_m = round(as.numeric(st_length(net$dropped))),
                                  geometry = st_transform(net$dropped, 4326)),
                            file.path(out_dir, "unreachable_streets.geojson"), delete_dsn = TRUE, quiet = TRUE))

# Arrays indexed by edge id (eid = position in edge_geom), so they survive subgraphs.
n_eid <- length(edge_geom)
eu <- ev <- rep(1L, n_eid)       # ids dropped with the small fragments stay 1 so indexing keeps its length
elen <- rep(NA_real_, n_eid); econn <- rep(FALSE, n_eid); epath <- rep(FALSE, n_eid)
el <- ends(G, E(G), names = FALSE)
eu[E(G)$eid] <- el[, 1]; ev[E(G)$eid] <- el[, 2]
elen[E(G)$eid] <- E(G)$len; econn[E(G)$eid] <- E(G)$conn; epath[E(G)$eid] <- E(G)$path
in_net <- !is.na(elen)

home_node <- st_nearest_feature(home_utm, st_as_sf(data.frame(x = node_xy[, 1], y = node_xy[, 2]),
                                                    coords = c("x", "y"), crs = 25832))
cat("Home snapped to the nearest node,",
    round(sqrt(sum((node_xy[home_node, ] - st_coordinates(home_utm)[1, 1:2])^2)), 1), "m away.\n")

# ------------------------------------------------------------
# 2. Which streets have been ridden? (from the track folder)
# ------------------------------------------------------------
# Every recorded point of a ride: lon, lat, and ele / time when the file has them (a GPX from a
# bike computer does; the planned route and plain lat,lon CSVs do not).
read_points <- function(f) {
  if (grepl("\\.gpx$", f, ignore.case = TRUE)) {
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

read_track <- function(f) {
  d <- read_points(f)
  d <- d[c(TRUE, diff(d$lat) != 0 | diff(d$lon) != 0), , drop = FALSE]
  if (nrow(d) < 2) return(NULL)
  xy <- st_coordinates(st_transform(st_as_sf(d, coords = c("lon", "lat"), crs = 4326), 25832))
  grp <- c(1L, 1L + cumsum(sqrt(rowSums(diff(xy)^2)) > track_gap_m))   # split at GPS dropouts
  parts <- split(seq_len(nrow(xy)), grp)
  parts <- parts[lengths(parts) >= 2]
  if (length(parts) == 0) return(NULL)
  st_multilinestring(lapply(parts, function(ix) xy[ix, , drop = FALSE]))
}

track_files <- list.files(rides_dir, pattern = "\\.(csv|gpx)$", recursive = TRUE, full.names = TRUE,
                          ignore.case = TRUE)
trk <- Filter(Negate(is.null), lapply(track_files, read_track))

# Coverage = share of an edge's sample points (every ~10 m, mid-segment) that fall inside the
# buffered tracks. Much faster than intersecting every edge with a huge polygon.
n_s  <- pmax(1L, as.integer(round(elen[in_net] / 10)))
samp <- st_cast(st_line_sample(edge_geom[in_net], n = n_s, type = "regular"), "POINT")
inside <- if (length(trk) == 0) rep(FALSE, length(samp)) else {      # no rides yet: nothing ridden
  trk_buf <- st_union(st_buffer(st_simplify(st_sfc(trk, crs = 25832), dTolerance = 2), ridden_buffer_m))
  lengths(st_intersects(samp, trk_buf)) > 0
}
cover <- rep(NA_real_, n_eid)
cover[in_net] <- as.numeric(rowsum(as.integer(inside), rep(seq_along(n_s), n_s))) / n_s
eunr_any <- in_net & !econn & cover < ridden_min_cover       # a street not ridden yet

# Unridden dead-end stubs of <= min_spur_m are optional (an out-and-back for a courtyard mouth).
unr_deg <- tabulate(c(eu[eunr_any], ev[eunr_any]), nbins = vcount(G))
eunr_req <- eunr_any & !(elen <= min_spur_m & (unr_deg[eu] == 1L | unr_deg[ev] == 1L))
eunr_req[is.na(eunr_req)] <- FALSE

tot_km   <- sum(elen[in_net & !econn]) / 1000
ridden_km <- sum(elen[in_net & !econn & !eunr_any]) / 1000
cat(length(trk), "ride track(s) read.", round(ridden_km, 1), "of", round(tot_km, 1),
    "km of street within", radius_km, "km of home are ridden (", round(100 * ridden_km / tot_km, 1), "%).\n")
E(G)$unr_any <- eunr_any[E(G)$eid]

# ------------------------------------------------------------
# 3. Planning one ride
# ------------------------------------------------------------
dh  <- distances(G, v = home_node, weights = E(G)$len)[1, ]       # network distance from home, per node
edh <- pmin(dh[eu], dh[ev]); edh[!in_net] <- Inf                  # ... and per edge
emid <- (node_xy[eu, , drop = FALSE] + node_xy[ev, , drop = FALSE]) / 2

# Hierholzer with a "continue closest to straight ahead" rule: every Eulerian circuit has the
# same edges and length whatever the order, so pick the order that flows instead of zig-zagging.
euler_smart <- function(g, nc, start_v) {
  ne <- ecount(g)
  elist <- ends(g, E(g), names = FALSE)
  f <- factor(c(elist[, 1], elist[, 2]), levels = seq_len(vcount(g)))
  inc_e <- split(rep(seq_len(ne), 2), f)
  inc_v <- split(c(elist[, 2], elist[, 1]), f)
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
      d0 <- nc[v, ] - nc[sv[sp - 1L], ]
      n0 <- sqrt(sum(d0^2))
      if (n0 > 0) {
        nxts <- inc_v[[v]][avail]
        dx <- nc[nxts, 1] - nc[v, 1]; dy <- nc[nxts, 2] - nc[v, 2]
        nn <- sqrt(dx * dx + dy * dy); nn[nn == 0] <- 1
        pick <- avail[which.max((dx * d0[1] + dy * d0[2]) / (nn * n0))]
      }
    }
    used[ie[pick]] <- TRUE
    sp <- sp + 1L; sv[sp] <- inc_v[[v]][pick]; se[sp] <- ie[pick]
  }
  list(vpath = rev(out_v[seq_len(op)]), epath = rev(out_e[seq_len(op)])[-1])
}

# A closed loop from home that rides every street in S (edge ids) at least once:
#  1. join the street fragments of S, and home, with shortest paths (nearest-first),
#  2. match the odd-degree vertices along shortest paths so every vertex is even,
#  3. walk an Eulerian circuit from home.
# Links and parity detours are priced with detour_bonus, so they favour unridden streets.
# Works on the part of the network around S and home, which keeps each solve fast.
solve_route <- function(S) {
  need <- unique(c(eu[S], ev[S], home_node))
  for (pad in c(1500, 4000, Inf)) {
    loc <- if (is.finite(pad))
      which(node_xy[, 1] >= min(node_xy[need, 1]) - pad & node_xy[, 1] <= max(node_xy[need, 1]) + pad &
            node_xy[, 2] >= min(node_xy[need, 2]) - pad & node_xy[, 2] <= max(node_xy[need, 2]) + pad)
    else seq_len(vcount(G))
    GL <- induced_subgraph(G, loc)
    cmL <- components(GL)$membership
    if (length(unique(cmL[match(need, loc)])) == 1L) break
  }
  h <- match(home_node, loc)
  req_idx <- which(E(GL)$eid %in% S)
  conn_mult <- ifelse(E(GL)$conn, conn_penalty, 1) * ifelse(E(GL)$path, path_link_mult, 1)
  # Linking: a street already in the ride is nearly free to pass along; an unridden street outside
  # it is cheaper than a ridden one (new street picked up on the way).
  in_S <- seq_len(ecount(GL)) %in% req_idx
  pc <- E(GL)$len * ifelse(in_S, 0.02, 1 - detour_bonus * E(GL)$unr_any) * conn_mult

  R <- subgraph_from_edges(GL, req_idx, delete.vertices = FALSE)
  comp_R <- components(R)$membership
  Tn <- unique(c(which(degree(R) > 0), h))
  link <- integer(0)
  if (length(unique(comp_R[Tn])) > 1L) {
    D <- distances(GL, v = Tn, to = Tn, weights = pc)
    cid <- comp_R[Tn]
    inset <- cid == cid[match(h, Tn)]
    while (!all(inset)) {
      sub <- D[inset, !inset, drop = FALSE]
      k <- which(sub == min(sub), arr.ind = TRUE)[1, ]
      a <- Tn[inset][k[1]]; b <- Tn[!inset][k[2]]
      sp <- shortest_paths(GL, a, b, weights = pc, output = "epath")$epath[[1]]
      link <- c(link, as.integer(sp))
      inset <- inset | (cid == cid[match(b, Tn)])
    }
  }
  ride_idx <- union(req_idx, link)

  odd <- which(degree(subgraph_from_edges(GL, ride_idx, delete.vertices = FALSE)) %% 2 == 1)
  dup <- integer(0)
  if (length(odd) > 0L) {
    # Parity: now a street already in the ride costs its full length to ride again (pure
    # repetition), while an unridden street outside the ride is the one that gets the discount.
    in_ride <- seq_len(ecount(GL)) %in% ride_idx
    pc <- E(GL)$len * (1 - detour_bonus * (E(GL)$unr_any & !in_ride)) * conn_mult
    D2 <- distances(GL, v = odd, to = odd, weights = pc)
    m <- nonbimatch(distancematrix(D2))
    pr <- m$halves[m$halves$Group1.Row != m$halves$Group2.Row, ]
    for (i in seq_len(nrow(pr))) {
      sp <- shortest_paths(GL, odd[pr$Group1.Row[i]], odd[pr$Group2.Row[i]],
                           weights = pc, output = "epath")$epath[[1]]
      dup <- c(dup, as.integer(sp))
    }
  }
  all_idx <- c(ride_idx, dup)
  GA <- make_graph(as.vector(t(ends(GL, all_idx, names = FALSE))), n = vcount(GL), directed = FALSE)
  E(GA)$eid <- E(GL)$eid[all_idx]
  stopifnot(all(degree(GA) %% 2 == 0))

  eul <- euler_smart(GA, node_xy[loc, , drop = FALSE], h)
  stopifnot(length(eul$epath) == ecount(GA), eul$vpath[1] == h, eul$vpath[length(eul$vpath)] == h)
  eid_seq <- E(GA)$eid[eul$epath]
  v_seq   <- loc[eul$vpath]

  first   <- !duplicated(eid_seq)
  is_new  <- first & eunr_any[eid_seq]
  len_m   <- sum(elen[eid_seq])
  new_m   <- sum(elen[eid_seq][is_new])
  mean_dh <- if (new_m > 0) sum(elen[eid_seq][is_new] * edh[eid_seq][is_new]) / new_m else 0
  # Where the repeated km come from: a new street ridden a second time (parity/out-and-back),
  # or street that a ride logged in rides/ already covers.
  list(eid_seq = eid_seq, v_seq = v_seq, is_new = is_new, len_m = len_m, new_m = new_m,
       repeat_m = len_m - new_m,
       twice_new_m = sum(elen[eid_seq][!first & eunr_any[eid_seq]]),
       old_m = sum(elen[eid_seq][!eunr_any[eid_seq] & !econn[eid_seq]]),
       conn_m = sum(elen[eid_seq][econn[eid_seq]]),
       path_m = sum(elen[eid_seq][epath[eid_seq]]),
       mean_dh_m = mean_dh, max_dh_m = max(dh[v_seq]),
       score = new_m / 1000 - w_repeat * (len_m - new_m) / 1000 - w_home * mean_dh / 1000)
}

pool <- which(eunr_req & is.finite(edh) & edh <= max_reach_km * 1000)
if (length(pool) == 0L) stop("No unridden streets left within ", max_reach_km, " km of home - raise max_reach_km or radius_km.")
cat(length(pool), "unridden candidate streets (", round(sum(elen[pool]) / 1000, 1), "km) within",
    max_reach_km, "km of home.\n")

# Grow a cluster of unridden streets around a seed street until it holds target_len metres:
# nearest to the seed first, with a pull toward home.
build_cluster <- function(seed_eid, target_len) {
  ds <- distances(G, v = eu[seed_eid], weights = E(G)$len)[1, ]
  prio <- pmin(ds[eu[pool]], ds[ev[pool]]) + w_cluster_home * edh[pool]
  o <- order(prio)
  k <- which(cumsum(elen[pool][o]) >= target_len)[1]
  if (is.na(k)) k <- length(o)
  pool[o[seq_len(k)]]
}

plan_from_seed <- function(seed_eid, target_len) {
  S <- build_cluster(seed_eid, target_len)
  r <- solve_route(S)
  r$seed <- seed_eid; r$target <- target_len
  r
}

# One seed per compass sector: the unridden street nearest home, nudged toward dense unridden
# areas so a seed is not an isolated stub.
bearing <- (atan2(emid[pool, 1] - node_xy[home_node, 1], emid[pool, 2] - node_xy[home_node, 2]) * 180 / pi + 360) %% 360
sector  <- floor(bearing / (360 / n_sectors))
seed_score <- edh[pool] - 60 * (unr_deg[eu[pool]] + unr_deg[ev[pool]])
seeds <- sapply(split(seq_along(pool), sector), function(ix) pool[ix[which.min(seed_score[ix])]])
target_m <- target_km * 1000

t0 <- Sys.time()
cands <- list()
for (s in seeds) {
  t1 <- max(1500, (target_m - 2 * edh[s]) / rho)
  cands[[length(cands) + 1]] <- tryCatch(plan_from_seed(s, t1), error = function(e) NULL)
}
cands <- Filter(Negate(is.null), cands)
cat(length(cands), "first-pass candidates in", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s:\n")
print(data.frame(
  sector = sapply(cands, function(r) floor((atan2(emid[r$seed, 1] - node_xy[home_node, 1],
                                                   emid[r$seed, 2] - node_xy[home_node, 2]) * 180 / pi + 360) %% 360 / (360 / n_sectors))),
  km = round(sapply(cands, function(r) r$len_m) / 1000, 1),
  new_pct = round(100 * sapply(cands, function(r) r$new_m / r$len_m)),
  twice_new_km = round(sapply(cands, function(r) r$twice_new_m) / 1000, 1),
  old_km = round(sapply(cands, function(r) r$old_m) / 1000, 1),
  far_km = round(sapply(cands, function(r) r$max_dh_m) / 1000, 1),
  score = round(sapply(cands, function(r) r$score), 1)), row.names = FALSE)

# Normalise to the wanted length so a longer first guess does not win on size alone, then
# length-tune the best few: rescale the cluster until the loop is within length_tol of target.
norm_score <- function(r) r$score * target_m / r$len_m
cands <- cands[order(-sapply(cands, norm_score))]
refine <- function(r) {
  best <- r
  for (it in 1:5) {
    if (abs(best$len_m - target_m) / target_m <= length_tol) break
    t_new <- best$target * (target_m / best$len_m)^0.9
    nxt <- tryCatch(plan_from_seed(best$seed, t_new), error = function(e) NULL)
    if (is.null(nxt)) break
    if (abs(nxt$len_m - target_m) < abs(best$len_m - target_m)) best <- nxt else break
  }
  best
}
tuned <- lapply(cands[seq_len(min(n_refine, length(cands)))], refine)
ok <- Filter(function(r) abs(r$len_m - target_m) / target_m <= length_tol, tuned)
chosen_set <- if (length(ok) > 0) ok else tuned
best <- chosen_set[[which.max(sapply(chosen_set, function(r) r$score))]]
cat("Planned in", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), "s.\n")

cat(sprintf("\nNext route: %.1f km loop | %.1f km new street (%.0f%%) | %.1f km repeated | farthest point %.1f km from home | new street averages %.1f km from home\n",
            best$len_m / 1000, best$new_m / 1000, 100 * best$new_m / best$len_m, best$repeat_m / 1000,
            best$max_dh_m / 1000, best$mean_dh_m / 1000))
cat(sprintf("Repeated km: %.1f on new streets ridden twice, %.1f on streets your logged rides already cover, %.2f on dismount connectors.\n",
            best$twice_new_m / 1000, best$old_m / 1000, best$conn_m / 1000))
cat(sprintf("Footways/paths in the route: %.1f km (%.0f%%) - drawn dashed on the map; check them before you go.\n",
            best$path_m / 1000, 100 * best$path_m / best$len_m))

# ------------------------------------------------------------
# 4. Draw it, add elevation, export GPX
# ------------------------------------------------------------
oriented_coords <- function(eid, from_node) {
  gm <- st_coordinates(edge_geom[[eid]])[, 1:2, drop = FALSE]
  if (max(abs(gm[1, ] - node_xy[from_node, ])) > 0.01) gm <- gm[nrow(gm):1, , drop = FALSE]
  gm
}
n_step <- length(best$eid_seq)
segs <- lapply(seq_len(n_step), function(i) oriented_coords(best$eid_seq[i], best$v_seq[i]))
route_coords <- do.call(rbind, lapply(seq_len(n_step), function(i) if (i > 1) segs[[i]][-1, , drop = FALSE] else segs[[i]]))
route_pts <- st_as_sf(as.data.frame(route_coords), coords = c("X", "Y"), crs = 25832)
route_wgs <- st_transform(route_pts, 4326)
xy_wgs <- st_coordinates(route_wgs)
cum_km <- c(0, cumsum(sqrt(diff(route_coords[, 1])^2 + diff(route_coords[, 2])^2))) / 1000

route_ele <- tryCatch({
  dem <- elevatr::get_elev_raster(locations = route_wgs, z = dem_zoom, clip = "bbox", src = "aws")
  as.numeric(terra::extract(terra::rast(dem), terra::vect(route_wgs))[, 2])
}, error = function(e) {
  message("Elevation lookup failed (", conditionMessage(e), ") - continuing without it.")
  rep(NA_real_, nrow(route_coords))
})
ascent_m <- function(e) {
  e <- e[!is.na(e)]
  if (length(e) < 5) return(NA_real_)
  sm <- as.numeric(stats::filter(e, rep(1 / 9, 9), sides = 2))
  sm[is.na(sm)] <- e[is.na(sm)]
  d <- diff(sm); sum(d[d > 0])
}
cat("Distance:", round(max(cum_km), 1), "km | ascent about", round(ascent_m(route_ele)), "m\n")

write_gpx_track <- function(df, filename, track_name) {
  pts <- ifelse(is.na(df$ele),
                sprintf('      <trkpt lat="%f" lon="%f"></trkpt>', df$lat, df$lon),
                sprintf('      <trkpt lat="%f" lon="%f"><ele>%.1f</ele></trkpt>', df$lat, df$lon, df$ele))
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>',
               '<gpx version="1.1" creator="R" xmlns="http://www.topografix.com/GPX/1/1">',
               sprintf('  <trk><name>%s</name>', track_name), '    <trkseg>', pts, '    </trkseg>',
               '  </trk>', '</gpx>'), filename)
}
dir.create(out_dir, showWarnings = FALSE)
gpx_file <- file.path(out_dir, "next_route.gpx")
write_gpx_track(data.frame(lon = xy_wgs[, 1], lat = xy_wgs[, 2], ele = route_ele), gpx_file,
                sprintf("next %.0f km (%.0f%% new)", best$len_m / 1000, 100 * best$new_m / best$len_m))
cat("Wrote", gpx_file, "\n")

# Segments for the picture and the map: green = a street ridden for the first time, orange =
# street ridden again (a repeat within this ride, or covered by a ride logged in rides/).
seg_sf <- st_sf(type = ifelse(econn[best$eid_seq], "gap in the map (cross on foot)",
                              ifelse(best$is_new, "new street", "repeat")),
                path = epath[best$eid_seq],
                geometry = st_sfc(lapply(segs, st_linestring), crs = 25832))

near <- in_net & is.finite(edh) & edh <= max_reach_km * 1000
p_plan <- ggplot() +
  geom_sf(data = edge_geom[near & !eunr_any & !econn], color = "grey85", linewidth = 0.2) +
  geom_sf(data = edge_geom[near & eunr_any], color = "#c62828", linewidth = 0.25, alpha = 0.6) +
  geom_sf(data = seg_sf, aes(color = type, linetype = path), linewidth = 0.9) +
  scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "22"), labels = c("street", "footway / path"), name = NULL) +
  geom_sf(data = home_utm, shape = 21, fill = "white", size = 3) +
  scale_color_manual(values = c("new street" = "#2e7d32", "repeat" = "#ef6c00",
                                "gap in the map (cross on foot)" = "#d500f9"), name = NULL) +
  labs(title = sprintf("Next ride: %.1f km, %.1f km on new streets (%.0f%%)", best$len_m / 1000,
                       best$new_m / 1000, 100 * best$new_m / best$len_m),
       subtitle = sprintf("grey = ridden, red = not ridden yet (within %d km of home); %d rides in the history",
                          max_reach_km, length(track_files))) +
  theme_minimal() + theme(legend.position = "bottom")
ggsave(file.path(out_dir, "next_route_plan.png"), p_plan, width = 10, height = 10, dpi = 110)

seg_map <- st_transform(seg_sf, 4326)
pal <- colorFactor(c("#ef6c00", "#2e7d32", "#d500f9"),
                   domain = c("repeat", "new street", "gap in the map (cross on foot)"))
route_map <- leaflet(seg_map) |>
  addProviderTiles(providers$CyclOSM, group = "CyclOSM") |>
  addProviderTiles(providers$OpenStreetMap.DE, group = "OSM (DE)") |>
  addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri streets") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addPolylines(data = seg_map[!seg_map$path, ], color = ~pal(type), weight = 5, opacity = 0.9, label = ~type,
               options = pathOptions(lineJoin = "miter", lineCap = "butt"), group = "Route") |>
  addPolylines(data = seg_map[seg_map$path, ], color = ~pal(type), weight = 5, opacity = 0.9, dashArray = "8,6",
               label = ~paste(type, "(footway / path)"), group = "Route") |>
  addCircleMarkers(lng = home_lonlat[["lon"]], lat = home_lonlat[["lat"]], radius = 7,
                   color = "black", fillColor = "white", fillOpacity = 1, label = "Home") |>
  addLegend("bottomright", pal = pal, values = c("new street", "repeat", "gap in the map (cross on foot)"), title = NULL) |>
  addLayersControl(baseGroups = c("CyclOSM", "OSM (DE)", "Esri streets", "Satellite"),
                   overlayGroups = "Route", options = layersControlOptions(collapsed = FALSE))
map_file <- file.path(out_dir, "next_route_map.html")
htmlwidgets::saveWidget(route_map, normalizePath(map_file, mustWork = FALSE), selfcontained = TRUE)
cat("Wrote", map_file, "and", file.path(out_dir, "next_route_plan.png"), "\n")

# ------------------------------------------------------------
# 5. Progress: everything ridden, with stats (needs at least one ride in rides_dir)
# ------------------------------------------------------------
# Per ride, in time order: distance, time, speed and ascent from the file, and how much street it
# covered for the first time. Streets use the same rule as everywhere else (ridden_min_cover of
# the length within ridden_buffer_m of a track), applied to the tracks cumulatively, so the
# per-ride numbers add up to the overall coverage. Time needs timestamps and ascent needs
# elevation in the file (a bike-computer GPX has both); where a file lacks them it says n/a.
if (length(track_files) > 0) {
  rd <- lapply(track_files, function(f) {
    d <- tryCatch(read_points(f), error = function(e) NULL)
    if (is.null(d) || nrow(d) < 2) { message("Skipping unreadable ride: ", f); return(NULL) }
    xy <- st_coordinates(st_transform(st_as_sf(d, coords = c("lon", "lat"), crs = 4326), 25832))
    step <- sqrt(rowSums(diff(xy)^2))
    ok <- step <= track_gap_m                       # a long jump is a GPS dropout, not riding
    has_t <- "time" %in% names(d) && sum(!is.na(d$time)) > 1
    dt <- if (has_t) as.numeric(diff(as.numeric(d$time))) else rep(NA_real_, length(step))
    moving <- ok & !is.na(dt) & dt > 0 & dt <= 30 & step / dt > 0.5     # above 1.8 km/h, no pauses
    list(start = if (has_t) min(d$time, na.rm = TRUE) else as.POSIXct(file.mtime(f), tz = "UTC"),
         km = sum(step[ok]) / 1000,
         moving_h  = if (has_t) sum(dt[moving]) / 3600 else NA_real_,
         elapsed_h = if (has_t) as.numeric(difftime(max(d$time, na.rm = TRUE), min(d$time, na.rm = TRUE), units = "hours")) else NA_real_,
         ascent = if ("ele" %in% names(d)) ascent_m(d$ele) else NA_real_)
  })
  keep_r <- !sapply(rd, is.null)
  rd <- rd[keep_r]; rfiles <- track_files[keep_r]
  ord <- order(sapply(rd, function(r) as.numeric(r$start)))
  rd <- rd[ord]; rfiles <- rfiles[ord]

  # Street coverage after each ride (cumulative), and the ride that first covered each street.
  sxy <- st_coordinates(samp)
  edge_of_sample <- rep(seq_along(n_s), n_s)
  non_conn <- !econn[in_net]
  inside_cum <- rep(FALSE, length(samp))
  cov_prev <- rep(FALSE, length(n_s))
  first_ride <- rep(NA_integer_, length(n_s))
  for (i in seq_along(rd)) {
    trk_i <- read_track(rfiles[i])
    if (!is.null(trk_i)) {
      buf <- st_buffer(st_simplify(st_sfc(trk_i, crs = 25832), dTolerance = 2), ridden_buffer_m)
      bb <- st_bbox(buf)
      cand <- which(sxy[, 1] >= bb["xmin"] & sxy[, 1] <= bb["xmax"] & sxy[, 2] >= bb["ymin"] & sxy[, 2] <= bb["ymax"])
      if (length(cand) > 0) inside_cum[cand[lengths(st_intersects(samp[cand], buf)) > 0]] <- TRUE
    }
    cov_now <- (as.numeric(rowsum(as.integer(inside_cum), edge_of_sample)) / n_s >= ridden_min_cover) & non_conn
    newly <- cov_now & !cov_prev
    first_ride[newly] <- i
    rd[[i]]$new_km  <- sum(elen[in_net][newly]) / 1000
    rd[[i]]$cum_pct <- 100 * sum(elen[in_net][cov_now]) / 1000 / net_km
    cov_prev <- cov_now
  }

  tab <- data.frame(
    ride = seq_along(rd),
    date = format(as.POSIXct(sapply(rd, function(r) as.numeric(r$start)), origin = "1970-01-01", tz = "UTC"),
                  "%Y-%m-%d %H:%M", tz = "Europe/Oslo"),
    file = basename(rfiles),
    km = sapply(rd, `[[`, "km"),
    moving_h = sapply(rd, `[[`, "moving_h"), elapsed_h = sapply(rd, `[[`, "elapsed_h"),
    ascent_m = sapply(rd, `[[`, "ascent"),
    new_street_km = sapply(rd, `[[`, "new_km"), covered_pct = sapply(rd, `[[`, "cum_pct"))
  tab$avg_kmh <- tab$km / tab$moving_h

  ridden_km_total <- sum(elen[in_net][cov_prev]) / 1000
  remaining_km <- net_km - ridden_km_total
  timed <- !is.na(tab$moving_h)
  tot_moving <- sum(tab$moving_h, na.rm = TRUE)
  tot <- list(rides = nrow(tab), km = sum(tab$km), moving_h = if (any(timed)) tot_moving else NA_real_,
              elapsed_h = if (any(timed)) sum(tab$elapsed_h, na.rm = TRUE) else NA_real_,
              ascent = if (any(!is.na(tab$ascent_m))) sum(tab$ascent_m, na.rm = TRUE) else NA_real_,
              avg_kmh = if (any(timed)) sum(tab$km[timed]) / tot_moving else NA_real_,
              new_km = ridden_km_total, pct = 100 * ridden_km_total / net_km,
              left_km = remaining_km, rides_to_go = ceiling(remaining_km / max(mean(tab$new_street_km), 1)))

  fmt <- function(x, d = 1) ifelse(is.na(x), "n/a", formatC(x, format = "f", digits = d, big.mark = ","))
  hm  <- function(h) ifelse(is.na(h), "n/a", sprintf("%d:%02d", as.integer(h), as.integer(round((h %% 1) * 60))))

  cat("\n==== Ride progress ====\n")
  cat(sprintf("%d ride(s) | %s km ridden | %s h moving (%s h elapsed) | %s m climbing | avg %s km/h\n",
              tot$rides, fmt(tot$km), hm(tot$moving_h), hm(tot$elapsed_h), fmt(tot$ascent, 0), fmt(tot$avg_kmh)))
  cat(sprintf("Street covered: %s of %s km (%s%%) | %s km to go, roughly %d more rides at the current pace\n",
              fmt(tot$new_km), fmt(net_km), fmt(tot$pct), fmt(tot$left_km), tot$rides_to_go))
  print(transform(tab, km = round(km, 1), moving_h = round(moving_h, 2), elapsed_h = round(elapsed_h, 2),
                  ascent_m = round(ascent_m), new_street_km = round(new_street_km, 1),
                  covered_pct = round(covered_pct, 2), avg_kmh = round(avg_kmh, 1)), row.names = FALSE)
  write.csv(tab, file.path(out_dir, "progress_rides.csv"), row.names = FALSE)

  # --- Stats page: totals as cards, then one row per ride ---
  esc <- htmltools::htmlEscape
  card <- function(label, value, unit = "") sprintf('<div class="card"><div class="v">%s<span>%s</span></div><div class="l">%s</div></div>', value, unit, label)
  cards <- paste(
    card("Rides", tot$rides), card("Distance", fmt(tot$km), " km"), card("Moving time", hm(tot$moving_h), " h"),
    card("Elapsed time", hm(tot$elapsed_h), " h"), card("Climbing", fmt(tot$ascent, 0), " m"),
    card("Average speed", fmt(tot$avg_kmh), " km/h"), card("Street covered", fmt(tot$new_km), " km"),
    card("Of the network", fmt(tot$pct), " %"), card("Left to ride", fmt(tot$left_km), " km"),
    card("Rides to go (est.)", tot$rides_to_go))
  rows <- paste(sprintf('<tr><td>%d</td><td>%s</td><td class="f">%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>',
                        tab$ride, esc(tab$date), esc(tab$file), fmt(tab$km), hm(tab$moving_h), hm(tab$elapsed_h),
                        fmt(tab$avg_kmh), fmt(tab$ascent_m, 0), fmt(tab$new_street_km), fmt(tab$covered_pct, 2)),
                collapse = "\n")
  page <- paste0('<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">',
    '<title>Ride progress</title><style>:root{color-scheme:light dark;--bg:#fff;--fg:#1d2326;--mut:#667;--card:#f3f6f4;--line:#dde3df;--acc:#2e7d32}',
    '@media(prefers-color-scheme:dark){:root{--bg:#14181a;--fg:#e6ebe8;--mut:#9aa6a0;--card:#1d2326;--line:#2c3438;--acc:#7bc67f}}',
    'body{margin:0;padding:24px 16px;background:var(--bg);color:var(--fg);font:15px/1.45 system-ui,sans-serif}main{max-width:1000px;margin:auto}',
    'h1{font-size:22px;margin:0 0 4px}p.s{color:var(--mut);margin:0 0 20px}.g{display:grid;grid-template-columns:repeat(auto-fill,minmax(150px,1fr));gap:10px;margin-bottom:26px}',
    '.card{background:var(--card);border-radius:10px;padding:12px 14px}.v{font-size:24px;font-weight:600;color:var(--acc)}.v span{font-size:13px;font-weight:400;color:var(--mut)}',
    '.l{font-size:12px;color:var(--mut);margin-top:2px}.w{overflow-x:auto}table{border-collapse:collapse;width:100%;min-width:720px}',
    'th,td{padding:7px 10px;text-align:right;border-bottom:1px solid var(--line);white-space:nowrap}th{font-size:12px;color:var(--mut);font-weight:500}',
    'td.f,th.f{text-align:left}td:nth-child(2),th:nth-child(2){text-align:left}</style></head><body><main>',
    '<h1>Ride progress</h1><p class="s">', esc(format(Sys.time(), "%Y-%m-%d %H:%M")), ' &middot; street network within ', radius_km,
    ' km of home &middot; a street counts once ', round(100 * ridden_min_cover), '% of it is within ', ridden_buffer_m, ' m of a recorded track</p>',
    '<div class="g">', cards, '</div><div class="w"><table><thead><tr><th>#</th><th>Date</th><th class="f">File</th><th>km</th><th>Moving</th><th>Elapsed</th><th>km/h</th><th>Climb m</th><th>New street km</th><th>Covered %</th></tr></thead><tbody>',
    rows, '</tbody></table></div><p class="s" style="margin-top:14px">n/a = the file has no timestamps or elevation (a bike-computer GPX has both).</p></main></body></html>')
  writeLines(page, file.path(out_dir, "progress_summary.html"), useBytes = TRUE)

  # --- Map of everything ridden: street colour = the ride that first covered it ---
  ridden_i <- which(!is.na(first_ride))
  rid_sf <- st_sf(ride = first_ride[ridden_i], date = tab$date[first_ride[ridden_i]],
                  geometry = st_transform(edge_geom[in_net][ridden_i], 4326))
  tl <- lapply(rfiles, read_track); tl_ok <- !sapply(tl, is.null)
  trk_sf <- st_sf(ride = which(tl_ok), date = tab$date[tl_ok],
                  geometry = st_transform(st_simplify(st_sfc(tl[tl_ok], crs = 25832), dTolerance = 2), 4326))
  pal_r <- colorNumeric("viridis", domain = c(1, max(2, length(rd))))
  mini <- sprintf('<div style="background:rgba(255,255,255,.92);color:#1d2326;padding:8px 10px;border-radius:6px;font:12px/1.5 system-ui,sans-serif"><b>%d ride(s)</b><br>%s km &middot; %s h moving<br>%s m climbing<br>%s km of street covered (%s%%)</div>',
                  tot$rides, fmt(tot$km), hm(tot$moving_h), fmt(tot$ascent, 0), fmt(tot$new_km), fmt(tot$pct))
  prog_map <- leaflet() |>
    addProviderTiles(providers$CyclOSM, group = "CyclOSM") |>
    addProviderTiles(providers$OpenStreetMap.DE, group = "OSM (DE)") |>
    addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri streets") |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
    addPolylines(data = rid_sf, color = ~pal_r(ride), weight = 4, opacity = 0.9,
                 label = ~paste0("First ridden on ride ", ride, " (", date, ")"), group = "Streets ridden") |>
    addPolylines(data = trk_sf, color = "#1565c0", weight = 1.5, opacity = 0.7,
                 label = ~paste0("Ride ", ride, " (", date, ")"), group = "Recorded tracks") |>
    hideGroup("Recorded tracks") |>
    addPolylines(data = seg_map, color = "#ef6c00", weight = 4, opacity = 0.9, dashArray = "6,6",
                 label = "Next planned route", group = "Next planned route") |>
    hideGroup("Next planned route") |>
    addCircleMarkers(lng = home_lonlat[["lon"]], lat = home_lonlat[["lat"]], radius = 7,
                     color = "black", fillColor = "white", fillOpacity = 1, label = "Home") |>
    addLegend("bottomright", pal = pal_r, values = c(1, max(2, length(rd))), title = "First ridden<br>on ride #") |>
    addControl(html = mini, position = "topright") |>
    addLayersControl(baseGroups = c("CyclOSM", "OSM (DE)", "Esri streets", "Satellite"),
                     overlayGroups = c("Streets ridden", "Recorded tracks", "Next planned route"),
                     options = layersControlOptions(collapsed = FALSE))
  htmlwidgets::saveWidget(prog_map, normalizePath(file.path(out_dir, "progress_map.html"), mustWork = FALSE), selfcontained = TRUE)
  cat("Wrote", file.path(out_dir, "progress_map.html"), ",", file.path(out_dir, "progress_summary.html"),
      "and", file.path(out_dir, "progress_rides.csv"), "\n")
}

