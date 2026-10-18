# ------------------------------------------------------------
# The Oslo street network: which ways count, noding into a graph, per-edge attributes, cache.
# Needs data/oslo_ways.gpkg, oslo_boundary.gpkg, oslo_districts.gpkg (see R/osm_extract.R).
# ------------------------------------------------------------

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

path_classes <- c("footway", "path", "pedestrian", "bridleway", "corridor")

# road = roads and cycleways, path = footways/paths/pedestrian streets, gravel = tracks
way_class <- function(highway) {
  ifelse(highway == "track", "gravel", ifelse(highway %in% path_classes, "path", "road"))
}

# Which ways count toward "every street in Oslo". Takes a data frame with highway and
# other_tags (OSM hstore text) and returns TRUE/FALSE per row.
#   In:  public roads except motorway/trunk/primary, service roads except driveways and parking
#        aisles, cycleways, footways/pedestrian streets that are not sidewalks or crossings,
#        paths that are paved/gravel/compacted or signed for bikes, tracks of grade1-2 or with a
#        paved/gravel/compacted surface.
#   Out: steps, bicycle=no/dismount, private ways, areas, and trails (dirt/ground surfaces,
#        mtb:scale >= 1, tracktype grade3-5).
#   Untagged paths/tracks (no surface, no tracktype) count in the city, but in Marka (in_marka)
#   they are most likely forest trails, so there they need a good surface or bike signs.
way_filter <- function(ways, in_marka = rep(FALSE, nrow(ways))) {
  hw <- ways$highway; tg <- ways$other_tags
  bicycle   <- tag_value(tg, "bicycle");   access  <- tag_value(tg, "access")
  service   <- tag_value(tg, "service");   footway <- tag_value(tg, "footway")
  cycleway  <- tag_value(tg, "cycleway");  surface <- tag_value(tg, "surface")
  tracktype <- tag_value(tg, "tracktype"); mtb     <- tag_value(tg, "mtb:scale")
  area      <- tag_value(tg, "area")

  exclude_classes <- c("motorway", "motorway_link", "trunk", "trunk_link", "primary", "primary_link",
                       "construction", "proposed", "raceway", "bus_guideway", "busway", "escape",
                       "platform", "elevator", "services", "rest_area", "street_lamp", "steps",
                       "bus_stop", "emergency_bay", "via_ferrata", "abandoned", "disused", "razed",
                       "no", "crossing", "traffic_signals")
  good_surface  <- c("paved", "asphalt", "concrete", "concrete:plates", "concrete:lanes", "paving_stones",
                     "sett", "cobblestone", "unhewn_cobblestone", "compacted", "fine_gravel", "gravel",
                     "pebblestone", "metal", "wood", "chipseal")
  trail_surface <- c("ground", "dirt", "earth", "grass", "mud", "sand", "rock", "roots", "stone",
                     "woodchips", "grass_paver", "stepping_stones")
  bike_signed <- bicycle %in% c("yes", "designated", "permissive")
  trail <- surface %in% trail_surface | grepl("^[1-6]", mtb) | tracktype %in% c("grade3", "grade4", "grade5")

  keep <- !hw %in% exclude_classes &
    !bicycle %in% c("no", "dismount") &
    (!access %in% c("private", "no", "customers", "permit", "delivery") | bike_signed) &
    !(hw == "service" & service %in% c("driveway", "parking_aisle", "drive-through")) &
    !footway %in% c("sidewalk", "crossing", "traffic_island", "access_aisle") &
    !cycleway %in% c("crossing", "sidewalk") &
    !area %in% "yes"

  untagged <- is.na(surface) & is.na(tracktype) & !in_marka
  keep_path  <- (hw %in% c("footway", "pedestrian", "corridor") & !trail) |
                (hw %in% c("path", "bridleway") & !trail & (surface %in% good_surface | bike_signed | untagged))
  keep_track <- hw == "track" & !trail & (tracktype %in% c("grade1", "grade2") | surface %in% good_surface | untagged)
  keep & ifelse(hw %in% path_classes, keep_path, ifelse(hw == "track", keep_track, TRUE))
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

build_network <- function(cfg) {
  dd <- cfg$data_dir
  raw <- st_read(file.path(dd, "oslo_ways.gpkg"), quiet = TRUE)
  boundary  <- st_read(file.path(dd, "oslo_boundary.gpkg"), quiet = TRUE)
  districts <- st_read(file.path(dd, "oslo_districts.gpkg"), quiet = TRUE)
  marka <- st_transform(districts[districts$district == "Marka", ], st_crs(raw))
  in_marka <- lengths(st_intersects(suppressWarnings(st_point_on_surface(st_geometry(raw))), marka)) > 0
  ways <- st_transform(raw[way_filter(raw, in_marka), ], 25832)
  message("Read ", nrow(raw), " highway lines; ", nrow(ways), " count as streets.")

  tg <- ways$other_tags
  bridge <- tag_value(tg, "bridge"); tunnel <- tag_value(tg, "tunnel"); layer <- tag_value(tg, "layer")
  ways$elevated <- (!is.na(bridge) & bridge != "no") | (!is.na(tunnel) & tunnel != "no") |
                   (!is.na(layer) & layer != "0")
  ways$cls <- way_class(ways$highway)
  ways_ls <- suppressWarnings(ways |> st_cast("MULTILINESTRING") |> st_cast("LINESTRING"))
  lines <- st_geometry(ways_ls)
  elevated <- ways_ls$elevated
  message(length(lines), " way segments; noding...")

  # OSM ways cross without sharing a node, so node them with a union (planar GEOS, not s2,
  # which crashes on messy OSM line topology on Windows), snap dead-end near-misses, node again.
  noded <- lines |> st_union() |> st_cast("LINESTRING")
  noded <- snap_dead_ends(noded, cfg$snap_m) |> st_union() |> st_cast("LINESTRING")
  noded <- noded[as.numeric(st_length(noded)) > 0]

  # Which original way does each noded edge lie on? (Noding dissolves attributes; the edge
  # midpoint sits exactly on its source way.)
  mid <- st_cast(st_line_sample(noded, sample = 0.5), "POINT")
  mid_buf <- st_buffer(mid, 0.5)
  near <- function(x) if (length(x) == 0) rep(FALSE, length(mid)) else lengths(st_intersects(mid_buf, x)) > 0
  on_elev <- near(lines[elevated]) & !near(lines[!elevated])       # bridge / tunnel / other layer
  src <- st_nearest_feature(mid, lines)
  e_cls  <- ways_ls$cls[src]
  e_name <- ways_ls$name[src]
  e_in_goal <- lengths(st_intersects(mid, st_transform(boundary, 25832))) > 0
  e_district <- districts$district[st_nearest_feature(mid, st_transform(districts, 25832))]
  e_district[!e_in_goal] <- NA_character_

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

  # Bridges, tunnels and underpasses: union-noding joins an underpass and the road above it. For
  # an edge on an elevated/underground way, an end node that ground edges also touch and that is
  # not one of the way's real end points is a false crossing: the edge gets its own copy of it.
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
  message(length(to_split), " false crossings between bridges/tunnels and the streets below removed.")

  # Bridge small fragments to the main network with straight dismount connectors; drop the rest.
  g0 <- make_graph(as.vector(rbind(u, v)), n = nrow(node_xy), directed = FALSE)
  cm <- components(g0)
  in_giant <- cm$membership == which.max(cm$csize)
  conn_geom <- list(); cu <- integer(0); cv <- integer(0); cl <- numeric(0)
  if (cm$no > 1) {
    far <- which(!in_giant); gi <- which(in_giant)
    pts_far <- st_as_sf(data.frame(x = node_xy[far, 1], y = node_xy[far, 2]), coords = c("x", "y"), crs = 25832)
    pts_gi  <- st_as_sf(data.frame(x = node_xy[gi, 1],  y = node_xy[gi, 2]),  coords = c("x", "y"), crs = 25832)
    nn <- gi[st_nearest_feature(pts_far, pts_gi)]
    d  <- sqrt(rowSums((node_xy[far, , drop = FALSE] - node_xy[nn, , drop = FALSE])^2))
    for (k in split(seq_along(far), cm$membership[far])) {
      b <- k[which.min(d[k])]
      if (d[b] > cfg$max_bridge_m) next
      cu <- c(cu, far[b]); cv <- c(cv, nn[b]); cl <- c(cl, d[b])
      conn_geom[[length(conn_geom) + 1]] <- st_linestring(rbind(node_xy[far[b], ], node_xy[nn[b], ]))
    }
    message(length(cu), " fragment(s) bridged with a dismount connector (<= ", cfg$max_bridge_m, " m).")
  }
  n_conn <- length(cu)
  edge_geom <- c(noded, st_sfc(conn_geom, crs = 25832))
  eu_all <- c(u, cu); ev_all <- c(v, cv)

  # Per-edge attributes, indexed by eid = position in edge_geom
  edges <- data.frame(len = c(len, cl), conn = c(rep(FALSE, n_line), rep(TRUE, n_conn)),
                      cls = c(e_cls, rep("connector", n_conn)), name = c(e_name, rep(NA, n_conn)),
                      in_goal = c(e_in_goal, rep(FALSE, n_conn)),
                      district = c(e_district, rep(NA, n_conn)), stringsAsFactors = FALSE)
  edges$path <- edges$cls == "path"

  G <- make_graph(as.vector(rbind(eu_all, ev_all)), n = nrow(node_xy), directed = FALSE)
  E(G)$eid <- seq_len(nrow(edges)); E(G)$len <- edges$len; E(G)$conn <- edges$conn
  E(G)$path <- edges$path; E(G)$cls <- edges$cls
  cm <- components(G)
  keep <- which(cm$membership == which.max(cm$csize))
  message("Keeping the largest component: ", length(keep), " of ", vcount(G), " nodes.")
  G <- induced_subgraph(G, keep)          # edge attribute eid survives; vertex ids are renumbered
  node_xy <- node_xy[keep, , drop = FALSE]
  edges$in_net <- seq_len(nrow(edges)) %in% E(G)$eid
  # Streets cut off from the main network by more than max_bridge_m can never be planned; kept
  # so they can be listed (and counted against the goal honestly).
  dropped_ids <- setdiff(seq_len(n_line), E(G)$eid)

  # Sample points every ~10 m along each street, used to test coverage by ride tracks
  sid <- which(edges$in_net & !edges$conn)
  n_s <- pmax(1L, as.integer(round(edges$len[sid] / 10)))
  samp <- st_coordinates(st_cast(st_line_sample(edge_geom[sid], n = n_s, type = "regular"), "POINT"))[, 1:2]

  list(G = G, node_xy = node_xy, edge_geom = edge_geom, edges = edges,
       dropped = edge_geom[dropped_ids], dropped_in_goal = e_in_goal[dropped_ids],
       samp_xy = samp, samp_eid = rep(sid, n_s), samp_n = setNames(n_s, sid),
       edge_wgs = st_transform(st_simplify(edge_geom, dTolerance = 2), 4326),
       boundary = st_transform(boundary, 4326), districts = st_transform(districts, 4326),
       version = cfg$network_version)
}

# Cached files hold UTF-8 street names; on a non-UTF-8 Windows locale R warns that it keeps them
# as UTF-8, which is exactly right - so only that warning is hidden.
read_cache <- function(f) withCallingHandlers(readRDS(f), warning = function(w)
  if (grepl("native encoding", conditionMessage(w))) invokeRestart("muffleWarning"))

load_network <- function(cfg, rebuild = FALSE) {
  dir.create(cfg$cache_dir, showWarnings = FALSE, recursive = TRUE)
  f <- file.path(cfg$cache_dir, paste0("network_", cfg$network_version, ".rds"))
  if (file.exists(f) && !rebuild) return(read_cache(f))
  t0 <- Sys.time()
  net <- build_network(cfg)
  saveRDS(net, f)
  message("Network built in ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), " min and cached.")
  net
}
