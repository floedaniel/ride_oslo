# ------------------------------------------------------------
# Plan the next ride: a closed loop from home of about target_km that maximises street not ridden
# yet, wasting little on streets already ridden and staying near home. Built per ride as a small
# Rural Postman problem (see solve_route()).
# ------------------------------------------------------------

mode_classes <- list(roads = "road", roads_paths = c("road", "path"), all = c("road", "path", "gravel"))

# Home node and network distance from home per node (dh) and per edge id (edh)
home_distances <- function(net, cfg) {
  home_utm <- st_transform(st_sfc(st_point(cfg$home_lonlat), crs = 4326), 25832)
  nx <- net$node_xy
  home_node <- st_nearest_feature(home_utm, st_as_sf(data.frame(x = nx[, 1], y = nx[, 2]), coords = c("x", "y"), crs = 25832))
  dh <- distances(net$G, v = home_node, weights = E(net$G)$len)[1, ]
  el <- ends(net$G, E(net$G), names = FALSE)
  edh <- rep(Inf, nrow(net$edges))
  edh[E(net$G)$eid] <- pmin(dh[el[, 1]], dh[el[, 2]])
  list(home_node = home_node, dh = dh, edh = edh)
}

# progress: optional function(value 0..1, detail) for a progress bar
plan_route <- function(net, prog, cfg, hd = home_distances(net, cfg), progress = function(value, detail) NULL) {
  G <- net$G; node_xy <- net$node_xy; edges <- net$edges
  home_node <- hd$home_node; dh <- hd$dh; edh <- hd$edh
  w_repeat <- cfg$w_repeat; w_home <- cfg$w_home; w_cluster_home <- cfg$w_cluster_home
  detour_bonus <- cfg$detour_bonus; conn_penalty <- cfg$conn_penalty; path_link_mult <- cfg$path_link_mult
  rho <- cfg$rho; length_tol <- cfg$length_tol; n_sectors <- cfg$n_sectors; n_refine <- cfg$n_refine
  target_m <- cfg$target_km * 1000

  # Arrays indexed by edge id (eid = position in edge_geom), so they survive subgraphs.
  n_eid <- nrow(edges)
  eu <- ev <- rep(1L, n_eid)
  el <- ends(G, E(G), names = FALSE)
  eu[E(G)$eid] <- el[, 1]; ev[E(G)$eid] <- el[, 2]
  elen <- ifelse(edges$in_net, edges$len, NA_real_)
  econn <- edges$conn; epath <- edges$path
  in_net <- edges$in_net
  allowed <- edges$cls %in% mode_classes[[cfg$mode]] | econn

  eunr_any <- in_net & !econn & edges$in_goal & !prog$ridden     # a street in Oslo not ridden yet
  unr_deg <- tabulate(c(eu[eunr_any & allowed], ev[eunr_any & allowed]), nbins = vcount(G))
  eunr_req <- eunr_any & allowed & !(elen <= cfg$min_spur_m & (unr_deg[eu] == 1L | unr_deg[ev] == 1L))
  eunr_req[is.na(eunr_req)] <- FALSE
  E(G)$unr_any <- (eunr_any & allowed)[E(G)$eid]
  E(G)$mult <- ifelse(E(G)$conn, conn_penalty, 1) * ifelse(E(G)$path, path_link_mult, 1) *
               ifelse(allowed[E(G)$eid], 1, cfg$off_mode_mult)
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
    conn_mult <- E(GL)$mult
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
    list(eid_seq = eid_seq, v_seq = v_seq, is_new = is_new, len_m = len_m, new_m = new_m,
         repeat_m = len_m - new_m,
         twice_new_m = sum(elen[eid_seq][!first & eunr_any[eid_seq]]),
         old_m = sum(elen[eid_seq][!eunr_any[eid_seq] & !econn[eid_seq]]),
         conn_m = sum(elen[eid_seq][econn[eid_seq]]),
         path_m = sum(elen[eid_seq][epath[eid_seq]]),
         mean_dh_m = mean_dh, max_dh_m = max(dh[v_seq]),
         score = new_m / 1000 - w_repeat * (len_m - new_m) / 1000 - w_home * mean_dh / 1000)
  }

  # Candidate streets: unridden, in Oslo, allowed by the mode, within reach. When everything
  # inside max_reach_km is ridden, reach doubles until something is found.
  reach_km <- cfg$max_reach_km
  repeat {
    pool <- which(eunr_req & is.finite(edh) & edh <= reach_km * 1000)
    if (length(pool) > 0L || reach_km > 100) break
    reach_km <- reach_km * 2
  }
  if (length(pool) == 0L) stop("No unridden streets left that this mode can reach - try another mode.")

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
    r <- solve_route(build_cluster(seed_eid, target_len))
    r$seed <- seed_eid; r$target <- target_len
    r
  }

  # One seed per compass sector: the unridden street nearest home, nudged toward dense unridden
  # areas so a seed is not an isolated stub.
  bearing <- (atan2(emid[pool, 1] - node_xy[home_node, 1], emid[pool, 2] - node_xy[home_node, 2]) * 180 / pi + 360) %% 360
  sector  <- floor(bearing / (360 / n_sectors))
  seed_score <- edh[pool] - 60 * (unr_deg[eu[pool]] + unr_deg[ev[pool]])
  seeds <- sapply(split(seq_along(pool), sector), function(ix) pool[ix[which.min(seed_score[ix])]])

  cands <- list()
  for (i in seq_along(seeds)) {
    progress(0.1 + 0.5 * i / length(seeds), sprintf("Trying direction %d of %d", i, length(seeds)))
    s <- seeds[i]
    t1 <- max(1500, (target_m - 2 * edh[s]) / rho)
    cands[[length(cands) + 1]] <- tryCatch(plan_from_seed(s, t1), error = function(e) NULL)
  }
  cands <- Filter(Negate(is.null), cands)
  if (length(cands) == 0L) stop("No route could be built from home.")

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
  progress(0.65, "Tuning the best routes to length")
  tuned <- lapply(cands[seq_len(min(n_refine, length(cands)))], refine)
  ok <- Filter(function(r) abs(r$len_m - target_m) / target_m <= length_tol, tuned)
  chosen_set <- if (length(ok) > 0) ok else tuned
  best <- chosen_set[[which.max(sapply(chosen_set, function(r) r$score))]]

  # Geometry of the route, in riding order
  oriented_coords <- function(eid, from_node) {
    gm <- st_coordinates(net$edge_geom[[eid]])[, 1:2, drop = FALSE]
    if (max(abs(gm[1, ] - node_xy[from_node, ])) > 0.01) gm <- gm[nrow(gm):1, , drop = FALSE]
    gm
  }
  n_step <- length(best$eid_seq)
  segs <- lapply(seq_len(n_step), function(i) oriented_coords(best$eid_seq[i], best$v_seq[i]))
  route_xy <- do.call(rbind, lapply(seq_len(n_step), function(i) if (i > 1) segs[[i]][-1, , drop = FALSE] else segs[[i]]))
  # Repeats come in two kinds: a street new in this ride but passed a second time (dead ends,
  # links, odd junctions), and a street already covered by an earlier ride
  seg_type <- ifelse(econn[best$eid_seq], "walk (gap in the map)",
              ifelse(best$is_new, "new street",
              ifelse(eunr_any[best$eid_seq], "second pass", "ridden before")))
  seg_sf <- st_sf(type = seg_type,
                  cls = edges$cls[best$eid_seq], name = edges$name[best$eid_seq],
                  geometry = st_sfc(lapply(segs, st_linestring), crs = 25832))

  progress(0.8, "Looking up elevation")
  pts_wgs <- st_transform(st_as_sf(as.data.frame(route_xy), coords = c("X", "Y"), crs = 25832), 4326)
  ele <- route_elevation(pts_wgs, cfg$dem_zoom)
  route <- data.frame(lon = st_coordinates(pts_wgs)[, 1], lat = st_coordinates(pts_wgs)[, 2], ele = ele,
                      km = c(0, cumsum(sqrt(rowSums(diff(route_xy)^2)))) / 1000)

  list(route = route, segs = st_transform(seg_sf, 4326),
       stats = list(km = best$len_m / 1000, new_km = best$new_m / 1000, repeat_km = best$repeat_m / 1000,
                    twice_new_km = best$twice_new_m / 1000, old_km = best$old_m / 1000,
                    walk_km = best$conn_m / 1000, path_km = best$path_m / 1000,
                    far_km = best$max_dh_m / 1000, mean_dist_km = best$mean_dh_m / 1000,
                    ascent_m = ascent_m(ele), reach_km = reach_km),
       settings = cfg[c("target_km", "mode", "w_repeat")], planned_at = Sys.time(),
       rides_n = nrow(prog$rides), ride_files = prog$rides$file)
}
