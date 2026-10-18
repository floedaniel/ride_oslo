# ------------------------------------------------------------
# Statistics and predictions. Pure functions on the network edge table and the rides table.
# ------------------------------------------------------------

goal_edges <- function(net) net$edges$in_net & !net$edges$conn & net$edges$in_goal

progress_totals <- function(net, prog) {
  g <- goal_edges(net); r <- prog$rides; len <- net$edges$len
  goal_km   <- sum(len[g]) / 1000
  ridden_km <- sum(len[g & prog$ridden]) / 1000
  list(rides = nrow(r), km = sum(r$km), moving_h = sum(r$moving_h, na.rm = TRUE),
       elapsed_h = sum(r$elapsed_h, na.rm = TRUE), ascent_m = sum(r$ascent_m, na.rm = TRUE),
       avg_kmh = if (sum(r$moving_h, na.rm = TRUE) > 0) sum(r$km[!is.na(r$moving_h)]) / sum(r$moving_h, na.rm = TRUE) else NA_real_,
       longest_km = if (nrow(r)) max(r$km) else NA_real_,
       goal_km = goal_km, ridden_km = ridden_km, remaining_km = goal_km - ridden_km,
       pct = if (goal_km > 0) 100 * ridden_km / goal_km else 0,
       efficiency = if (sum(r$km) > 0) ridden_km / sum(r$km) else NA_real_,
       unreachable_km = sum(as.numeric(st_length(net$dropped[net$dropped_in_goal]))) / 1000,
       first_date = if (nrow(r)) min(r$start) else NA, last_date = if (nrow(r)) max(r$start) else NA)
}

# Goal km, ridden km and % per group of a per-edge grouping (district, way class)
group_progress <- function(net, prog, group) {
  g <- goal_edges(net); len <- net$edges$len / 1000
  key <- group[g]
  out <- data.frame(group = sort(unique(key)), stringsAsFactors = FALSE)
  out$goal_km   <- as.numeric(tapply(len[g], key, sum)[out$group])
  out$ridden_km <- as.numeric(tapply(len[g] * prog$ridden[g], key, sum)[out$group])
  out$pct <- 100 * out$ridden_km / out$goal_km
  out[order(-out$pct, out$group), ]
}
district_progress <- function(net, prog) group_progress(net, prog, net$edges$district)
class_progress    <- function(net, prog) {
  lab <- c(road = "Roads & cycleways", path = "Footways & paths", gravel = "Gravel roads")
  group_progress(net, prog, unname(lab[net$edges$cls]))
}

weekly_rides <- function(rides) {
  if (nrow(rides) == 0) return(data.frame(week = as.Date(character(0)), rides = integer(0), km = numeric(0), new_km = numeric(0)))
  d <- as.Date(rides$start)
  wk <- d - (as.integer(format(d, "%u")) - 1)            # Monday of the week
  all_wk <- seq(min(wk), max(wk), by = 7)
  data.frame(week = all_wk,
             rides  = as.integer(table(factor(wk, levels = all_wk))),
             km     = as.numeric(tapply(rides$km, factor(wk, levels = all_wk), sum, default = 0)),
             new_km = as.numeric(tapply(rides$new_km, factor(wk, levels = all_wk), sum, default = 0)))
}

# Rides per week from the gaps between rides (needs at least 2 rides on different days)
rides_per_week <- function(start) {
  if (length(start) < 2) return(NA_real_)
  span_d <- as.numeric(difftime(max(start), min(start), units = "days"))
  if (span_d < 1) return(NA_real_)
  (length(start) - 1) / span_d * 7
}

# Pace and finish estimates.
#   rides         rides table (start, km, moving_h, new_km)
#   remaining_km  goal km not ridden yet
#   remaining     data frame (len_km, dist_km) of the unridden goal streets, dist_km = network
#                 distance from home; used by the distance-aware estimate
#   ride_dist_km  per ride: mean distance from home of the street it covered first
# Simple estimate: future rides keep today's new km per ride.
# Distance-aware: the remaining streets get farther away; a ride of average length spends
# 2 x distance getting there and back, and only the rest of the ride finds new street (at the
# rate measured on the "working" part of past rides).
predict_finish <- function(rides, remaining_km, remaining, ride_dist_km, today = Sys.time(), recent_days = 28) {
  n <- nrow(rides)
  na <- NA_real_
  recent <- n > 0 & as.numeric(difftime(today, rides$start, units = "days")) <= recent_days
  rate <- function(sel) {
    r <- rides[sel, , drop = FALSE]
    if (nrow(r) == 0) return(list(rides = 0L, new_per_ride = na, efficiency = na, rides_per_week = na,
                                  new_per_week = na, avg_kmh = na, ride_km = na))
    mh <- sum(r$moving_h, na.rm = TRUE)
    rpw <- rides_per_week(r$start)
    list(rides = nrow(r), new_per_ride = mean(r$new_km),
         efficiency = if (sum(r$km) > 0) sum(r$new_km) / sum(r$km) else na,
         rides_per_week = rpw, new_per_week = mean(r$new_km) * rpw,
         avg_kmh = if (mh > 0) sum(r$km[!is.na(r$moving_h)]) / mh else na, ride_km = mean(r$km))
  }
  all <- rate(rep(TRUE, n)); rec <- rate(recent)

  est <- function(p) {
    if (is.na(p$new_per_ride) || p$new_per_ride <= 0) return(list(rides_left = na, ride_km_left = na, hours_left = na, finish = NA))
    rides_left <- remaining_km / p$new_per_ride
    ride_km <- remaining_km / p$efficiency
    list(rides_left = rides_left, ride_km_left = ride_km, hours_left = ride_km / p$avg_kmh,
         finish = if (is.na(p$rides_per_week)) NA else as.Date(today) + rides_left / p$rides_per_week * 7)
  }

  # Distance-aware simulation, nearest streets first
  sim <- NULL; dist_est <- list(rides_left = na, ride_km_left = na, hours_left = na, finish = NA)
  if (n > 0 && nrow(remaining) > 0 && sum(rides$new_km) > 0) {
    work_km <- pmax(rides$km - 2 * ride_dist_km, 0.3 * rides$km)
    e_work <- sum(rides$new_km) / sum(work_km)
    L <- all$ride_km
    rem <- remaining[order(remaining$dist_km), ]
    cum <- cumsum(rem$len_km); total <- sum(rem$len_km)
    done <- 0; k <- 0; per_ride <- numeric(0)
    while (done < total && k < 1e5) {
      d <- rem$dist_km[min(which(cum > done)[1], nrow(rem))]
      add <- max(L - 2 * d, 0.3 * L) * e_work
      done <- min(total, done + add); k <- k + 1; per_ride[k] <- done
    }
    sim <- per_ride
    dist_est <- list(rides_left = k, ride_km_left = k * L, hours_left = k * L / all$avg_kmh,
                     finish = if (is.na(all$rides_per_week)) NA else as.Date(today) + k / all$rides_per_week * 7)
  }
  list(all = all, recent = rec, simple = est(all), simple_recent = est(rec), distance = dist_est,
       sim_cum_km = sim, remaining_km = remaining_km)
}

# Ridden km over time: history, plus the simple and distance-aware projections from today.
projection_series <- function(rides, pred, goal_km, today = Sys.time()) {
  hist <- if (nrow(rides)) data.frame(date = as.Date(rides$start), ridden_km = cumsum(rides$new_km), series = "Ridden") else
    data.frame(date = as.Date(character(0)), ridden_km = numeric(0), series = character(0))
  rpw <- pred$all$rides_per_week
  if (is.na(rpw) || is.na(pred$all$new_per_ride)) return(hist)
  now_km <- goal_km - pred$remaining_km; d0 <- as.Date(today)
  n_s <- ceiling(pred$simple$rides_left)
  k <- unique(round(seq(0, n_s, length.out = min(200, n_s + 1))))
  simple <- data.frame(date = d0 + k / rpw * 7, ridden_km = pmin(goal_km, now_km + k * pred$all$new_per_ride),
                       series = "At current pace")
  out <- rbind(hist, simple)
  if (!is.null(pred$sim_cum_km)) {
    s <- pred$sim_cum_km; k <- unique(round(seq(1, length(s), length.out = min(300, length(s)))))
    out <- rbind(out, data.frame(date = c(d0, d0 + k / rpw * 7), ridden_km = c(now_km, now_km + s[k]),
                                 series = "Distance-aware"))
  }
  out
}

# Trailing rolling mean: each value is the mean of itself and up to k-1 values before it
rolling_mean <- function(x, k) {
  if (length(x) == 0) return(numeric(0))
  vapply(seq_along(x), function(i) mean(x[max(1, i - k + 1):i]), numeric(1))
}
