# ------------------------------------------------------------
# All settings in one place. The app overrides target_km, mode and w_repeat per plan.
# ------------------------------------------------------------

default_config <- function() list(
  home_lonlat = c(lon = 10.780919, lat = 59.932804),   # every route starts and ends here

  # The ride
  target_km    = 20,     # wanted length of the ride
  length_tol   = 0.08,   # a route within +/- this share of target_km is accepted
  max_reach_km = 7,      # first look for unridden streets this close to home (along the network);
                         # widened automatically once everything inside is ridden
  mode         = "all",  # which ways a route may use: "roads" (roads + cycleways),
                         # "roads_paths" (+ footways/paths), "all" (+ gravel tracks)

  # What the planner optimises (per route, in km, so they are directly comparable):
  #   score = new_km - w_repeat * repeated_km - w_home * (mean distance from home of the new km)
  w_repeat       = 0.5,   # cost of riding street you have already ridden
  w_home         = 0.4,   # cost of the new street being far from home
  w_cluster_home = 0.5,   # while growing a cluster, how strongly nearness to home beats nearness to the seed
  rho            = 1.35,  # first guess of ride length / street length (parity + links); refined by solving
  detour_bonus   = 0.35,  # a link or parity detour through an unridden street is this much cheaper
  conn_penalty   = 4,     # a straight-line dismount connector costs this many times its length
  path_link_mult = 1.25,  # a footway/path costs this many times its length when only a link or detour
  off_mode_mult  = 5,     # a way outside the chosen mode costs this many times its length (used only
                          # when there is no other way through)
  n_sectors      = 12,    # compass sectors that each propose a candidate route
  n_refine       = 3,     # best candidates that get length-tuned

  # Ridden detection: a street counts as ridden once ridden_min_cover of its length lies within
  # ridden_buffer_m of a recorded track
  ridden_buffer_m  = 12,
  ridden_min_cover = 0.8,
  track_gap_m      = 150,  # a jump longer than this in a track is a GPS dropout, not riding
  min_spur_m       = 0,    # dead-end stubs up to this long are optional. 0 = every street is required

  # Network build
  snap_m          = 2,     # dead-end ways ending this close to another street are joined to it
  max_bridge_m    = 60,    # longest straight-line dismount connector we'll invent
  network_version = "oslo_v1",   # bump after changing the street rules to force a rebuild

  # Files
  rides_dir      = "done_rides",
  next_dir       = "next_ride",
  progress_dir   = "progress",
  data_dir       = "data",
  cache_dir      = "data/cache",
  dem_zoom       = 13
)
