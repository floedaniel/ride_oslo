# ============================================================
# Ride every street in Oslo - plan the next ride without the app.
# Same engine as the app (app.R): reads done_rides/, writes next_ride/next_ride.gpx.
# Settings live in R/config.R; override them here if you like, e.g. cfg$target_km <- 30.
# ============================================================
source("R/setup.R")

cfg <- default_config()
net  <- load_network(cfg)          # first run builds the Oslo network (a few minutes), then cached
prog <- load_rides(cfg, net)
if (nrow(prog$skipped)) message("Skipped: ", paste(prog$skipped$file, prog$skipped$reason, sep = " - ", collapse = "; "))

tot <- progress_totals(net, prog)
cat(sprintf("%d ride(s) | %.1f of %.1f km of Oslo street ridden (%.2f%%)\n",
            tot$rides, tot$ridden_km, tot$goal_km, tot$pct))

plan <- plan_route(net, prog, cfg)
f <- save_next_ride(plan, cfg)
s <- plan$stats
cat(sprintf("Next ride: %.1f km | %.1f km new (%.0f%%) | %.1f km repeated | %.0f m climbing\nWrote %s\n",
            s$km, s$new_km, 100 * s$new_km / s$km, s$repeat_km, s$ascent_m, f))
