# Packages and functions shared by app.R and plan_next_ride.R
options(encoding = "UTF-8")
suppressPackageStartupMessages({
  for (p in c("sf", "igraph", "nbpMatching", "dplyr", "leaflet", "elevatr", "terra", "FITfileR"))
    library(p, character.only = TRUE)
})
for (f in c("config.R", "network.R", "rides.R", "stats.R", "planner.R", "export.R", "heatmap.R"))
  source(file.path("R", f), encoding = "UTF-8")
