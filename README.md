# Ride every street in Oslo

A Shiny app that plans the next loop from home covering as much unridden street as possible,
and shows what you have ridden, statistics and predictions. The goal is every street in Oslo
municipality: roads, cycleways, footways and paths, and Marka gravel roads (no trails).

## Run it

Open `ride_oslo.Rproj` in RStudio, open `app.R`, press **Run App** (or `shiny::runApp()`).

- **Ridden**: map of every street ridden (coloured by ride), what's left, % per district.
- **Next ride**: choose length / what to ride on / repeat tolerance, press **Plan next ride**,
  then **Download GPX**. Add a finished ride with the upload button.
- **Statistics**: totals, charts, per-district and per-way-type progress, all rides, and
  predictions (km left, rides/hours left, finish date - simple and distance-aware).
- **Heatmap**: glowing heatmap of every bike ride inside Oslo - the Strava history
  (`data/strava/`) plus the project rides - with year/source filters and stats (most ridden
  streets, rides per year, when you ride, km per district, records). Strava is display only:
  it never counts toward the project goal or the planner.

Without the app: `source("plan_next_ride.R")` does the same planning with the settings in
`R/config.R`.

## Workflow

1. Plan in the app, load `next_ride/next_ride.gpx` on the Wahoo.
2. Ride it.
3. Put the `.fit` file in `done_rides/` (upload in the app or copy it there).
4. Plan again.

`done_rides/` is the whole history: delete a file and those streets count as unridden again.

## Folders

| Folder            | What it is                                                          |
|-------------------|---------------------------------------------------------------------|
| `done_rides/`     | Every ride done for this project (`.fit`, `.gpx` or `lat,lon` `.csv`). |
| `next_ride/`      | Always exactly one file: `next_ride.gpx`.                           |
| `R/`              | The engine: config, network, rides, planner, stats, export.         |
| `tests/testthat/` | Unit tests: `testthat::test_dir("tests/testthat")` after `source("R/setup.R")`. |
| `data/`           | Oslo boundary, districts and streets from OSM; `strava/` holds the Strava history (heatmap only); `cache/` holds the built network, per-ride coverage and the last plan. |
| `legacy_scripts/`, `legacy_outputs/` | Old scripts and outputs. Not used.               |

## Updating the map data

Download a fresh `norway-latest.osm.pbf` from Geofabrik, then:

```r
source("R/setup.R"); source("R/osm_extract.R")
extract_oslo("path/to/norway-latest.osm.pbf")
```

and bump `network_version` in `R/config.R` so the network is rebuilt. Do the same after changing
the street rules in `way_filter()` (`R/network.R`).
