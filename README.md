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

## Methods

### 1. The street network (the goal)

- **Source.** OpenStreetMap, Norway extract from Geofabrik. The Oslo municipality boundary
  (OSM admin level 7) and the 15 bydeler + Sentrum (level 9) come from the same file;
  everything inside Oslo but outside the bydeler is the district **Marka**
  (`R/osm_extract.R`).
- **What counts as a street** (`way_filter()` in `R/network.R`):
  - *In:* public roads except motorway/trunk/primary; service roads except driveways and
    parking aisles; cycleways; footways and pedestrian streets that are not sidewalks or
    crossings; paths that are paved/gravel/compacted or signed for bikes; tracks of grade 1-2
    or with a paved/gravel/compacted surface.
  - *Out:* steps, `bicycle=no`/`dismount`, private ways, and trails (dirt/ground surfaces,
    `mtb:scale` >= 1, tracktype grade 3-5).
  - Untagged paths and tracks count in the city, but in Marka they are most likely forest
    trails, so there they need a good surface or bike signs.
- **Routing margin.** Ways within 1 km of the border are kept so a route can use a street
  just outside Oslo, but only streets whose midpoint lies inside Oslo count toward the goal.
- **Building the graph.** OSM ways cross without sharing nodes, so all ways are merged and
  split at every crossing. Dead ends that stop within 2 m of another street are joined to it.
  A crossing between a bridge or tunnel and the street below is not a junction, so those
  false crossings are split apart again. Small disconnected pieces are joined to the main
  network with a straight "walk" link of at most 60 m; pieces farther away are reported as
  unreachable. Only the largest connected part is kept.
- Result: about **3,800 km** of street in the goal and 56 km that cannot be reached.

### 2. What counts as ridden

- Every street gets sample points about every 10 m.
- Each ride track is split at GPS gaps over 150 m and buffered by **12 m**.
- A street counts as ridden once **80 %** of its sample points lie inside the buffer of any
  ride in `done_rides/`.
- Rides are applied in time order, so each street records the ride that covered it first,
  and the "new street per ride" numbers add up to the total.

### 3. Planning the next ride

The task: a closed loop from home of about the chosen length that rides as much unridden
street as possible, wastes little on streets already ridden, and stays near home. This is a
*Rural Postman Problem* (visit a chosen set of streets with the shortest closed route), which
has no fast exact solution, so the planner (`R/planner.R`) uses a heuristic in five steps:

1. **Candidates.** Unridden goal streets that the chosen ride mode allows, within 7 km of home
   along the network. When everything inside that is ridden, the reach doubles until
   something is found.
2. **One seed per direction.** The compass around home is split into 12 sectors. In each
   sector the seed is the unridden street nearest home, nudged toward places with many
   unridden streets close together (score = network distance from home - 60 m x unridden
   neighbours), so a seed is not an isolated stub.
3. **Grow a cluster.** From each seed, unridden streets are added in order of
   *distance to the seed + 0.5 x distance from home* until their total length reaches a first
   guess: (ride length - 2 x distance to the seed) / 1.35. The 1.35 is the expected ratio of
   ride length to street length after links and repeats.
4. **Turn the cluster into a loop.**
   1. *Link.* Join the separate pieces of the cluster, and home, with shortest paths, nearest
      piece first. Link costs favour useful detours: streets already in the cluster are
      almost free, unridden streets are 35 % cheaper than ridden ones, footways/paths cost
      x1.25, walk links x4, and ways outside the chosen mode x5.
   2. *Make every junction even.* A loop that uses every street once needs an even number of
      street ends at every junction. The odd junctions are paired with a minimum-cost
      matching (`nbpMatching`) and the shortest path between each pair is ridden twice.
      Unridden streets are again discounted, so the extra riding picks up new streets where it
      can. This is where "second pass" km come from: dead ends ridden in and out, and these
      pairing paths.
   3. *Walk the loop.* An Eulerian circuit from home (Hierholzer's algorithm). Every valid
      circuit has the same length, so at each junction it continues as straight ahead as
      possible, which gives a ride that flows instead of zig-zagging.
5. **Score, tune and choose.**
   - Score = new km - *w_repeat* x repeated km - 0.4 x mean distance from home of the new km.
   - The 12 candidates are compared on score per km of the wanted length. The best 3 are
     length-tuned by rescaling their cluster (up to 5 tries) until the loop is within 8 % of
     the wanted length. The highest score among those wins.
   - Elevation comes from AWS terrain tiles (zoom 13). The route is written as
     `next_ride/next_ride.gpx`, and each part is labelled new street, second pass, ridden
     before, or walk.

Settings in the app:

- **Tolerance for repeating streets** sets *w_repeat*. 1 to 5 maps to 1.0, 0.75, 0.5, 0.3 and
  0.15: low tolerance accepts longer detours to avoid repeats, high tolerance gives tighter
  loops.
- **Ride on** limits which ways the route may use and which streets it targets: roads and
  cycleways only, plus footways and paths, or everything including gravel roads.

All other weights are in `R/config.R`.

### 4. Predictions

- **Pace** is measured two ways: over all rides, and over the last 4 weeks.
  - New street per ride
  - Efficiency: new street km / km ridden
  - Rides per week, from the time between rides; needs 2 rides on different days
- **Simple estimate:**
  - Rides left = street left / new street per ride
  - Km left to ride = street left / efficiency
  - Hours left = km left / average moving speed
  - Finish date = rides left / rides per week
- **Distance-aware estimate.** The streets left get farther from home, and every ride has to
  get there and back.
  - For past rides, the "working" part of each ride is ride km - 2 x the mean distance from
    home of the streets it covered first (at least 30 % of the ride). This gives the
    efficiency while actually exploring.
  - Future rides of average length then work through the remaining streets nearest-first,
    each spending 2 x the distance to the next street on getting there and back.
  - The number of simulated rides is the estimate. It is always the more pessimistic one,
    and the more realistic one later in the project.

### 5. Heatmap

- **Rides.** All bike rides from the Strava history (`data/strava/`) plus the project rides,
  clipped to the Oslo border.
- **Glow.** Oslo is divided into a 20 m grid. Each cell counts how many rides passed through
  it, on a log scale, so streets ridden hundreds of times do not drown out the rest. Two
  blurred copies (spreads of 40 m and 140 m) are added on top for the glow.
- **Street statistics.** A ride counts as having passed a street when half of the street lies
  within 20 m of the track. That is looser than the project rule, because the old Strava
  files are simplified outlines that cut corners.
  - A street name is credited to a ride when the ride covered at least 200 m of it, or half
    of a shorter street.
  - Names shorter than 250 m in total, such as junctions, are left out of the top list.
- "Oslo streets ever ridden" is therefore approximate (shown with ~). The project's own
  coverage (section 2) uses the strict rule and is not affected.

## Updating the map data

Download a fresh `norway-latest.osm.pbf` from Geofabrik, then:

```r
source("R/setup.R"); source("R/osm_extract.R")
extract_oslo("path/to/norway-latest.osm.pbf")
```

and bump `network_version` in `R/config.R` so the network is rebuilt. Do the same after changing
the street rules in `way_filter()` (`R/network.R`).
