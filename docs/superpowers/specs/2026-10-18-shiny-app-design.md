# Ride Oslo – Shiny app design

Date: 2026-10-18 · Status: approved in chat, awaiting spec review

## Goal

Ride every street in Oslo municipality. A local Shiny app (run from RStudio, single user) that
plans the next ride and shows progress and predictions. `done_rides/` stays the only source of
truth for what has been ridden.

## Decisions (from the conversation)

- The app does everything: upload/drop ride → plan → download GPX. A thin `plan_next_ride.R`
  keeps working without the app, using the same functions.
- Only rides in `done_rides/` count (no Strava history).
- Goal area = Oslo municipality (480 km², incl. Marka). Routes start and end at home.
- Street rules for the goal ("what counts"):
  - In: public roads except motorway/trunk/primary (as today), service roads except
    driveway/parking aisle, cycleways, footways/pedestrian streets that are not sidewalks or
    crossings, paths that are paved/gravel/compacted or signed bicycle=yes/designated, tracks of
    grade1–2 or with a paved/gravel/compacted surface.
  - Out: steps, bicycle=no/dismount, private/no access, trails (path/track with surface
    ground/dirt/grass/mud/sand/rock, mtb:scale ≥ 1, tracktype grade3–5, untagged-surface paths
    in Marka are out unless bicycle-signed).
- Data source: OpenStreetMap only (Norway extract, Geofabrik). Bymiljøetaten WFS and the
  Kartverket WMS were considered and rejected (not a street network / images only).
- Districts: the 15 bydeler + Sentrum from OSM (admin_level 9), plus "Marka" = inside Oslo
  but outside every bydel.
- Settings editable in the app: ride length (km), roads only vs. include paths, tolerance for
  repeats (w_repeat). Everything else in `R/config.R`.

## Structure

```
app.R               Shiny UI + server (bslib page_navbar, leaflet, plotly, DT)
plan_next_ride.R    thin script: load config, plan, write next_ride/next_ride.gpx + progress/
R/config.R          all settings (home, paths, weights, buffers) as one list
R/osm_extract.R     one-off: Oslo boundary, districts, streets from the Norway pbf -> data/
R/network.R         street-rule filter, noding, graph build, cache (data/cache/)
R/rides.R           read .fit/.gpx/.csv, per-ride stats, per-ride coverage (cached)
R/planner.R         the existing route solver (logic unchanged), settings as arguments
R/stats.R           summary stats + predictions (pure functions)
R/export.R          GPX writer
tests/testthat/     tests for stats, predictions, readers, street rules
data/               oslo_boundary.gpkg, oslo_districts.gpkg, oslo_streets.gpkg, cache/
```

Data flow: `osm_extract` (once) → `network` (cached rds) → `rides` reads `done_rides/` and
caches coverage per file (keyed by name + size + mtime) → `stats` and `planner` consume network +
coverage → app and script render.

## Pages

1. **Ridden** – leaflet map. Streets coloured by the ride that first covered them; toggle to
   ridden / not ridden. Overlays: GPS tracks, districts shaded by % done, unreachable streets.
2. **Next ride** – sidebar: ride length, roads only / paths, repeat tolerance, "Plan next ride"
   (progress bar), "Download GPX", file upload (saved to `done_rides/`, triggers re-read).
   Main: route map (new / repeat / path dashed / walk), value boxes (km, % new, repeated km,
   ascent), elevation profile.
3. **Statistics**
   - Totals: rides, km, moving time, climbing, avg speed, km new street, % of Oslo.
   - Charts: cumulative coverage over time, new km per ride, efficiency (new ÷ ridden km) per
     ride, % done per district, done by way type (road / path / gravel), rides per week.
   - Ride table (DT).
   - Predictions: km left; new km per ride and per week (all time and last 4 weeks); rides left,
     riding km left, hours left, finish date at current pace; a distance-aware estimate that adds
     transit to and from remaining streets (from network distance to home); projection chart with
     an optimistic–pessimistic band. Shown as "n/a" until there is enough data (≥ 2 rides for
     per-week rates).

## Errors and edge cases

- Unreadable ride file: skipped with a visible notification, never crashes the app.
- No rides: maps and stats render empty states; planning works.
- Planning failure: error message in the app, previous plan kept.
- `next_ride/` always holds exactly one file: `next_ride.gpx`.

## Testing

- testthat: stats/prediction functions on small hand-made inputs; `.fit` and `.gpx` readers on the
  real ride; street-rule filter on tagged examples (sidewalk out, gravel track in, trail out).
- Manual: run the app with the real ride, check each page, plan once, download the GPX.

## Out of scope

Strava sync, multi-user/hosting, NVDB/Elveg comparison, turn-by-turn directions.
