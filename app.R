# ============================================================
# Ride every street in Oslo - Shiny app
# Run: shiny::runApp() in this folder (or "Run App" in RStudio).
# Pages: Ridden (map of everything ridden), Next ride (plan + download), Statistics (+ predictions)
# ============================================================
source("R/setup.R")
suppressPackageStartupMessages({ library(shiny); library(bslib); library(plotly); library(DT) })
# Pin the UI functions to bslib/plotly so packages loaded in the session (shinydashboard, bs4Dash,
# graphics::layout, ...) cannot mask them
for (fn in c("page_navbar", "nav_panel", "nav_spacer", "nav_item", "card", "card_header", "card_footer",
             "value_box", "value_box_theme", "layout_columns", "layout_sidebar", "sidebar", "bs_theme"))
  assign(fn, getExportedValue("bslib", fn))
layout <- plotly::layout

cfg <- default_config()
net <- load_network(cfg)
hd  <- home_distances(net, cfg)
strava_acts <- strava_activities(cfg, net)     # Strava history: heatmap only, never the project goal
goal <- goal_edges(net)
home <- cfg$home_lonlat

col_new <- "#2e7d32"; col_repeat <- "#ef6c00"; col_old <- "#795548"; col_walk <- "#d500f9"; col_todo <- "#c62828"
fmt  <- function(x, d = 1) ifelse(is.na(x), "n/a", formatC(x, format = "f", digits = d, big.mark = " "))
km_txt <- function(x, d = 1) ifelse(is.na(x), "n/a", paste(fmt(x, d), "km"))
fmt_h <- function(h) ifelse(is.na(h), "n/a", sprintf("%d:%02d", floor(h), round((h %% 1) * 60)))
fmt_d <- function(d) if (length(d) == 0 || is.na(d)) "n/a" else format(as.Date(d), "%d %b %Y")
mode_labels <- c("Roads & cycleways" = "roads", "+ footways & paths" = "roads_paths", "+ gravel roads (everything)" = "all")

base_groups <- c("Esri streets", "CyclOSM", "OSM (DE)", "Satellite")
# Every map opens on Esri streets; the other backgrounds stay in the layer menu
default_base <- function(m) hideGroup(m, setdiff(base_groups, "Esri streets"))
base_map <- function() {
  leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
    # Same backgrounds as the legacy scripts - none need an API key (CartoDB/Stadia/Thunderforest do)
    addProviderTiles(providers$CyclOSM, group = "CyclOSM") |>
    addProviderTiles(providers$OpenStreetMap.DE, group = "OSM (DE)") |>
    addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri streets") |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
    addCircleMarkers(lng = home[["lon"]], lat = home[["lat"]], radius = 7, color = "black",
                     fillColor = "white", fillOpacity = 1, weight = 2, label = "Home")
}
# Date x-axis that stays readable with a single date (plotly would zoom to milliseconds)
date_axis <- function(d, pad_days = 10) {
  ax <- list(type = "date", tickformat = "%d %b %Y", title = "")
  if (length(unique(d)) <= 1 && length(d)) ax$range <- as.character(c(min(d) - pad_days, max(d) + pad_days))
  ax
}
# Heatmap page styling
col_strava <- "#b5338a"; col_project <- "#fe9f6d"
heat_pal <- colorNumeric(colorRampPalette(c("#1a053300", "#4a0c6b99", "#8c2981dd", "#de4968", "#fe9f6d", "#fdd49e", "#ffffff"),
                                          alpha = TRUE)(256), domain = c(0, 1), na.color = "transparent", alpha = TRUE)
heat_vb <- function(title, id, sub = NULL, tone)
  value_box(title, textOutput(id), if (!is.null(sub)) p(textOutput(sub, inline = TRUE)), class = tone)
heat_years_all <- range(c(as.integer(format(do.call(c, lapply(strava_acts, `[[`, "date")), "%Y")), as.integer(format(Sys.Date(), "%Y"))))
card_plot <- function(title, id, height = "300px") card(full_screen = TRUE, card_header(title), plotlyOutput(id, height = height))

# ---------------------------------------------------------------- UI
ui <- page_navbar(
  title = "Ride Oslo", id = "page",
  # App chrome: summer fjord palette (www/ride.css); charts and maps keep their own colours
  theme = bs_theme(version = 5, bg = "#FAEFED", fg = "#0E3540", primary = "#006078", secondary = "#82BAC4",
                   info = "#82BAC4", warning = "#E37C78", "navbar-bg" = "#006078",
                   base_font = font_google("Figtree", local = TRUE),
                   heading_font = font_google("Familjen Grotesk", local = TRUE),
                   "border-radius" = "0.6rem"),
  fillable = FALSE,
  header = tags$head(tags$link(rel = "stylesheet", href = "ride.css")),

  nav_panel("Ridden",
    layout_columns(fill = FALSE, col_widths = c(3, 3, 3, 3),
      value_box("Oslo done", textOutput("vb_pct"), p(textOutput("vb_pct_sub", inline = TRUE)), class = "vb-hero"),
      value_box("Street ridden", textOutput("vb_ridden"), class = "tone-sky"),
      value_box("Left to ride", textOutput("vb_left"), class = "tone-rose"),
      value_box("Rides", textOutput("vb_rides"), class = "tone-coral")),
    card(full_screen = TRUE, leafletOutput("map_ridden", height = "calc(100vh - 260px)"))
  ),

  nav_panel("Next ride",
    layout_sidebar(
      sidebar = sidebar(width = 300,
        sliderInput("target_km", "Ride length (km)", min = 5, max = 80, value = cfg$target_km, step = 1),
        radioButtons("mode", "Ride on", choices = mode_labels, selected = cfg$mode),
        sliderInput("repeat_tol", "Tolerance for repeating streets", min = 1, max = 5, value = 3, step = 1,
                    ticks = FALSE),
        helpText("1 = avoid repeats hard (detours to new streets), 5 = don't mind repeats (shorter, tighter loops)."),
        actionButton("plan", "Plan next ride", class = "btn-coral btn-lg w-100"),
        downloadButton("download", "Download GPX", class = "btn-outline-primary w-100"),
        hr(),
        fileInput("upload", "Add a finished ride (.fit, .gpx)", accept = c(".fit", ".gpx", ".csv"), multiple = TRUE),
        helpText("Saved to done_rides/. Or copy the file there yourself - the app notices.")
      ),
      uiOutput("plan_warning"),
      layout_columns(fill = FALSE, col_widths = c(3, 3, 3, 3),
        value_box("Distance", textOutput("vb_plan_km"), class = "vb-hero"),
        value_box("New street", textOutput("vb_plan_new"), p(textOutput("vb_plan_new_pct", inline = TRUE)), class = "tone-sky"),
        value_box("Repeated", textOutput("vb_plan_rep"), p(textOutput("vb_plan_rep_sub", inline = TRUE)), class = "tone-rose"),
        value_box("Climbing", textOutput("vb_plan_asc"), p(textOutput("vb_plan_far", inline = TRUE)), class = "tone-coral")),
      card(full_screen = TRUE, leafletOutput("map_next", height = "calc(100vh - 330px)")),
      card(fill = FALSE, card_header("Elevation"), plotlyOutput("elev", height = "180px"))
    )
  ),

  nav_panel("Statistics",
    layout_columns(fill = FALSE, col_widths = c(2, 2, 2, 2, 2, 2),
      value_box("Oslo done", textOutput("st_pct"), class = "vb-hero"),
      value_box("Distance ridden", textOutput("st_km"), class = "tone-sky"),
      value_box("Moving time", textOutput("st_time"), class = "tone-rose"),
      value_box("Climbing", textOutput("st_asc"), class = "tone-coral"),
      value_box("Avg speed", textOutput("st_speed"), class = "tone-sky"),
      value_box("Efficiency", textOutput("st_eff"), p("new street per km ridden"), class = "tone-rose")),
    layout_columns(col_widths = c(5, 7),
      card(card_header("Predictions"), tableOutput("pred_table"),
           card_footer(textOutput("pred_note"))),
      card_plot("Ridden street over time and projection", "p_projection", "380px")),
    layout_columns(col_widths = c(6, 6),
      card_plot("Efficiency over time", "p_new_per_ride"),
      card_plot("Pace over time", "p_weekly")),
    layout_columns(col_widths = c(7, 5),
      card_plot("Progress per district", "p_district", "460px"),
      layout_columns(col_widths = 12,
        card_plot("Progress per way type", "p_class", "200px"),
        card(card_header("More numbers"), tableOutput("more_table")))),
    card(fill = FALSE, card_header("All rides"), DTOutput("ride_table", fill = FALSE))
  ),
  nav_panel("Heatmap",
    layout_sidebar(
      sidebar = sidebar(width = 280,
        sliderInput("heat_years", "Years", min = heat_years_all[1], max = heat_years_all[2], value = heat_years_all,
                    step = 1, sep = "", ticks = FALSE),
        checkboxGroupInput("heat_src", "Rides from", choices = c("Strava history", "Ride Oslo project"),
                           selected = c("Strava history", "Ride Oslo project")),
        tags$p(class = "sidebar-note", "Every bike ride inside Oslo. Strava rides are shown here only - they do not count toward the project goal or the planner. Zoom in to see the individual tracks.")
      ),
      layout_columns(fill = FALSE, col_widths = c(3, 3, 3, 3),
        heat_vb("Rides in Oslo", "hv_rides", "hv_rides_sub", "vb-hero"),
        heat_vb("Km inside Oslo", "hv_km", "hv_km_sub", "tone-sky"),
        heat_vb("Oslo streets ever ridden", "hv_pct", "hv_pct_sub", "tone-rose"),
        heat_vb("Hottest street", "hv_top", "hv_top_sub", "tone-coral")),
      card(full_screen = TRUE, leafletOutput("map_heat", height = "calc(100vh - 330px)")),
      layout_columns(col_widths = c(4, 4, 4),
        card(card_header("Most ridden streets"), tableOutput("heat_top")),
        card(full_screen = TRUE, card_header("Rides per year"), plotlyOutput("heat_years_plot", height = "320px")),
        card(full_screen = TRUE, card_header("When you ride"), plotlyOutput("heat_calendar", height = "320px"))),
      layout_columns(col_widths = c(6, 6),
        card(full_screen = TRUE, card_header("Km ridden per district"), plotlyOutput("heat_district", height = "380px")),
        card(card_header("Records"), tableOutput("heat_records")))
    )
  ),
  nav_spacer(),
  nav_item(tags$small(class = "text-muted", textOutput("updated", inline = TRUE)))
)

# ---------------------------------------------------------------- server
server <- function(input, output, session) {

  # done_rides/ is watched: adding or removing a file re-reads (only new files cost time)
  rides_sig <- reactivePoll(2000, session,
    checkFunc = function() { f <- ride_files(cfg); paste(f, file.mtime(f), file.size(f), collapse = "|") },
    valueFunc = function() ride_files(cfg))

  prog <- reactive({
    files <- rides_sig()
    withProgress(message = "Reading rides", value = 0.3, load_rides(cfg, net, files))
  })
  observeEvent(prog(), {
    sk <- prog()$skipped
    for (i in seq_len(nrow(sk))) showNotification(sprintf("Skipped %s: %s", sk$file[i], sk$reason[i]), type = "warning", duration = 10)
  })

  tot <- reactive(progress_totals(net, prog()))
  ride_dist <- reactive({                      # mean distance from home of the street each ride covered first
    p <- prog(); fr <- p$first_ride; ok <- !is.na(fr) & goal & is.finite(hd$edh)
    num <- tapply(net$edges$len[ok] * hd$edh[ok], fr[ok], sum); den <- tapply(net$edges$len[ok], fr[ok], sum)
    out <- rep(0, nrow(p$rides)); out[as.integer(names(num))] <- num / den / 1000
    out
  })
  pred <- reactive({
    p <- prog(); rem <- goal & !p$ridden & is.finite(hd$edh)
    predict_finish(p$rides, tot()$remaining_km,
                   data.frame(len_km = net$edges$len[rem] / 1000, dist_km = hd$edh[rem] / 1000), ride_dist())
  })
  output$updated <- renderText(sprintf("%d rides in done_rides/", nrow(prog()$rides)))

  heat_server(input, output, session, prog)

  # ---------------- Ridden page
  output$vb_pct     <- renderText(paste0(fmt(tot()$pct, 2), " %"))
  output$vb_pct_sub <- renderText(sprintf("of %s km of street in Oslo", fmt(tot()$goal_km, 0)))
  output$vb_ridden  <- renderText(paste(fmt(tot()$ridden_km), "km"))
  output$vb_left    <- renderText(paste(fmt(tot()$remaining_km, 0), "km"))
  output$vb_rides   <- renderText(tot()$rides)

  output$map_ridden <- renderLeaflet({
    p <- prog(); dp <- district_progress(net, p)
    d <- net$districts; d$pct <- dp$pct[match(d$district, dp$group)]; d$pct[is.na(d$pct)] <- 0
    pal_d <- colorNumeric("Greens", domain = c(0, max(1, d$pct)))
    m <- base_map() |>
      addPolygons(data = d, fillColor = ~pal_d(pct), fillOpacity = 0.45, color = "#555", weight = 1,
                  label = ~sprintf("%s: %.1f %% done", district, pct), group = "Districts") |>
      addPolylines(data = net$boundary, color = "#333", weight = 2, fill = FALSE, group = "Oslo border")
    todo <- goal & !p$ridden
    for (k in c(road = "roads", path = "paths", gravel = "gravel roads") |> names()) {
      ids <- which(todo & net$edges$cls == k)
      if (length(ids)) m <- m |> addPolylines(data = st_combine(net$edge_wgs[ids]), color = col_todo, weight = 1,
                                             opacity = 0.35, dashArray = if (k == "road") NULL else "3,3",
                                             group = paste("Not ridden:", c(road = "roads", path = "paths", gravel = "gravel")[k]))
    }
    ids <- which(p$ridden & net$edges$in_net)
    groups_over <- c("Districts", "Oslo border", "Not ridden: roads", "Not ridden: paths", "Not ridden: gravel")
    if (length(ids)) {
      rs <- st_sf(ride = p$first_ride[ids], geometry = net$edge_wgs[ids])
      rs$date <- format(as.Date(p$rides$start[rs$ride]), "%d %b %Y")
      pal_r <- colorNumeric("viridis", domain = c(1, max(2, nrow(p$rides))))
      m <- m |> addPolylines(data = rs, color = ~pal_r(ride), weight = 3.5, opacity = 0.95,
                             label = ~sprintf("First ridden on ride %d (%s)", ride, date), group = "Ridden streets") |>
        addPolylines(data = st_sf(geometry = p$tracks), color = "#1565c0", weight = 1.5, opacity = 0.6, group = "GPS tracks") |>
        addLegend("bottomright", pal = pal_r, values = c(1, max(2, nrow(p$rides))), title = "First ridden<br>on ride #")
      groups_over <- c("Ridden streets", "GPS tracks", groups_over)
    }
    if (any(net$dropped_in_goal))
      m <- m |> addPolylines(data = st_transform(net$dropped[net$dropped_in_goal], 4326), color = "#000", weight = 2,
                             label = "Cut off from the network - cannot be planned", group = "Unreachable")
    m |> addLayersControl(baseGroups = base_groups,
                          overlayGroups = c(groups_over, "Unreachable"),
                          options = layersControlOptions(collapsed = TRUE)) |>
      hideGroup(c("Districts", "Oslo border", "GPS tracks", "Unreachable", "Not ridden: roads", "Not ridden: paths",
                  "Not ridden: gravel")) |>
      default_base() |>
      setView(home[["lon"]], home[["lat"]], zoom = 13)
  })

  # ---------------- Next ride page
  plan <- reactiveVal(load_last_plan(cfg))

  observeEvent(input$upload, {
    up <- input$upload
    dir.create(cfg$rides_dir, showWarnings = FALSE)
    ok <- file.copy(up$datapath, file.path(cfg$rides_dir, up$name), overwrite = TRUE)
    showNotification(sprintf("Added %d ride file(s) to done_rides/", sum(ok)), type = "message")
  })

  observeEvent(input$plan, {
    c2 <- cfg; c2$target_km <- input$target_km; c2$mode <- input$mode
    c2$w_repeat <- c(1.0, 0.75, 0.5, 0.3, 0.15)[input$repeat_tol]
    p <- withProgress(message = "Planning the next ride", value = 0.05, {
      tryCatch(plan_route(net, prog(), c2, hd, progress = function(v, d) setProgress(v, detail = d)),
               error = function(e) { showNotification(paste("Planning failed:", conditionMessage(e)), type = "error", duration = NULL); NULL })
    })
    if (!is.null(p)) { save_next_ride(p, cfg); plan(p); showNotification("New route saved to next_ride/next_ride.gpx", type = "message") }
  })

  output$plan_warning <- renderUI({
    p <- plan()
    if (is.null(p)) return(div(class = "alert alert-info", "No route planned yet - press 'Plan next ride'."))
    if (!setequal(p$ride_files, prog()$rides$file))
      div(class = "alert alert-warning", "Your rides have changed since this route was planned - plan again to use them.")
  })
  ps <- reactive(plan()$stats)
  output$vb_plan_km      <- renderText(if (is.null(ps())) "-" else paste(fmt(ps()$km), "km"))
  output$vb_plan_new     <- renderText(if (is.null(ps())) "-" else paste(fmt(ps()$new_km), "km"))
  output$vb_plan_new_pct <- renderText(if (is.null(ps())) "" else sprintf("%.0f %% of the ride", 100 * ps()$new_km / ps()$km))
  output$vb_plan_rep     <- renderText(if (is.null(ps())) "-" else paste(fmt(ps()$repeat_km), "km"))
  output$vb_plan_rep_sub <- renderText(if (is.null(ps())) "" else sprintf("%s km second passes, %s km ridden before, %s km walking", fmt(ps()$twice_new_km), fmt(ps()$old_km), fmt(ps()$walk_km)))
  output$vb_plan_asc     <- renderText(if (is.null(ps())) "-" else paste(fmt(ps()$ascent_m, 0), "m"))
  output$vb_plan_far     <- renderText(if (is.null(ps())) "" else sprintf("farthest %s km from home", fmt(ps()$far_km)))

  output$map_next <- renderLeaflet({
    m <- base_map(); p <- plan()
    rid <- which(prog()$ridden & net$edges$in_net)
    if (length(rid)) m <- m |> addPolylines(data = st_combine(net$edge_wgs[rid]), color = "#888", weight = 2, opacity = 0.5, group = "Already ridden")
    if (is.null(p)) return(m |> hideGroup("Already ridden") |> default_base() |> setView(home[["lon"]], home[["lat"]], zoom = 13))
    # "repeat" = plans made before second pass / ridden before were split
    lv <- c("new street", "second pass", "ridden before", "walk (gap in the map)", "repeat")
    pal <- colorFactor(c(col_new, col_repeat, col_old, col_walk, col_repeat), levels = lv)
    s <- p$segs
    lab <- ifelse(is.na(s$name), s$type, paste0(s$name, " - ", s$type))
    m |> addPolylines(data = s[s$cls == "road" | s$cls == "connector", ], color = ~pal(type), weight = 5, opacity = 0.9,
                      label = lab[s$cls == "road" | s$cls == "connector"], group = "Route") |>
      addPolylines(data = s[s$cls %in% c("path", "gravel"), ], color = ~pal(type), weight = 5, opacity = 0.9, dashArray = "8,6",
                   label = paste(lab[s$cls %in% c("path", "gravel")], "(path / gravel)"), group = "Route") |>
      addLegend("bottomright", colors = pal(intersect(lv, s$type)), labels = intersect(lv, s$type), title = "Dashed = path / gravel") |>
      hideGroup("Already ridden") |> default_base() |>
      addLayersControl(baseGroups = base_groups, overlayGroups = c("Route", "Already ridden"),
                       options = layersControlOptions(collapsed = TRUE)) |>
      fitBounds(min(p$route$lon), min(p$route$lat), max(p$route$lon), max(p$route$lat))
  })

  output$elev <- renderPlotly({
    p <- plan(); req(p, any(!is.na(p$route$ele)))
    plot_ly(p$route, x = ~km, y = ~ele, type = "scatter", mode = "lines", fill = "tozeroy", source = "elev",
            line = list(color = col_new), hovertemplate = "%{x:.1f} km<br>%{y:.0f} m<extra></extra>") |>
      layout(xaxis = list(title = "km", range = c(0, max(p$route$km))), yaxis = list(title = "m", rangemode = "normal"),
             margin = list(t = 5, b = 30), hovermode = "x") |>
      event_register("plotly_hover")
  })
  # Hovering the elevation profile shows that point on the route map
  # suppressWarnings: the listener exists before the chart is drawn (no plan yet, or tab not opened)
  observeEvent(suppressWarnings(event_data("plotly_hover", source = "elev")), {
    h <- suppressWarnings(event_data("plotly_hover", source = "elev")); p <- plan(); req(p, h$x)
    i <- which.min(abs(p$route$km - h$x[1]))
    leafletProxy("map_next") |> clearGroup("hover") |>
      addCircleMarkers(lng = p$route$lon[i], lat = p$route$lat[i], radius = 8, color = "black", weight = 2,
                       fillColor = "yellow", fillOpacity = 1, group = "hover", label = sprintf("%.1f km", p$route$km[i]))
  })

  output$download <- downloadHandler(
    filename = function() "next_ride.gpx",
    content = function(file) {
      src <- file.path(cfg$next_dir, "next_ride.gpx")
      validate(need(file.exists(src), "Plan a ride first"))
      file.copy(src, file)
    })

  # ---------------- Statistics page
  output$st_pct   <- renderText(paste0(fmt(tot()$pct, 2), " %"))
  output$st_km    <- renderText(paste(fmt(tot()$km), "km"))
  output$st_time  <- renderText(fmt_h(tot()$moving_h))
  output$st_asc   <- renderText(paste(fmt(tot()$ascent_m, 0), "m"))
  output$st_speed <- renderText(paste(fmt(tot()$avg_kmh), "km/h"))
  output$st_eff   <- renderText(paste0(fmt(100 * tot()$efficiency, 0), " %"))

  output$pred_table <- renderTable({
    pr <- pred(); a <- pr$all; r <- pr$recent
    row <- function(label, x, y, z) data.frame(" " = label, "All-time pace" = x, "Last 4 weeks" = y, "Distance-aware" = z, check.names = FALSE)
    rbind(
      row("Street left", km_txt(pr$remaining_km, 0), "", ""),
      row("New street per ride", km_txt(a$new_per_ride), km_txt(r$new_per_ride), "drops with distance"),
      row("New street per week", km_txt(a$new_per_week), km_txt(r$new_per_week), ""),
      row("Rides per week", fmt(a$rides_per_week), fmt(r$rides_per_week), fmt(a$rides_per_week)),
      row("Rides left", fmt(pr$simple$rides_left, 0), fmt(pr$simple_recent$rides_left, 0), fmt(pr$distance$rides_left, 0)),
      row("Km left to ride", fmt(pr$simple$ride_km_left, 0), fmt(pr$simple_recent$ride_km_left, 0), fmt(pr$distance$ride_km_left, 0)),
      row("Riding hours left", fmt_h(pr$simple$hours_left), fmt_h(pr$simple_recent$hours_left), fmt_h(pr$distance$hours_left)),
      row("Finished on", fmt_d(pr$simple$finish), fmt_d(pr$simple_recent$finish), fmt_d(pr$distance$finish)))
  }, striped = TRUE, spacing = "s", width = "100%", align = "lrrr")
  output$pred_note <- renderText(paste(
    "Distance-aware: the streets left get farther from home, so an average ride spends more of itself getting there and back.",
    if (is.na(pred()$all$rides_per_week)) "Rides per week and finish dates need at least 2 rides on different days." else ""))

  output$p_projection <- renderPlotly({
    s <- projection_series(prog()$rides, pred(), tot()$goal_km)
    req(nrow(s) > 0)
    cols <- c("Ridden" = col_new, "At current pace" = "#1565c0", "Distance-aware" = col_repeat)
    plot_ly(s, x = ~date, y = ~ridden_km, color = ~series, colors = cols, type = "scatter", mode = "lines+markers",
            marker = list(size = 4), hovertemplate = "%{x}<br>%{y:.0f} km<extra>%{fullData.name}</extra>") |>
      add_lines(x = range(s$date), y = tot()$goal_km, color = I("#999"), line = list(dash = "dot"), name = "All of Oslo", inherit = FALSE) |>
      layout(yaxis = list(title = "km of street ridden"), xaxis = date_axis(s$date), legend = list(orientation = "h"))
  })

  # Development over time: one unit per chart, date axis, faint per-ride/per-week line + bold rolling average
  output$p_new_per_ride <- renderPlotly({
    r <- prog()$rides; req(nrow(r) > 0)
    r$date <- as.POSIXct(r$start); r$eff <- 100 * r$new_km / r$km
    r$avg <- rolling_mean(r$eff, 5)
    plot_ly(r, x = ~date) |>
      add_trace(y = ~eff, name = "Each ride", type = "scatter", mode = "lines+markers",
                line = list(color = "#a5d6a7", width = 1), marker = list(color = col_new, size = 8),
                text = ~sprintf("%s<br>%.1f km ridden, %.1f km new street", file, km, new_km),
                hovertemplate = "%{y:.0f} % new<br>%{text}<extra></extra>") |>
      add_trace(y = ~avg, name = "Average of last 5 rides", type = "scatter", mode = "lines",
                line = list(color = col_new, width = 3), hovertemplate = "Last 5 rides: %{y:.0f} %<extra></extra>") |>
      layout(xaxis = date_axis(as.Date(r$date)), yaxis = list(title = "% of the ride on new street", range = c(0, 105)),
             legend = list(orientation = "h"))
  })

  output$p_weekly <- renderPlotly({
    w <- weekly_rides(prog()$rides); req(nrow(w) > 0)
    w$avg <- rolling_mean(w$new_km, 4)
    plot_ly(w, x = ~week) |>
      add_trace(y = ~new_km, name = "Each week", type = "scatter", mode = "lines+markers",
                line = list(color = "#a5d6a7", width = 1), marker = list(color = col_new, size = 7),
                text = ~sprintf("%d ride(s), %.1f km ridden", rides, km),
                hovertemplate = "Week of %{x|%d %b}: %{y:.1f} km new street<br>%{text}<extra></extra>") |>
      add_trace(y = ~avg, name = "4-week average", type = "scatter", mode = "lines",
                line = list(color = col_new, width = 3), hovertemplate = "4-week average: %{y:.1f} km/week<extra></extra>") |>
      layout(xaxis = date_axis(w$week, pad_days = 14), yaxis = list(title = "new street km per week", rangemode = "tozero"),
             legend = list(orientation = "h"))
  })

  bar_progress <- function(d) {
    d$group <- factor(d$group, levels = rev(d$group))
    plot_ly(d, y = ~group, x = ~pct, type = "bar", orientation = "h", marker = list(color = col_new),
            text = ~sprintf("%.1f %% (%.0f of %.0f km)", pct, ridden_km, goal_km), textposition = "none",
            hovertemplate = "%{y}: %{text}<extra></extra>") |>
      layout(xaxis = list(title = "% ridden", rangemode = "tozero"), yaxis = list(title = ""), margin = list(l = 10))
  }
  output$p_district <- renderPlotly(bar_progress(district_progress(net, prog())))
  output$p_class    <- renderPlotly(bar_progress(class_progress(net, prog())))

  output$more_table <- renderTable({
    t <- tot(); r <- prog()$rides
    data.frame(What = c("Street in Oslo (goal)", "Unreachable (cut off in the map)", "Longest ride", "Average ride",
                        "Average new street per ride", "Elapsed time (incl. stops)", "First ride", "Latest ride"),
               Value = c(km_txt(t$goal_km, 0), km_txt(t$unreachable_km), km_txt(t$longest_km),
                         paste(fmt(if (nrow(r)) mean(r$km) else NA), "km"), paste(fmt(if (nrow(r)) mean(r$new_km) else NA), "km"),
                         fmt_h(t$elapsed_h), fmt_d(t$first_date), fmt_d(t$last_date)))
  }, striped = TRUE, spacing = "s", width = "100%", align = "lr")

  output$ride_table <- renderDT({
    r <- prog()$rides
    datatable(data.frame(`#` = r$ride, Date = format(r$start, "%Y-%m-%d %H:%M"), File = r$file, km = round(r$km, 1),
                         `New street km` = round(r$new_km, 1), `Efficiency %` = round(100 * r$new_km / r$km),
                         Moving = fmt_h(r$moving_h), `km/h` = round(r$avg_kmh, 1), `Climb m` = round(r$ascent_m),
                         check.names = FALSE),
              rownames = FALSE, fillContainer = FALSE, options = list(pageLength = 10, order = list(list(0, "desc"))))
  })
}

# ---------------------------------------------------------------- heatmap server (called from server())
heat_server <- function(input, output, session, prog) {
  acts_all <- reactive(c(strava_acts, project_activities(prog(), net)))
  acts <- debounce(reactive(heat_filter(acts_all(), input$heat_years, input$heat_src %||% character(0))), 400)
  hs <- reactive(heat_summary(acts(), net))
  proj_pct <- reactive(progress_totals(net, prog())$pct)

  output$hv_rides     <- renderText(hs()$rides)
  output$hv_rides_sub <- renderText(sprintf("%s - %s", fmt_d(hs()$first), fmt_d(hs()$last)))
  output$hv_km        <- renderText(km_txt(hs()$km_oslo, 0))
  output$hv_km_sub    <- renderText(sprintf("%s km in total on these rides", fmt(hs()$km_total, 0)))
  output$hv_pct       <- renderText(paste0("~ ", fmt(100 * hs()$touched_km / hs()$goal_km), " %"))
  output$hv_pct_sub   <- renderText(sprintf("%s of %s km - project alone: %s %%", fmt(hs()$touched_km, 0), fmt(hs()$goal_km, 0), fmt(proj_pct(), 2)))
  output$hv_top       <- renderText(if (nrow(hs()$top_streets)) hs()$top_streets$street[1] else "-")
  output$hv_top_sub   <- renderText(if (nrow(hs()$top_streets)) sprintf("%d rides", hs()$top_streets$rides[1]) else "")

  # The glow is drawn with the map (proxy updates did not reach the map reliably); the current
  # view is kept when a filter changes
  output$map_heat <- renderLeaflet({
    a <- acts(); g <- heat_glow(a, net, glow = 1)
    ctr <- isolate(input$map_heat_center); zm <- isolate(input$map_heat_zoom)
    # Standard background maps; the glow sits in its own pane above them so no tile layer can cover it
    m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addMapPane("glow", zIndex = 410) |>
      addProviderTiles(providers$CyclOSM, group = "CyclOSM") |>
      addProviderTiles(providers$OpenStreetMap.DE, group = "OSM (DE)") |>
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Esri streets") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addPolylines(data = net$boundary, color = "#333", weight = 1.5, dashArray = "4,4", group = "Oslo border")
    if (!is.null(g))
      m <- m |> addRasterImage(g, colors = heat_pal, opacity = 1, project = TRUE, maxBytes = 40 * 1024^2, group = "Glow", options = gridOptions(pane = "glow")) |>
        addPolylines(data = st_transform(st_simplify(do.call(c, lapply(a, `[[`, "track")), dTolerance = 3), 4326),
                     color = "#b5338a", weight = 1.5, opacity = 0.6, group = "Tracks", options = pathOptions(pane = "glow")) |>
        groupOptions("Tracks", zoomLevels = 14:19)
    m |> hideGroup(c("Tracks", "Oslo border")) |> default_base() |>
      addLayersControl(baseGroups = base_groups, overlayGroups = c("Glow", "Tracks", "Oslo border"),
                          options = layersControlOptions(collapsed = TRUE)) |>
      setView(ctr$lng %||% 10.76, ctr$lat %||% 59.93, zoom = zm %||% 12)
  })

  output$heat_top <- renderTable({
    t <- head(hs()$top_streets, 12); req(nrow(t) > 0)
    data.frame(`#` = seq_len(nrow(t)), Street = t$street, Rides = t$rides, `Street km` = fmt(t$km), check.names = FALSE)
  }, striped = TRUE, spacing = "s", width = "100%", align = "llrr")

  output$heat_years_plot <- renderPlotly({
    y <- hs()$per_year; req(nrow(y) > 0)
    plot_ly(y, x = ~year, y = ~rides, color = ~source, colors = c("Strava history" = col_strava, "Ride Oslo project" = col_project),
            type = "bar", customdata = ~round(km), hovertemplate = "%{x}: %{y} rides, %{customdata} km in Oslo<extra>%{fullData.name}</extra>") |>
      layout(barmode = "stack", xaxis = list(title = "", dtick = 1), yaxis = list(title = "rides"), legend = list(orientation = "h"))
  })

  output$heat_calendar <- renderPlotly({
    my <- hs()$month_year; req(!is.null(my))
    plot_ly(x = month.abb, y = rownames(my), z = matrix(as.numeric(my), nrow = nrow(my)), type = "heatmap", xgap = 2, ygap = 2,
            colorscale = list(c(0, "#F4ECEA"), c(0.01, "#3b0f70"), c(0.4, "#b5338a"), c(0.75, "#fe9f6d"), c(1, "#fcfdbf")),
            hovertemplate = "%{x} %{y}: %{z} rides<extra></extra>", showscale = FALSE) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "", type = "category", autorange = "reversed"))
  })

  output$heat_district <- renderPlotly({
    d <- hs()$district_km; req(nrow(d) > 0)
    d$district <- factor(d$district, levels = rev(d$district))
    plot_ly(d, y = ~district, x = ~km, type = "bar", orientation = "h", marker = list(color = col_project),
            hovertemplate = "%{y}: %{x:.0f} km<extra></extra>") |>
      layout(xaxis = list(title = "km ridden inside the district"), yaxis = list(title = ""), margin = list(l = 10))
  })

  output$heat_records <- renderTable({
    s <- hs(); req(s$rides > 0)
    yrs <- unique(s$per_year$year)
    mm <- which(s$month_year == max(s$month_year), arr.ind = TRUE)[1, ]
    hot <- s$passes >= 10 & net$edges$in_goal & !net$edges$conn
    data.frame(What = c("First ride", "Latest ride", "Longest stretch inside Oslo", "Km on these rides (incl. outside Oslo)",
                        "Average rides per active year", "Busiest month", "Streets ridden 10+ times"),
               Value = c(fmt_d(s$first), fmt_d(s$last), km_txt(s$longest_km), km_txt(s$km_total, 0),
                         fmt(s$rides / length(yrs)), sprintf("%s %s (%d rides)", month.name[mm[2]], rownames(s$month_year)[mm[1]], max(s$month_year)),
                         km_txt(sum(net$edges$len[hot]) / 1000)))
  }, striped = TRUE, spacing = "s", width = "100%", align = "lr")
}

shinyApp(ui, server)
