# Laguna Air Quality Dashboard (R Shiny)
# Style inspired by Plume Labs air quality pages.
#
# Files needed in the SAME folder as this script:
#   laguna_hourly_clean.csv   (made by csst101_week2_import_explore.R)
#   weather_laguna.csv        (made by csst101_week5_add_weather.R) - optional, for temperature
#   forecast_next_day.csv     (from the CSST102 model)              - optional, for the AI forecast
#
# To run: open this file in RStudio and click "Run App" (top right of the script panel).
# First time only: install.packages(c("shiny", "plotly", "leaflet"))

library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(leaflet)

# ---- Data -------------------------------------------------------------------
air <- read_csv("laguna_hourly_clean.csv", show_col_types = FALSE) %>%
  arrange(datetime)

if (file.exists("weather_laguna.csv")) {
  weather <- read_csv("weather_laguna.csv", show_col_types = FALSE) %>%
    select(datetime, temperature)
  air <- air %>% left_join(weather, by = "datetime")
} else {
  air$temperature <- NA_real_
}

forecast <- NULL
if (file.exists("forecast_next_day.csv")) {
  forecast <- read_csv("forecast_next_day.csv", show_col_types = FALSE)
}

# Start on the newest reading that also has weather (if weather exists)
if (any(!is.na(air$temperature))) {
  default_time <- max(air$datetime[!is.na(air$temperature)])
} else {
  default_time <- max(air$datetime)
}

# ---- Labels, colors, advice -------------------------------------------------
level_labels <- c("Good", "Fair", "Moderate", "Poor", "Very Poor")
level_cols   <- c("#2ebf7e", "#b7d63a", "#ffc107", "#ff8a1f", "#e5393a")

advice_items <- list(
  "Outdoor sports" = c("ok", "ok", "care", "avoid", "avoid"),
  "Bring baby out" = c("ok", "ok", "care", "avoid", "avoid"),
  "Eating outside" = c("ok", "ok", "ok", "care", "avoid")
)
advice_symbol <- c(ok = "\u2713", care = "!", avoid = "\u2717")

advice_text <- c(
  "The air is clean. Enjoy your outdoor activities.",
  "The air quality is acceptable. Very sensitive people may want to take it easier outdoors.",
  "Sensitive groups, such as children, the elderly, and people with asthma, should limit long outdoor activity.",
  "The air is polluted. Everyone should reduce long or heavy outdoor activity.",
  "The air is very polluted. Avoid outdoor activity and keep windows closed."
)

towns <- tibble(
  town = c("Bi\u00f1an", "Cabuyao", "Calamba"),
  lat  = c(14.3427, 14.2726, 14.2117),
  lon  = c(121.0807, 121.1262, 121.1653)
)

hour_choices <- setNames(as.character(0:23), sprintf("%02d:00", 0:23))

# ---- Style -----------------------------------------------------------------
css <- "
body { background:#f4f6fb; font-family: 'Segoe UI', Arial, sans-serif; color:#1b1f33; }
.topbar { display:flex; justify-content:space-between; align-items:center; flex-wrap:wrap;
          padding:10px 4px; margin-bottom:10px; }
.brand { font-weight:700; font-size:20px; color:#3b6cff; letter-spacing:0.5px; }
.controls { display:flex; gap:12px; align-items:flex-end; }
.controls .form-group { margin-bottom:0; }
.hero { background:#4473ff; color:white; padding:34px 28px; border-radius:6px 6px 0 0; }
.hero h1 { margin:0 0 10px 0; font-size:34px; font-weight:700; }
.hero p { margin:0; font-size:17px; opacity:0.95; }
.card { background:white; border-radius:6px; padding:20px 24px; margin-bottom:16px;
        box-shadow:0 1px 4px rgba(20,30,70,0.08); }
.card.status { border-radius:0 0 6px 6px; }
.tag { color:#3b6cff; font-weight:700; letter-spacing:2px; font-size:13px; margin-bottom:8px; }
.topline { display:flex; justify-content:space-between; align-items:center; }
.temp { font-weight:600; color:#333; }
.status-main { display:flex; justify-content:space-between; align-items:center; margin:8px 0 4px 0; }
.level-label { font-size:42px; font-weight:800; line-height:1.1; }
.level-sub { color:#555; margin-top:4px; }
.badge-circle { width:78px; height:78px; border-radius:50%; color:white; font-size:40px;
                font-weight:800; display:flex; align-items:center; justify-content:center; }
.badge-circle.small { width:54px; height:54px; font-size:26px; }
hr { border-top:1px solid #e6e9f2; margin:14px 0; }
.chips { display:flex; flex-wrap:wrap; gap:14px; margin-bottom:10px; }
.chip { font-weight:700; letter-spacing:1px; font-size:13px; color:#6a7087; text-transform:uppercase; }
.chip .sym { display:inline-block; width:20px; font-weight:800; }
.sym-ok { color:#2ebf7e; } .sym-care { color:#ff8a1f; } .sym-avoid { color:#e5393a; }
.message { font-size:17px; line-height:1.5; }
.note { color:#6a7087; font-size:13px; margin-top:8px; }
.annual { color:#444; margin-top:6px; }
.annual b { margin-left:8px; }
.bar { display:flex; height:10px; border-radius:6px; overflow:hidden; margin:8px 0; background:#e9ecf5; }
.legend { display:flex; flex-wrap:wrap; gap:14px; font-size:13px; color:#444; }
.dot { display:inline-block; width:10px; height:10px; border-radius:50%; margin-right:5px; }
.pgrid { display:grid; grid-template-columns:repeat(3, 1fr); gap:14px; margin-top:6px; }
.pbox { background:#f4f6fb; border-radius:6px; padding:12px; text-align:center; }
.pval { font-size:26px; font-weight:700; }
.plab { color:#6a7087; font-weight:700; letter-spacing:1px; font-size:13px; }
.forecast-row { display:flex; justify-content:space-between; align-items:center; }
.footer { color:#6a7087; font-size:12px; padding:6px 4px 24px 4px; }
.shiny-options-group { margin-bottom:6px; }
"

# ---- User interface ---------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$title("Air Quality in Laguna"), tags$style(HTML(css))),

  div(class = "topbar",
      div(class = "brand", "LAGUNA AIR QUALITY"),
      div(class = "controls",
          dateInput("day", "Date", value = as_date(default_time),
                    min = as_date(min(air$datetime)), max = as_date(max(air$datetime)),
                    width = "150px"),
          selectInput("hour", "Hour", choices = hour_choices,
                      selected = as.character(hour(default_time)), width = "110px"))),

  fluidRow(
    column(5,
           div(class = "hero",
               h1("Air quality in Laguna"),
               p("Air quality report for Bi\u00f1an, Cabuyao, and Calamba")),
           uiOutput("status"),
           uiOutput("forecast")),
    column(7,
           div(class = "card",
               radioButtons("range", NULL, choices = c("LIVE", "WEEK", "MONTH", "YEAR"),
                            selected = "LIVE", inline = TRUE),
               plotlyOutput("trend", height = "320px"),
               uiOutput("annual")),
           uiOutput("levels"),
           uiOutput("pollutants"))
  ),

  fluidRow(
    column(12,
           div(class = "card",
               div(class = "tag", "MAP OF LAGUNA"),
               leafletOutput("map", height = "380px"),
               p(class = "note",
                 "Circles show the air quality level at the selected time. The three towns have almost identical values in the source data, so they share the same color.")))
  ),

  div(class = "footer",
      "Data: hourly air quality from OpenWeatherMap (via a Kaggle dataset) and weather from Open-Meteo. ",
      "Values are modeled estimates, not local sensor readings. ",
      "The advice shown is general guidance, not medical advice.")
)

# ---- Server -----------------------------------------------------------------
server <- function(input, output, session) {

  asof <- reactive({
    req(input$day, input$hour)
    as.POSIXct(paste(input$day, sprintf("%02d:00:00", as.integer(input$hour))), tz = "UTC")
  })

  row <- reactive({
    r <- air %>%
      filter(datetime <= asof()) %>%
      slice_max(datetime, n = 1, with_ties = FALSE)
    validate(need(nrow(r) > 0, "No reading at or before this time."))
    r
  })

  annual_avg <- reactive({
    air %>%
      filter(datetime > asof() - days(365), datetime <= asof()) %>%
      summarise(m = mean(pm2_5, na.rm = TRUE)) %>%
      pull(m)
  })

  # ---- Status card ----
  output$status <- renderUI({
    r  <- row()
    lv <- as.integer(r$aqi_level)
    temp_txt <- if (is.na(r$temperature)) "" else paste0(round(r$temperature), "\u00b0C")

    chips <- lapply(names(advice_items), function(nm) {
      kind <- advice_items[[nm]][lv]
      div(class = "chip",
          span(class = paste("sym", paste0("sym-", kind)), advice_symbol[[kind]]),
          nm)
    })

    div(class = "card status",
        div(class = "topline",
            div(class = "tag", paste("READING \u00b7", format(r$datetime, "%a %d %b %Y, %I:%M %p"))),
            div(class = "temp", temp_txt)),
        div(class = "status-main",
            div(div(class = "level-label", level_labels[lv]),
                div(class = "level-sub",
                    paste0("Level ", lv, " of 5  \u00b7  PM2.5 ", round(r$pm2_5, 1), " \u00b5g/m\u00b3"))),
            div(class = "badge-circle", style = paste0("background:", level_cols[lv]), "!")),
        hr(),
        div(class = "chips", chips),
        div(class = "message", advice_text[lv]))
  })

  # ---- AI forecast card (needs forecast_next_day.csv from CSST102) ----
  output$forecast <- renderUI({
    if (is.null(forecast)) {
      div(class = "card",
          div(class = "tag", "AI FORECAST"),
          p(class = "note",
            "The forecast from the CSST102 model will appear here. Add forecast_next_day.csv with the columns date and predicted_level (1 to 5)."))
    } else {
      f   <- forecast %>% slice_tail(n = 1)
      lv  <- as.integer(f$predicted_level)
      lbl <- if ("predicted_label" %in% names(f)) as.character(f$predicted_label) else level_labels[lv]
      txt <- if ("note" %in% names(f)) as.character(f$note) else ""
      div(class = "card",
          div(class = "tag", "AI FORECAST (EXPERIMENTAL)"),
          div(class = "forecast-row",
              div(div(class = "level-label", style = "font-size:30px;", lbl),
                  div(class = "level-sub", paste("Predicted level for", f$date))),
              div(class = "badge-circle small", style = paste0("background:", level_cols[lv]), lv)),
          p(class = "note", txt))
    }
  })

  # ---- Trend chart (LIVE / WEEK / MONTH / YEAR) ----
  output$trend <- renderPlotly({
    t <- asof()
    d <- switch(input$range,
      LIVE  = air %>% filter(datetime > t - hours(48), datetime <= t) %>%
                transmute(x = datetime, y = pm2_5),
      WEEK  = air %>% filter(datetime > t - days(7), datetime <= t) %>%
                transmute(x = datetime, y = pm2_5),
      MONTH = air %>% filter(datetime > t - days(30), datetime <= t) %>%
                mutate(x = floor_date(datetime, "day")) %>%
                group_by(x) %>% summarise(y = mean(pm2_5), .groups = "drop"),
      YEAR  = air %>% filter(datetime > t - days(365), datetime <= t) %>%
                mutate(x = floor_date(datetime, "day")) %>%
                group_by(x) %>% summarise(y = mean(pm2_5), .groups = "drop")
    )
    validate(need(nrow(d) > 1, "Not enough data for this range."))

    r        <- row()
    line_col <- level_cols[as.integer(r$aqi_level)]
    avg      <- annual_avg()

    plot_ly(d, x = ~x, y = ~y, type = "scatter", mode = "lines",
            line = list(color = line_col, width = 3),
            hovertemplate = "%{y:.1f} \u00b5g/m\u00b3<extra></extra>") %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "PM2.5 (\u00b5g/m\u00b3)", rangemode = "tozero"),
        shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                           y0 = avg, y1 = avg,
                           line = list(color = "#888888", dash = "dot", width = 1))),
        margin = list(t = 10)
      )
  })

  output$annual <- renderUI({
    div(class = "annual",
        "- - -  Annual average",
        tags$b(paste0(round(annual_avg(), 1), " \u00b5g/m\u00b3 PM2.5")))
  })

  # ---- Share of hours at each level over the last year ----
  output$levels <- renderUI({
    t  <- asof()
    yr <- air %>% filter(datetime > t - days(365), datetime <= t)
    validate(need(nrow(yr) > 0, "No data for the last year."))

    share <- yr %>%
      count(aqi_level) %>%
      mutate(p = 100 * n / sum(n))

    pct <- sapply(1:5, function(l) {
      v <- share$p[share$aqi_level == l]
      if (length(v) == 0) 0 else v
    })

    segs <- lapply(1:5, function(l) {
      if (pct[l] < 0.3) return(NULL)
      div(style = sprintf("width:%.2f%%; background:%s;", pct[l], level_cols[l]))
    })
    legend <- lapply(1:5, function(l) {
      span(span(class = "dot", style = paste0("background:", level_cols[l])),
           paste0(level_labels[l], " ", round(pct[l], 1), "%"))
    })

    div(class = "card",
        div(class = "tag", "SHARE OF HOURS AT EACH LEVEL (LAST 12 MONTHS)"),
        div(class = "bar", segs),
        div(class = "legend", legend))
  })

  # ---- Main pollutants ----
  output$pollutants <- renderUI({
    r <- row()
    pols <- list(c("PM2.5", "pm2_5"), c("PM10", "pm10"), c("NO2", "no2"),
                 c("O3", "o3"), c("SO2", "so2"), c("CO", "co"))
    boxes <- lapply(pols, function(p) {
      div(class = "pbox",
          div(class = "pval", round(r[[p[2]]], 1)),
          div(class = "plab", p[1]))
    })
    div(class = "card",
        div(class = "tag", "MAIN POLLUTANTS (\u00b5g/m\u00b3)"),
        div(class = "pgrid", boxes))
  })

  # ---- Map of Laguna ----
  output$map <- renderLeaflet({
    r  <- row()
    lv <- as.integer(r$aqi_level)
    town_now <- towns %>%
      mutate(fill  = level_cols[lv],
             label = paste0(town, ": ", level_labels[lv]),
             popup = paste0("<b>", town, "</b><br>", level_labels[lv],
                            "<br>PM2.5: ", round(r$pm2_5, 1), " \u00b5g/m\u00b3"))
    leaflet(town_now) %>%
      addTiles() %>%
      addCircleMarkers(lng = ~lon, lat = ~lat, radius = 24, color = "white", weight = 2,
                       fillColor = ~fill, fillOpacity = 0.85,
                       label = ~label, popup = ~popup) %>%
      setView(lng = 121.12, lat = 14.28, zoom = 11)
  })
}

shinyApp(ui, server)
