# CSST101 - Week 5: Add weather data and improve the analysis
# Project: Air quality in Laguna (Binan, Cabuyao, Calamba)
#
# Weather source: Open-Meteo Historical Weather API (free, no account or key).
# It is modeled (reanalysis) weather for a point, not a local weather station.
#
# Run the Week 2 script first (it creates laguna_hourly_clean.csv).
# You need an internet connection. Keep this script in the same folder, then use
# Session > Set Working Directory > To Source File Location.

library(tidyverse)
library(lubridate)
library(jsonlite)

# ---- 1. Download hourly weather ----------------------------------------------
# One point in the middle of the three towns (average of their coordinates).
lat <- 14.2757
lon <- 121.1241

# The weather ends a few days before the air data on purpose, because the
# newest days are often not available yet.
url <- paste0(
  "https://archive-api.open-meteo.com/v1/archive",
  "?latitude=", lat, "&longitude=", lon,
  "&start_date=2023-11-06&end_date=2026-10-15",
  "&hourly=temperature_2m,relative_humidity_2m,precipitation,wind_speed_10m",
  "&timezone=Asia%2FManila"
)

raw <- fromJSON(url)

# The times are Philippine local time. The air quality file uses the same
# clock numbers (labeled UTC by R), so we label these the same way to match.
weather <- tibble(
  datetime    = as.POSIXct(raw$hourly$time, format = "%Y-%m-%dT%H:%M", tz = "UTC"),
  temperature = raw$hourly$temperature_2m,        # degrees C
  humidity    = raw$hourly$relative_humidity_2m,  # percent
  rain_mm     = raw$hourly$precipitation,         # millimeters per hour
  wind_kmh    = raw$hourly$wind_speed_10m         # km per hour
)

glimpse(weather)
summary(weather)
write_csv(weather, "weather_laguna.csv")

# ---- 2. Join weather to the air quality data -----------------------------------
air <- read_csv("laguna_hourly_clean.csv")

air_w <- air %>%
  inner_join(weather, by = "datetime") %>%
  drop_na(temperature, humidity, rain_mm, wind_kmh) %>%
  mutate(
    season     = factor(season, levels = c("Dry", "Rainy")),
    year_f     = factor(year),
    hour_f     = factor(hour),
    rain_group = case_when(rain_mm == 0   ~ "No rain",
                           rain_mm <= 2.5 ~ "Light rain",
                           TRUE           ~ "Moderate/heavy rain"),
    rain_group = factor(rain_group,
                        levels = c("No rain", "Light rain", "Moderate/heavy rain"))
  )

nrow(air)      # air quality rows before joining
nrow(air_w)    # rows that also have weather
write_csv(air_w, "laguna_air_weather.csv")

# ---- 3. Do weather variables relate to PM2.5? (Spearman correlation) ------------
weather_vars <- c("temperature", "humidity", "rain_mm", "wind_kmh")

weather_cor <- map_dfr(weather_vars, function(v) {
  t <- cor.test(air_w$pm2_5, air_w[[v]], method = "spearman", exact = FALSE)
  tibble(variable = v,
         rho      = round(unname(t$estimate), 3),
         p_value  = signif(t$p.value, 3))
}) %>%
  mutate(strength = case_when(abs(rho) >= 0.7 ~ "strong",
                              abs(rho) >= 0.4 ~ "moderate",
                              abs(rho) >= 0.2 ~ "weak",
                              TRUE            ~ "very weak")) %>%
  arrange(desc(abs(rho)))
print(weather_cor)

# ---- 4. PM2.5 on dry, light-rain, and heavy-rain hours --------------------------
rain_table <- air_w %>%
  group_by(rain_group) %>%
  summarise(hours  = n(),
            median = round(median(pm2_5), 2),
            mean   = round(mean(pm2_5), 2),
            .groups = "drop")
print(rain_table)

kruskal.test(pm2_5 ~ rain_group, data = air_w)

# ---- 5. Does weather improve the model? -----------------------------------------
model_time <- lm(log1p(pm2_5) ~ season + year_f + hour_f, data = air_w)
model_all  <- lm(log1p(pm2_5) ~ temperature + humidity + rain_mm + wind_kmh +
                   season + year_f + hour_f, data = air_w)

model_compare <- tibble(
  model     = c("Time only (season, year, hour)", "Time + weather"),
  r_squared = round(c(summary(model_time)$r.squared,
                      summary(model_all)$r.squared), 3)
)
print(model_compare)

round(summary(model_all)$coefficients[weather_vars, ], 4)

# ---- 6. Chart 7: PM2.5 by rain group ----------------------------------------------
chart7 <- ggplot(air_w, aes(rain_group, pm2_5, fill = rain_group)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_cartesian(ylim = c(0, 40)) +
  scale_fill_manual(values = c("No rain" = "#fdae6b",
                               "Light rain" = "#9ecae1",
                               "Moderate/heavy rain" = "#3182bd")) +
  labs(title = "PM2.5 on Dry and Rainy Hours",
       subtitle = "Axis cut at 40 to show the main range",
       x = "Rain at that hour", y = "PM2.5 (µg/m³)") +
  theme_minimal() +
  theme(legend.position = "none")
print(chart7)
ggsave("chart7_pm25_by_rain.png", chart7, width = 7, height = 4.5, dpi = 150)

# ---- 7. Chart 8: PM2.5 and wind speed ----------------------------------------------
chart8 <- ggplot(air_w, aes(wind_kmh, pm2_5)) +
  geom_point(alpha = 0.05, color = "grey40") +
  geom_smooth(color = "#d95f0e") +
  coord_cartesian(ylim = c(0, 40)) +
  labs(title = "PM2.5 and Wind Speed",
       subtitle = "Axis cut at 40; the line shows the overall trend",
       x = "Wind speed (km/h)", y = "PM2.5 (µg/m³)") +
  theme_minimal()
print(chart8)
ggsave("chart8_pm25_vs_wind.png", chart8, width = 7, height = 4.5, dpi = 150)

# ---- 8. Save results ------------------------------------------------------------------
write_csv(weather_cor,   "results_weather_correlation.csv")
write_csv(rain_table,    "results_rain.csv")
write_csv(model_compare, "results_model_compare.csv")

