# CSST101 - Week 3: Cleaning, descriptive statistics, and visualization
# Project: Air quality in Laguna (Binan, Cabuyao, Calamba)
#
# Run csst101_week2_import_explore.R first. It creates laguna_hourly_clean.csv.
# Keep this script in the same folder, then use
# Session > Set Working Directory > To Source File Location.

library(tidyverse)
library(lubridate)

# ---- 1. Load the cleaned hourly data ---------------------------------------
df <- read_csv("laguna_hourly_clean.csv") %>%
  mutate(
    season    = factor(season, levels = c("Dry", "Rainy")),
    aqi_label = factor(aqi_level, levels = 1:5,
                       labels = c("Good", "Fair", "Moderate", "Poor", "Very Poor"))
  )

glimpse(df)

# ---- 2. Summary statistics -------------------------------------------------
pollutants <- c("pm2_5", "pm10", "no2", "o3", "so2", "co", "no", "nh3")

# Overall: mean, median, standard deviation, min, max for each pollutant
overall_stats <- df %>%
  summarise(across(all_of(pollutants),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        median = ~median(.x, na.rm = TRUE),
                        sd = ~sd(.x, na.rm = TRUE),
                        min = ~min(.x, na.rm = TRUE),
                        max = ~max(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(),
               names_to = c("pollutant", "stat"),
               names_pattern = "(.*)_(mean|median|sd|min|max)$") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))
print(overall_stats)

# PM2.5 by season
season_stats <- df %>%
  group_by(season) %>%
  summarise(hours  = n(),
            mean   = round(mean(pm2_5, na.rm = TRUE), 2),
            median = round(median(pm2_5, na.rm = TRUE), 2),
            sd     = round(sd(pm2_5, na.rm = TRUE), 2),
            .groups = "drop")
print(season_stats)

# PM2.5 by year
year_stats <- df %>%
  group_by(year) %>%
  summarise(hours = n(),
            mean  = round(mean(pm2_5, na.rm = TRUE), 2),
            max   = round(max(pm2_5, na.rm = TRUE), 2),
            .groups = "drop")
print(year_stats)

# Air quality level counts
aqi_counts <- df %>% count(aqi_label) %>% mutate(percent = round(100 * n / sum(n), 1))
print(aqi_counts)

# ---- 3. Chart 1: PM2.5 over time (monthly average) -------------------------
chart1 <- df %>%
  mutate(ym = floor_date(datetime, "month")) %>%
  group_by(ym) %>%
  summarise(avg_pm25 = mean(pm2_5, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(ym, avg_pm25)) +
  geom_line(linewidth = 0.8, color = "#2c7fb8") +
  geom_point(color = "#2c7fb8") +
  labs(title = "Monthly Average PM2.5 in Laguna",
       subtitle = "Biñan, Cabuyao, and Calamba, Nov 2023 to Oct 2026",
       x = "Month", y = "PM2.5 (µg/m³)") +
  theme_minimal()
print(chart1)
ggsave("chart1_pm25_over_time.png", chart1, width = 8, height = 4.5, dpi = 150)

# ---- 4. Chart 2: PM2.5 by hour of day --------------------------------------
# Only March 2025 onward is used, because earlier data is not recorded every hour.
chart2 <- df %>%
  filter(datetime >= as.POSIXct("2025-03-01", tz = "UTC")) %>%
  group_by(hour) %>%
  summarise(avg_pm25 = mean(pm2_5, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(hour, avg_pm25)) +
  geom_line(linewidth = 0.8, color = "#d95f0e") +
  geom_point(color = "#d95f0e") +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  labs(title = "Average PM2.5 by Hour of Day",
       subtitle = "Hourly data, March 2025 to Oct 2026",
       x = "Hour of day (0 to 23)", y = "Average PM2.5 (µg/m³)") +
  theme_minimal()
print(chart2)
ggsave("chart2_pm25_by_hour.png", chart2, width = 8, height = 4.5, dpi = 150)

# ---- 5. Chart 3: PM2.5 in dry vs rainy season ------------------------------
chart3 <- ggplot(df, aes(season, pm2_5, fill = season)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_cartesian(ylim = c(0, 40)) +
  scale_fill_manual(values = c("Dry" = "#fdae6b", "Rainy" = "#9ecae1")) +
  labs(title = "PM2.5 in the Dry and Rainy Season",
       subtitle = "Dry: Dec to May, Rainy: Jun to Nov (axis cut at 40 to show the main range)",
       x = "Season", y = "PM2.5 (µg/m³)") +
  theme_minimal() +
  theme(legend.position = "none")
print(chart3)
ggsave("chart3_pm25_by_season.png", chart3, width = 6, height = 4.5, dpi = 150)

# ---- 6. Chart 4: how the pollutants relate (correlation heatmap) -----------
cm <- cor(df[pollutants], use = "pairwise.complete.obs")
cm_long <- as.data.frame(as.table(cm))
names(cm_long) <- c("var1", "var2", "r")

chart4 <- ggplot(cm_long, aes(var1, var2, fill = r)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(r, 2)), size = 3) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       limits = c(-1, 1), name = "Correlation") +
  labs(title = "Correlation Between Pollutants",
       x = NULL, y = NULL) +
  theme_minimal()
print(chart4)
ggsave("chart4_pollutant_correlation.png", chart4, width = 6.5, height = 5.5, dpi = 150)

# ---- 7. Chart 5: air quality levels ----------------------------------------
chart5 <- ggplot(aqi_counts, aes(aqi_label, n, fill = aqi_label)) +
  geom_col() +
  geom_text(aes(label = paste0(percent, "%")), vjust = -0.4) +
  scale_fill_manual(values = c("Good" = "#31a354", "Fair" = "#a1d99b",
                               "Moderate" = "#fed976", "Poor" = "#fd8d3c",
                               "Very Poor" = "#e31a1c")) +
  labs(title = "How Often Each Air Quality Level Occurs",
       subtitle = "OpenWeatherMap scale, hourly readings",
       x = "Air quality level", y = "Number of hours") +
  theme_minimal() +
  theme(legend.position = "none")
print(chart5)
ggsave("chart5_aqi_levels.png", chart5, width = 6.5, height = 4.5, dpi = 150)

# ---- 8. Save the summary tables --------------------------------------------
write_csv(overall_stats, "summary_overall.csv")
write_csv(season_stats,  "summary_season.csv")
write_csv(year_stats,    "summary_year.csv")
