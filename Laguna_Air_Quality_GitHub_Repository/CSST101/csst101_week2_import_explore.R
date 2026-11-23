# CSST101 - Week 2: Dataset import, exploration, and preparation
# Project: Air quality in Laguna (Binan, Cabuyao, Calamba)
# Data: Philippine Cities Air Quality Index Data (Kaggle, OpenWeatherMap), Nov 2023 - Oct 2026
#
# Before running: put laguna_air_quality_2023-2026.csv in the same folder as this
# script, then in RStudio use Session > Set Working Directory > To Source File Location.
# First time only: install.packages("tidyverse")

library(tidyverse)
library(lubridate)

# ---- 1. Import ------------------------------------------------------------
aq <- read_csv("laguna_air_quality_2023-2026.csv")

glimpse(aq)          # column names and types
dim(aq)              # rows and columns
head(aq)

# ---- 2. Explore -----------------------------------------------------------
range(aq$datetime)                 # date range
count(aq, town)                    # rows per town
colSums(is.na(aq))                 # missing values per column
sum(duplicated(aq[c("town", "datetime")]))   # duplicate town-hour rows
summary(aq)

# AQI level counts (OpenWeatherMap scale: 1 = Good ... 5 = Very Poor)
count(aq, aqi_level) %>% mutate(percent = round(100 * n / sum(n), 1))

# ---- 3. Clean -------------------------------------------------------------
# Placeholder values (-9999 and similar) were already blanked out as NA.
# Keep only rows that have PM2.5 and PM10.
aq_clean <- aq %>%
  filter(!is.na(pm2_5), !is.na(pm10)) %>%
  distinct(town, datetime, .keep_all = TRUE)

nrow(aq) - nrow(aq_clean)          # how many rows were removed

# ---- 4. Prepare (new columns) ---------------------------------------------
aq_clean <- aq_clean %>%
  mutate(
    year       = year(datetime),
    month      = month(datetime),
    hour       = hour(datetime),
    season     = if_else(month %in% 6:11, "Rainy", "Dry"),   # PH: rainy Jun-Nov
    aqi_label  = factor(aqi_level, levels = 1:5,
                        labels = c("Good", "Fair", "Moderate", "Poor", "Very Poor"))
  )

# ---- 5. Check: are the three towns really different? ------------------------
# The towns are close together, so the source may give them the same values.
town_spread <- aq_clean %>%
  group_by(datetime) %>%
  summarise(n_towns = n(),
            spread  = max(pm2_5) - min(pm2_5),
            .groups = "drop") %>%
  filter(n_towns == 3)

summary(town_spread$spread)                 # difference in PM2.5 between towns
mean(town_spread$spread == 0) * 100         # percent of hours with identical PM2.5

# ---- 6. One Laguna series (one row per hour) --------------------------------
laguna_hourly <- aq_clean %>%
  group_by(datetime) %>%
  summarise(across(c(pm2_5, pm10, no2, o3, so2, co, no, nh3), mean),
            aqi_level = round(mean(aqi_level)),
            .groups = "drop") %>%
  mutate(
    year   = year(datetime),
    month  = month(datetime),
    hour   = hour(datetime),
    season = if_else(month %in% 6:11, "Rainy", "Dry")
  )

dim(laguna_hourly)

# ---- 7. Quick look: monthly average PM2.5 ----------------------------------
laguna_hourly %>%
  mutate(ym = floor_date(datetime, "month")) %>%
  group_by(ym) %>%
  summarise(avg_pm25 = mean(pm2_5), .groups = "drop") %>%
  ggplot(aes(ym, avg_pm25)) +
  geom_line() +
  labs(title = "Monthly average PM2.5 in Laguna (Binan, Cabuyao, Calamba)",
       x = "Month", y = "PM2.5 (ug/m3)")

# ---- 8. Save the cleaned data ------------------------------------------------
write_csv(aq_clean, "laguna_air_quality_clean.csv")
write_csv(laguna_hourly, "laguna_hourly_clean.csv")
