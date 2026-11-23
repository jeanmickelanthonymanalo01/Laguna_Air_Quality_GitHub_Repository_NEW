# CSST101 - Week 4: Statistical analysis and interpretation of results
# Project: Air quality in Laguna (Binan, Cabuyao, Calamba)
#
# Research question (CSST101): Which time factors and pollutants are related
# to PM2.5 (air quality) in Laguna?
#
# Run the Week 2 script first (it creates laguna_hourly_clean.csv).
# Keep this script in the same folder, then use
# Session > Set Working Directory > To Source File Location.

library(tidyverse)
library(lubridate)

# ---- 1. Load data ------------------------------------------------------------
df <- read_csv("laguna_hourly_clean.csv") %>%
  mutate(
    season = factor(season, levels = c("Dry", "Rainy")),
    year_f = factor(year),
    hour_f = factor(hour)
  )

# Why Spearman and Wilcoxon/Kruskal tests?
# PM2.5 is very skewed (many low values, a few huge ones), so tests that use
# ranks fit better than tests that assume a normal distribution.
hist(df$pm2_5, breaks = 80, main = "Distribution of PM2.5", xlab = "PM2.5 (ug/m3)")

# ---- 2. Which pollutants move with PM2.5? (Spearman correlation) -------------
others <- c("pm10", "no2", "o3", "so2", "co", "no", "nh3")

cor_results <- map_dfr(others, function(v) {
  t <- cor.test(df$pm2_5, df[[v]], method = "spearman", exact = FALSE)
  tibble(variable = v,
         rho      = round(unname(t$estimate), 3),
         p_value  = signif(t$p.value, 3))
}) %>%
  mutate(strength = case_when(abs(rho) >= 0.7 ~ "strong",
                              abs(rho) >= 0.4 ~ "moderate",
                              abs(rho) >= 0.2 ~ "weak",
                              TRUE            ~ "very weak")) %>%
  arrange(desc(abs(rho)))
print(cor_results)

# ---- 3. Dry vs rainy season (Wilcoxon rank-sum test) -------------------------
season_medians <- df %>%
  group_by(season) %>%
  summarise(hours = n(),
            median = round(median(pm2_5), 2),
            mean   = round(mean(pm2_5), 2),
            .groups = "drop")
print(season_medians)

w <- wilcox.test(pm2_5 ~ season, data = df, exact = FALSE)
print(w)

# Chance that a random dry-season hour has higher PM2.5 than a random rainy hour
n_dry   <- sum(df$season == "Dry")
n_rainy <- sum(df$season == "Rainy")
prob_dry_higher <- unname(w$statistic) / (n_dry * n_rainy)
prob_dry_higher

# ---- 4. Does PM2.5 change by hour of day? (Kruskal-Wallis) -------------------
# Only March 2025 onward, because earlier data is not recorded every hour.
hourly <- df %>% filter(datetime >= as.POSIXct("2025-03-01", tz = "UTC"))

hour_table <- hourly %>%
  group_by(hour) %>%
  summarise(median = round(median(pm2_5), 2),
            mean   = round(mean(pm2_5), 2),
            .groups = "drop")
print(hour_table, n = 24)
hour_table %>% slice_max(mean, n = 1)    # hour with the highest average
hour_table %>% slice_min(mean, n = 1)    # hour with the lowest average

kruskal.test(pm2_5 ~ hour_f, data = hourly)

# ---- 5. Does PM2.5 change by year? (Kruskal-Wallis) ---------------------------
year_table <- df %>%
  group_by(year) %>%
  summarise(hours = n(),
            median = round(median(pm2_5), 2),
            mean   = round(mean(pm2_5), 2),
            max    = round(max(pm2_5), 2),
            .groups = "drop")
print(year_table)

kruskal.test(pm2_5 ~ year_f, data = df)

# ---- 6. Simple model: how much do season, year, and hour explain? -----------
# log1p() is used because PM2.5 is very skewed.
model <- lm(log1p(pm2_5) ~ season + year_f + hour_f, data = df)
summary(model)$r.squared                  # share of variation explained (0 to 1)

# ---- 7. Look closer at the 2024 spikes ----------------------------------------
daily <- df %>%
  mutate(date = as_date(datetime)) %>%
  group_by(date) %>%
  summarise(avg_pm25 = round(mean(pm2_5), 1), hours = n(), .groups = "drop") %>%
  arrange(desc(avg_pm25))
print(head(daily, 15))

df %>%
  arrange(desc(pm2_5)) %>%
  select(datetime, pm2_5, pm10, co, no2) %>%
  head(5)

# ---- 8. Chart: PM2.5 by year --------------------------------------------------
chart6 <- ggplot(df, aes(year_f, pm2_5, fill = year_f)) +
  geom_boxplot(outlier.alpha = 0.2) +
  coord_cartesian(ylim = c(0, 40)) +
  labs(title = "PM2.5 by Year in Laguna",
       subtitle = "Axis cut at 40 to show the main range; 2023 covers Nov to Dec only",
       x = "Year", y = "PM2.5 (µg/m³)") +
  theme_minimal() +
  theme(legend.position = "none")
print(chart6)
ggsave("chart6_pm25_by_year.png", chart6, width = 6.5, height = 4.5, dpi = 150)

# ---- 9. Save results ---------------------------------------------------------
write_csv(cor_results,    "results_correlation.csv")
write_csv(season_medians, "results_season.csv")
write_csv(hour_table,     "results_hour.csv")
write_csv(year_table,     "results_year.csv")
write_csv(daily,          "results_daily_pm25.csv")
