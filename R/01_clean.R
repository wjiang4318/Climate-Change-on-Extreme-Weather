# 01_clean.R ------------------------------------------------------------------
# Load NOAA daily weather + storm event data for Southern California and South
# Carolina, standardize columns, and flag which days fall inside a storm event.
#
# Run this first. 02_eda.R and 03_models.R assume the objects created here are
# in the environment.
# -----------------------------------------------------------------------------

library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(patchwork)
library(olsrr)


# 1. Load ---------------------------------------------------------------------

California_weather <- read.csv("data/raw/South Cali_daily_1945.csv")
Carolina_weather   <- read.csv("data/raw/South Carolina_daily_1937.csv")
California_storm   <- read.csv("data/raw/South California Storm Event.csv")
Carolina_storm     <- read.csv("data/raw/South Carolina Storm Event.csv")


# 2. Column mapping and weather-type dummies ----------------------------------

Relevant_variables <- list(
  "STATION" = "Station_ID",
  "NAME" = "Station_Name", "DATE" = "Date", "AWND" = "Average_Wind_Speed",
  "FMTM" = "Fastest_Mile_Time", "PRCP" = "Precipitation", "SNOW" = "Snowfall",
  "SNWD" = "Snow_Depth", "TAVG" = "Avg_Temperature", "TMAX" = "Max_Temperature",
  "TMIN" = "Min_Temperature", "WT01" = "Fog", "WT02" = "Heavy_Fog",
  "WT03" = "Thunder", "WT08" = "Smoke", "WT10" = "Tornado",
  "WT11" = "Damaging_Winds", "WT16" = "Rain", "WT18" = "Snow"
)

Weather_type <- c("Fog", "Heavy_Fog", "Thunder", "Smoke",
                  "Tornado", "Damaging_Winds", "Rain", "Snow")


# 3. Clean --------------------------------------------------------------------

clean_weather_data <- function(df, mapping, dummy_vars) {
  df <- df[, colnames(df) %in% names(mapping), drop = FALSE]
  colnames(df) <- sapply(colnames(df), function(col) mapping[[col]])
  
  # NOAA leaves the weather-type flags blank rather than writing a 0.
  df %>% mutate(across(all_of(dummy_vars), ~ ifelse(is.na(.), 0, .)))
}

California_weather <- clean_weather_data(California_weather, Relevant_variables, Weather_type)
Carolina_weather   <- clean_weather_data(Carolina_weather,   Relevant_variables, Weather_type)


# 4. Missing data -------------------------------------------------------------

cat("Percentage of Missing Data (California):\n")
print(round((colSums(is.na(California_weather)) / nrow(California_weather)) * 100, 2))

cat("\nPercentage of Missing Data (South Carolina):\n")
print(round((colSums(is.na(Carolina_weather)) / nrow(Carolina_weather)) * 100, 2))

# Dropped for excessive missingness.
California_weather <- California_weather %>%
  select(-Average_Wind_Speed, -Fastest_Mile_Time, -Avg_Temperature)
Carolina_weather <- Carolina_weather %>%
  select(-Average_Wind_Speed, -Fastest_Mile_Time, -Avg_Temperature)

# 5. Drop infrequent weather types --------------------------------------------

event_counts_summary <- data.frame(
  California_Count = sapply(Weather_type, function(e) sum(California_weather[[e]] == 1, na.rm = TRUE)),
  Carolina_Count   = sapply(Weather_type, function(e) sum(Carolina_weather[[e]]   == 1, na.rm = TRUE))
)
cat("\nWeather Type Counts:\n")
print(event_counts_summary)

California_weather <- California_weather %>% select(-Tornado, -Damaging_Winds, -Snow)
Carolina_weather   <- Carolina_weather   %>% select(-Tornado, -Damaging_Winds, -Snow)


# 6. Collapse Fog / Heavy_Fog (interchangeable in the NOAA documentation) ------

California_weather <- California_weather %>%
  mutate(Fog = ifelse(Fog == 1 | Heavy_Fog == 1, 1, 0)) %>% select(-Heavy_Fog)
Carolina_weather <- Carolina_weather %>%
  mutate(Fog = ifelse(Fog == 1 | Heavy_Fog == 1, 1, 0)) %>% select(-Heavy_Fog)

summary(California_weather)
# ~30% of days had fog, 0.08% thunder, 37% smoke, 15.5% rain.
# Mean precipitation 0.026 in/day; mean max temp 70.7F, mean min temp 57.4F.
summary(Carolina_weather)
# ~42.7% fog, 14.3% thunder, 25.9% smoke, 32.8% rain.
# Mean precipitation 0.14 in/day; mean max temp 76.2F, mean min temp 55.5F.


# 7. Dates --------------------------------------------------------------------

California_weather$Date <- as.Date(California_weather$Date)
Carolina_weather$Date   <- as.Date(Carolina_weather$Date)
California_weather$Year <- year(California_weather$Date)
Carolina_weather$Year   <- year(Carolina_weather$Date)

California_storm$BEGIN_DATE <- as.Date(California_storm$BEGIN_DATE, format = "%m/%d/%Y")
California_storm$END_DATE   <- as.Date(California_storm$END_DATE,   format = "%m/%d/%Y")
Carolina_storm$BEGIN_DATE   <- as.Date(Carolina_storm$BEGIN_DATE,   format = "%m/%d/%Y")
Carolina_storm$END_DATE     <- as.Date(Carolina_storm$END_DATE,     format = "%m/%d/%Y")


# 8. Deduplicate storm events -------------------------------------------------
# The same storm is logged once per reporting office with minor differences in
# fields like begin time, while county, dates and event type stay identical.
# Without this the frequency counts are inflated.

deduplicate_storms <- function(storm_data) {
  storm_data %>%
    select(CZ_NAME_STR, BEGIN_DATE, EVENT_TYPE, END_DATE) %>%
    distinct()
}

California_storm_unique <- deduplicate_storms(California_storm)
Carolina_storm_unique   <- deduplicate_storms(Carolina_storm)

data.frame(
  Location = c("California", "California", "Carolina", "Carolina"),
  Stage = c("Before", "After", "Before", "After"),
  Count = c(nrow(California_storm), nrow(California_storm_unique),
            nrow(Carolina_storm),   nrow(Carolina_storm_unique))
)


# 9. Flag storm days ----------------------------------------------------------
flag_storms <- function(weather_data, storm_data) {
  storm_data <- storm_data %>%
    filter(!is.na(BEGIN_DATE), !is.na(END_DATE)) %>%
    mutate(.storm_row = row_number())
  
  matches <- weather_data %>%
    select(Date) %>%
    distinct() %>%
    inner_join(storm_data, by = join_by(between(Date, BEGIN_DATE, END_DATE))) %>%
    group_by(Date) %>%
    slice_max(.storm_row, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    transmute(Date, Storm_Flag = 1, Event_Type = EVENT_TYPE)
  
  weather_data %>%
    left_join(matches, by = "Date") %>%
    mutate(Storm_Flag = ifelse(is.na(Storm_Flag), 0, Storm_Flag))
}

California_weather <- flag_storms(California_weather, California_storm_unique)
Carolina_weather   <- flag_storms(Carolina_weather,   Carolina_storm_unique)

cat("\nStorm days flagged — California:", sum(California_weather$Storm_Flag),
    "| South Carolina:", sum(Carolina_weather$Storm_Flag), "\n")


# 10. Storm event frequencies -------------------------------------------------

count_storm_events <- function(storm_data) {
  storm_data %>%
    group_by(EVENT_TYPE) %>%
    summarize(Frequency = n(), .groups = "drop") %>%
    arrange(desc(Frequency))
}

california_storm_event_counts <- count_storm_events(California_storm_unique)
carolina_storm_event_counts   <- count_storm_events(Carolina_storm_unique)

california_storm_event_counts
carolina_storm_event_counts
