# ── Data Setup for Flight Weather Project ─────────────────────────────────────
# CCI26 Statistical Modelling Project
# "How do weather conditions affect flight delays at major US airports?"
#
# This script documents exactly how the datasets were built and lets teammates
# either (A) load the pre-built .rds files directly, or (B) rebuild from scratch.
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(readr)

# ── OPTION A: Load pre-built datasets (fastest) ───────────────────────────────
# All three .rds files must be in your working directory.
# Ask a teammate to share them, or run Option B below to rebuild.

flights_weather <- readRDS("flights_weather.rds")  # main analysis dataset
# 545,526 rows × 19 columns

# Columns:
#   From BTS flight data:
#     FL_DATE          – flight date (character "YYYY-MM-DD")
#     AIRLINE          – airline code
#     ORIGIN           – departure airport (ATL / DEN / LAX / MCO / ORD)
#     ORIGIN_CITY      – city name of origin
#     DEST             – destination airport code
#     DEST_CITY        – city name of destination
#     DEP_DELAY        – departure delay in minutes (negative = early)
#     ARR_DELAY        – arrival delay in minutes
#     CANCELLED        – 1 = cancelled, 0 = not cancelled
#     CANCELLATION_CODE– reason for cancellation (A/B/C/D or NA)
#     DELAY_DUE_WEATHER– minutes of delay attributed to weather
#     DELAY_DUE_CARRIER– minutes of delay attributed to carrier
#     DELAY_DUE_NAS    – minutes of delay attributed to NAS
#     AIR_TIME         – actual flight time in minutes
#     DISTANCE         – distance in miles
#     date             – FL_DATE as Date object
#   From NOAA ISD weather (daily aggregates, origin airport):
#     temp_mean        – mean daily temperature at origin (°C)
#     temp_max         – max daily temperature at origin (°C)
#     temp_min         – min daily temperature at origin (°C)
#
# NOTE: precipitation and wind speed were NOT included despite being planned.
# The NOAA download loop only parsed the TMP field. This is a known limitation.

cat("Rows:", nrow(flights_weather), "\n")
cat("Date range:", range(as.Date(flights_weather$FL_DATE)), "\n")
cat("Airports:", sort(unique(flights_weather$ORIGIN)), "\n")
cat("Weather NAs (temp_mean):", sum(is.na(flights_weather$temp_mean)), "\n")


# ── Model-ready dataset ───────────────────────────────────────────────────────
# Filters applied for the GAM: remove cancellations and COVID disruption period.
# COVID exclusion: 2020-03-01 to 2021-06-01 (arbitrary but standard in the lit).

library(lubridate)

flights_model <- flights_weather |>
  filter(
    !is.na(DEP_DELAY),
    CANCELLED == 0,
    !(FL_DATE >= as.Date("2020-03-01") & FL_DATE <= as.Date("2021-06-01"))
  ) |>
  mutate(
    month  = as.integer(month(as.Date(FL_DATE))),
    ORIGIN = factor(ORIGIN)
  )

cat("Model dataset rows:", nrow(flights_model), "\n")


# ── OPTION B: Rebuild from scratch ────────────────────────────────────────────

## Step 1 – Flight data
# Source: Kaggle BTS dataset "US Flight Delays and Cancellations"
# Download URL (requires free Kaggle account):
#   https://www.kaggle.com/datasets/robikscube/flight-delay-dataset-20182022
# File used: flights_sample_3m.csv.zip  (place in ~/Downloads/)
#
# unzip("~/Downloads/flights_sample_3m.csv.zip", exdir = "~/Downloads/")
# flights_raw <- read.csv("~/Downloads/flights_sample_3m.csv")
# flights <- flights_raw |>
#   select(FL_DATE, AIRLINE, ORIGIN, ORIGIN_CITY, DEST, DEST_CITY,
#          DEP_DELAY, ARR_DELAY, CANCELLED, CANCELLATION_CODE,
#          DELAY_DUE_WEATHER, DELAY_DUE_CARRIER, DELAY_DUE_NAS,
#          AIR_TIME, DISTANCE)
# flights_5 <- flights |>
#   filter(ORIGIN %in% c("ATL", "ORD", "DEN", "LAX", "MCO")) |>
#   mutate(date = as.Date(FL_DATE))
# saveRDS(flights_5, "flights_5airports.rds")


## Step 2 – Weather data (NOAA ISD, downloaded directly — no account needed)
# Station IDs used (NOAA Integrated Surface Database):
#   ATL (Hartsfield-Jackson):  72219013874
#   ORD (Chicago O'Hare):      72530094846
#   DEN (Denver Intl):         72565003017  ← NOTE: 72206003017 failed, this is the fix
#   LAX (Los Angeles Intl):    72295023174
#   MCO (Orlando Intl):        72205012815

# parse_temp <- function(tmp) {
#   val <- as.numeric(substr(tmp, 1, 5)) / 10
#   ifelse(abs(val) > 80, NA, val)   # removes physically impossible values
# }
#
# get_noaa_weather <- function(station_id, year) {
#   url <- paste0("https://www.ncei.noaa.gov/data/global-hourly/access/",
#                 year, "/", station_id, ".csv")
#   read_csv(url, show_col_types = FALSE)
# }
#
# station_noaa <- c(
#   ATL = "72219013874", ORD = "72530094846", DEN = "72565003017",
#   LAX = "72295023174", MCO = "72205012815"
# )
#
# weather_all <- list()
# for (airport in names(station_noaa)) {
#   for (yr in 2019:2023) {
#     message("Downloading ", airport, " - ", yr)
#     tryCatch({
#       raw <- get_noaa_weather(station_noaa[airport], yr)
#       daily <- raw |>
#         mutate(date = as.Date(DATE), temp = parse_temp(TMP)) |>
#         filter(!is.na(temp)) |>
#         group_by(date) |>
#         summarise(temp_mean = mean(temp, na.rm = TRUE),
#                   temp_max  = max(temp, na.rm = TRUE),
#                   temp_min  = min(temp, na.rm = TRUE),
#                   .groups   = "drop") |>
#         mutate(airport = airport)
#       weather_all[[paste(airport, yr)]] <- daily
#     }, error = function(e) message("Failed: ", airport, yr, " - ", e$message))
#   }
# }
# weather_daily <- bind_rows(weather_all) |>
#   distinct(airport, date, .keep_all = TRUE)
# saveRDS(weather_daily, "weather_daily.rds")


## Step 3 – Merge
# flights_5 <- readRDS("flights_5airports.rds") |> mutate(date = as.Date(FL_DATE))
# weather_daily <- readRDS("weather_daily.rds")
# flights_weather <- flights_5 |>
#   left_join(weather_daily, by = c("ORIGIN" = "airport", "date" = "date"))
# saveRDS(flights_weather, "flights_weather.rds")


# ── Data quality notes ────────────────────────────────────────────────────────
# STRENGTHS:
#   - Large sample: ~545k flights across 5 climatically diverse airports
#   - Official government sources (BTS + NOAA) — not scraped/secondary
#   - 5-year span (2019–2023) captures seasonal variation well
#   - DEN and ORD provide cold extremes; MCO/LAX provide warm/mild contrast
#
# LIMITATIONS:
#   - Temperature only — no precipitation or wind speed in the final data
#     (code had these planned but the NOAA parse loop was temperature-only)
#   - Only origin airport weather — destination conditions are ignored
#   - COVID exclusion (Mar 2020–Jun 2021) removes ~15 months; boundary is debatable
#   - Kaggle dataset is a sample (~3M flights), not the full BTS universe
#   - No time-of-day weather — daily mean temp is a simplification
