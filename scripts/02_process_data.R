# scripts/02_process_data.R
# This script processes raw flight and weather data into a unified, model-ready dataset.

library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(purrr)
library(stringr)

set.seed(42)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

airports <- c("ORD", "PHX", "ATL", "DFW", "DEN")

# Map airports to their respective timezones to align METAR UTC valid times with local departure times
tz_map <- c(
  "ATL" = "America/New_York",
  "ORD" = "America/Chicago",
  "DFW" = "America/Chicago",
  "DEN" = "America/Denver",
  "PHX" = "America/Phoenix"
)

# 1. Load and process weather data
cat("Loading weather data...\n")
weather_files <- list.files("data/raw", pattern = "weather_.*\\.csv", full.names = TRUE)
weather_raw <- map_df(weather_files, read_csv, show_col_types = FALSE)

weather <- weather_raw %>%
  mutate(Origin = str_remove(station, "^K")) %>%
  filter(Origin %in% airports) %>%
  # METAR 'valid' time is in UTC
  mutate(ObsTime = ymd_hms(valid, tz = "UTC"),
         # Round observation time to the nearest hour to align with flight schedule
         MergeTime = round_date(ObsTime, "hour")) %>%
  select(Origin, MergeTime, tmpf, p01i, sknt, vsby, wxcodes) %>%
  group_by(Origin, MergeTime) %>%
  # In case of multiple observations per hour, take the mean for numeric or first for text
  summarise(
    tmpf = mean(tmpf, na.rm = TRUE),
    p01i = sum(p01i, na.rm = TRUE),
    sknt = mean(sknt, na.rm = TRUE),
    # Use min for visibility since worse visibility governs
    vsby = min(vsby, na.rm = TRUE),
    wxcodes = first(na.omit(wxcodes)),
    .groups = "drop"
  )

# Forward fill small gaps in weather data
weather <- weather %>%
  group_by(Origin) %>%
  arrange(MergeTime) %>%
  fill(tmpf, p01i, sknt, vsby, wxcodes, .direction = "down") %>%
  ungroup() %>%
  mutate(
    # Fill NAs in p01i with 0 (no precipitation)
    p01i = replace_na(p01i, 0),
    # Create binary indicator for thunderstorms or severe weather code variants
    has_storm = if_else(str_detect(replace_na(wxcodes, ""), "TS|TSRA|FC"), 1, 0)
  )

# 2. Process Flight Data Iteratively to manage memory footprint
cat("Loading and processing flight data...\n")
flight_files <- list.files("data/raw", pattern = "flights_.*\\.zip", full.names = TRUE)

cols_to_keep <- c("FlightDate", "Origin", "Dest", "CRSDepTime", "DepDelay", 
                  "ArrDelay", "DepDel15", "ArrDel15", "Cancelled", "WeatherDelay")

processed_flights <- list()

for (file in flight_files) {
  cat(sprintf("Processing %s...\n", basename(file)))
  # Read needed columns only and filter by Focus Airports
  df_raw <- tryCatch({
    read_csv(file, col_select = any_of(cols_to_keep), show_col_types = FALSE) %>%
      filter(Origin %in% airports)
  }, error = function(e) {
    cat("  Error reading file, skipping.\n")
    return(NULL)
  })
  
  if (is.null(df_raw) || nrow(df_raw) == 0) next
  
  # Process departure time
  df_raw <- df_raw %>%
    mutate(
      CRSDepTime_char = as.character(CRSDepTime),
      # Fix midnight represented as 2400
      CRSDepTime_char = if_else(CRSDepTime_char == "2400", "0000", CRSDepTime_char),
      crs_dep_pad = str_pad(CRSDepTime_char, 4, pad = "0"),
      dt_str = paste(FlightDate, crs_dep_pad)
    )
  
  # Safely assign timezone and convert to UTC for accurate merging with METAR
  df_list <- list()
  for (ap in airports) {
    ap_df <- df_raw %>% filter(Origin == ap)
    if (nrow(ap_df) > 0) {
      suppressWarnings({
        ap_df$LocalDT <- ymd_hm(ap_df$dt_str, tz = tz_map[[ap]])
        ap_df$MergeTime <- floor_date(with_tz(ap_df$LocalDT, "UTC"), "hour")
      })
      df_list[[ap]] <- ap_df
    }
  }
  processed_flights[[file]] <- bind_rows(df_list)
}

flights <- bind_rows(processed_flights)

# 3. Merge Flights and Weather
cat("Merging flights and weather metrics...\n")
model_data <- flights %>%
  inner_join(weather, by = c("Origin", "MergeTime")) %>%
  # Drop flights with missing critical weather parameters even after forward filling
  drop_na(tmpf, sknt, vsby)

# 4. Final cleaning and adjustments for GAM
model_data <- model_data %>%
  mutate(
    # Ensure binary format for indicator variables
    DepDel15 = as.numeric(DepDel15),
    ArrDel15 = as.numeric(ArrDel15),
    Cancelled = as.numeric(Cancelled),
    WeatherDelay = as.numeric(replace_na(WeatherDelay, 0))
  ) %>%
  select(FlightDate, Origin, Dest, LocalDT, MergeTime, DepDelay, ArrDelay, DepDel15, ArrDel15, 
         Cancelled, WeatherDelay, tmpf, p01i, sknt, vsby, has_storm, wxcodes)

# Save processed dataset
cat("Saving processed model data...\n")
saveRDS(model_data, "data/processed/flights_weather_2024.rds")

cat("Data processing complete! Final dataset has", nrow(model_data), "rows.\n")
