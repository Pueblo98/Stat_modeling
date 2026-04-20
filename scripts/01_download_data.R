dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# Increase timeout for large files
options(timeout = 600)

# 1. Download BTS On-Time Performance Data for 2024
# According to BTS PREZIP directory structure documentation
year <- 2024
months <- 1:12
for (m in months) {
  url <- sprintf("https://transtats.bts.gov/PREZIP/On_Time_Reporting_Carrier_On_Time_Performance_1987_present_%d_%d.zip", year, m)
  dest_file <- sprintf("data/raw/flights_%d_%02d.zip", year, m)
  if (!file.exists(dest_file)) {
    cat(sprintf("Downloading flights for %d-%02d...\n", year, m))
    tryCatch({
      download.file(url, dest_file, mode = "wb", quiet = TRUE)
      cat("Success.\n")
    }, error = function(e) {
      cat(sprintf("Failed to download month %d: %s\n", m, e$message))
    })
  } else {
    cat(sprintf("File %s already exists.\n", dest_file))
  }
}

# 2. Download Weather Data using riem
if (!requireNamespace("riem", quietly = TRUE)) {
  install.packages("riem", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr", repos = "https://cloud.r-project.org")
}
if (!requireNamespace("readr", quietly = TRUE)) {
  install.packages("readr", repos = "https://cloud.r-project.org")
}

library(riem)
library(dplyr)
library(readr)

airports <- c("ORD", "PHX", "ATL", "DFW", "DEN")
# RIEM uses station IDs which are typically the airport code prefixed with K for mainland US
stations <- paste0("K", airports)

start_date <- "2024-01-01"
end_date <- "2024-12-31"

for (i in seq_along(stations)) {
  station <- stations[i]
  dest_file <- sprintf("data/raw/weather_%s_2024.csv", airports[i])
  if (!file.exists(dest_file)) {
    cat(sprintf("Downloading weather data for %s...\n", airports[i]))
    tryCatch({
      weather_data <- riem_measures(station = station, date_start = start_date, date_end = end_date)
      write_csv(weather_data, dest_file)
      cat("Success.\n")
    }, error = function(e) {
      cat(sprintf("Failed to download weather for %s: %s\n", station, e$message))
    })
  } else {
    cat(sprintf("File %s already exists.\n", dest_file))
  }
}

cat("Data sourcing complete.\n")
