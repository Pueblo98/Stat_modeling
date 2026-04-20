# Data Sourcing and Processing Plan

## 1. Primary Datasets

### A. Flight Data (US Bureau of Transportation Statistics)
**Source:** [BTS Marketing Carrier On-Time Performance](https://www.transtats.bts.gov/)
**Format:** CSV
**Variables of Interest:**
- `FL_DATE`: Flight Date
- `ORIGIN` / `DEST`: Airport Codes (e.g., ORD, ATL, DFW)
- `DEP_DEL15`, `ARR_DEL15`: Cancellation & Delay indicators (Binary)
- `DEP_DELAY`, `ARR_DELAY`: Continuous delay duration in minutes
- `CRS_DEP_TIME`: Planned departure time
- `WEATHER_DELAY`: Known delay directly attributed to weather

### B. Weather Data (METAR)
**Source:** [Iowa State University ASOS/AWOS Network](https://mesonet.agron.iastate.edu/request/download.phtml) or `riem` R package.
**Format:** Hourly observations per airport
**Variables of Interest:**
- `tmpf`: Air temperature (Fahrenheit)
- `p01i`: Precipitation in the last hour
- `sknt`: Wind speed in knots
- `vby`: Visibility
- `presentwx`: Weather codes (for extracting thunderstorms: `TS` or `TSRA`)

## 2. Selection Scope
Given the scale of the BTS data, we will initially scope the analysis for feasibility:
- **Timeframe:** Jan 1, 2024 through Dec 31, 2024 (a year with record disruptions).
- **Airports:** Focus on the top 5 busiest US hubs with historically varied weather (e.g., `ORD` for winter/storms, `PHX` for extreme heat, `ATL` for rain/storms).

## 3. Merging and Alignment Strategy
A critical challenge is aligning hourly weather with individual flight rows.

**Steps:**
1. Floor the `CRS_DEP_TIME` (scheduled departure time) to the nearest hour.
2. Join the flight dataset with the METAR dataset based on `ORIGIN` == Airport ID, and `Date + Hour` == METAR Observation Time.
3. Handle missing weather data systematically (forward-fill for small gaps, drop if gap is significant).

## 4. Processing Output
The final script in the `scripts/` directory will output an analytical dataset (e.g., `data/processed/flights_weather_2024.rds`).
This dataset will contain one row per flight, including the flight outcomes and the observed weather parameters at the origin airport at the scheduled departure time.
