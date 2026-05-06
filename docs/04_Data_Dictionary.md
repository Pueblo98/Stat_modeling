# Data Dictionary: Processed Model Dataset

This document details the final variables available in the processed output file (`data/processed/flights_weather_2024.rds`).
This dataset was assembled from historical METAR weather readings and BTS Airline On-Time Performance data and acts as the foundational entity for our Generalized Additive Models (GAMs).

## 1. Flight Descriptors 
*   **`FlightDate`** *(Date)*: The local calendar date of the flight.
*   **`Origin`** *(String)*: The 3-letter IATA code of the departure airport (e.g., ORD, ATL, PHX).
*   **`Dest`** *(String)*: The 3-letter IATA code of the destination airport.

## 2. Timestamps
*   **`LocalDT`** *(POSIXct Datetime)*: The exact scheduled departure date and time converted structurally into the local timezone for the specific `Origin` airport (e.g., America/Chicago for ORD).
*   **`MergeTime`** *(POSIXct Datetime)*: A UTC (Universal Coordinated Time) representation of `LocalDT` floored to the nearest hour. This exists primarily as the primary key mapped to historic Hourly METAR API responses, bridging local flight departures to global clock weather events safely.

## 3. Delay & Disruption Outcomes (Response Variables)
*   **`DepDel15`** *(Numeric/Binary)*: Outcome indicator strictly defining if the aircraft departed equal to or greater than 15 minutes after its scheduled time. `1` = Delayed, `0` = On-time/Early. (Ideal limit choice for Logistic GAM predictions).
*   **`ArrDel15`** *(Numeric/Binary)*: Equivalent binary indicator marking if the flight arrived 15+ minutes late.
*   **`Cancelled`** *(Numeric/Binary)*: Binary indicator showing if a flight was completely canceled. `1` = Canceled, `0` = Flow.
*   **`DepDelay`** *(Numeric)*: Continuous measurement defining exactly how many minutes departure occurred post-schedule.
*   **`ArrDelay`** *(Numeric)*: Continuous measurement defining exactly how many minutes arrival occurred post-schedule.
*   **`WeatherDelay`** *(Numeric)*: An exclusive DOT classification stating the number of delay minutes objectively attributed to severe meteorological events by the carrier. 

## 4. Meteorological Indicators (Independent Predictors)
*   **`tmpf`** *(Numeric)*: The measured air temperature in Fahrenheit at the origin airport at `MergeTime`. (GAM basis: Extreme heat degrades lift; extreme cold triggers de-icing rules).
*   **`p01i`** *(Numeric)*: Measured inches of liquid precipitation deposited at the terminal in the last hour.
*   **`sknt`** *(Numeric)*: The measured sustained wind speed in Knots. 
*   **`vsby`** *(Numeric)*: Measured standard linear ground visibility in miles. Low integers trigger protective spacing measures.
*   **`has_storm`** *(Numeric/Binary)*: A custom-engineered feature evaluating standard string weather descriptions. If the METAR report contained variants of `TS` (Thunderstorms), `TSRA` (Thunderstorm Rain), or `FC` (Funnel Cloud), this is forced to `1`. Otherwise, `0`. (Easier dimension array for models than categorical combinations).
*   **`wxcodes`** *(String)*: The raw string of current METAR meteorological conditions for reference contexts (e.g., BR=Mist, FZ=Freezing).
