# Statistical Modeling Project: Flight Delays & Extreme Weather

## Overview
This project investigates the impact of extreme weather conditions (temperature extremes, thunderstorms, and precipitation) on flight delays and cancellations at major US airports.

## Methodology
The core analysis relies on **Generalized Additive Models (GAMs)** to capture the non-linear "smooth effects" of weather variables (like the sharp increase in delays at very high temperatures due to plane weight limits) while controlling for spatial and temporal variables (airport fixed effects, hour-of-day, day-of-week).

## Project Structure
- `data/`
  - `raw/`: Unprocessed data from US BTS and METAR (Ignored by Git)
  - `processed/`: Cleaned and merged analysis datasets (Ignored by Git)
- `docs/`: Project planning documents and methodological outline
- `notebooks/`: Exploratory Data Analysis (EDA)
- `scripts/`: Production R/Python scripts for data processing and model fitting
- `outputs/`: Saved models and figures
