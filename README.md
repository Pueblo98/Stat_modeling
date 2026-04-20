# Statistical Modeling Project: Flight Delays & Extreme Weather

## Overview

This project investigates the impact of extreme weather conditions (temperature extremes, thunderstorms, precipitation, wind, and visibility) on flight delays and cancellations at five major US airports in 2024. It is structured in three sequential phases:

| Phase | Goal | Output |
|-------|------|--------|
| 1 — Explanatory | Identify *what* drives disruptions and model non-linear weather effects | Partial effect plots, concurvity checks, edf summaries |
| 2 — Predictive | Predict disruption probability and delay duration on held-out flights | AUC, Brier score, RMSE, calibration plots |
| 3 — Dashboard | Interactive tool to explore effects and run live predictions | R Shiny app |

## Methodology

The core statistical tool is a **Generalized Additive Model (GAM)** using the `mgcv` package (`bam()` for large-data efficiency). Two models are fit:

- **M1 — Logistic GAM**: Predicts P(flight disrupted), where disrupted = delayed >15 min or cancelled. Uses smooth `s()` terms for each weather variable to capture non-linear thresholds (e.g., delay spike above 95°F due to density altitude limits at PHX).
- **M2 — Gamma GAM**: Predicts expected delay duration (minutes) for flights that are delayed, using a log link.

Both models include airport fixed effects and temporal controls (departure hour as a cyclic spline, day-of-week, month).

## Airports

ORD (Chicago), PHX (Phoenix), ATL (Atlanta), DFW (Dallas), DEN (Denver) — selected for geographic and meteorological diversity.

## Data Sources

- **Flight data**: US Bureau of Transportation Statistics On-Time Performance, 2024 (downloaded as monthly `.zip` files).
- **Weather data**: Iowa State ASOS/METAR network via the `riem` R package — hourly observations matched to each flight's scheduled departure hour.

## Script Execution Order

```
scripts/01_download_data.R        # Download BTS + METAR raw data
scripts/02_process_data.R         # Merge, clean, feature-engineer → .rds
scripts/03_model_binary.R         # Fit M1 (logistic GAM), diagnostics
scripts/04_model_duration.R       # Fit M2 (Gamma GAM), diagnostics
scripts/05_evaluate_predictions.R # Test-set metrics and calibration
scripts/06_partial_effects.R      # Save all partial effect plots
scripts/07_shiny_dashboard.R      # Launch interactive Shiny app
```

## Project Structure

```
data/
  raw/          Raw BTS zips + METAR CSVs (git-ignored)
  processed/    Merged analytical dataset (git-ignored)
docs/
  01_Data_Sourcing_and_Processing_Plan.md
  02_Modeling_Strategy.md         ← GAM equations, phases 1 & 2 detail
  03_Dashboard_and_Visualization_Plan.md ← Shiny app spec
notebooks/      EDA notebooks
scripts/        R production scripts
outputs/
  figures/      Partial effect plots (PNG)
  m1_binary_gam.rds
  m2_duration_gam.rds
```

## Key Design Decisions

- `bam()` over `gam()` — handles 500k–1M row datasets via discrete smoothing.
- Temporal train/test split (Jan–Sep train, Oct–Dec test) — simulates real forecasting deployment.
- Cyclic spline for hour-of-day `s(dep_hour, bs = "cc")` — enforces continuity at midnight.
- All code in pure R with relative paths for full reproducibility.
