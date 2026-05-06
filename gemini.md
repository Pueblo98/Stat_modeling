# Gemini Assistant Guidelines for Statistical Modeling Project

Hello Gemini! When working on this repository, please strictly adhere to the following project directives and constraints:

## 1. Ultimate Project Goal
The final objective of this codebase is to **predict future flight cancellations and delays** utilizing historical US flight data and weather observations. Every step in data processing, feature engineering, and modeling should align with building predictive capabilities.

## 2. Code Language & Reproducibility
- **Pure R**: All implementation code must be written in **R scripts**.
- **Reproducibility First**:
  - Always use functions like `set.seed()` when stochastic components are involved.
  - Rely entirely on relative paths (e.g., `data/raw/`, `docs/`) rather than absolute paths to ensure the project can be run universally.
  - Keep data downloading and processing steps programmatic and strictly documented.

## 3. Project Documentation Context
Before implementing changes, ensure you are familiar with our foundational documents:
- **`docs/01_Data_Sourcing_and_Processing_Plan.md`**: Outlines our approach for fetching BTS flight data and METAR weather data, detailing how to correctly floor, merge, and clean the timestamps.
- **`docs/02_Modeling_Strategy.md`**: Directs the statistical methodologies, explicitly requiring the use of Generalized Additive Models (GAMs) via the `mgcv` package to identify non-linear relationships between weather triggers and delays.

## 4. Modeling Directives
- **Model Choice**: Utilize Logistic GAMs for prediction of binary cancellation/delay events (>15 mins), and continuous distributions (e.g., Gamma, Gaussian) for predicting exact delay duration.
- **Diagnostics Check**: You are expected to check basis dimensions and evaluate for concurvity.
- **Visualization**: Output clean, standardized plots of partial effects (e.g., via `gratia` or standard base plot functions).

Thank you for helping us confidently model and predict future flight disruptions!
