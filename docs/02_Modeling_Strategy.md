# Modeling Strategy: Generalized Additive Models (GAM)

## 1. Objective

We aim to model the relationship between weather variables ($X$) and flight outcomes ($Y$) without imposing rigid linear assumptions. Given that physical limits often trigger non-linear responses (e.g., planes cannot take off above a certain temperature due to air density reducing lift), a GAM is the ideal statistical tool. The project is broken into two modeling phases:

- **Phase 1 — Explanatory**: Identify *what* drives cancellations and delays (inference focus, partial effect plots, hypothesis testing).
- **Phase 2 — Predictive**: Use the fitted models to *predict* future disruption probability and expected delay duration (held-out evaluation, calibration).

---

## 2. Response Variables

We fit two separate models, each targeting a different outcome:

| Model | Response | Distribution | Link | Script |
|-------|----------|--------------|------|--------|
| M1 | Binary delay flag (`DEP_DEL15 == 1` OR `CANCELLED == 1`) | Bernoulli | logit | `03_model_binary.R` |
| M2 | Delay duration in minutes (`DEP_DELAY`, flights > 0 min only) | Gamma | log | `04_model_duration.R` |

M1 answers: *"Will this flight be disrupted?"*
M2 answers: *"If it is delayed, by how many minutes?"*

---

## 3. Covariates and Feature Engineering

All features are generated in `02_process_data.R` and stored in `data/processed/flights_weather_2024.rds`.

### Weather Features (smooth terms)
| Variable | Source | Engineering |
|----------|--------|-------------|
| `tmpf` | METAR | Temperature (°F) at scheduled departure hour |
| `p01i` | METAR | Precipitation (inches) in last hour |
| `sknt` | METAR | Wind speed (knots) |
| `vsby` | METAR | Visibility (miles) |
| `thunderstorm` | METAR `presentwx` | Binary flag: 1 if `TS` or `TSRA` present |

### Temporal Controls (parametric terms)
| Variable | Engineering |
|----------|-------------|
| `dep_hour` | Integer 0–23 from `CRS_DEP_TIME` |
| `day_of_week` | Factor, Mon–Sun |
| `month` | Factor, Jan–Dec |

### Spatial Controls
| Variable | Engineering |
|----------|-------------|
| `ORIGIN` | Factor with 5 levels (ORD, PHX, ATL, DFW, DEN); acts as airport fixed effect |

---

## 4. GAM Equation

### M1: Logistic GAM (Binary Disruption)

$$\text{logit}(P(\text{disrupted}_i)) = \beta_0 + s(\text{tmpf}_i) + s(\text{p01i}_i) + s(\text{sknt}_i) + s(\text{vsby}_i) + \beta_{\text{thunder}} \cdot \text{thunderstorm}_i + \alpha_{\text{airport}(i)} + \gamma_{\text{hour}(i)} + \delta_{\text{dow}(i)}$$

### M2: Gamma GAM (Delay Duration)

$$\log(E[\text{DEP\_DELAY}_i]) = \beta_0 + s(\text{tmpf}_i) + s(\text{p01i}_i) + s(\text{sknt}_i) + s(\text{vsby}_i) + \beta_{\text{thunder}} \cdot \text{thunderstorm}_i + \alpha_{\text{airport}(i)} + \gamma_{\text{hour}(i)} + \delta_{\text{dow}(i)}$$

**Component notes:**
- `s()` uses thin plate regression splines (default in `mgcv`) with basis dimension `k = 10` as a starting point, verified with `gam.check()`.
- Airport effects $\alpha$ are fit as fixed factor levels (5 airports is small enough to avoid the need for random effects).
- Hour effects $\gamma$ are fit as a cyclic cubic spline `s(dep_hour, bs = "cc")` to enforce continuity at midnight.

### Implementation: `bam()` vs `gam()`

We use `bam()` (Big Additive Models) from `mgcv` throughout because the 2024 BTS dataset for 5 airports will have ~500k–1M rows. `bam()` uses discrete smoothing and is memory-efficient for datasets of this scale.

```r
library(mgcv)

m1 <- bam(
  disrupted ~ s(tmpf, k = 10) + s(p01i, k = 10) + s(sknt, k = 10) +
              s(vsby, k = 10) + thunderstorm +
              ORIGIN + s(dep_hour, bs = "cc", k = 24) + day_of_week,
  data    = train_df,
  family  = binomial(link = "logit"),
  method  = "fREML",
  discrete = TRUE
)
```

---

## 5. Phase 1 — Explanatory Analysis

**Goal**: Understand which weather variables matter and *how* their effect is shaped (linear vs. threshold vs. U-shaped).

### Steps
1. **Fit M1 and M2** on the full 2024 dataset.
2. **Check basis dimensions** with `gam.check(m1)`: p-values for `k-index` should be > 0.05, indicating `k` is large enough.
3. **Check concurvity** with `concurvity(m1, full = FALSE)`: values above 0.8 on the `estimate` column flag multicollinearity among smooth terms (e.g., temperature and precipitation may co-vary seasonally).
4. **Summarize** with `summary(m1)`: inspect edf (effective degrees of freedom) for each smooth — edf ≈ 1 means linear, edf >> 1 means strong non-linearity.
5. **Plot partial effects** using `gratia::draw(m1)` or `plot.gam(m1, pages = 1)`.

### Expected Findings (hypotheses to confirm)
- Temperature: U-shaped or J-shaped curve — delays increase sharply below ~20°F (ice/deicing) and above ~95°F (density altitude limits, especially PHX).
- Precipitation: Monotonically increasing effect, with a sharp inflection around 0.1–0.5 inches/hour.
- Wind speed: Threshold effect around 25–30 knots (crosswind/tailwind operating limits).
- Visibility: Strong inverse non-linear effect; very low visibility (< 1 mile) triggers sharp increase in cancellations.
- Thunderstorm: Positive coefficient — ground stops triggered by convective activity.

---

## 6. Phase 2 — Predictive Modeling

**Goal**: Evaluate how well the fitted GAMs generalize to unseen flights and produce calibrated risk scores.

### Train/Test Split
- **Training**: Jan–Sep 2024 (9 months)
- **Test**: Oct–Dec 2024 (3 months, held out entirely)
- Split is temporal (not random) to simulate real-world deployment where we predict future flights.

```r
set.seed(42)
train_df <- flights_weather |> filter(month(FL_DATE) <= 9)
test_df  <- flights_weather |> filter(month(FL_DATE) >= 10)
```

### Evaluation Metrics

**M1 (Binary)**:
- AUC-ROC (discrimination)
- Brier Score (calibration)
- Confusion matrix at threshold p = 0.30 (tunable in dashboard)

**M2 (Duration, on delayed flights)**:
- RMSE and MAE on held-out delayed flights
- Coverage of 90% prediction intervals (using `mgcv` prediction SE)

### Prediction Workflow
New data (e.g., a specific flight on a future date with a weather forecast) is fed through the same feature engineering pipeline, then:

```r
pred_prob <- predict(m1, newdata = new_flight, type = "response")
pred_mins <- predict(m2, newdata = new_flight, type = "response")
```

This is the core of the interactive dashboard (Phase 3).

---

## 7. Diagnostics Checklist

Before finalizing any model, verify all of the following:

- [ ] `gam.check()`: k-index p-values > 0.05 for all smooth terms
- [ ] `concurvity()`: no pair above 0.8 on `estimate`
- [ ] Residual plots: no obvious patterns in residuals vs. fitted values
- [ ] For M1: calibration plot (predicted probability vs. observed rate in decile bins)
- [ ] For M2: log-residuals approximately normal (QQ plot)

---

## 8. Script Execution Order

```
scripts/
  01_download_data.R        # Download BTS zips + METAR via riem
  02_process_data.R         # Unzip, filter, merge, feature engineering → .rds
  03_model_binary.R         # Fit M1, diagnostics, save model object
  04_model_duration.R       # Fit M2, diagnostics, save model object
  05_evaluate_predictions.R # Test set evaluation, metrics, calibration plots
  06_partial_effects.R      # Generate and save all partial effect plots
  07_shiny_dashboard.R      # Launch interactive Shiny app
```

All outputs (model `.rds` files, figures) are saved to `outputs/`.
