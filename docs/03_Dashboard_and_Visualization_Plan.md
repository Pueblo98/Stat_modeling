# Dashboard & Visualization Plan

## 1. Overview

After fitting the two GAMs (M1: binary disruption probability, M2: expected delay duration), we build an interactive **R Shiny dashboard** (`scripts/07_shiny_dashboard.R`) that serves two purposes:

1. **Explore** — display the partial effect plots from Phase 1 so users can see *how* each weather variable drives disruptions.
2. **Predict** — let users dial in a specific scenario (airport + date/time + weather forecast) and get a live disruption probability and expected delay.

---

## 2. Dashboard Layout

The app uses a `navbarPage` layout with two tabs:

### Tab 1: "Explore Weather Effects"

Shows the partial effects from M1 and M2 side by side.

**Controls (sidebar):**
- Dropdown: Select model (M1 — Disruption Probability, M2 — Delay Duration)
- Dropdown: Select variable to plot (Temperature, Precipitation, Wind Speed, Visibility)
- Checkbox: Overlay rug plot of observed data density

**Main panel:**
- `gratia`-style smooth plot with 95% confidence ribbon
- Below the plot: a short plain-English interpretation automatically generated based on the shape (e.g., "Delays increase sharply above 95°F, consistent with density altitude limits")
- A summary table of edf values for all smooth terms (so users can see degree of non-linearity at a glance)

### Tab 2: "Predict a Flight"

Users input a hypothetical flight scenario and get real-time predictions from the saved model objects.

**Input panel (sliders + dropdowns):**

| Input | Widget | Range / Options |
|-------|--------|----------------|
| Airport | `selectInput` | ORD, PHX, ATL, DFW, DEN |
| Month | `selectInput` | Jan–Dec |
| Day of week | `selectInput` | Mon–Sun |
| Departure hour | `sliderInput` | 0–23 |
| Temperature (°F) | `sliderInput` | -20 to 120 |
| Precipitation (in/hr) | `sliderInput` | 0 to 2.0 |
| Wind speed (knots) | `sliderInput` | 0 to 60 |
| Visibility (miles) | `sliderInput` | 0 to 10 |
| Thunderstorm present | `checkboxInput` | TRUE / FALSE |

**Output panel:**

- **Disruption risk gauge**: A `plotly` gauge showing P(disrupted) from M1 with color zones (green < 20%, yellow 20–40%, red > 40%)
- **Expected delay**: A numeric output from M2 showing "Expected delay if disrupted: X minutes" with a ±1 SE interval
- **Comparison bar**: Horizontal bar comparing the scenario delay against airport average delay for the chosen conditions

---

## 3. Partial Effect Plot Specifications

All saved partial effect plots (generated in `06_partial_effects.R`) follow these conventions:

- **Package**: `gratia` (wraps `ggplot2`, consistent theming)
- **Theme**: `theme_minimal()` + custom title and axis labels
- **Color**: Single color per smooth, with a shaded 95% credible interval
- **Annotations**: Vertical dashed lines marking operationally meaningful thresholds (e.g., 32°F freezing, 0.25 in/hr heavy rain, 25 knot crosswind limit)
- **File format**: `.png` at 300 DPI, saved to `outputs/figures/`

Example annotations to draw on temperature plot:
```r
geom_vline(xintercept = 32,  linetype = "dashed", color = "steelblue",  label = "Freezing") +
geom_vline(xintercept = 95,  linetype = "dashed", color = "firebrick",  label = "Density altitude limit") +
```

---

## 4. Implementation Notes

### Loading pre-fitted models
The dashboard loads pre-saved model objects at startup so there is no fitting delay:

```r
m1 <- readRDS("outputs/m1_binary_gam.rds")
m2 <- readRDS("outputs/m2_duration_gam.rds")
```

### Reactive prediction
When any input slider changes, a `reactive()` block rebuilds the `new_data` data frame and calls `predict()`:

```r
prediction <- reactive({
  nd <- data.frame(
    ORIGIN      = input$airport,
    tmpf        = input$temperature,
    p01i        = input$precip,
    sknt        = input$wind,
    vsby        = input$visibility,
    thunderstorm = as.integer(input$thunderstorm),
    dep_hour    = input$hour,
    day_of_week = input$dow,
    month       = input$month
  )
  list(
    prob  = predict(m1, newdata = nd, type = "response"),
    mins  = predict(m2, newdata = nd, type = "response", se.fit = TRUE)
  )
})
```

### Required packages
```r
library(shiny)
library(ggplot2)
library(gratia)
library(plotly)
library(mgcv)
library(dplyr)
```

---

## 5. Deployment

The app can be run locally with `Rscript scripts/07_shiny_dashboard.R` or deployed to **shinyapps.io** using `rsconnect::deployApp()`. The dashboard does not require the raw data — only the two `.rds` model objects in `outputs/`.
