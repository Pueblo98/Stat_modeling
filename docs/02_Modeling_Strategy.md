# Modeling Strategy: Generalized Additive Models (GAM)

## 1. Objective
We aim to model the relationship between weather variables ($X$) and flight delays ($Y$) without imposing rigid linear assumptions. Given that biological or physical limits often trigger non-linear responses (e.g., planes can't take off above a certain threshold temperature due to air density), a GAM is the ideal statistical tool.

## 2. Model Formulation
We propose using the `mgcv` package in R (specifically the `bam()` function, which is optimized for large datasets).

### Defining the Response Variable
We can run two models:
1. **Logistic GAM:** Where the outcome $Y_i \sim \text{Bernoulli}(p)$ and $p$ is the probability of a delay > 15 minutes.
2. **Gamma/Gaussian GAM:** Where $Y_i$ is the exact delay time in minutes (excluding early/on-time flights).

### The GAM Equation
$$ g(E[Y_i]) = \beta_0 + s(\text{Temperature}_i) + s(\text{Precipitation}_i) + s(\text{WindSpeed}_i) + \alpha_{\text{airport}(i)} + \gamma_{\text{hour}(i)} $$

**Components:**
- $g(\cdot)$: Link function (logit for binary delay, log for continuous minutes).
- $s()$: Smooth functions (e.g., thin plate regression splines or cubic splines) to capture the non-linear weather effects.
- $\alpha_{\text{airport}}$: Random or fixed effects accounting for baseline congestion at specific airports.
- $\gamma_{\text{hour}}$: Categorical fixed effects accounting for peak vs. off-peak hours in a day.

## 3. Assumptions and Basis Dimensions
A critical part of our modeling effort will be checking GAM assumptions:
- **Basis Dimension ($k$):** We need to set the `k` parameter high enough to capture complexity but employ penalization to prevent overfitting. We will use `gam.check()` to ensure `k` is sufficient.
- **Concurvity:** Evaluated to ensure the non-linear terms aren't perfectly explaining each other (similar to collinearity in general linear models).

## 4. Visualization & Output
A key feature of this assignment will be standardizing visually appealing plots.
- We will focus on utilizing `plot.gam()` or the `gratia` package to visualize the partial effect of temperature. We expect to see a flat line at moderate temperatures and a sharp non-linear spike in delays as it approaches extremely hot or freezing conditions.
