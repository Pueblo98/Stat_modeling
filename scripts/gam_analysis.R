library(mgcv)
library(ggplot2)
library(dplyr)
library(lubridate)

# ── Load & prep ────────────────────────────────────────────────────────────────
flights_weather <- readRDS("flights_weather.rds")

flights_model <- flights_weather |>
  filter(
    !is.na(DEP_DELAY),
    CANCELLED == 0,
    !(FL_DATE >= as.Date("2020-03-01") & FL_DATE <= as.Date("2021-06-01"))
  ) |>
  mutate(
    month  = as.integer(lubridate::month(FL_DATE)),
    ORIGIN = factor(ORIGIN)
  )

cat("Rows in model dataset:", nrow(flights_model), "\n")
cat("Airports:", levels(flights_model$ORIGIN), "\n")

# ── Fit GAM ───────────────────────────────────────────────────────────────────
set.seed(42)
gam_fit <- bam(
  DEP_DELAY ~ s(temp_mean, k = 10) + s(month, bs = "cc", k = 12) + ORIGIN,
  data   = flights_model,
  method = "fREML"
)

summary(gam_fit)

# ── Diagnostics ───────────────────────────────────────────────────────────────
png("gam_diagnostics.png", width = 1200, height = 1000, res = 150)
par(mfrow = c(2, 2))
gam.check(gam_fit)
dev.off()

# ── Smooth plots ──────────────────────────────────────────────────────────────
png("gam_smooths.png", width = 1400, height = 600, res = 150)
par(mfrow = c(1, 2))
plot(gam_fit, pages = 0, shade = TRUE, seWithMean = TRUE)
dev.off()

# ── Predicted delay by temp, per airport ──────────────────────────────────────
newdata <- expand.grid(
  temp_mean = seq(min(flights_model$temp_mean, na.rm = TRUE),
                  max(flights_model$temp_mean, na.rm = TRUE), length.out = 200),
  month     = 7L,
  ORIGIN    = levels(flights_model$ORIGIN)
)

preds <- predict(gam_fit, newdata = newdata, se.fit = TRUE)
newdata$fit <- preds$fit
newdata$lwr <- preds$fit - 1.96 * preds$se.fit
newdata$upr <- preds$fit + 1.96 * preds$se.fit

p <- ggplot(newdata, aes(x = temp_mean, y = fit, colour = ORIGIN, fill = ORIGIN)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1) +
  labs(
    title    = "Predicted departure delay by daily mean temperature",
    subtitle = "Holding month = July; 95% CI shaded",
    x        = "Daily mean temperature (°F)",
    y        = "Predicted departure delay (min)",
    colour   = "Airport",
    fill     = "Airport"
  ) +
  theme_minimal()

ggsave("gam_temp_effect.png", p, width = 8, height = 5, dpi = 150)

cat("Done. Outputs: gam_diagnostics.png, gam_smooths.png, gam_temp_effect.png\n")

# ── Robustness check: Tweedie GAM ─────────────────────────────────────────────
# Tweedie handles right-skewed, non-negative data — better suited to delays
# DEP_DELAY can be negative (early departures), so we floor at 0
flights_model <- flights_model |>
  mutate(delay_pos = pmax(DEP_DELAY, 0))

# Subsample for speed — shape comparison doesn't need all 430k rows
set.seed(42)
flights_tw <- flights_model[sample(nrow(flights_model), 50000), ]

gam_tw <- bam(
  delay_pos ~ s(temp_mean, k = 10) + s(month, bs = "cc", k = 12) + ORIGIN,
  data   = flights_tw,
  family = tw(),
  method = "fREML"
)

cat("\n── Tweedie GAM summary ──\n")
summary(gam_tw)

# Compare smooth shapes side by side
png("gam_robustness_smooths.png", width = 1600, height = 700, res = 150)
par(mfrow = c(2, 2))
plot(gam_fit, select = 1, main = "Gaussian: s(temp_mean)", shade = TRUE, seWithMean = TRUE)
plot(gam_tw,  select = 1, main = "Tweedie: s(temp_mean)",  shade = TRUE, seWithMean = TRUE)
plot(gam_fit, select = 2, main = "Gaussian: s(month)",     shade = TRUE, seWithMean = TRUE)
plot(gam_tw,  select = 2, main = "Tweedie: s(month)",      shade = TRUE, seWithMean = TRUE)
dev.off()

cat("Robustness output: gam_robustness_smooths.png\n")
