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
    month    = as.integer(lubridate::month(FL_DATE)),
    ORIGIN   = factor(ORIGIN),
    delayed  = as.integer(DEP_DELAY > 15)   # binary: 1 = significant delay
  )

cat("Delay rate overall:", round(mean(flights_model$delayed), 3), "\n")
cat("Delay rate by airport:\n")
print(tapply(flights_model$delayed, flights_model$ORIGIN, mean) |> round(3))

# ── Fit logistic GAM ──────────────────────────────────────────────────────────
set.seed(42)
gam_logit <- bam(
  delayed ~ s(temp_mean, k = 10) + s(month, bs = "cc", k = 12) + ORIGIN,
  data   = flights_model,
  family = binomial(link = "logit"),
  method = "fREML"
)

summary(gam_logit)

# ── Smooth plots ──────────────────────────────────────────────────────────────
png("logit_smooths.png", width = 1400, height = 600, res = 150)
par(mfrow = c(1, 2))
plot(gam_logit, pages = 0, shade = TRUE, seWithMean = TRUE,
     trans = plogis, shift = coef(gam_logit)[1])
dev.off()

# ── Predicted P(delay > 15 min) by temp, per airport ─────────────────────────
newdata <- expand.grid(
  temp_mean = seq(min(flights_model$temp_mean, na.rm = TRUE),
                  max(flights_model$temp_mean, na.rm = TRUE), length.out = 200),
  month     = 7L,
  ORIGIN    = levels(flights_model$ORIGIN)
)

preds <- predict(gam_logit, newdata = newdata, type = "response", se.fit = TRUE)
newdata$prob <- preds$fit
newdata$lwr  <- plogis(qlogis(preds$fit) - 1.96 * preds$se.fit)
newdata$upr  <- plogis(qlogis(preds$fit) + 1.96 * preds$se.fit)

p1 <- ggplot(newdata, aes(x = temp_mean, y = prob, colour = ORIGIN, fill = ORIGIN)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.15, colour = NA) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, NA)) +
  labs(
    title    = "Probability of departure delay > 15 min by temperature",
    subtitle = "Holding month = July; 95% CI shaded",
    x        = "Daily mean temperature (°F)",
    y        = "P(delay > 15 min)",
    colour   = "Airport", fill = "Airport"
  ) +
  theme_minimal()

ggsave("logit_temp_effect.png", p1, width = 8, height = 5, dpi = 150)

# ── Insurance risk heatmap: airport × month ───────────────────────────────────
# Use median observed temp per airport-month as realistic input
median_temps <- flights_model |>
  group_by(ORIGIN, month) |>
  summarise(temp_mean = median(temp_mean, na.rm = TRUE), .groups = "drop")

heat_preds <- predict(gam_logit, newdata = median_temps, type = "response")
median_temps$risk <- heat_preds

p2 <- ggplot(median_temps, aes(x = factor(month, labels = month.abb), y = ORIGIN, fill = risk)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = scales::percent(risk, accuracy = 1)), size = 3) +
  scale_fill_gradient2(
    low = "#2ecc71", mid = "#f39c12", high = "#e74c3c",
    midpoint = 0.3, labels = scales::percent_format()
  ) +
  labs(
    title = "Travel insurance risk: P(delay > 15 min) by airport and month",
    subtitle = "Based on median observed temperature per airport-month",
    x = "Month", y = "Airport", fill = "Risk"
  ) +
  theme_minimal()

ggsave("logit_risk_heatmap.png", p2, width = 10, height = 4, dpi = 150)

cat("Done. Outputs: logit_smooths.png, logit_temp_effect.png, logit_risk_heatmap.png\n")
