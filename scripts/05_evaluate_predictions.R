# 05_evaluate_predictions.R
# Test-set evaluation for M1 (logistic GAM) and M2 (Gamma GAM)
# Test set: months 10-12 (Oct-Dec 2024)
# Outputs: outputs/evaluation_metrics.csv, outputs/figures/m1_calibration.png

library(mgcv)
library(pROC)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(lubridate)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

# ── Load models ───────────────────────────────────────────────────────────────
cat("Loading models...\n")
if (!file.exists("outputs/m1_binary_gam.rds")) stop("Run 03_model_binary.R first.")
if (!file.exists("outputs/m2_duration_gam.rds")) stop("Run 04_model_duration.R first.")

m1 <- readRDS("outputs/m1_binary_gam.rds")
m2 <- readRDS("outputs/m2_duration_gam.rds")

# ── Load data and derive features ─────────────────────────────────────────────
cat("Loading processed data...\n")
flights <- readRDS("data/processed/flights_weather_2024.rds")

flights <- flights |>
  mutate(
    disrupted   = as.integer(DepDel15 == 1 | Cancelled == 1),
    dep_hour    = as.integer(format(LocalDT, "%H")),
    day_of_week = weekdays(as.Date(FlightDate), abbreviate = TRUE),
    fl_month    = as.integer(month(as.Date(FlightDate)))
  )

test_df <- flights |> filter(fl_month >= 10)
cat(sprintf("Test rows: %d\n", nrow(test_df)))

# Align factor levels (fl_month is now a numeric smooth — no factor alignment needed)
test_df$Origin      <- factor(test_df$Origin,      levels = levels(m1$model$Origin))
test_df$day_of_week <- factor(test_df$day_of_week, levels = levels(m1$model$day_of_week))

# ── M1 Evaluation ─────────────────────────────────────────────────────────────
cat("\n--- M1 Evaluation (Binary Disruption) ---\n")

m1_vars  <- c("disrupted", "tmpf", "p01i", "sknt", "vsby", "has_storm",
              "dep_hour", "fl_month", "Origin", "day_of_week")
test_m1  <- test_df |> drop_na(all_of(m1_vars))
cat(sprintf("Test rows for M1: %d\n", nrow(test_m1)))

test_m1$pred_prob <- as.numeric(predict(m1, newdata = test_m1, type = "response"))

# AUC-ROC
roc_obj <- roc(test_m1$disrupted, test_m1$pred_prob, quiet = TRUE)
auc_val <- as.numeric(auc(roc_obj))
cat(sprintf("AUC-ROC:           %.4f\n", auc_val))

# Brier score
brier      <- mean((test_m1$pred_prob - test_m1$disrupted)^2)
null_brier <- mean((mean(test_m1$disrupted) - test_m1$disrupted)^2)
brier_skill <- 1 - brier / null_brier
cat(sprintf("Brier Score:       %.4f\n", brier))
cat(sprintf("Brier Skill Score: %.4f\n", brier_skill))

# Confusion matrix at threshold 0.30
THRESHOLD          <- 0.30
test_m1$pred_class <- as.integer(test_m1$pred_prob >= THRESHOLD)
cm <- table(Predicted = test_m1$pred_class, Actual = test_m1$disrupted)
cat(sprintf("\nConfusion Matrix (threshold = %.2f):\n", THRESHOLD))
print(cm)

TP <- cm["1", "1"]; FP <- cm["1", "0"]
FN <- cm["0", "1"]; TN <- cm["0", "0"]

precision   <- TP / (TP + FP)
recall      <- TP / (TP + FN)
f1          <- 2 * precision * recall / (precision + recall)
specificity <- TN / (TN + FP)
accuracy    <- (TP + TN) / sum(cm)

cat(sprintf("Precision:         %.4f\n", precision))
cat(sprintf("Recall:            %.4f\n", recall))
cat(sprintf("F1 Score:          %.4f\n", f1))
cat(sprintf("Specificity:       %.4f\n", specificity))
cat(sprintf("Accuracy:          %.4f\n", accuracy))

# ── M1 Calibration Plot ───────────────────────────────────────────────────────
# Equal-count decile bins (quantile-based), compare mean predicted vs. observed rate
test_m1$prob_decile <- cut(
  test_m1$pred_prob,
  breaks         = quantile(test_m1$pred_prob, probs = seq(0, 1, 0.1), na.rm = TRUE),
  include.lowest = TRUE,
  labels         = FALSE
)

calib_df <- test_m1 |>
  group_by(prob_decile) |>
  summarise(
    mean_pred = mean(pred_prob),
    obs_rate  = mean(disrupted),
    n         = n(),
    .groups   = "drop"
  )

calib_plot <- ggplot(calib_df, aes(x = mean_pred, y = obs_rate)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "grey50", linewidth = 0.8) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(aes(size = n), color = "steelblue", alpha = 0.85) +
  scale_size_continuous(name = "Flights in bin", range = c(2, 8)) +
  labs(
    title    = "M1 Calibration: Predicted vs. Observed Disruption Rate",
    subtitle = "Decile bins | Test set Oct–Dec 2024",
    x        = "Mean Predicted Probability",
    y        = "Observed Disruption Rate"
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 13)

ggsave("outputs/figures/m1_calibration.png", calib_plot,
       width = 7, height = 7, dpi = 300)
cat("\nCalibration plot -> outputs/figures/m1_calibration.png\n")

# ── M2 Evaluation ─────────────────────────────────────────────────────────────
cat("\n--- M2 Evaluation (Delay Duration, delayed flights only) ---\n")

m2_vars <- c("DepDelay", "tmpf", "p01i", "sknt", "vsby", "has_storm",
             "dep_hour", "fl_month", "Origin", "day_of_week")

test_m2 <- test_df |>
  filter(DepDelay > 0) |>
  drop_na(all_of(m2_vars))

test_m2$Origin      <- factor(test_m2$Origin,      levels = levels(m2$model$Origin))
test_m2$day_of_week <- factor(test_m2$day_of_week, levels = levels(m2$model$day_of_week))

cat(sprintf("Test rows for M2: %d\n", nrow(test_m2)))

test_m2$pred_delay <- as.numeric(predict(m2, newdata = test_m2, type = "response"))

rmse <- sqrt(mean((test_m2$DepDelay - test_m2$pred_delay)^2))
mae  <- mean(abs(test_m2$DepDelay - test_m2$pred_delay))
cat(sprintf("RMSE: %.2f minutes\n", rmse))
cat(sprintf("MAE:  %.2f minutes\n", mae))

# 90% PI using the Gamma distribution's dispersion parameter.
# The link-scale SE only measures uncertainty in the mean, not prediction scatter.
# Gamma PI: shape = 1/dispersion, rate = shape/mean_prediction
phi   <- m2$scale                # estimated dispersion (Var(Y) = phi * mu^2)
shape <- 1 / phi                 # shape parameter of Gamma
mu    <- test_m2$pred_delay      # predicted mean (already on response scale)
pi_lo <- qgamma(0.05, shape = shape, rate = shape / mu)
pi_hi <- qgamma(0.95, shape = shape, rate = shape / mu)
coverage_90 <- mean(test_m2$DepDelay >= pi_lo & test_m2$DepDelay <= pi_hi, na.rm = TRUE)
cat(sprintf("90%% PI Coverage:  %.4f (target: 0.90)\n", coverage_90))

# ── Save all metrics ──────────────────────────────────────────────────────────
metrics_m1 <- data.frame(
  model  = "M1_logistic",
  metric = c("AUC_ROC", "Brier_Score", "Brier_Skill", "Precision",
             "Recall", "F1", "Specificity", "Accuracy"),
  value  = round(c(auc_val, brier, brier_skill, precision,
                   recall, f1, specificity, accuracy), 4)
)

metrics_m2 <- data.frame(
  model  = "M2_gamma",
  metric = c("RMSE_min", "MAE_min", "PI90_coverage"),
  value  = round(c(rmse, mae, coverage_90), 4)
)

write_csv(rbind(metrics_m1, metrics_m2), "outputs/evaluation_metrics.csv")
cat("\nAll metrics saved -> outputs/evaluation_metrics.csv\n")
cat("Script 05 complete.\n")
