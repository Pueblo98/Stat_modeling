# 04_model_duration.R
# Fit M2: Gamma GAM for departure delay duration (DepDelay > 0 minutes)
# Train: months 1-9, filtered to delayed flights only
# Outputs: outputs/m2_duration_gam.rds, outputs/m2_edf_table.rds, diagnostic PNGs

library(mgcv)
library(gratia)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

dir.create("outputs",         showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

set.seed(42)

# ── Load and validate data ────────────────────────────────────────────────────
cat("Loading processed data...\n")
flights <- readRDS("data/processed/flights_weather_2024.rds")

required_cols <- c("FlightDate", "LocalDT", "Origin", "DepDelay",
                   "tmpf", "p01i", "sknt", "vsby", "has_storm")
missing <- setdiff(required_cols, names(flights))
if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse = ", "))
cat(sprintf("Dataset loaded: %d rows\n", nrow(flights)))

# ── Derive modeling features ──────────────────────────────────────────────────
flights <- flights |>
  mutate(
    dep_hour    = as.integer(format(LocalDT, "%H")),
    day_of_week = weekdays(as.Date(FlightDate), abbreviate = TRUE),
    fl_month    = as.integer(month(as.Date(FlightDate)))
  )

# ── Train split then filter to delayed flights only ───────────────────────────
train_df      <- flights |> filter(fl_month <= 9)
train_delayed <- train_df |> filter(DepDelay > 0)

cat(sprintf("Train total: %d | Delayed only: %d (%.1f%%)\n",
            nrow(train_df), nrow(train_delayed),
            100 * nrow(train_delayed) / nrow(train_df)))

# Gamma requires strictly positive response
stopifnot("DepDelay must be strictly positive" = all(train_delayed$DepDelay > 0))

# ── Factor levels ─────────────────────────────────────────────────────────────
train_delayed$Origin      <- factor(train_delayed$Origin,
                                     levels = c("ORD", "PHX", "ATL", "DFW", "DEN"))
train_delayed$day_of_week <- factor(train_delayed$day_of_week,
                                     levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# ── Drop NAs only in model variables ─────────────────────────────────────────
model_vars <- c("DepDelay", "tmpf", "p01i", "sknt", "vsby", "has_storm",
                "dep_hour", "fl_month", "Origin", "day_of_week")
n_before      <- nrow(train_delayed)
train_delayed <- train_delayed |> drop_na(all_of(model_vars))
cat(sprintf("Rows dropped for NA: %d\n", n_before - nrow(train_delayed)))
cat(sprintf("Final training rows (delayed): %d\n", nrow(train_delayed)))

# ── Fit M2 ────────────────────────────────────────────────────────────────────
cat("\nFitting M2 (Gamma GAM)...\n")
t0 <- proc.time()

m2 <- bam(
  DepDelay ~ s(tmpf,     k = 20) +
             s(p01i,     k = 10) +
             s(sknt,     k = 10) +
             s(vsby,     k = 10) +
             has_storm +
             Origin +
             s(dep_hour, bs = "cc", k = 24) +
             day_of_week +
             s(fl_month, bs = "cc", k = 6),
  data     = train_delayed,
  family   = Gamma(link = "log"),
  method   = "fREML",
  discrete = TRUE
)

cat(sprintf("M2 fitted in %.1f seconds.\n", (proc.time() - t0)["elapsed"]))

# ── Diagnostics ───────────────────────────────────────────────────────────────
cat("\n--- M2 Summary ---\n")
print(summary(m2))

cat("\n--- gam.check ---\n")
png("outputs/figures/m2_gam_check.png", width = 1200, height = 1000, res = 150)
gam.check(m2)
dev.off()
cat("gam.check plot -> outputs/figures/m2_gam_check.png\n")

cat("\n--- Concurvity ---\n")
conc2 <- concurvity(m2, full = FALSE)
print(round(conc2$estimate, 3))
if (any(conc2$estimate > 0.8, na.rm = TRUE)) {
  warning("High concurvity detected in M2.")
} else {
  cat("No concurvity issues in M2.\n")
}

appraise_plot2 <- appraise(m2, point_col = "darkorange", point_alpha = 0.3)
ggsave("outputs/figures/m2_appraise.png", appraise_plot2,
       width = 12, height = 9, dpi = 300)
cat("Appraise plot -> outputs/figures/m2_appraise.png\n")

# ── Log-residual QQ plot (Gamma-specific check) ───────────────────────────────
# Log-scale residuals should be approximately normal for Gamma/log-link model
log_resid <- log(train_delayed$DepDelay) - log(fitted(m2))
png("outputs/figures/m2_log_residual_qq.png", width = 800, height = 700, res = 150)
qqnorm(log_resid,
       main  = "M2: Log-Residual Q-Q Plot",
       pch   = 16,
       col   = rgb(0.2, 0.5, 0.8, 0.3),
       cex   = 0.5)
qqline(log_resid, col = "firebrick", lwd = 2)
dev.off()
cat("Log-residual Q-Q plot -> outputs/figures/m2_log_residual_qq.png\n")

# ── EDF table ─────────────────────────────────────────────────────────────────
m2_edf <- data.frame(
  term    = rownames(summary(m2)$s.table),
  edf     = round(summary(m2)$s.table[, "edf"], 3),
  p_value = round(summary(m2)$s.table[, "p-value"], 4)
)
cat("\n--- M2 Smooth Term EDF Summary ---\n")
print(m2_edf)
saveRDS(m2_edf, "outputs/m2_edf_table.rds")

# ── Save model ────────────────────────────────────────────────────────────────
saveRDS(m2, "outputs/m2_duration_gam.rds")
cat("\nM2 saved -> outputs/m2_duration_gam.rds\n")
cat("Script 04 complete.\n")
