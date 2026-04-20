# 03_model_binary.R
# Fit M1: Logistic GAM for binary flight disruption
# disrupted = 1 if DepDel15==1 OR Cancelled==1
# Train: months 1-9 (Jan-Sep 2024)
# Outputs: outputs/m1_binary_gam.rds, outputs/m1_edf_table.rds, diagnostic PNGs

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

required_cols <- c("FlightDate", "LocalDT", "Origin", "DepDel15", "Cancelled",
                   "DepDelay", "tmpf", "p01i", "sknt", "vsby", "has_storm")
missing <- setdiff(required_cols, names(flights))
if (length(missing) > 0) stop("Missing columns: ", paste(missing, collapse = ", "))
cat(sprintf("Dataset loaded: %d rows\n", nrow(flights)))

# ── Derive modeling features ──────────────────────────────────────────────────
flights <- flights |>
  mutate(
    disrupted   = as.integer(DepDel15 == 1 | Cancelled == 1),
    dep_hour    = as.integer(format(LocalDT, "%H")),
    day_of_week = weekdays(as.Date(FlightDate), abbreviate = TRUE),
    fl_month    = as.integer(month(as.Date(FlightDate)))
  )

# ── Train / test split (temporal) ────────────────────────────────────────────
train_df <- flights |> filter(fl_month <= 9)
test_df  <- flights |> filter(fl_month >= 10)
cat(sprintf("Train: %d rows | Test: %d rows\n", nrow(train_df), nrow(test_df)))
cat(sprintf("Train disruption rate: %.3f\n", mean(train_df$disrupted, na.rm = TRUE)))

# ── Factor levels — hard-coded so predict() never fails on unseen levels ──────
train_df$Origin      <- factor(train_df$Origin,
                                levels = c("ORD", "PHX", "ATL", "DFW", "DEN"))
train_df$day_of_week <- factor(train_df$day_of_week,
                                levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))

# ── Drop NAs only in model variables ─────────────────────────────────────────
model_vars <- c("disrupted", "tmpf", "p01i", "sknt", "vsby", "has_storm",
                "dep_hour", "fl_month", "Origin", "day_of_week")
n_before   <- nrow(train_df)
train_df   <- train_df |> drop_na(all_of(model_vars))
cat(sprintf("Rows dropped for NA: %d\n", n_before - nrow(train_df)))
cat(sprintf("Final training rows: %d\n", nrow(train_df)))

# ── Fit M1 ────────────────────────────────────────────────────────────────────
cat("\nFitting M1 (logistic GAM)...\n")
t0 <- proc.time()

m1 <- bam(
  disrupted ~ s(tmpf,     k = 10) +
              s(p01i,     k = 10) +
              s(sknt,     k = 10) +
              s(vsby,     k = 10) +
              has_storm +
              Origin +
              s(dep_hour, bs = "cc", k = 24) +
              day_of_week +
              s(fl_month, bs = "cc", k = 6),
  data     = train_df,
  family   = binomial(link = "logit"),
  method   = "fREML",
  discrete = TRUE
)

cat(sprintf("M1 fitted in %.1f seconds.\n", (proc.time() - t0)["elapsed"]))

# ── Diagnostics ───────────────────────────────────────────────────────────────
cat("\n--- M1 Summary ---\n")
print(summary(m1))

cat("\n--- gam.check (k-index p-values should be > 0.05) ---\n")
png("outputs/figures/m1_gam_check.png", width = 1200, height = 1000, res = 150)
gam.check(m1)
dev.off()
cat("gam.check plot -> outputs/figures/m1_gam_check.png\n")

cat("\n--- Concurvity (values > 0.8 flag multicollinearity) ---\n")
conc <- concurvity(m1, full = FALSE)
print(round(conc$estimate, 3))
if (any(conc$estimate > 0.8, na.rm = TRUE)) {
  warning("High concurvity detected (>0.8) — inspect terms above.")
} else {
  cat("No concurvity issues (all estimates <= 0.8).\n")
}

appraise_plot <- appraise(m1, point_col = "steelblue", point_alpha = 0.3)
ggsave("outputs/figures/m1_appraise.png", appraise_plot,
       width = 12, height = 9, dpi = 300)
cat("Appraise plot -> outputs/figures/m1_appraise.png\n")

# ── EDF table ─────────────────────────────────────────────────────────────────
m1_edf <- data.frame(
  term    = rownames(summary(m1)$s.table),
  edf     = round(summary(m1)$s.table[, "edf"], 3),
  p_value = round(summary(m1)$s.table[, "p-value"], 4)
)
cat("\n--- Smooth Term EDF Summary ---\n")
print(m1_edf)
saveRDS(m1_edf, "outputs/m1_edf_table.rds")

# ── Save model ────────────────────────────────────────────────────────────────
saveRDS(m1, "outputs/m1_binary_gam.rds")
cat("\nM1 saved -> outputs/m1_binary_gam.rds\n")
cat("Script 03 complete.\n")
