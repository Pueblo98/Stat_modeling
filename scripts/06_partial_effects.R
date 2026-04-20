# 06_partial_effects.R
# Generate partial effect plots for M1 and M2 with operational threshold annotations
# Outputs: PNG files in outputs/figures/

library(mgcv)
library(gratia)
library(ggplot2)
library(dplyr)
library(patchwork)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

cat("Loading models...\n")
if (!file.exists("outputs/m1_binary_gam.rds")) stop("Run 03_model_binary.R first.")
if (!file.exists("outputs/m2_duration_gam.rds")) stop("Run 04_model_duration.R first.")

m1 <- readRDS("outputs/m1_binary_gam.rds")
m2 <- readRDS("outputs/m2_duration_gam.rds")

# ── Threshold annotation helper ───────────────────────────────────────────────
# thresholds: list of lists with fields x, label, color
add_thresholds <- function(p, thresholds) {
  for (th in thresholds) {
    p <- p +
      geom_vline(xintercept = th$x, linetype = "dashed",
                 color = th$color, linewidth = 0.7) +
      annotate("text",
               x     = th$x,
               y     = Inf,
               label = th$label,
               color = th$color,
               angle = 90,
               vjust = -0.4,
               hjust = 1.1,
               size  = 3.2)
  }
  p
}

# ── Threshold specifications ──────────────────────────────────────────────────
THRESHOLDS <- list(
  tmpf = list(
    list(x = 32, label = "32°F Freezing",           color = "steelblue"),
    list(x = 95, label = "95°F Density Alt. Limit", color = "firebrick")
  ),
  p01i = list(list(x = 0.25, label = "0.25 in/hr Heavy Rain",  color = "steelblue")),
  sknt = list(list(x = 25,   label = "25 kt Crosswind Limit",  color = "darkorange")),
  vsby = list(list(x = 3,    label = "3 mi Visibility Min.",   color = "purple"))
)

# ── Plot factory ──────────────────────────────────────────────────────────────
# Uses smooth_estimates() (tidy df) rather than draw() for stable custom layers
make_smooth_plot <- function(model, term_str, x_label, y_label, title,
                             thresholds = list()) {
  se_df <- smooth_estimates(model, select = term_str)

  # Identify the predictor column (first non-metadata column)
  meta  <- c(".smooth", ".type", ".by", ".estimate", ".se",
             ".crit", ".lower_ci", ".upper_ci")
  x_col <- setdiff(names(se_df), meta)[1]
  se_df$.x <- se_df[[x_col]]

  # Compute 95% CI bounds (gratia 0.11+ returns .se but not pre-computed bounds)
  crit <- qnorm(0.975)
  se_df$.lower_ci <- se_df$.estimate - crit * se_df$.se
  se_df$.upper_ci <- se_df$.estimate + crit * se_df$.se

  p <- ggplot(se_df, aes(x = .x, y = .estimate)) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
                fill = "steelblue", alpha = 0.22) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_hline(yintercept = 0, color = "grey40",
               linetype = "solid", linewidth = 0.4) +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 12))

  add_thresholds(p, thresholds)
}

# ── M1 partial effects ────────────────────────────────────────────────────────
cat("Generating M1 partial effect plots...\n")
y1 <- "Partial Effect on log-odds(Disrupted)"

p_m1_tmpf <- make_smooth_plot(m1, "s(tmpf)", "Temperature (°F)",     y1,
  "Temperature Effect on Disruption Probability", THRESHOLDS$tmpf)
p_m1_p01i <- make_smooth_plot(m1, "s(p01i)", "Precipitation (in/hr)", y1,
  "Precipitation Effect on Disruption Probability", THRESHOLDS$p01i)
p_m1_sknt <- make_smooth_plot(m1, "s(sknt)", "Wind Speed (knots)",    y1,
  "Wind Speed Effect on Disruption Probability", THRESHOLDS$sknt)
p_m1_vsby <- make_smooth_plot(m1, "s(vsby)", "Visibility (miles)",    y1,
  "Visibility Effect on Disruption Probability", THRESHOLDS$vsby)

# Hour-of-day — cyclic spline, annotate AM/PM peaks instead of thresholds
p_m1_hour <- make_smooth_plot(m1, "s(dep_hour)", "Departure Hour (0–23)", y1,
  "Time-of-Day Effect on Disruption Probability") +
  geom_vline(xintercept = c(7, 17), linetype = "dotted",
             color = "grey55", linewidth = 0.6) +
  annotate("text", x = 7,  y = Inf, label = "AM peak", angle = 90,
           vjust = -0.4, hjust = 1.1, size = 3, color = "grey50") +
  annotate("text", x = 17, y = Inf, label = "PM peak", angle = 90,
           vjust = -0.4, hjust = 1.1, size = 3, color = "grey50")

# Individual saves
ggsave("outputs/figures/m1_partial_tmpf.png",  p_m1_tmpf, width = 8, height = 5, dpi = 300)
ggsave("outputs/figures/m1_partial_p01i.png",  p_m1_p01i, width = 8, height = 5, dpi = 300)
ggsave("outputs/figures/m1_partial_sknt.png",  p_m1_sknt, width = 8, height = 5, dpi = 300)
ggsave("outputs/figures/m1_partial_vsby.png",  p_m1_vsby, width = 8, height = 5, dpi = 300)
ggsave("outputs/figures/m1_partial_hour.png",  p_m1_hour, width = 8, height = 5, dpi = 300)

# Composite panel
m1_panel <- (p_m1_tmpf | p_m1_p01i) / (p_m1_sknt | p_m1_vsby) +
  plot_annotation(
    title    = "M1 (Logistic GAM): Weather Partial Effects on Disruption Probability",
    subtitle = "Shaded band = 95% credible interval  |  Dashed lines = operational thresholds",
    theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                     plot.subtitle = element_text(color = "grey40", size = 10))
  )

ggsave("outputs/figures/m1_partial_effects_panel.png", m1_panel,
       width = 14, height = 10, dpi = 300)
cat("M1 plots saved.\n")

# ── M2 partial effects ────────────────────────────────────────────────────────
cat("Generating M2 partial effect plots...\n")
y2 <- "Partial Effect on log(Expected Delay, min)"

p_m2_tmpf <- make_smooth_plot(m2, "s(tmpf)", "Temperature (°F)",     y2,
  "Temperature Effect on Delay Duration", THRESHOLDS$tmpf)
p_m2_p01i <- make_smooth_plot(m2, "s(p01i)", "Precipitation (in/hr)", y2,
  "Precipitation Effect on Delay Duration", THRESHOLDS$p01i)
p_m2_sknt <- make_smooth_plot(m2, "s(sknt)", "Wind Speed (knots)",    y2,
  "Wind Speed Effect on Delay Duration", THRESHOLDS$sknt)
p_m2_vsby <- make_smooth_plot(m2, "s(vsby)", "Visibility (miles)",    y2,
  "Visibility Effect on Delay Duration", THRESHOLDS$vsby)

p_m2_hour <- make_smooth_plot(m2, "s(dep_hour)", "Departure Hour (0–23)", y2,
  "Time-of-Day Effect on Delay Duration") +
  geom_vline(xintercept = c(7, 17), linetype = "dotted",
             color = "grey55", linewidth = 0.6) +
  annotate("text", x = 7,  y = Inf, label = "AM peak", angle = 90,
           vjust = -0.4, hjust = 1.1, size = 3, color = "grey50") +
  annotate("text", x = 17, y = Inf, label = "PM peak", angle = 90,
           vjust = -0.4, hjust = 1.1, size = 3, color = "grey50")

ggsave("outputs/figures/m2_partial_tmpf.png",  p_m2_tmpf, width = 8, height = 5, dpi = 300)
ggsave("outputs/figures/m2_partial_p01i.png",  p_m2_p01i, width = 8, height = 5, dpi = 300)
ggsave("outputs/figures/m2_partial_sknt.png",  p_m2_sknt, width = 8, height = 5, dpi = 300)
ggsave("outputs/figures/m2_partial_vsby.png",  p_m2_vsby, width = 8, height = 5, dpi = 300)
ggsave("outputs/figures/m2_partial_hour.png",  p_m2_hour, width = 8, height = 5, dpi = 300)

m2_panel <- (p_m2_tmpf | p_m2_p01i) / (p_m2_sknt | p_m2_vsby) +
  plot_annotation(
    title    = "M2 (Gamma GAM): Weather Partial Effects on Delay Duration",
    subtitle = "Shaded band = 95% credible interval  |  Dashed lines = operational thresholds",
    theme    = theme(plot.title    = element_text(face = "bold", size = 14),
                     plot.subtitle = element_text(color = "grey40", size = 10))
  )

ggsave("outputs/figures/m2_partial_effects_panel.png", m2_panel,
       width = 14, height = 10, dpi = 300)
cat("M2 plots saved.\n")
cat("Script 06 complete.\n")
