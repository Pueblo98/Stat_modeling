# 08_combined_effects.R
# Combined multi-variable visualizations:
#   1. All 6 smooth partial effects on one page (M1 and M2)
#   2. 2D prediction heatmaps — combined effect of two variables at once
#   3. Contribution waterfall charts — how all factors add up for specific scenarios

library(mgcv)
library(ggplot2)
library(dplyr)
library(patchwork)
library(gratia)

dir.create("outputs/figures", showWarnings = FALSE, recursive = TRUE)

m1 <- readRDS("outputs/m1_binary_gam.rds")
m2 <- readRDS("outputs/m2_duration_gam.rds")

# ── Shared constants ──────────────────────────────────────────────────────────
BASE_SCENARIO <- list(
  Origin      = factor("ORD", levels = c("ORD","PHX","ATL","DFW","DEN")),
  tmpf        = 55,
  p01i        = 0,
  sknt        = 10,
  vsby        = 10,
  has_storm   = 0L,
  dep_hour    = 10L,
  fl_month    = 6L,
  day_of_week = factor("Mon", levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
)

make_nd <- function(...) {
  args <- modifyList(BASE_SCENARIO, list(...))
  as.data.frame(args)
}

# ── Helper: compute CI from smooth_estimates ──────────────────────────────────
se_with_ci <- function(model, term_str) {
  se_df  <- smooth_estimates(model, select = term_str)
  meta   <- c(".smooth",".type",".by",".estimate",".se",".crit",".lower_ci",".upper_ci")
  x_col  <- setdiff(names(se_df), meta)[1]
  se_df$.x        <- se_df[[x_col]]
  crit            <- qnorm(0.975)
  se_df$.lower_ci <- se_df$.estimate - crit * se_df$.se
  se_df$.upper_ci <- se_df$.estimate + crit * se_df$.se
  se_df
}

smooth_panel <- function(model, y_label, title, color) {
  vars <- list(
    list(term="s(tmpf)",     xlab="Temperature (°F)",     vlines=c(32,95)),
    list(term="s(p01i)",     xlab="Precipitation (in/hr)",vlines=c(0.25)),
    list(term="s(sknt)",     xlab="Wind Speed (knots)",   vlines=c(25)),
    list(term="s(vsby)",     xlab="Visibility (miles)",   vlines=c(3)),
    list(term="s(dep_hour)", xlab="Departure Hour",       vlines=c(7,17)),
    list(term="s(fl_month)", xlab="Month (1=Jan)",        vlines=NULL)
  )

  plots <- lapply(vars, function(v) {
    df <- se_with_ci(model, v$term)
    p <- ggplot(df, aes(x=.x, y=.estimate)) +
      geom_ribbon(aes(ymin=.lower_ci, ymax=.upper_ci), fill=color, alpha=0.2) +
      geom_line(color=color, linewidth=0.9) +
      geom_hline(yintercept=0, color="grey50", linewidth=0.3) +
      labs(x=v$xlab, y=NULL) +
      theme_minimal(base_size=10) +
      theme(plot.title=element_text(size=9, face="bold"))
    if (!is.null(v$vlines))
      for (vl in v$vlines)
        p <- p + geom_vline(xintercept=vl, linetype="dashed", color="grey40", linewidth=0.5)
    p
  })

  wrap_plots(plots, ncol=3) +
    plot_annotation(
      title    = title,
      subtitle = paste(y_label, "| 95% CI shaded | dashed = operational threshold"),
      theme    = theme(
        plot.title    = element_text(face="bold", size=14),
        plot.subtitle = element_text(color="grey40", size=10)
      )
    )
}

cat("Saving all-smooths panels...\n")
p_m1_all <- smooth_panel(m1, "Partial effect on log-odds(Disrupted)",
                         "M1 — All Weather & Temporal Effects on Disruption Probability",
                         "steelblue")
p_m2_all <- smooth_panel(m2, "Partial effect on log(Delay minutes)",
                         "M2 — All Weather & Temporal Effects on Delay Duration",
                         "darkorange")

ggsave("outputs/figures/m1_all_smooths.png", p_m1_all, width=14, height=9,  dpi=300)
ggsave("outputs/figures/m2_all_smooths.png", p_m2_all, width=14, height=9,  dpi=300)
cat("All-smooths panels saved.\n")


# ─────────────────────────────────────────────────────────────────────────────
# 2.  2D PREDICTION HEATMAPS  (combined effect of two variables)
# ─────────────────────────────────────────────────────────────────────────────
# Each heatmap varies two variables across their realistic range while holding
# all others at the base scenario. This shows combined, not just individual, effects.

make_heatmap <- function(model, xvar, yvar, xseq, yseq,
                         xlab, ylab, title, subtitle,
                         response_label, use_response=TRUE) {
  grid <- expand.grid(plot_x=xseq, plot_y=yseq)
  nd   <- make_nd()
  nd   <- nd[rep(1, nrow(grid)), ]
  nd[[xvar]] <- grid$plot_x
  nd[[yvar]] <- grid$plot_y
  rownames(nd) <- NULL

  nd$pred   <- if (use_response) {
    as.numeric(predict(model, newdata=nd, type="response"))
  } else {
    exp(as.numeric(predict(model, newdata=nd, type="link")))
  }
  nd$plot_x <- grid$plot_x
  nd$plot_y <- grid$plot_y

  ggplot(nd, aes(x=plot_x, y=plot_y, fill=pred)) +
    geom_tile() +
    scale_fill_distiller(palette="RdYlGn", direction=-1,
                         name=response_label) +
    labs(title=title, subtitle=subtitle, x=xlab, y=ylab) +
    theme_minimal(base_size=12) +
    theme(plot.title=element_text(face="bold"))
}

cat("Saving 2D heatmaps...\n")

# Temperature × Precipitation  →  Disruption probability
h1 <- make_heatmap(m1, "tmpf","p01i",
                   seq(-10,105,length.out=60), seq(0,1.5,length.out=60),
                   "Temperature (°F)","Precipitation (in/hr)",
                   "Combined: Temperature × Precipitation on Disruption Risk",
                   "All other variables held at base scenario (ORD, Mon, 10am, Jun, 10 kts, 10 mi vis)",
                   "P(Disrupted)")

# Temperature × Wind Speed  →  Disruption probability
h2 <- make_heatmap(m1, "tmpf","sknt",
                   seq(-10,105,length.out=60), seq(0,55,length.out=60),
                   "Temperature (°F)","Wind Speed (knots)",
                   "Combined: Temperature × Wind Speed on Disruption Risk",
                   "All other variables held at base scenario",
                   "P(Disrupted)")

# Precipitation × Visibility  →  Disruption probability
h3 <- make_heatmap(m1, "p01i","vsby",
                   seq(0,1.5,length.out=60), seq(0,10,length.out=60),
                   "Precipitation (in/hr)","Visibility (miles)",
                   "Combined: Precipitation × Visibility on Disruption Risk",
                   "All other variables held at base scenario",
                   "P(Disrupted)")

# Temperature × Precipitation  →  Expected delay minutes (M2)
h4 <- make_heatmap(m2, "tmpf","p01i",
                   seq(-10,105,length.out=60), seq(0,1.5,length.out=60),
                   "Temperature (°F)","Precipitation (in/hr)",
                   "Combined: Temperature × Precipitation on Expected Delay Duration",
                   "All other variables held at base scenario (if delayed)",
                   "Expected Delay (min)", use_response=TRUE)

ggsave("outputs/figures/heatmap_tmpf_x_p01i_disruption.png", h1, width=9, height=7, dpi=300)
ggsave("outputs/figures/heatmap_tmpf_x_sknt_disruption.png", h2, width=9, height=7, dpi=300)
ggsave("outputs/figures/heatmap_p01i_x_vsby_disruption.png", h3, width=9, height=7, dpi=300)
ggsave("outputs/figures/heatmap_tmpf_x_p01i_delay.png",      h4, width=9, height=7, dpi=300)

# 2×2 composite of all four heatmaps
heatmap_panel <- (h1 | h2) / (h3 | h4) +
  plot_annotation(
    title    = "Combined Weather Effects — 2D Prediction Heatmaps",
    subtitle = "Each cell shows the model's combined prediction as two variables vary simultaneously",
    theme    = theme(plot.title    = element_text(face="bold", size=14),
                     plot.subtitle = element_text(color="grey40", size=10))
  )
ggsave("outputs/figures/heatmap_panel_all.png", heatmap_panel, width=16, height=12, dpi=300)
cat("Heatmaps saved.\n")


# ─────────────────────────────────────────────────────────────────────────────
# 3.  CONTRIBUTION WATERFALL CHARTS  (how all factors add up for one prediction)
# ─────────────────────────────────────────────────────────────────────────────
# Uses predict(type="terms") which returns each term's additive contribution
# to the linear predictor for a specific scenario.

TERM_LABELS <- c(
  "s(tmpf)"      = "Temperature",
  "s(p01i)"      = "Precipitation",
  "s(sknt)"      = "Wind Speed",
  "s(vsby)"      = "Visibility",
  "has_storm"    = "Thunderstorm",
  "Origin"       = "Airport",
  "s(dep_hour)"  = "Hour of Day",
  "day_of_week"  = "Day of Week",
  "s(fl_month)"  = "Month"
)

contribution_chart <- function(model, nd, scenario_title, intercept_label, color_pos, color_neg) {
  terms_mat  <- predict(model, newdata=nd, type="terms")
  intercept  <- as.numeric(coef(model)["(Intercept)"])
  contribs   <- as.numeric(terms_mat[1, ])
  term_names <- colnames(terms_mat)

  df <- data.frame(
    term   = term_names,
    label  = TERM_LABELS[term_names],
    value  = contribs
  ) |>
    mutate(label = ifelse(is.na(label), term, label)) |>
    arrange(desc(abs(value)))

  # Add intercept row at bottom
  df <- rbind(
    df,
    data.frame(term="(Intercept)", label="Baseline\n(Intercept)", value=intercept)
  )
  df$label <- factor(df$label, levels=rev(df$label))
  df$direction <- ifelse(df$value >= 0, "Increases risk", "Decreases risk")
  df$direction[df$label=="Baseline\n(Intercept)"] <- "Baseline"

  ggplot(df, aes(x=label, y=value, fill=direction)) +
    geom_col(width=0.65) +
    geom_hline(yintercept=0, linewidth=0.4, color="grey30") +
    geom_text(aes(label=sprintf("%+.2f", value),
                  hjust=ifelse(value >= 0, -0.1, 1.1)),
              size=3.2, color="grey20") +
    scale_fill_manual(
      values=c("Increases risk"="firebrick","Decreases risk"="steelblue","Baseline"="grey60"),
      name=NULL
    ) +
    coord_flip(ylim=c(min(df$value)*1.3, max(df$value)*1.3)) +
    labs(
      title    = scenario_title,
      subtitle = sprintf("%s | Total linear predictor = %.3f", intercept_label,
                         intercept + sum(contribs)),
      x = NULL, y = "Contribution to Linear Predictor"
    ) +
    theme_minimal(base_size=11) +
    theme(plot.title=element_text(face="bold", size=12),
          legend.position="bottom")
}

cat("Saving contribution waterfall charts...\n")

# Four contrasting scenarios
scenarios <- list(
  list(
    label = "Clear Winter Morning — ORD",
    sub   = "M1: log-odds(Disrupted)",
    nd    = make_nd(Origin=factor("ORD",levels=c("ORD","PHX","ATL","DFW","DEN")),
                    tmpf=25, p01i=0, sknt=8, vsby=10, has_storm=0L, dep_hour=8L, fl_month=1L)
  ),
  list(
    label = "Thunderstorm Summer Afternoon — ATL",
    sub   = "M1: log-odds(Disrupted)",
    nd    = make_nd(Origin=factor("ATL",levels=c("ORD","PHX","ATL","DFW","DEN")),
                    tmpf=88, p01i=0.8, sknt=20, vsby=2, has_storm=1L, dep_hour=16L, fl_month=7L)
  ),
  list(
    label = "Extreme Heat Midday — PHX",
    sub   = "M1: log-odds(Disrupted)",
    nd    = make_nd(Origin=factor("PHX",levels=c("ORD","PHX","ATL","DFW","DEN")),
                    tmpf=110, p01i=0, sknt=12, vsby=10, has_storm=0L, dep_hour=13L, fl_month=7L)
  ),
  list(
    label = "Blizzard Evening — DEN",
    sub   = "M1: log-odds(Disrupted)",
    nd    = make_nd(Origin=factor("DEN",levels=c("ORD","PHX","ATL","DFW","DEN")),
                    tmpf=15, p01i=0.5, sknt=35, vsby=0.5, has_storm=0L, dep_hour=19L, fl_month=12L)
  )
)

charts <- lapply(scenarios, function(s) {
  contribution_chart(m1, s$nd, s$label, s$sub, "firebrick", "steelblue")
})

waterfall_panel <- (charts[[1]] | charts[[2]]) / (charts[[3]] | charts[[4]]) +
  plot_annotation(
    title    = "How All Factors Combine: Contribution to Disruption Risk",
    subtitle = "Each bar shows one factor's additive push (+) or pull (−) on the log-odds of disruption",
    theme    = theme(plot.title    = element_text(face="bold", size=15),
                     plot.subtitle = element_text(color="grey40", size=10))
  )

ggsave("outputs/figures/contribution_waterfall_panel.png",
       waterfall_panel, width=18, height=14, dpi=300)
cat("Waterfall panel saved.\n")
cat("Script 08 complete. Outputs in outputs/figures/\n")
