# 07_shiny_dashboard.R
# Interactive Shiny dashboard: explore GAM partial effects + predict flight disruptions
# Run: Rscript scripts/07_shiny_dashboard.R
# Requires: outputs/m1_binary_gam.rds, outputs/m2_duration_gam.rds

library(shiny)
library(ggplot2)
library(gratia)
library(plotly)
library(mgcv)
library(dplyr)

# ── Load models once at startup (global scope — shared across all sessions) ───
cat("Loading models for dashboard...\n")

if (!file.exists("outputs/m1_binary_gam.rds")) stop("Run 03_model_binary.R first.")
if (!file.exists("outputs/m2_duration_gam.rds")) stop("Run 04_model_duration.R first.")

m1 <- readRDS("outputs/m1_binary_gam.rds")
m2 <- readRDS("outputs/m2_duration_gam.rds")

# EDF tables (pre-computed in scripts 03/04; fall back to re-computing if absent)
m1_edf <- if (file.exists("outputs/m1_edf_table.rds")) {
  readRDS("outputs/m1_edf_table.rds")
} else {
  data.frame(term    = rownames(summary(m1)$s.table),
             edf     = round(summary(m1)$s.table[, "edf"], 3),
             p_value = round(summary(m1)$s.table[, "p-value"], 4))
}

m2_edf <- if (file.exists("outputs/m2_edf_table.rds")) {
  readRDS("outputs/m2_edf_table.rds")
} else {
  data.frame(term    = rownames(summary(m2)$s.table),
             edf     = round(summary(m2)$s.table[, "edf"], 3),
             p_value = round(summary(m2)$s.table[, "p-value"], 4))
}

# Pre-compute smooth estimates for all terms (makes Tab 1 renders instant)
SMOOTH_VARS <- c("tmpf", "p01i", "sknt", "vsby", "dep_hour", "fl_month")
smooth_data <- list(
  M1 = setNames(lapply(SMOOTH_VARS, function(v) smooth_estimates(m1, select = paste0("s(", v, ")"))), SMOOTH_VARS),
  M2 = setNames(lapply(SMOOTH_VARS, function(v) smooth_estimates(m2, select = paste0("s(", v, ")"))), SMOOTH_VARS)
)

# Airport average delay from training data stored inside m2 object
airport_avg_delay <- m2$model |>
  group_by(Origin) |>
  summarise(avg_delay = mean(DepDelay, na.rm = TRUE), .groups = "drop")

# ── Operational thresholds ────────────────────────────────────────────────────
THRESHOLDS <- list(
  tmpf = list(list(x = 32,   label = "32°F Freezing",           color = "steelblue"),
              list(x = 95,   label = "95°F Density Alt. Limit", color = "firebrick")),
  p01i = list(list(x = 0.25, label = "0.25 in/hr Heavy Rain",   color = "steelblue")),
  sknt = list(list(x = 25,   label = "25 kt Crosswind Limit",   color = "darkorange")),
  vsby = list(list(x = 3,    label = "3 mi Visibility Min.",    color = "purple"))
)

# Auto-narrative based on EDF value
interpret_smooth <- function(model_key, var_key) {
  edf_table  <- if (model_key == "M1") m1_edf else m2_edf
  term_name  <- paste0("s(", var_key, ")")
  edf_val    <- edf_table$edf[edf_table$term == term_name]
  model_label <- if (model_key == "M1") "disruption probability" else "delay duration"
  var_labels  <- c(tmpf = "temperature", p01i = "precipitation",
                   sknt = "wind speed",  vsby = "visibility",
                   dep_hour = "departure hour")
  nonlin_str <- if (length(edf_val) == 0 || is.na(edf_val)) "unclear non-linearity"
                else if (edf_val > 3) "strong non-linear threshold behavior"
                else if (edf_val > 1.5) "moderate non-linearity"
                else "approximately linear relationship"

  sprintf("Effect of %s on %s (EDF = %s): shows %s. Dashed lines mark operational limits.",
          var_labels[var_key], model_label,
          if (length(edf_val) > 0) round(edf_val, 2) else "N/A",
          nonlin_str)
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title = "Flight Delay GAM Dashboard",

  # ── Tab 1: Explore Weather Effects ─────────────────────────────────────────
  tabPanel("Explore Weather Effects",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput("explore_model", "Model:",
                    choices = c("M1 — Disruption Probability" = "M1",
                                "M2 — Delay Duration"         = "M2")),
        selectInput("explore_var", "Weather Variable:",
                    choices = c("Temperature (°F)"        = "tmpf",
                                "Precipitation (in/hr)"   = "p01i",
                                "Wind Speed (knots)"      = "sknt",
                                "Visibility (miles)"      = "vsby",
                                "Departure Hour (cyclic)" = "dep_hour",
                                "Month of Year (cyclic)"  = "fl_month")),
        checkboxInput("show_rug", "Show data density rug", value = FALSE),
        hr(),
        h5("Interpretation"),
        textOutput("smooth_narrative")
      ),
      mainPanel(
        width = 9,
        plotOutput("smooth_plot", height = "420px"),
        hr(),
        h5("Smooth Term Summary"),
        tableOutput("edf_table")
      )
    )
  ),

  # ── Tab 2: Predict a Flight ─────────────────────────────────────────────────
  tabPanel("Predict a Flight",
    sidebarLayout(
      sidebarPanel(
        width = 4,
        h4("Flight Scenario"),
        selectInput("pred_airport", "Airport:",
                    choices = c("ORD", "PHX", "ATL", "DFW", "DEN")),
        selectInput("pred_month", "Month:",
                    choices = setNames(as.character(1:12), month.name)),
        selectInput("pred_dow", "Day of Week:",
                    choices = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
        sliderInput("pred_hour", "Departure Hour:",
                    min = 0, max = 23, value = 8, step = 1),
        hr(),
        h4("Weather Conditions"),
        sliderInput("pred_tmpf", "Temperature (°F):",
                    min = -20, max = 120, value = 55, step = 1),
        sliderInput("pred_p01i", "Precipitation (in/hr):",
                    min = 0, max = 2.0, value = 0, step = 0.01),
        sliderInput("pred_sknt", "Wind Speed (knots):",
                    min = 0, max = 60, value = 10, step = 1),
        sliderInput("pred_vsby", "Visibility (miles):",
                    min = 0, max = 10, value = 10, step = 0.5),
        checkboxInput("pred_storm", "Thunderstorm present?", value = FALSE)
      ),
      mainPanel(
        width = 8,
        fluidRow(
          column(6,
            h4("Disruption Risk (M1)"),
            plotlyOutput("risk_gauge", height = "300px"),
            uiOutput("risk_label")
          ),
          column(6,
            h4("Expected Delay if Delayed (M2)"),
            uiOutput("delay_output"),
            br(),
            plotOutput("delay_bar", height = "200px")
          )
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Tab 1: smooth plot ──────────────────────────────────────────────────────
  output$smooth_plot <- renderPlot({
    var_key   <- input$explore_var
    model_key <- input$explore_model

    se_df <- smooth_data[[model_key]][[var_key]]

    meta  <- c(".smooth", ".type", ".by", ".estimate", ".se",
               ".crit", ".lower_ci", ".upper_ci")
    x_col <- setdiff(names(se_df), meta)[1]
    se_df$.x <- se_df[[x_col]]
    crit <- qnorm(0.975)
    se_df$.lower_ci <- se_df$.estimate - crit * se_df$.se
    se_df$.upper_ci <- se_df$.estimate + crit * se_df$.se

    y_label <- if (model_key == "M1") "Partial Effect on log-odds(Disrupted)"
               else "Partial Effect on log(Expected Delay, min)"
    x_label <- switch(var_key,
      tmpf     = "Temperature (°F)",
      p01i     = "Precipitation (in/hr)",
      sknt     = "Wind Speed (knots)",
      vsby     = "Visibility (miles)",
      dep_hour = "Departure Hour (0–23)",
      fl_month = "Month of Year (1–12)"
    )

    p <- ggplot(se_df, aes(x = .x, y = .estimate)) +
      geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci),
                  fill = "steelblue", alpha = 0.22) +
      geom_line(color = "steelblue", linewidth = 1) +
      geom_hline(yintercept = 0, color = "grey40",
                 linetype = "solid", linewidth = 0.4) +
      labs(title = paste(model_key, "Partial Effect:", x_label),
           x = x_label, y = y_label) +
      theme_minimal(base_size = 13)

    # Threshold annotations
    th_list <- THRESHOLDS[[var_key]]
    if (!is.null(th_list)) {
      for (th in th_list) {
        p <- p +
          geom_vline(xintercept = th$x, linetype = "dashed",
                     color = th$color, linewidth = 0.8) +
          annotate("text", x = th$x, y = Inf, label = th$label,
                   color = th$color, angle = 90,
                   vjust = -0.4, hjust = 1.1, size = 3.5)
      }
    }

    # Hour-of-day AM/PM peak markers
    if (var_key == "dep_hour") {
      p <- p +
        geom_vline(xintercept = c(7, 17), linetype = "dotted",
                   color = "grey55", linewidth = 0.6) +
        annotate("text", x = 7,  y = Inf, label = "AM peak", angle = 90,
                 vjust = -0.4, hjust = 1.1, size = 3, color = "grey50") +
        annotate("text", x = 17, y = Inf, label = "PM peak", angle = 90,
                 vjust = -0.4, hjust = 1.1, size = 3, color = "grey50")
    }

    # Optional rug from stored training data
    if (input$show_rug) {
      raw_x <- tryCatch({
        mdl <- if (model_key == "M1") m1 else m2
        mdl$model[[var_key]]
      }, error = function(e) NULL)
      if (!is.null(raw_x)) {
        p <- p + geom_rug(data = data.frame(.x = raw_x), aes(x = .x),
                          alpha = 0.04, color = "grey30", sides = "b")
      }
    }
    p
  })

  output$smooth_narrative <- renderText({ interpret_smooth(input$explore_model, input$explore_var) })

  output$edf_table <- renderTable({
    if (input$explore_model == "M1") m1_edf else m2_edf
  }, digits = 3, striped = TRUE, hover = TRUE)

  # ── Tab 2: reactive prediction ──────────────────────────────────────────────
  prediction <- reactive({
    nd <- data.frame(
      Origin      = factor(input$pred_airport,
                           levels = c("ORD", "PHX", "ATL", "DFW", "DEN")),
      tmpf        = as.numeric(input$pred_tmpf),
      p01i        = as.numeric(input$pred_p01i),
      sknt        = as.numeric(input$pred_sknt),
      vsby        = as.numeric(input$pred_vsby),
      has_storm   = as.integer(input$pred_storm),
      dep_hour    = as.integer(input$pred_hour),
      fl_month    = as.integer(input$pred_month),
      day_of_week = factor(input$pred_dow,
                           levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
    )

    prob <- tryCatch(
      as.numeric(predict(m1, newdata = nd, type = "response")),
      error = function(e) { warning(e$message); NA_real_ }
    )

    m2_link <- tryCatch(
      predict(m2, newdata = nd, type = "link", se.fit = TRUE),
      error = function(e) { warning(e$message); list(fit = NA_real_, se.fit = NA_real_) }
    )

    list(
      prob     = prob,
      delay    = exp(m2_link$fit),
      delay_lo = exp(m2_link$fit - 1.96 * m2_link$se.fit),
      delay_hi = exp(m2_link$fit + 1.96 * m2_link$se.fit)
    )
  })

  # Plotly gauge
  output$risk_gauge <- renderPlotly({
    prob <- prediction()$prob
    if (is.na(prob)) prob <- 0

    bar_color <- if (prob < 0.20) "#2e7d32"
                 else if (prob < 0.40) "#f9a825"
                 else "#c62828"

    plot_ly(
      type  = "indicator",
      mode  = "gauge+number",
      value = round(prob * 100, 1),
      number = list(suffix = "%", font = list(size = 28)),
      title  = list(text = "P(Disrupted)", font = list(size = 15)),
      gauge  = list(
        axis  = list(range = list(0, 100), ticksuffix = "%"),
        bar   = list(color = bar_color),
        steps = list(
          list(range = c(0,  20), color = "#e8f5e9"),
          list(range = c(20, 40), color = "#fff9c4"),
          list(range = c(40, 100), color = "#ffebee")
        ),
        threshold = list(line = list(color = "black", width = 3),
                         thickness = 0.75, value = 30)
      )
    ) |>
      layout(margin = list(t = 60, b = 20, l = 20, r = 20))
  })

  output$risk_label <- renderUI({
    prob <- prediction()$prob
    if (is.na(prob)) return(p("Prediction unavailable."))
    level <- if (prob < 0.20) "Low risk"
             else if (prob < 0.40) "Moderate risk"
             else "High risk"
    color <- if (prob < 0.20) "#2e7d32"
             else if (prob < 0.40) "darkorange"
             else "#c62828"
    tags$p(
      style = sprintf("color:%s; font-weight:bold; font-size:16px;", color),
      sprintf("%s: %.1f%% chance of disruption", level, prob * 100)
    )
  })

  output$delay_output <- renderUI({
    pred <- prediction()
    if (is.na(pred$delay)) return(p("Delay prediction unavailable."))
    tagList(
      tags$h3(sprintf("%.0f minutes", pred$delay),
              style = "color: darkorange; margin: 0;"),
      tags$p(sprintf("95%% CI: %.0f – %.0f min", pred$delay_lo, pred$delay_hi),
             style = "color: grey50; font-size: 13px;"),
      tags$p("Expected departure delay if this flight is delayed.",
             style = "color: grey60; font-size: 12px;")
    )
  })

  # Comparison bar: scenario vs. airport average
  output$delay_bar <- renderPlot({
    pred    <- prediction()
    airport <- input$pred_airport
    if (is.na(pred$delay)) return(NULL)

    avg <- airport_avg_delay$avg_delay[airport_avg_delay$Origin == airport]
    if (length(avg) == 0 || is.na(avg)) return(NULL)

    bar_df <- data.frame(
      label = c("Your Scenario", paste(airport, "Avg (train)")),
      delay = c(pred$delay, avg),
      fill  = c("darkorange", "steelblue")
    )

    ggplot(bar_df, aes(x = label, y = delay, fill = fill)) +
      geom_col(width = 0.5, show.legend = FALSE) +
      scale_fill_identity() +
      geom_text(aes(label = sprintf("%.0f min", delay)),
                hjust = -0.15, size = 4.5) +
      coord_flip(ylim = c(0, max(bar_df$delay) * 1.45)) +
      labs(title = sprintf("vs. %s training average", airport),
           x = NULL, y = "Expected Delay (minutes)") +
      theme_minimal(base_size = 12)
  })
}

app <- shinyApp(ui = ui, server = server)
runApp(app, host = "0.0.0.0", port = 7777, launch.browser = TRUE)
