# =============================================================================
# 09_actuarial_pricing.R
# Actuarial Pricing Model — FlightGuard Parametric Delay Insurance
#
# Product:  Pays $10/minute of departure delay (cap $400) OR $200 flat if
#           cancelled.  Trigger: DepDel15 == 1 OR Cancelled == 1.
#
# Framework follows NAIC / CAS ratemaking standards:
#   - Frequency × Severity pure-premium method
#   - Multiplicative rating plan with base rate + relativities
#   - Expense & profit provision → gross premium
#   - A/E (Actual vs Expected) back-test on hold-out set
#   - Regulatory factor exhibits (ASOP 25 credibility, ASOP 12 classification)
#   - Rate filing summary tables (CSV)
# =============================================================================

library(mgcv)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(readr)
library(lubridate)

dir.create("outputs/actuarial", showWarnings = FALSE, recursive = TRUE)
dir.create("outputs/figures",   showWarnings = FALSE, recursive = TRUE)

cat("Loading models and data...\n")
m1 <- readRDS("outputs/m1_binary_gam.rds")
m2 <- readRDS("outputs/m2_duration_gam.rds")

flights_raw <- readRDS("data/processed/flights_weather_2024.rds")

flights <- flights_raw |>
  mutate(
    disrupted   = as.integer(DepDel15 == 1 | Cancelled == 1),
    dep_hour    = as.integer(format(LocalDT, "%H")),
    day_of_week = weekdays(as.Date(FlightDate), abbreviate = TRUE),
    fl_month    = as.integer(month(as.Date(FlightDate))),
    fl_month_label = month.abb[fl_month],
    Origin      = factor(Origin, levels = c("ORD","PHX","ATL","DFW","DEN")),
    day_of_week = factor(day_of_week,
                         levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
  )

# ─── SECTION 1:  PRODUCT & BENEFIT STRUCTURE ─────────────────────────────────
# FlightGuard parametric policy per flight:
#   Cancellation benefit:  $200 flat
#   Delay benefit:         $10 × DEP_DELAY (minutes), cap $400
#   Combined benefit:      max of the above if both occur (take cancellation)

DELAY_RATE   <- 10    # $/minute
DELAY_CAP    <- 400   # $ max delay payout
CANCEL_BEN   <- 200   # $ cancellation flat benefit

# Observed loss per flight (ground truth — used for A/E)
flights <- flights |>
  mutate(
    actual_loss = case_when(
      Cancelled == 1              ~ as.numeric(CANCEL_BEN),
      DepDel15  == 1              ~ pmin(DELAY_RATE * DepDelay, DELAY_CAP),
      TRUE                        ~ 0
    )
  )

cat(sprintf("Mean observed loss per flight: $%.2f\n", mean(flights$actual_loss, na.rm=TRUE)))

# ─── SECTION 2:  PREDICTED FREQUENCY & SEVERITY FOR EVERY FLIGHT ─────────────

# Frequency: P(disrupted) from M1
flights$freq_pred <- as.numeric(predict(m1, newdata = flights, type = "response"))

# Severity on delayed flights: E[DepDelay | DepDelay > 0] from M2
# Convert to dollar severity: $10/min × E[delay], then cap at $400 conceptually
# (We use the expected delay, capping is handled via the benefit structure above)
flights$sev_pred_min  <- as.numeric(predict(m2, newdata = flights, type = "response"))
flights$sev_pred_usd  <- pmin(DELAY_RATE * flights$sev_pred_min, DELAY_CAP)

# Pure Premium (expected loss per policy):
#   PP = P(disrupted) × E[benefit | disrupted]
# For simplicity, severity when disrupted blends delay & cancellation payments.
# We approximate E[benefit | disrupted] from M2 severity (delay duration model).
flights$pure_premium <- flights$freq_pred * flights$sev_pred_usd

cat(sprintf("Mean modelled pure premium:   $%.2f\n", mean(flights$pure_premium, na.rm=TRUE)))

# ─── SECTION 3:  EXPENSE & PROFIT PROVISIONS ─────────────────────────────────
# Standard insurance expense structure (parametric travel product):
#   Variable expense ratio (VER) : commission + admin per policy = 20%
#   Fixed expense per policy (FEP): IT, compliance overhead       = $2.50
#   Profit & contingency provision (PCP)                          = 5%
#   Target Loss Ratio (TLR) = 1 - VER - PCP                      = 75%
#
# Gross Premium = (Pure Premium + FEP) / (1 - VER - PCP)

VER <- 0.20   # variable expense ratio
FEP <- 2.50   # fixed expense per policy ($)
PCP <- 0.05   # profit & contingency provision

TLR <- 1 - VER - PCP   # target loss ratio = 0.75

flights$gross_premium_raw <- (flights$pure_premium + FEP) / (1 - VER - PCP)

# Minimum premium floor (regulatory & underwriting requirement)
MIN_PREMIUM <- 5.00
flights$gross_premium <- pmax(flights$gross_premium_raw, MIN_PREMIUM)

cat(sprintf("Target loss ratio:             %.0f%%\n",   TLR * 100))
cat(sprintf("Mean gross premium:           $%.2f\n",    mean(flights$gross_premium)))
cat(sprintf("%% at minimum premium floor:   %.1f%%\n",
            100 * mean(flights$gross_premium == MIN_PREMIUM)))

# ─── SECTION 4:  MULTIPLICATIVE RATING PLAN — BASE RATE & RELATIVITIES ───────
# Standard ratemaking: Gross Premium = Base Rate × (product of relativities)
# Base state: ORD, Monday, hour=10, month=6, avg weather conditions.
# Each factor's relativity = predicted PP at that level / predicted PP at base.

BASE_ND <- data.frame(
  Origin      = factor("ORD", levels = levels(flights$Origin)),
  tmpf        = 55, p01i = 0, sknt = 10, vsby = 10, has_storm = 0L,
  dep_hour    = 10L, fl_month = 6L,
  day_of_week = factor("Mon", levels = levels(flights$day_of_week))
)

base_freq  <- as.numeric(predict(m1, newdata = BASE_ND, type = "response"))
base_sev   <- as.numeric(predict(m2, newdata = BASE_ND, type = "response"))
base_pp    <- base_freq * pmin(DELAY_RATE * base_sev, DELAY_CAP)
base_gross <- (base_pp + FEP) / (1 - VER - PCP)

cat(sprintf("\nBase rate (ORD, Mon, 10am, Jun, clear): $%.2f\n", base_gross))

# ── Airport relativities ──────────────────────────────────────────────────────
airport_rel <- lapply(c("ORD","PHX","ATL","DFW","DEN"), function(ap) {
  nd <- BASE_ND; nd$Origin <- factor(ap, levels = levels(flights$Origin))
  f  <- as.numeric(predict(m1, newdata=nd, type="response"))
  s  <- as.numeric(predict(m2, newdata=nd, type="response"))
  pp <- f * pmin(DELAY_RATE * s, DELAY_CAP)
  data.frame(Factor="Airport", Level=ap,
             Freq_Rel   = round(f  / base_freq,  3),
             Sev_Rel    = round(s  / base_sev,   3),
             PP_Rel     = round(pp / base_pp,    3),
             Pure_Prem  = round(pp, 2),
             Gross_Prem = round((pp + FEP) / (1-VER-PCP), 2))
}) |> bind_rows()

# ── Season (month) relativities — 4 weather seasons ──────────────────────────
season_map <- c("1"="Winter","2"="Winter","3"="Spring","4"="Spring",
                "5"="Spring","6"="Summer","7"="Summer","8"="Summer",
                "9"="Fall",  "10"="Fall", "11"="Fall", "12"="Winter")

month_rel <- lapply(1:12, function(mo) {
  nd <- BASE_ND; nd$fl_month <- as.integer(mo)
  f  <- as.numeric(predict(m1, newdata=nd, type="response"))
  s  <- as.numeric(predict(m2, newdata=nd, type="response"))
  pp <- f * pmin(DELAY_RATE * s, DELAY_CAP)
  data.frame(Factor="Month", Level=month.abb[mo],
             Season     = season_map[as.character(mo)],
             Freq_Rel   = round(f  / base_freq,  3),
             Sev_Rel    = round(s  / base_sev,   3),
             PP_Rel     = round(pp / base_pp,    3),
             Pure_Prem  = round(pp, 2),
             Gross_Prem = round((pp + FEP) / (1-VER-PCP), 2))
}) |> bind_rows()

# ── Departure hour tier relativities (4 tiers per day) ───────────────────────
hour_tiers <- data.frame(
  hour    = 0:23,
  hr_tier = case_when(
    0:23 %in%  0:5  ~ "Night (00-05)",
    0:23 %in%  6:11 ~ "Morning (06-11)",
    0:23 %in% 12:17 ~ "Afternoon (12-17)",
    TRUE             ~ "Evening (18-23)"
  )
)

hour_rel <- lapply(unique(hour_tiers$hr_tier), function(tier) {
  rep_hour <- hour_tiers$hour[hour_tiers$hr_tier == tier][1]
  nd <- BASE_ND; nd$dep_hour <- as.integer(rep_hour)
  f  <- as.numeric(predict(m1, newdata=nd, type="response"))
  s  <- as.numeric(predict(m2, newdata=nd, type="response"))
  pp <- f * pmin(DELAY_RATE * s, DELAY_CAP)
  data.frame(Factor="Hour Tier", Level=tier,
             Freq_Rel   = round(f  / base_freq,  3),
             Sev_Rel    = round(s  / base_sev,   3),
             PP_Rel     = round(pp / base_pp,    3),
             Pure_Prem  = round(pp, 2),
             Gross_Prem = round((pp + FEP) / (1-VER-PCP), 2))
}) |> bind_rows() |> arrange(Level)

# ── Weather score tiers (5 levels per variable) ───────────────────────────────
weather_levels <- list(
  Temperature = list(var="tmpf", vals=c(15, 35, 60, 85, 105),
                     labels=c("Extreme Cold","Cold","Moderate","Hot","Extreme Heat")),
  Precipitation= list(var="p01i", vals=c(0, 0.1, 0.3, 0.7, 1.5),
                      labels=c("None","Light","Moderate","Heavy","Extreme")),
  Wind         = list(var="sknt", vals=c(5, 15, 25, 35, 50),
                      labels=c("Calm","Breezy","Windy","Very Windy","Severe")),
  Visibility   = list(var="vsby", vals=c(0.5, 2, 5, 8, 10),
                      labels=c("Near Zero","Low","Reduced","Good","Clear"))
)

wx_rel <- lapply(names(weather_levels), function(nm) {
  wl <- weather_levels[[nm]]
  lapply(seq_along(wl$vals), function(i) {
    nd <- BASE_ND; nd[[wl$var]] <- wl$vals[i]
    f  <- as.numeric(predict(m1, newdata=nd, type="response"))
    s  <- as.numeric(predict(m2, newdata=nd, type="response"))
    pp <- f * pmin(DELAY_RATE * s, DELAY_CAP)
    data.frame(Factor=nm, Level=wl$labels[i],
               Freq_Rel   = round(f  / base_freq,  3),
               Sev_Rel    = round(s  / base_sev,   3),
               PP_Rel     = round(pp / base_pp,    3),
               Pure_Prem  = round(pp, 2),
               Gross_Prem = round((pp + FEP) / (1-VER-PCP), 2))
  }) |> bind_rows()
}) |> bind_rows()

# Combine all relativities into one rate filing exhibit
relativity_exhibit <- bind_rows(
  airport_rel |> select(Factor, Level, Freq_Rel, Sev_Rel, PP_Rel, Pure_Prem, Gross_Prem),
  month_rel   |> select(Factor, Level, Freq_Rel, Sev_Rel, PP_Rel, Pure_Prem, Gross_Prem),
  hour_rel    |> select(Factor, Level, Freq_Rel, Sev_Rel, PP_Rel, Pure_Prem, Gross_Prem),
  wx_rel      |> select(Factor, Level, Freq_Rel, Sev_Rel, PP_Rel, Pure_Prem, Gross_Prem)
)

write_csv(relativity_exhibit, "outputs/actuarial/rate_relativity_exhibit.csv")
cat("Rate relativity exhibit saved.\n")
print(head(as.data.frame(relativity_exhibit), 50))

# ─── SECTION 5:  A/E (ACTUAL vs EXPECTED) BACK-TEST ─────────────────────────
# Test set: Oct-Dec 2024.  A/E > 1 = under-priced; A/E < 1 = over-priced.
# Target A/E ≈ 1.0 portfolio-wide.  Segments with A/E > 1.15 need rate increase.

test_df <- flights |>
  filter(fl_month >= 10) |>
  mutate(hr_tier = case_when(
    dep_hour %in% 0:5   ~ "Night (00-05)",
    dep_hour %in% 6:11  ~ "Morning (06-11)",
    dep_hour %in% 12:17 ~ "Afternoon (12-17)",
    TRUE                 ~ "Evening (18-23)"
  ))

# Overall A/E
overall_ae <- sum(test_df$actual_loss, na.rm=TRUE) /
              sum(test_df$pure_premium, na.rm=TRUE)
cat(sprintf("\nOverall A/E (test set, Oct-Dec): %.4f\n", overall_ae))

ae_by <- function(df, group_var) {
  df |>
    group_by(across(all_of(group_var))) |>
    summarise(
      Exposures    = n(),
      Actual_Loss  = sum(actual_loss,  na.rm=TRUE),
      Expected_PP  = sum(pure_premium, na.rm=TRUE),
      AE_Ratio     = round(Actual_Loss / Expected_PP, 4),
      Actual_LR    = round(Actual_Loss / sum(gross_premium, na.rm=TRUE), 4),
      Avg_Gross_PM = round(mean(gross_premium, na.rm=TRUE), 2),
      .groups="drop"
    ) |>
    mutate(
      AE_Flag = case_when(
        AE_Ratio > 1.15 ~ "NEEDS RATE INCREASE",
        AE_Ratio < 0.85 ~ "NEEDS RATE DECREASE",
        TRUE             ~ "Adequate"
      )
    )
}

ae_airport  <- ae_by(test_df, "Origin")
ae_month    <- ae_by(test_df, "fl_month_label")
ae_hr_tier  <- ae_by(test_df, "hr_tier")

cat("\n--- A/E by Airport ---\n");  print(ae_airport)
cat("\n--- A/E by Month ---\n");    print(ae_month)
cat("\n--- A/E by Hour Tier ---\n");print(ae_hr_tier)

write_csv(ae_airport,  "outputs/actuarial/ae_by_airport.csv")
write_csv(ae_month,    "outputs/actuarial/ae_by_month.csv")
write_csv(ae_hr_tier,  "outputs/actuarial/ae_by_hour_tier.csv")

# ─── SECTION 6:  RATE INDICATION ─────────────────────────────────────────────
# Indicated Rate Change (IRC) per segment = A/E × (1 / TLR) - 1
# IRC > 0  → need a rate increase
# IRC < 0  → can decrease rates
# Credibility-weighted: blend segment indication with overall indication

# Bühlmann credibility: full credibility at k = 1082 claims (standard P&C)
K_FULL_CRED <- 1082

ae_airport <- ae_airport |>
  mutate(
    Claims         = as.integer(Actual_Loss / mean(test_df$actual_loss[test_df$actual_loss > 0], na.rm=TRUE)),
    Credibility    = round(pmin(sqrt(Claims / K_FULL_CRED), 1), 4),
    Indicated_IRC  = round(AE_Ratio / TLR - 1, 4),
    Overall_IRC    = round(overall_ae / TLR - 1, 4),
    Credible_IRC   = round(Credibility * Indicated_IRC + (1-Credibility) * Overall_IRC, 4),
    Recommended_RC = paste0(ifelse(Credible_IRC >= 0, "+", ""), round(Credible_IRC*100, 1), "%")
  )

cat("\n--- Rate Indications by Airport (with Bühlmann credibility) ---\n")
print(ae_airport |> select(Origin, Exposures, Credibility, Indicated_IRC, Credible_IRC, Recommended_RC))
write_csv(ae_airport, "outputs/actuarial/rate_indications_airport.csv")

# ─── SECTION 7:  REGULATORY FACTOR JUSTIFICATION EXHIBIT ─────────────────────
# ASOP 12 requires statistical support for each rating variable.
# We report: edf (degree of non-linearity), chi-square stat, p-value, sample size.

m1_summary <- summary(m1)
m2_summary <- summary(m2)

factor_justification <- data.frame(
  Variable    = c("Temperature (tmpf)", "Precipitation (p01i)",
                  "Wind Speed (sknt)",  "Visibility (vsby)",
                  "Departure Hour",     "Month (seasonal)"),
  Model       = "M1 + M2",
  M1_EDF      = round(m1_summary$s.table[, "edf"],     3),
  M1_ChiSq    = round(m1_summary$s.table[, "Chi.sq"],  1),
  M1_pvalue   = signif(m1_summary$s.table[, "p-value"],3),
  M2_EDF      = round(m2_summary$s.table[, "edf"],     3),
  M2_F        = round(m2_summary$s.table[, "F"],        2),
  M2_pvalue   = signif(m2_summary$s.table[, "p-value"],3),
  Nonlinearity= ifelse(m1_summary$s.table[,"edf"] > 3, "Strong", "Moderate"),
  Justification = c(
    "Density altitude limits (>95F) and de-icing rules (<32F) create threshold effects",
    "Precipitation directly causes taxiway slowdowns and low-visibility procedures",
    "Crosswind operational limits (~25 kt) create sharp threshold in delay risk",
    "IFR minimums (< 3 mi) trigger spacing rules; strong non-linear below threshold",
    "Congestion-driven; afternoon bank stacking increases cascading delay risk",
    "Summer convective season and winter ice events drive significant rate variation"
  )
)
write_csv(factor_justification, "outputs/actuarial/factor_justification_exhibit.csv")
cat("\n--- Factor Justification Exhibit ---\n")
print(factor_justification |> select(Variable, M1_EDF, M1_pvalue, M2_EDF, M2_pvalue, Nonlinearity))

# ─── SECTION 8:  RATE BOOK — PRACTICAL PREMIUM GRID ─────────────────────────
# A rate book is what an underwriter or booking engine actually queries.
# Grid: Airport × Season × Hour Tier × Weather Score (5 levels)
# Weather Score = weighted sum of risk factors, binned 1-5.

# Create a weather risk score (1=best, 5=worst) as a composite rating variable
# Score derived from individual relativities to stay consistent with the GAM.
flights <- flights |>
  mutate(
    wx_score = cut(
      # normalize each variable contribution and sum to composite index
      scale(tmpf - 55)[,1] * (-0.3) +   # cold and heat both raise risk
      abs(scale(tmpf - 55)[,1]) * 0.4 +
      scale(p01i)[,1] * 0.4 +
      scale(sknt)[,1] * 0.2 +
      (10 - vsby) / 10 * 0.5 +
      has_storm * 1.5,
      breaks = quantile(
        scale(tmpf - 55)[,1] * (-0.3) +
        abs(scale(tmpf - 55)[,1]) * 0.4 +
        scale(p01i)[,1] * 0.4 +
        scale(sknt)[,1] * 0.2 +
        (10 - vsby) / 10 * 0.5 +
        has_storm * 1.5,
        probs = seq(0,1,0.2), na.rm=TRUE),
      labels = c("1-Best","2-Good","3-Average","4-Poor","5-Worst"),
      include.lowest = TRUE
    ),
    season = season_map[as.character(fl_month)],
    hr_tier = case_when(
      dep_hour %in% 0:5   ~ "Night",
      dep_hour %in% 6:11  ~ "Morning",
      dep_hour %in% 12:17 ~ "Afternoon",
      TRUE                 ~ "Evening"
    )
  )

rate_book <- flights |>
  group_by(Origin, season, hr_tier, wx_score) |>
  summarise(
    Policies     = n(),
    Avg_Pure_PP  = round(mean(pure_premium,   na.rm=TRUE), 2),
    Avg_Gross_PM = round(mean(gross_premium,  na.rm=TRUE), 2),
    Avg_ActLoss  = round(mean(actual_loss,    na.rm=TRUE), 2),
    Loss_Ratio   = round(mean(actual_loss, na.rm=TRUE) /
                         mean(gross_premium, na.rm=TRUE), 3),
    .groups="drop"
  ) |>
  filter(!is.na(wx_score)) |>
  arrange(Origin, season, hr_tier, wx_score)

write_csv(rate_book, "outputs/actuarial/rate_book.csv")
cat(sprintf("\nRate book saved (%d cells).\n", nrow(rate_book)))

# ─── SECTION 9:  PORTFOLIO PROFITABILITY SUMMARY ─────────────────────────────
portfolio <- flights |>
  summarise(
    Total_Policies      = n(),
    Total_Earned_Prem   = sum(gross_premium, na.rm=TRUE),
    Total_Pure_Prem     = sum(pure_premium,  na.rm=TRUE),
    Total_Actual_Loss   = sum(actual_loss,   na.rm=TRUE),
    Modelled_Loss_Ratio = round(Total_Pure_Prem  / Total_Earned_Prem, 4),
    Actual_Loss_Ratio   = round(Total_Actual_Loss / Total_Earned_Prem, 4),
    Expense_Ratio       = round(VER + FEP * n() / Total_Earned_Prem, 4),
    Modelled_Combined   = round((Total_Pure_Prem  + VER * Total_Earned_Prem + FEP * n()) / Total_Earned_Prem, 4),
    Actual_Combined     = round((Total_Actual_Loss + VER * Total_Earned_Prem + FEP * n()) / Total_Earned_Prem, 4),
    Modelled_Profit_Pct = round(1 - Modelled_Combined, 4),
    Actual_Profit_Pct   = round(1 - Actual_Combined,   4)
  )

cat("\n============================================================\n")
cat("PORTFOLIO PROFITABILITY SUMMARY\n")
cat("============================================================\n")
cat(sprintf("  Total Policies:          %s\n",  format(portfolio$Total_Policies, big.mark=",")))
cat(sprintf("  Total Earned Premium:    $%s\n", format(round(portfolio$Total_Earned_Prem), big.mark=",")))
cat(sprintf("  Modelled Loss Ratio:     %.1f%%\n", portfolio$Modelled_Loss_Ratio * 100))
cat(sprintf("  Actual Loss Ratio:       %.1f%%\n", portfolio$Actual_Loss_Ratio   * 100))
cat(sprintf("  Expense Ratio:           %.1f%%\n", portfolio$Expense_Ratio       * 100))
cat(sprintf("  Modelled Combined Ratio: %.1f%%\n", portfolio$Modelled_Combined   * 100))
cat(sprintf("  Actual Combined Ratio:   %.1f%%\n", portfolio$Actual_Combined     * 100))
cat(sprintf("  Modelled Profit Margin:  %.1f%%\n", portfolio$Modelled_Profit_Pct * 100))
cat(sprintf("  Actual Profit Margin:    %.1f%%\n", portfolio$Actual_Profit_Pct   * 100))
cat("============================================================\n")

write_csv(as.data.frame(t(portfolio)), "outputs/actuarial/portfolio_summary.csv",
          col_names=FALSE)

# ─── SECTION 10:  VISUALIZATIONS ─────────────────────────────────────────────

# 10a.  Relativity chart — all factors combined
rel_plot <- relativity_exhibit |>
  mutate(label = paste0(Factor, "\n", Level)) |>
  ggplot(aes(x=reorder(label, PP_Rel), y=PP_Rel, fill=Factor)) +
  geom_col(show.legend=FALSE) +
  geom_hline(yintercept=1, linetype="dashed", color="grey40") +
  geom_text(aes(label=sprintf("%.2f×", PP_Rel),
                vjust=ifelse(PP_Rel >= 1, -0.3, 1.3)), size=2.5) +
  facet_wrap(~Factor, scales="free_x") +
  labs(title="Rate Relativities by Factor",
       subtitle="Pure-premium relativity vs base scenario (ORD, Mon, 10am, Jun, clear)",
       x=NULL, y="Pure Premium Relativity") +
  theme_minimal(base_size=10) +
  theme(axis.text.x=element_text(angle=35, hjust=1, size=8),
        plot.title=element_text(face="bold", size=13))

ggsave("outputs/figures/actuarial_relativities.png", rel_plot,
       width=16, height=8, dpi=300)

# 10b.  A/E chart by airport
ae_plot <- ae_airport |>
  ggplot(aes(x=Origin, y=AE_Ratio, fill=AE_Flag)) +
  geom_col(width=0.6) +
  geom_hline(yintercept=1,    linetype="solid",  color="black", linewidth=0.8) +
  geom_hline(yintercept=1.15, linetype="dashed", color="firebrick",  linewidth=0.6) +
  geom_hline(yintercept=0.85, linetype="dashed", color="steelblue",  linewidth=0.6) +
  geom_text(aes(label=sprintf("%.2f\n%s", AE_Ratio, Recommended_RC)),
            vjust=-0.3, size=3.5) +
  scale_fill_manual(values=c("NEEDS RATE INCREASE"="firebrick",
                              "NEEDS RATE DECREASE"="steelblue",
                              "Adequate"="#2e7d32"), name=NULL) +
  scale_y_continuous(labels=scales::percent) +
  labs(title="A/E Ratio by Airport — Test Set (Oct–Dec 2024)",
       subtitle="Dashed bands: ±15% adequacy corridor | With credibility-weighted rate change indication",
       x="Airport", y="Actual / Expected Loss Ratio") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold"), legend.position="bottom")

ggsave("outputs/figures/actuarial_ae_airport.png", ae_plot,
       width=10, height=6, dpi=300)

# 10c.  Rate book heatmap: airport × season (avg gross premium)
rb_heat <- rate_book |>
  group_by(Origin, season) |>
  summarise(Avg_Gross_PM = round(mean(Avg_Gross_PM), 2), .groups="drop") |>
  ggplot(aes(x=season, y=Origin, fill=Avg_Gross_PM)) +
  geom_tile(color="white", linewidth=0.5) +
  geom_text(aes(label=sprintf("$%.2f", Avg_Gross_PM)), size=4) +
  scale_fill_distiller(palette="YlOrRd", direction=1, name="Avg\nGross\nPremium") +
  labs(title="Rate Book: Average Gross Premium by Airport × Season",
       subtitle="FlightGuard parametric delay insurance | Includes expense & profit load",
       x="Season", y="Airport") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold"))

ggsave("outputs/figures/actuarial_rate_book_heatmap.png", rb_heat,
       width=9, height=5, dpi=300)

# 10d.  Loss ratio distribution across all policies
lr_hist <- flights |>
  filter(gross_premium > 0) |>
  mutate(policy_lr = actual_loss / gross_premium) |>
  ggplot(aes(x=policy_lr)) +
  geom_histogram(bins=60, fill="steelblue", color="white", alpha=0.8) +
  geom_vline(xintercept=TLR, linetype="dashed", color="firebrick", linewidth=1) +
  annotate("text", x=TLR+0.05, y=Inf, label=sprintf("Target LR = %.0f%%", TLR*100),
           color="firebrick", hjust=0, vjust=1.5, size=3.5) +
  scale_x_continuous(labels=scales::percent, limits=c(0,3)) +
  labs(title="Per-Policy Loss Ratio Distribution",
       subtitle="Most policies = 0 (no claim); spike at left. Target = red dashed line.",
       x="Loss / Gross Premium", y="Number of Policies") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold"))

ggsave("outputs/figures/actuarial_lr_distribution.png", lr_hist,
       width=9, height=5, dpi=300)

cat("\nScript 09 complete. All outputs in outputs/actuarial/ and outputs/figures/\n")
