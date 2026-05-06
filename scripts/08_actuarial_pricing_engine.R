# 08_actuarial_pricing_engine.R
# Simulates live quoting of parametric insurance policies across the Oct-Dec 2024 test suite.
# Evaluates total premiums collected against total claims paid.

library(mgcv)
library(dplyr)
library(readr)
library(lubridate)
library(tidyr)

dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

# ── 1. Load Data and Models ───────────────────────────────────────────────────
cat("Loading datasets and models...\n")
flights <- readRDS("data/processed/flights_weather_2024.rds")

if (!file.exists("outputs/m1_binary_gam.rds") | !file.exists("outputs/m2_duration_gam.rds")) {
  stop("Missing model RDS files. Run scripts 03 and 04 first.")
}

m1 <- readRDS("outputs/m1_binary_gam.rds")
m2 <- readRDS("outputs/m2_duration_gam.rds")

# Feature engineering to match training
flights <- flights |>
  mutate(
    dep_hour    = as.integer(format(LocalDT, "%H")),
    day_of_week = factor(weekdays(as.Date(FlightDate), abbreviate = TRUE), levels = levels(m1$model$day_of_week)),
    month       = factor(as.character(month(as.Date(FlightDate))), levels = as.character(1:12)),
    fl_month    = month(as.Date(FlightDate)),
    Origin      = factor(Origin, levels = levels(m1$model$Origin))
  )

# Extract testing partition
test_df <- flights |> filter(fl_month >= 10)

# Drop rows with NAs in model variables
model_vars <- c("tmpf", "p01i", "sknt", "vsby", "has_storm", "dep_hour", "Origin", "day_of_week", "month")
n_before <- nrow(test_df)
test_df <- test_df |> drop_na(all_of(model_vars))
cat(sprintf("Evaluating %d test flights (dropped %d for missing weather)\n", nrow(test_df), n_before - nrow(test_df)))

# ── 2. Run Actuarial Inference ────────────────────────────────────────────────
cat("Generating statistical inferences...\n")

# Frequency (M1): Probability of disruption
cat("  -> Calculating P(Delay > 14 mins or Cancelled)...\n")
prob_delay <- predict(m1, newdata = test_df, type = "response")

# Severity (M2): Expected duration conditional on delay > 0
cat("  -> Calculating Expected Delay Durations and Standard Errors...\n")
m2_preds <- predict(m2, newdata = test_df, type = "link", se.fit = TRUE)

# Expected duration in minutes (exp to back-transform from log link)
exp_duration <- exp(m2_preds$fit)
# Upper 95% bound approximation for duration risk loading (Z score ~1.64 for 95% upper)
# We use standard error on the link scale, then back-transform
exp_duration_upper <- exp(m2_preds$fit + 1.645 * m2_preds$se.fit)

# Add inferences to the dataframe
pricing_df <- test_df |>
  mutate(
    Prob_Delay_M1   = as.numeric(prob_delay),
    Exp_Duration_M2 = as.numeric(exp_duration),
    Exp_Duration_UB = as.numeric(exp_duration_upper)
  )

# ── 3. Apply Insurance Policy Logic ───────────────────────────────────────────
cat("Executing Parametric Pricing Strategy...\n")

RATE_PER_MIN    <- 2.00
MAX_MINUTES     <- 240   # 4 hour absolute limit
EXPENSE_RATIO   <- 0.15  # Administrative costs
PROFIT_MARGIN   <- 0.10  # Target UW profit
COMBINED_MARGIN <- EXPENSE_RATIO + PROFIT_MARGIN

pricing_df <- pricing_df |>
  mutate(
    # --- NEW UNDERWRITING MANUAL RULES ---
    # Rule 1: Embargo Sale. If GAM frequency dictates an >80% chance of disruption, refuse to underwrite.
    Underwriting_Action = if_else(Prob_Delay_M1 >= 0.80, "EMBARGO_SALE", "BIND_POLICY"),
    
    # Rule 2: Limit Reduction. If departure hour is >= 18:00 (Night), slash the maximum limits from 4 hours to 2 hours.
    Current_Max_Mins = if_else(dep_hour >= 18, 120, MAX_MINUTES),
    
    # Financial Severity Cap (applying the dynamic limit)
    Expected_Severity = pmin(Exp_Duration_M2, Current_Max_Mins) * RATE_PER_MIN,
    Upper_Severity    = pmin(Exp_Duration_UB, Current_Max_Mins) * RATE_PER_MIN,
    
    # Standard Error Gap equates to Actuarial Risk Severity Profile
    Severity_Risk_Gap = Upper_Severity - Expected_Severity,
    
    # 1. Pure Premium (Expected Loss)
    Pure_Premium_USD = Prob_Delay_M1 * Expected_Severity,
    
    # 2. Additive Risk Loading (Charging for high uncertainty in weather patterns)
    Risk_Loading_USD = 0.25 * Severity_Risk_Gap * Prob_Delay_M1,
    
    # 3. Final Gross Premium Rate (Factoring in Target Operating Ratios)
    Gross_Premium_USD = (Pure_Premium_USD + Risk_Loading_USD) / (1 - COMBINED_MARGIN),
    
    # If policy is embargoed, zero out the quoted premium.
    Gross_Premium_USD = if_else(Underwriting_Action == "EMBARGO_SALE", 0, Gross_Premium_USD)
  )

# ── 4. Profitability Backtesting Simulation ───────────────────────────────────
cat("Simulating Real-World Payouts against the Actual Test Flights...\n")

pricing_df <- pricing_df |>
  mutate(
    # Ground Truth Validation: What actually happened to this specific flight?
    Actual_Observed_Delay = case_when(
      Cancelled == 1 ~ Current_Max_Mins,      # Max payout if entirely cancelled
      DepDelay > 14  ~ pmin(DepDelay, Current_Max_Mins), # Payout triggered
      TRUE           ~ 0                 # Under 15 mins = 0 payout
    ),
    
    Potential_Claim_USD = Actual_Observed_Delay * RATE_PER_MIN,
    
    # If the policy was embargoed, we pay absolutely nothing regardless of delay
    Actual_Claim_Paid_USD = if_else(Underwriting_Action == "EMBARGO_SALE", 0, Potential_Claim_USD),
    
    Underwriting_Profit_USD = Gross_Premium_USD - Actual_Claim_Paid_USD
  )

# Aggregate Fund Performance
total_premium   <- sum(pricing_df$Gross_Premium_USD, na.rm = TRUE)
total_claims    <- sum(pricing_df$Actual_Claim_Paid_USD, na.rm = TRUE)
total_uw_profit <- total_premium - total_claims
loss_ratio      <- total_claims / total_premium

cat("\n============================================\n")
cat("      ACTUARIAL SIMULATION RESULTS\n")
cat("============================================\n")
cat(sprintf("Total Flights Insured:  %s\n", format(nrow(pricing_df), big.mark = ",")))
cat(sprintf("Policies Embargoed:   %s\n", format(sum(pricing_df$Underwriting_Action == "EMBARGO_SALE"), big.mark = ",")))
cat(sprintf("Total Premium Bound:  $%s\n", format(round(total_premium, 2), big.mark = ",")))
cat(sprintf("Total Claims Settled: $%s\n", format(round(total_claims, 2), big.mark = ",")))
cat(sprintf("Underwriting Profit:  $%s\n", format(round(total_uw_profit, 2), big.mark = ",")))
cat(sprintf("Realized Loss Ratio:  %.2f%%\n", loss_ratio * 100))
cat("============================================\n")

if (loss_ratio < (1 - COMBINED_MARGIN)) {
  cat("STATUS: HIGHLY PROFITABLE. Model accurately predicted extreme weather triggers.\n")
} else if (loss_ratio < 1) {
  cat("STATUS: MARGINALLY PROFITABLE. Unexpected weather events eroded target margins.\n")
} else {
  cat("STATUS: INSOLVENT. Catastrophic disruptions breached capitalization limits.\n")
}

# ── 5. Save Output ────────────────────────────────────────────────────────────
output_data <- pricing_df |>
  select(FlightDate, Origin, Dest, tmpf, sknt, p01i, Prob_Delay_M1, Exp_Duration_M2,
         Underwriting_Action, Current_Max_Mins,
         Pure_Premium_USD, Risk_Loading_USD, Gross_Premium_USD, 
         Actual_Observed_Delay, Actual_Claim_Paid_USD, Underwriting_Profit_USD) |>
  arrange(FlightDate, Origin)

write_csv(output_data, "outputs/actuarial_dashboard.csv")
cat("\nLine-by-line financial ledger saved -> outputs/actuarial_dashboard.csv\n")
cat("Script 08 complete.\n")
