# Actuarial Application: Parametric Flight Delay Pricing Models

## 1. Executive Summary & Objective
This document transitions our machine-learning predictive work into a highly specific, concrete Actuarial Pricing Engine. The goal is to programmatically generate an individual **Gross Premium (GP)** for any given flight utilizing our Generalized Additive Models (M1 and M2). This framework acts as a foundational blueprint for developing an automated Parametric Insurance product that requires no human adjudicators.

---

## 2. Concrete Product Architecture

We will design a **Dynamic Duration Parametric Product**. 
Instead of paying a flat $100 for any delay, the policy compensates travelers proportionally to the time they lose, matching classical indemnity principles while utilizing modern parametric triggers.

### Policy Specifications:
*   **Trigger Mechanism:** Verification via the BTS API or FAA Data stating a flight has departed its origin terminal $\ge 15$ minutes late.
*   **Deductible (Wait Period):** The first 14 minutes of any departure delay incur a $\$0$ payout.
*   **Indemnity Rate:** The traveler is compensated at a continuous formulation of **$\$2.00$ per delayed minute**, starting from the 15th minute.
*   **Policy Limit (Max Payout):** The payout maxes out at a 4-hour (240 minutes) delay ceiling to prevent catastrophic insurer losses. Maximum payout = $240 \text{ min} \times \$2.00 = \$480.00$. Cancellations automatically trigger the max payout limit ($\$480.00$).

---

## 3. Mathematical Mapping of Models to Actuarial Risk

To accurately charge a premium, the algorithm must predict the absolute expected loss on a mathematically continuous curve.

### A. Frequency: Evaluating Claim Probability ($f$)
We define Frequency $f$ as the exact probability a flight experiences a valid disruption (Delay $\ge 15$ mins or Cancelled). 
*   **Engine Source:** `outputs/m1_binary_gam.rds` (Logistic Regression GAM).
*   **Extraction Method:** We extract the probability strictly executing: 
    ```r
    p_hat <- predict(m1, newdata = flight_vector, type = "response")
    ```

### B. Severity: Evaluating Expected Cost ($s$)
We define Severity $s$ as the monetary value of the claim given that a delay has already triggered. 
*   **Engine Source:** `outputs/m2_duration_gam.rds` (Gamma GAM, Log-Link).
*   **Extraction Method:** M2 predicts the total delay duration $T$ in minutes. 
    ```r # We predict the link function and exponentiate to get the expected duration (mu):
    mu_duration <- exp(predict(m2, newdata = flight_vector, type = "link"))
    ```
*   **Conditional Formulation:** Since our product pays $\$2.00$ per minute, the expected monetary severity $E[S]$ is defined by capping the duration:
    $$ E[S] = \$2.00 \times \min(\mu_{duration}, 240) $$

---

## 4. Actuarial Pricing Algorithm: From Pure to Gross Premium

Actuaries use specialized loading structures to safeguard against volatility. We will use the **Standard Deviation Loading Principle**.

### Step 1: Base Expected Loss (Pure Premium)
The absolute minimum mathematical breakeven price. 
$$ \text{Pure Premium } (EL) = f \times E[S] $$

### Step 2: Risk Margin (Standard Deviation Loading)
Because weather varies unpredictably, GAM models have standard errors. The insurer must generate reserve capital for bad days. We extract `se.fit` from our model predictions.
$$ \text{Standard Error Component (SE)} = \text{Upper 95\% Bound of } \mu_{duration} - \mu_{duration} $$
$$ Risk Loading = 0.25 \times SE \times \$2.00 \times f $$
*(Here, we are heavily loading the premium if the GAM is unconfident in its weather prediction).*

### Step 3: Expense Ratio & Profit Target
The insurer incurs fixed costs to maintain databases and process credit cards. We set concrete administrative goals:
*   **$e$ (Expense Ratio):** $15\%$ of final premium.
*   **$\pi$ (Target Profit Margin):** $10\%$ of final premium.
Combined, denominator loading is $1 - (0.15 + 0.10) = 0.75$.

### Step 4: Final Gross Premium ($GP$) Equation
$$ \text{Gross Premium (GP)} = \frac{\text{Pure Premium} + \text{Risk Loading}}{1 - (e + \pi)} $$

---

## 5. Concrete R Implementation Plan (`scripts/08_Pricing_Engine.R`)

In the next phase, we will write a highly explicit R script that structurally executes the above equations on our Testing Dataset (Flights from Oct-Dec 2024).

### R Pipeline Operations:
1.  **Data Ingestion:** Load `data/processed/flights_weather_2024.rds`, isolating the testing partition (`fl_month >= 10`).
2.  **Inference Matrix:**
    *   Bind `predict(m1, se.fit = TRUE)` and `predict(m2, se.fit = TRUE)`.
3.  **Financial Transformations:**
    *   Generate a `Pure_Premium` vector.
    *   Generate a `Risk_Loading` vector.
    *   Generate a `Gross_Premium` vector.
4.  **Profitability Backtest Simulation:**
    *   We will calculate the **Actual Loss** across the test set (If a flight was actually delayed, what would the insurer have paid?).
    *   Calculate **Total Premiums Collected** vs **Total Claims Paid** to strictly prove the algorithm yields a solvent combined operating ratio $< 100\%$.

### Output Architecture (`outputs/actuarial_dashboard.csv`)
The engine will output a final file mapped exactly to these specific headers to be ingested by underwriting auditors:
*   `Origin_Airport`, `Scheduled_Time`, `Temp_F`, `Wind_Kt` (Base Risk Identifiers)
*   `Prob_Delay_M1`, `Exp_Duration_M2` (GAM Raw Outputs)
*   `Pure_Premium_USD`, `Gross_Premium_USD` (Calculated Quotes)
*   `Actual_Observed_Delay`, `Actual_Claim_Paid_USD` (Backtest Verification)
*   `Underwriting_Profit_USD` (GP - Actual Claim)

---

## 6. Regulatory Compliance (IFRS 17 Alignment)

To legally launch this program, a state Department of Insurance (DOI) mandates that pricing cannot be mathematically discriminatory or overly obscure.
Our explicit selection of Generalized Additive Models directly rectifies these issues:
1.  **Transparency:** By calling `summary(m2)$s.table`, an auditor can see the precise non-linear mathematical curve that dictates why heavily snowing conditions at ORD bump the premium from $\$15$ to $\$35$.
2.  **Decomposing the Premium:** Because GAMs use additive basis splines, we can decompose the `Gross Premium` structurally indicating exactly how much of a single user's price tag was attributed to high wind versus baseline airport congestion, satisfying federal explainable AI (XAI) mandates.
