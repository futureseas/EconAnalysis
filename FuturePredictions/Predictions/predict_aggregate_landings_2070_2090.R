# Integrated Pipeline: Predict Annual, Daily, Monthly Participation and Monthly Landings

library(tidyverse)
library(lubridate)

# --- Step 1: Load and Predict Annual Participation ---
annual_model <- readRDS("annual_logit.RDS")
annual_data <- readRDS("annual_data_part.RDS")  # includes lagged SDMs, prices, etc.

# Filter future prediction years and scale vars as done in training
future_data <- annual_data %>%
  filter(set_year >= 2025) %>%  # adjust as needed
  group_by(group_all) %>%
  mutate(across(
    c(starts_with("lag_"), length, lat_cgo, fishmeal_price),
    ~ scale(.)[, 1]
  )) %>% ungroup()

future_data$annual_part_prob <- predict(annual_model, newdata = future_data, type = "response") %>%
  as.vector()

future_data <- future_data %>%
  mutate(pred_annual_participation = ifelse(annual_part_prob > 0.5, 1, 0))

# --- Step 2: Placeholder for Daily Participation Predictions ---
# Assume we will read in or compute daily participation with SDM + economics per cluster
# Expected output: daily_participation_df with columns VESSEL_NUM, DATE, cluster, participates_today

# Placeholder:
daily_participation_df <- tibble(
  VESSEL_NUM = integer(),
  DATE = as.Date(character()),
  cluster = integer(),
  participates_today = integer()  # 1 if yes, 0 if not
)

# --- Step 3: Aggregate Daily to Monthly Participation ---
monthly_participation <- daily_participation_df %>%
  mutate(
    year = year(DATE),
    month = month(DATE)
  ) %>%
  group_by(year, month, cluster) %>%
  summarize(
    vessels_participating = n_distinct(VESSEL_NUM[participates_today == 1]),
    .groups = "drop"
  )

# --- Step 4: Predict Monthly Landings (MSQD, PSDN, NANC) ---
# These are already modeled in landing_model_est_MSQD.R and landing_model_est_NANC.R

# Load fitted models (assumed saved from brm with file=...)
fit_MSQD <- readRDS("Landings/Estimations/fit_qMSQD_boost_GAM.rds")
fit_NANC <- readRDS("Landings/Estimations/fit_qNANC_boost_GAM.rds")
# (Add fit_PSDN when ready)

# Load monthly prediction dataset (future SDM, prices, port clusters, etc.)
monthly_data <- readRDS("Landings/Data/future_monthly_data.rds")  # Placeholder path

# Merge predicted monthly participation
monthly_data <- monthly_data %>%
  left_join(monthly_participation, by = c("LANDING_YEAR" = "year", "LANDING_MONTH" = "month", "port_cluster_ID" = "cluster"))

# Predict MSQD
monthly_data$pred_log_MSQD <- predict(fit_MSQD, newdata = monthly_data, resp = "logMSQDLandings") %>%
  as.data.frame() %>% pull(Estimate)

# Predict NANC
monthly_data$pred_log_NANC <- predict(fit_NANC, newdata = monthly_data, resp = "logNANCLandings") %>%
  as.data.frame() %>% pull(Estimate)

# Optional: convert from log-scale
monthly_data <- monthly_data %>%
  mutate(
    MSQD_Landings = exp(pred_log_MSQD),
    NANC_Landings = exp(pred_log_NANC)
    # Add PSDN_Landings when available
  )

# --- Final Output ---
# monthly_data now includes predicted landings per port-month-cluster
saveRDS(monthly_data, "Landings/Output/predicted_monthly_landings.rds")
