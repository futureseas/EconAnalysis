###
# Future Predictions 2070-2090 #
###

rm(list = ls(all.names = TRUE)) 
gc()

# Load packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)


#### Anual participation ####

## ---- Load Annual Participation Model and Data ----
annual_model <- readRDS("C:/GitHub/EconAnalysis/FuturePredictions/Predictions/Models/annual_logit.RDS")
annual_data <- readRDS("C:/GitHub/EconAnalysis/FuturePredictions/Predictions/Data/annual_data_part.RDS") 


#---- Create future database ----
vars_to_average <- setdiff(names(annual_data), c("VESSEL_NUM", "set_year", "year_participation"))
vessel_means <- annual_data %>%
  group_by(VESSEL_NUM) %>%
  summarise(across(all_of(vars_to_average), mean, na.rm = TRUE), .groups = "drop")

future_years <- 2070:2090

future_data <- expand.grid(
    VESSEL_NUM = unique(annual_data$VESSEL_NUM),
    set_year = future_years) %>%
  as_tibble()

future_data <- future_data %>%
  left_join(vessel_means, by = "VESSEL_NUM") %>%
  select(VESSEL_NUM, set_year, everything())


#---- Merge with future SDMs ----

##---- Load Future SDMs ----
PSDN_SDM_day <- read_csv("FuturePredictions/SDM/Future/PSDN_FutureSDM_port_day.csv")
ALBC_SDM_day <- read_csv("FuturePredictions/SDM/Future/ALBC_FutureSDM_port_day.csv")
HERR_SDM_day <- read_csv("FuturePredictions/SDM/Future/HERR_FutureSDM_port_day.csv")
JMCK_SDM_day <- read_csv("FuturePredictions/SDM/Future/JMCK_FutureSDM_port_day.csv")
MSQD_SDM_day <- read_csv("FuturePredictions/SDM/Future/MSQD_FutureSDM_port_day.csv")
NANC_SDM_day <- read_csv("FuturePredictions/SDM/Future/NANC_FutureSDM_port_day_merged.csv")
CMCK_SDM_day <- read_csv("FuturePredictions/SDM/Future/CMCK_FutureSDM_port_day.csv")


##---- Get annual average by port ----

PSDN_SDM_year <- PSDN_SDM_day %>% 
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarize(SDM_60_GFDL = mean(SDM_60_GFDL, na.rm = TRUE),
            SDM_60_IPSL = mean(SDM_60_IPSL, na.rm = TRUE),
            SDM_60_HADL = mean(SDM_60_HADL, na.rm = TRUE))
ALBC_SDM_year <- ALBC_SDM_day %>% 
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarize(SDM_90_GFDL = mean(SDM_90_GFDL, na.rm = TRUE),
            SDM_90_IPSL = mean(SDM_90_IPSL, na.rm = TRUE),
            SDM_90_HADL = mean(SDM_90_HADL, na.rm = TRUE))
HERR_SDM_year <- HERR_SDM_day %>% 
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarize(SDM_20_GFDL = mean(SDM_20_GFDL, na.rm = TRUE),
            SDM_20_IPSL = mean(SDM_20_IPSL, na.rm = TRUE),
            SDM_20_HADL = mean(SDM_20_HADL, na.rm = TRUE))
JMCK_SDM_year <- JMCK_SDM_day %>% 
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarize(SDM_60_GFDL = mean(SDM_60_GFDL, na.rm = TRUE),
            SDM_60_IPSL = mean(SDM_60_IPSL, na.rm = TRUE),
            SDM_60_HADL = mean(SDM_60_HADL, na.rm = TRUE))
MSQD_SDM_year <- MSQD_SDM_day %>% 
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarize(SDM_90_GFDL = mean(SDM_90_GFDL, na.rm = TRUE),
            SDM_90_IPSL = mean(SDM_90_IPSL, na.rm = TRUE),
            SDM_90_HADL = mean(SDM_90_HADL, na.rm = TRUE))
NANC_SDM_year <- NANC_SDM_day %>% 
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarize(SDM_60_GFDL = mean(SDM_60_GFDL, na.rm = TRUE),
            SDM_60_IPSL = mean(SDM_60_IPSL, na.rm = TRUE),
            SDM_60_HADL = mean(SDM_60_HADL, na.rm = TRUE))
CMCK_SDM_year <- CMCK_SDM_day %>% 
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarize(SDM_60_GFDL = mean(SDM_60_GFDL, na.rm = TRUE),
            SDM_60_IPSL = mean(SDM_60_IPSL, na.rm = TRUE),
            SDM_60_HADL = mean(SDM_60_HADL, na.rm = TRUE))

##---- Link SDM to cluster (group_all) ----


#---- Predict annual participation ----
future_data$annual_part_prob <- predict(annual_model, newdata = future_data, type = "response") %>%
  as.vector()
future_data <- future_data %>%
  mutate(pred_annual_participation = ifelse(annual_part_prob > 0.5, 1, 0))





# ---- Calculate monthly participation ----

# Define month of the season per cluster to do predictions....

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
