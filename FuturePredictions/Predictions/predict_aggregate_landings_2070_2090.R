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
# annual_model <- readRDS("C:/GitHub/EconAnalysis/FuturePredictions/Predictions/Models/annual_logit.RDS")
# annual_data <- readRDS("C:/GitHub/EconAnalysis/FuturePredictions/Predictions/Data/annual_data_part.RDS") 
# 
# 
# #---- Create future database ----
# vars_to_average <- setdiff(names(annual_data), c("VESSEL_NUM", "set_year", "year_participation"))
# vessel_means <- annual_data %>%
#   group_by(VESSEL_NUM) %>%
#   summarise(across(all_of(vars_to_average), mean, na.rm = TRUE), .groups = "drop")
# 
# future_years <- 2070:2090
# 
# future_data <- expand.grid(
#     VESSEL_NUM = unique(annual_data$VESSEL_NUM),
#     set_year = future_years) %>%
#   as_tibble()
# 
# future_data <- future_data %>%
#   left_join(vessel_means, by = "VESSEL_NUM") %>%
#   select(VESSEL_NUM, set_year, everything())
# 
# 
# #---- Merge with future SDMs ----
# 
# ##---- Load Future SDMs ----
# PSDN_SDM_day <- read_csv("FuturePredictions/SDM/Future/PSDN_FutureSDM_port_day.csv")
# ALBC_SDM_day <- read_csv("FuturePredictions/SDM/Future/ALBC_FutureSDM_port_day.csv")
# HERR_SDM_day <- read_csv("FuturePredictions/SDM/Future/HERR_FutureSDM_port_day.csv")
# JMCK_SDM_day <- read_csv("FuturePredictions/SDM/Future/JMCK_FutureSDM_port_day.csv")
# MSQD_SDM_day <- read_csv("FuturePredictions/SDM/Future/MSQD_FutureSDM_port_day.csv")
# NANC_SDM_day <- read_csv("FuturePredictions/SDM/Future/NANC_FutureSDM_port_day_merged.csv")
# CMCK_SDM_day <- read_csv("FuturePredictions/SDM/Future/CMCK_FutureSDM_port_day.csv")
# 
# 
# ##---- Get annual average by port ----
# 
# PSDN_SDM_year <- PSDN_SDM_day %>% 
#   group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
#   summarize(lag_mean_avail_PSDN_GFDL = mean(SDM_60_GFDL, na.rm = TRUE),
#             lag_mean_avail_PSDN_IPSL = mean(SDM_60_IPSL, na.rm = TRUE),
#             lag_mean_avail_PSDN_HADL = mean(SDM_60_HADL, na.rm = TRUE))
# MSQD_SDM_year <- MSQD_SDM_day %>% 
#   group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
#   summarize(lag_mean_avail_MSQD_GFDL = mean(SDM_90_GFDL, na.rm = TRUE),
#             lag_mean_avail_MSQD_IPSL = mean(SDM_90_IPSL, na.rm = TRUE),
#             lag_mean_avail_MSQD_HADL = mean(SDM_90_HADL, na.rm = TRUE))
# 
# SDM_year <- merge(PSDN_SDM_year, MSQD_SDM_year, by = c("LANDING_YEAR", "PORT_AREA_CODE"))
# 
# 
# ##---- Link SDM to cluster (group_all) ----
# 
# ### Cluster 4 
# 
# ports_c4 <- c("SFA", "LAA", "MNA", "SBA", "MRA")
# SDM_year_c4 <- SDM_year %>% 
#   filter(PORT_AREA_CODE %in% ports_c4) %>%
#   group_by(LANDING_YEAR) %>% 
#   summarize(lag_mean_avail_PSDN_GFDL = mean(lag_mean_avail_PSDN_GFDL, na.rm = TRUE),
#             lag_mean_avail_PSDN_IPSL = mean(lag_mean_avail_PSDN_IPSL, na.rm = TRUE),
#             lag_mean_avail_PSDN_HADL = mean(lag_mean_avail_PSDN_HADL, na.rm = TRUE),
#             lag_mean_avail_MSQD_GFDL = mean(lag_mean_avail_MSQD_GFDL, na.rm = TRUE),
#             lag_mean_avail_MSQD_IPSL = mean(lag_mean_avail_MSQD_IPSL, na.rm = TRUE),
#             lag_mean_avail_MSQD_HADL = mean(lag_mean_avail_MSQD_HADL, na.rm = TRUE)) %>%
#   mutate(LANDING_YEAR = LANDING_YEAR + 1,
#          group_all = 4) %>%
#   rename(set_year = LANDING_YEAR)
#   
#   
# ### Cluster 5 
# 
# ports_c5 <- c("MNA", "SBA", "MRA", "LAA", "NPA", "SFA", "CBA", "CLO", "CWA", "CLW")
# SDM_year_c5 <- SDM_year %>% 
#   filter(PORT_AREA_CODE %in% ports_c4) %>%
#   group_by(LANDING_YEAR) %>% 
#   summarize(lag_mean_avail_PSDN_GFDL = mean(lag_mean_avail_PSDN_GFDL, na.rm = TRUE),
#             lag_mean_avail_PSDN_IPSL = mean(lag_mean_avail_PSDN_IPSL, na.rm = TRUE),
#             lag_mean_avail_PSDN_HADL = mean(lag_mean_avail_PSDN_HADL, na.rm = TRUE),
#             lag_mean_avail_MSQD_GFDL = mean(lag_mean_avail_MSQD_GFDL, na.rm = TRUE),
#             lag_mean_avail_MSQD_IPSL = mean(lag_mean_avail_MSQD_IPSL, na.rm = TRUE),
#             lag_mean_avail_MSQD_HADL = mean(lag_mean_avail_MSQD_HADL, na.rm = TRUE)) %>%
#   mutate(LANDING_YEAR = LANDING_YEAR + 1,
#          group_all = 5) %>%
#   rename(set_year = LANDING_YEAR)
# 
# 
# ### Cluster 6 
# 
# ports_c6 <- c("CBA", "CLO", "CLW", "CWA", "NPS")
# SDM_year_c6 <- SDM_year %>% 
#   filter(PORT_AREA_CODE %in% ports_c4) %>%
#   group_by(LANDING_YEAR) %>% 
#   summarize(lag_mean_avail_PSDN_GFDL = mean(lag_mean_avail_PSDN_GFDL, na.rm = TRUE),
#             lag_mean_avail_PSDN_IPSL = mean(lag_mean_avail_PSDN_IPSL, na.rm = TRUE),
#             lag_mean_avail_PSDN_HADL = mean(lag_mean_avail_PSDN_HADL, na.rm = TRUE),
#             lag_mean_avail_MSQD_GFDL = mean(lag_mean_avail_MSQD_GFDL, na.rm = TRUE),
#             lag_mean_avail_MSQD_IPSL = mean(lag_mean_avail_MSQD_IPSL, na.rm = TRUE),
#             lag_mean_avail_MSQD_HADL = mean(lag_mean_avail_MSQD_HADL, na.rm = TRUE)) %>%
#   mutate(LANDING_YEAR = LANDING_YEAR + 1,
#          group_all = 6) %>%
#   rename(set_year = LANDING_YEAR)
# 
# ### Cluster 7 
# 
# ports_c7 <- c("LAA", "MNA", "MRA", "SBA", "SFA", "SDA")
# SDM_year_c7 <- SDM_year %>% 
#   filter(PORT_AREA_CODE %in% ports_c4) %>%
#   group_by(LANDING_YEAR) %>% 
#   summarize(lag_mean_avail_PSDN_GFDL = mean(lag_mean_avail_PSDN_GFDL, na.rm = TRUE),
#             lag_mean_avail_PSDN_IPSL = mean(lag_mean_avail_PSDN_IPSL, na.rm = TRUE),
#             lag_mean_avail_PSDN_HADL = mean(lag_mean_avail_PSDN_HADL, na.rm = TRUE),
#             lag_mean_avail_MSQD_GFDL = mean(lag_mean_avail_MSQD_GFDL, na.rm = TRUE),
#             lag_mean_avail_MSQD_IPSL = mean(lag_mean_avail_MSQD_IPSL, na.rm = TRUE),
#             lag_mean_avail_MSQD_HADL = mean(lag_mean_avail_MSQD_HADL, na.rm = TRUE)) %>%
#   mutate(LANDING_YEAR = LANDING_YEAR + 1,
#          group_all = 7) %>%
#   rename(set_year = LANDING_YEAR)
# 
# ### Merge 
# SDM_year_all <- rbind(SDM_year_c4,SDM_year_c5,SDM_year_c6,SDM_year_c7)
# future_data <- future_data %>%
#   left_join(SDM_year_all, by = c("set_year", "group_all")) %>%
#   mutate(PSDN_closure = 0)
# 
# 
# #---- Predict annual participation ----
# 
# # Define climate models
# climate_models <- c("GFDL", "IPSL", "HADL")
# 
# # Initialize list to store prediction results
# participation_predictions <- list()
# 
# for (cm in climate_models) {
#   
#   # Dynamically assign the correct predictors for each model
#   future_data_model <- future_data %>%
#     mutate(
#       lag_mean_avail_PSDN = .data[[paste0("lag_mean_avail_PSDN_", cm)]],
#       lag_mean_avail_MSQD = .data[[paste0("lag_mean_avail_MSQD_", cm)]]
#     )
#   
#   # Predict probability
#   future_data_model_pred <- predict(annual_model, newdata = future_data_model, type = "response")
#   future_data_model<- cbind(future_data_model, future_data_model_pred)
#   
#   # Predict binary participation
#   future_data_model <- future_data_model %>%
#     mutate(
#       pred_annual_participation = ifelse(Estimate > 0.5, 1, 0),
#       climate_model = cm
#     )
#   
#   # Store
#   participation_predictions[[cm]] <- future_data_model
# }
# 
# # Combine all into one data frame
# future_participation_all <- bind_rows(participation_predictions)
# 


### ALL VESSEL PARTICIPATE EVERYYEAR!


# ---- Calculate monthly participation ----

# Define month of the season per cluster to do predictions....

annual_data <- readRDS("C:/GitHub/EconAnalysis/FuturePredictions/Predictions/Data/annual_data_part.RDS") 

# Define future years
future_years <- 2090:2100

# Create a sequence of valid dates for those years
all_dates <- expand.grid(
  year = future_years,
  month = 1:12,
  day = 1:31
) %>%
  mutate(date = suppressWarnings(make_date(year, month, day))) %>%
  filter(!is.na(date)) %>%
  select(set_year = year, month, day)

# Combine with vessels
future_data <- expand.grid(
  VESSEL_NUM = unique(annual_data$VESSEL_NUM)
) %>%
  as_tibble() %>%
  crossing(all_dates)  
rm(list = c("all_dates"))

# Include clusters
clusters <- annual_data %>% select(group_all, VESSEL_NUM) %>% unique()
Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\PAM_Vessel_Groups.csv")
Tickets_clust <- merge(Tickets_exp_md, PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
rm(PAM_Vessel_Groups)
Tickets_clust <- Tickets_clust[!is.na(Tickets_clust$group_all), ]


# Include alternatives by clusters

google_dir <- "G:/Mi unidad/"

c4_alt = readRDS(paste0(google_dir, "Data/Anonymised data/part_model_c4.rds")) %>% 
  dplyr::select("selection") %>% 
  unique() %>% 
  mutate(group_all = 4)

c5_alt = readRDS(paste0(google_dir, "Data/Anonymised data/part_model_c5.rds")) %>% 
  dplyr::select("selection") %>% 
  unique() %>% 
  mutate(group_all = 5)

c6_alt = readRDS(paste0(google_dir, "Data/Anonymised data/part_model_c6.rds")) %>% 
  dplyr::select("selection") %>% 
  unique() %>% 
  mutate(group_all = 6)

c7_alt = readRDS(paste0(google_dir, "Data/Anonymised data/part_model_c7.rds")) %>% 
  dplyr::select("selection") %>% 
  unique() %>% 
  mutate(group_all = 7)




#########################################




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
