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


# ---- Calculate monthly participation ----

# Define month of the season per cluster to do predictions....

annual_data <- readRDS("C:/GitHub/EconAnalysis/FuturePredictions/Predictions/Data/annual_data_part.RDS") 

# Define future years
future_years <- 2080:2090

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
rm(annual_data)
future_data <- left_join(future_data, clusters, by = ("VESSEL_NUM"))

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

alternatives_cluster <- rbind(c4_alt, c5_alt, c6_alt, c7_alt)
future_data <- left_join(future_data, alternatives_cluster, by = ("group_all"))



# Add SDMs (GAM Boost) -- Calculate 30-day moving average (excluding current day) for each port

library(dplyr)
library(zoo)

process_sdm_data <- function(file_path, sdm_prefix, species_code) {
  read.csv(file_path) %>%
    mutate(date = as.Date(date)) %>%
    group_by(PORT_AREA_CODE) %>%
    arrange(date) %>%
    mutate(across(starts_with(sdm_prefix), 
                  ~rollapply(.x, width = 30, FUN = mean, align = "right", fill = NA), 
                  .names = "MA_{.col}")) %>%
    mutate(across(starts_with(paste0("MA_", sdm_prefix)), 
                  lag, .names = "lag_{.col}")) %>%
    ungroup() %>%
    mutate(date = as.Date(paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY, sep = "-"))) %>%
    select(date, PORT_AREA_CODE,
           starts_with(paste0("lag_MA_", sdm_prefix))) %>%
    rename_with(
      ~gsub(paste0("lag_MA_", sdm_prefix), paste0(species_code, "_SDM_30day_MA_"), .x),
      starts_with(paste0("lag_MA_", sdm_prefix))
    )
}

MSQD_sdm_data_MA <- process_sdm_data(
  file_path = "C:/GitHub/EconAnalysis/FuturePredictions/SDM/Future/MSQD_FutureSDM_port_day.csv",
  sdm_prefix = "SDM_90_",
  species_code = "MSQD"
)

PSDN_sdm_data_MA <- process_sdm_data(
  file_path = "C:/GitHub/EconAnalysis/FuturePredictions/SDM/Future/PSDN_FutureSDM_port_day.csv",
  sdm_prefix = "SDM_60_",
  species_code = "PSDN"
)

NANC_sdm_data_MA <- process_sdm_data(
  file_path = "C:/GitHub/EconAnalysis/FuturePredictions/SDM/Future/NANC_FutureSDM_port_day_merged.csv",
  sdm_prefix = "SDM_60_",
  species_code = "NANC"
)

CMCK_sdm_data_MA <- process_sdm_data(
  file_path = "C:/GitHub/EconAnalysis/FuturePredictions/SDM/Future/CMCK_FutureSDM_port_day.csv",
  sdm_prefix = "SDM_60_",
  species_code = "CMCK"
)

ALBC_sdm_data_MA <- process_sdm_data(
  file_path = "C:/GitHub/EconAnalysis/FuturePredictions/SDM/Future/ALBC_FutureSDM_port_day.csv",
  sdm_prefix = "SDM_90_",
  species_code = "ALBC"
)

JMCK_sdm_data_MA <- process_sdm_data(
  file_path = "C:/GitHub/EconAnalysis/FuturePredictions/SDM/Future/JMCK_FutureSDM_port_day.csv",
  sdm_prefix = "SDM_60_",
  species_code = "JMCK"
)

# Index average for YTNA BTNA and DCRB

library(lubridate)

process_monthly_avail <- function(file_path, cohort) {
  readRDS(file_path) %>%
    select(selection, mean_avail, set_date) %>%
    distinct() %>%
    mutate(
      month = month(set_date),
      cohort = cohort
    ) %>%
    group_by(month, selection, cohort) %>%
    summarize(mean_avail = mean(mean_avail, na.rm = TRUE), .groups = "drop")
}

avail_c4 <- process_monthly_avail(paste0(google_dir, "Data/Anonymised data/part_model_c4.rds"), "c4")
avail_c5 <- process_monthly_avail(paste0(google_dir, "Data/Anonymised data/part_model_c5.rds"), "c5")
avail_c6 <- process_monthly_avail(paste0(google_dir, "Data/Anonymised data/part_model_c6.rds"), "c6")
avail_c7 <- process_monthly_avail(paste0(google_dir, "Data/Anonymised data/part_model_c7.rds"), "c7")

avail_all <- bind_rows(avail_c4, avail_c5, avail_c6, avail_c7) %>% 
  group_by(month, selection) %>%
  summarize(mean_avail_GFDL = mean(mean_avail, na.rm = TRUE),
            mean_avail_IPSL = mean(mean_avail, na.rm = TRUE),
            mean_avail_HADL = mean(mean_avail, na.rm = TRUE), .groups = "drop")


# Merge data to create mean_avail

future_data2 <- future_data %>%
  left_join(avail_all, by = c("month", "selection")) %>%
  mutate(date = as.Date(paste(set_year, month, day, sep = "-")),
         PORT_AREA_CODE = toupper(substr(selection, 1, 3)),
         SPECIES = toupper(substr(selection, 5, 8)))

future_data3 <- future_data2 %>%
  left_join(MSQD_sdm_data_MA, 
            by = c("date" = "date", "PORT_AREA_CODE" = "PORT_AREA_CODE")) %>%
  left_join(PSDN_sdm_data_MA, 
            by = c("date" = "date", "PORT_AREA_CODE" = "PORT_AREA_CODE")) %>%
  left_join(NANC_sdm_data_MA, 
            by = c("date" = "date", "PORT_AREA_CODE" = "PORT_AREA_CODE")) %>%
  left_join(CMCK_sdm_data_MA, 
            by = c("date" = "date", "PORT_AREA_CODE" = "PORT_AREA_CODE")) %>%
  left_join(JMCK_sdm_data_MA, 
            by = c("date" = "date", "PORT_AREA_CODE" = "PORT_AREA_CODE")) %>%
  left_join(ALBC_sdm_data_MA, 
            by = c("date" = "date", "PORT_AREA_CODE" = "PORT_AREA_CODE"))


future_data <- future_data3 %>%
  dplyr::mutate(mean_avail_GFDL = case_when(
    SPECIES == "MSQD" ~ MSQD_SDM_30day_MA_GFDL,
    SPECIES == "NANC" ~ NANC_SDM_30day_MA_GFDL,
    SPECIES == "CMCK" ~ CMCK_SDM_30day_MA_GFDL,
    SPECIES == "PSDN" ~ PSDN_SDM_30day_MA_GFDL,
    SPECIES == "ALBC" ~ PSDN_SDM_30day_MA_GFDL,
    SPECIES == "JMCK" ~ PSDN_SDM_30day_MA_GFDL,
    TRUE ~ mean_avail_GFDL  # Keep original value for other cases
  )) %>%
  dplyr::mutate(mean_avail_IPSL = case_when(
    SPECIES == "MSQD" ~ MSQD_SDM_30day_MA_IPSL,
    SPECIES == "NANC" ~ NANC_SDM_30day_MA_IPSL,
    SPECIES == "CMCK" ~ CMCK_SDM_30day_MA_IPSL,
    SPECIES == "PSDN" ~ PSDN_SDM_30day_MA_IPSL,
    SPECIES == "ALBC" ~ PSDN_SDM_30day_MA_IPSL,
    SPECIES == "JMCK" ~ PSDN_SDM_30day_MA_IPSL,
    TRUE ~ mean_avail_IPSL  # Keep original value for other cases
  )) %>%
  dplyr::mutate(mean_avail_HADL = case_when(
    SPECIES == "MSQD" ~ MSQD_SDM_30day_MA_HADL,
    SPECIES == "NANC" ~ NANC_SDM_30day_MA_HADL,
    SPECIES == "CMCK" ~ CMCK_SDM_30day_MA_HADL,
    SPECIES == "PSDN" ~ PSDN_SDM_30day_MA_HADL,
    SPECIES == "ALBC" ~ PSDN_SDM_30day_MA_HADL,
    SPECIES == "JMCK" ~ PSDN_SDM_30day_MA_HADL,
    TRUE ~ mean_avail_HADL # Keep original value for other cases
  )) %>%
  dplyr::select(-any_of(c("SPECIES", "PORT_AREA_CODE", 
                          "MSQD_SDM_30day_MA_GFDL", 
                          "CMCK_SDM_30day_MA_GFDL", 
                          "NANC_SDM_30day_MA_GFDL", 
                          "PSDN_SDM_30day_MA_GFDL", 
                          "ALBC_SDM_30day_MA_GFDL", 
                          "JMCK_SDM_30day_MA_GFDL",
                          "MSQD_SDM_30day_MA_IPSL", 
                          "CMCK_SDM_30day_MA_IPSL", 
                          "NANC_SDM_30day_MA_IPSL", 
                          "PSDN_SDM_30day_MA_IPSL", 
                          "ALBC_SDM_30day_MA_IPSL", 
                          "JMCK_SDM_30day_MA_IPSL",
                          "MSQD_SDM_30day_MA_HADL", 
                          "CMCK_SDM_30day_MA_HADL", 
                          "NANC_SDM_30day_MA_HADL", 
                          "PSDN_SDM_30day_MA_HADL", 
                          "ALBC_SDM_30day_MA_HADL", 
                          "JMCK_SDM_30day_MA_HADL",
                          "date"))) 

rm(list = c("avail_all", "avail_c4", "avail_c5", "avail_c6", "avail_c7", "future_data2",
            "future_data3", "clusters", "c4_alt", "c5_alt", "c6_alt", "c7_alt", 
            "MSQD_sdm_data_MA", "PSDN_sdm_data_MA", "NANC_sdm_data_MA", "CMCK_sdm_data_MA", 
            "JMCK_sdm_data_MA", "ALBC_sdm_data_MA")) 


# Cargar modelos nested logit

model_c4 <- readRDS("C:/GitHub/EconAnalysis/FuturePredictions/output/NL_participation_model_c4_for_predictions_model.rds")
model_c5 <- readRDS("C:/GitHub/EconAnalysis/FuturePredictions/output/NL_participation_model_c5_for_predictions_model.rds")
model_c6 <- readRDS("C:/GitHub/EconAnalysis/FuturePredictions/output/NL_participation_model_c6_for_predictions_model.rds")
model_c7 <- readRDS("C:/GitHub/EconAnalysis/FuturePredictions/output/NL_participation_model_c7_for_predictions_model.rds")


# Add other covariates

future_data_c4 <- future_data %>% 
  filter(group_all == 4) %>%
  mutate(
    date = as.Date(paste(set_year, month, day, sep = "-")),
    PORT_AREA_CODE = toupper(substr(selection, 1, 3)),
    SPECIES = toupper(substr(selection, 5, 8)),
    weekend = as.integer(weekdays(date) %in% c("Saturday", "Sunday")),
    d_d = 0
  )

historical_data <- readRDS(paste0(google_dir, "Data/Anonymised data/part_model_c4.rds")) %>%
  filter(selection != "No-Participation") %>%
  mutate(
    PORT_AREA_CODE = toupper(substr(selection, 1, 3)),
    SPECIES = toupper(substr(selection, 5, 8)))

mean_price_by_specie <- historical_data %>%
  group_by(SPECIES) %>%
  summarise(mean_price = mean(mean_price, na.rm = TRUE))

wind_by_port_month <- historical_data %>%
  group_by(PORT_AREA_CODE, set_month) %>%
  summarise(wind_max_220_mh = mean(wind_max_220_mh, na.rm = TRUE))

dist_to_area_by_specie_port <- historical_data %>%
  group_by(SPECIES, PORT_AREA_CODE) %>%
  summarise(dist_port_to_catch_area_zero = mean(dist_port_to_catch_area_zero, na.rm = TRUE))

unem_by_port <- historical_data %>%
  group_by(PORT_AREA_CODE) %>%
  summarise(unem_rate = mean(unem_rate, na.rm = TRUE))

dist_to_cog_by_port_vessel <- historical_data %>%
  mutate(fished_vessel_anon = group_all * 100 + fished_vessel_anon ) %>%
  group_by(PORT_AREA_CODE, fished_vessel_anon) %>%
  summarise(dist_to_cog = mean(dist_to_cog, na.rm = TRUE))


future_data_c4 <- future_data_c4 %>%
  left_join(mean_price_by_specie, by = c("SPECIES")) %>%
  left_join(wind_by_port_month, by = c("PORT_AREA_CODE", "month" = "set_month")) %>%
  left_join(dist_to_area_by_specie_port, by = c("SPECIES", "PORT_AREA_CODE")) %>%
  left_join(unem_by_port, by = c("PORT_AREA_CODE")) %>%
  left_join(dist_to_cog_by_port_vessel, by = c("PORT_AREA_CODE", "VESSEL_NUM" = "fished_vessel_anon")) %>%
  mutate(
    mean_price = ifelse(selection == "No-Participation", 0, mean_price),
    wind_max_220_mh = ifelse(selection == "No-Participation", 0, wind_max_220_mh),
    dist_port_to_catch_area_zero = ifelse(selection == "No-Participation", 0, dist_port_to_catch_area_zero),
    unem_rate = ifelse(selection == "No-Participation", 0, unem_rate),
    dist_to_cog = ifelse(selection == "No-Participation", 0, dist_to_cog)
  ) %>%
  dplyr::select(-c(PORT_AREA_CODE, SPECIES))

future_data_c4$selection <- tolower(gsub("-", "_", future_data_c4$selection))

database <- future_data_c4 %>%
  pivot_wider(
    names_from = selection,                  # Unique values for alternatives
    values_from = c(mean_avail_GFDL, mean_avail_IPSL, mean_avail_HADL, mean_price, wind_max_220_mh, dist_to_cog, 
                    dist_port_to_catch_area_zero, unem_rate, d_d)) %>%
  dplyr::mutate(unem_rate = unem_rate_sfa_nanc)


#########################################

## Predict!!





#########################################


# Assume we will read in or compute daily participation with SDM + economics per cluster
# Expected output: daily_participation_df with columns VESSEL_NUM, DATE, cluster, participates_today


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


#### ANNUAL LANDING BY PORTS BY DIFFERENT CLIMATE MODELS VS HISTORICAL ANUAL LANDINGS
