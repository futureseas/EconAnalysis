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

#google_dir <- "G:/Mi unidad/"
google_dir <- "H:/My Drive/"

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


######################################################

## PREDICT FOR EACH CLUSTER AND CLIMATE MODEL

library(apollo)
apollo_initialise()
setwd("C:/GitHub/EconAnalysis/FuturePredictions/")

groups <- c(4) ### Agregar mas clusters despues
scenarios <- c("GFDL", "HADL", "IPSL")

for (grp in groups) {
  # Load model
  model_path <- paste0("C:/GitHub/EconAnalysis/FuturePredictions/output/NL_participation_model_c", grp, "_for_predictions_model.rds")
  model <- readRDS(model_path)
  
  # Filter future data
  future_data_group <- future_data %>%
    filter(group_all == grp) %>%
    mutate(
      date = as.Date(paste(set_year, month, day, sep = "-")),
      PORT_AREA_CODE = toupper(substr(selection, 1, 3)),
      SPECIES = toupper(substr(selection, 5, 8)),
      weekend = as.integer(weekdays(date) %in% c("Saturday", "Sunday")),
      d_d = 0,
      d_cd = 0
    )
  
  historical_data <- readRDS(paste0(google_dir, paste0("Data/Anonymised data/part_model_c", paste0(grp, ".rds")))) %>%
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
  
  
    # Merge covariates (reuse historical data from c4 for now)
  future_data_group <- future_data_group %>%
    left_join(mean_price_by_specie, by = c("SPECIES")) %>%
    left_join(wind_by_port_month, by = c("PORT_AREA_CODE", "month" = "set_month")) %>%
    left_join(dist_to_area_by_specie_port, by = c("SPECIES", "PORT_AREA_CODE")) %>%
    left_join(unem_by_port, by = c("PORT_AREA_CODE")) %>%
    left_join(dist_to_cog_by_port_vessel, by = c("PORT_AREA_CODE", "VESSEL_NUM" = "fished_vessel_anon")) %>%
    mutate(
      mean_price = ifelse(selection == "No-Participation", 0, mean_price),
      mean_price_3 = mean_price,
      wind_max_220_mh = ifelse(selection == "No-Participation", 0, wind_max_220_mh),
      dist_port_to_catch_area_zero = ifelse(selection == "No-Participation", 0, dist_port_to_catch_area_zero),
      unem_rate = ifelse(selection == "No-Participation", 0, unem_rate),
      dist_to_cog = ifelse(selection == "No-Participation", 0, dist_to_cog)
    ) %>%
    dplyr::select(-c(PORT_AREA_CODE, SPECIES))
  
  future_data_group$selection <- tolower(gsub("-", "_", future_data_group$selection))
  
  for (scenario in scenarios) {
    # Set scenario-specific availability
    avail_var <- paste0("mean_avail_", scenario)
    
    database <- future_data_group %>%
      mutate(mean_avail = .data[[avail_var]]) %>%
      pivot_wider(
        names_from = selection,
        values_from = c(mean_avail, mean_avail_GFDL, mean_avail_IPSL, mean_avail_HADL,
                        mean_price, mean_price_3, wind_max_220_mh, dist_to_cog,
                        dist_port_to_catch_area_zero, unem_rate, d_d, d_cd)
      ) %>%
      dplyr::mutate(unem_rate = 
                      rowMeans(dplyr::select(., starts_with("unem_rate_") & !ends_with("nopart")), 
                               na.rm = TRUE)) %>%
      rename(fished_vessel_anon = VESSEL_NUM) %>%
      mutate(btnaclosure = 0, psdnclosure = 0)
    
    apollo_control = list(
      modelName       = paste0("NL_participation_model_c", grp, "_for_predictions"),
      indivID         = "fished_vessel_anon",
      outputDirectory = "output",
      panelData       = TRUE,
      nCores          = 14,
      workInLogs      = TRUE
    )
    
    database$choice <- rep(1, nrow(database))
    
    apollo_beta <- model$estimate

    source(paste0("C:/GitHub/EconAnalysis/FuturePredictions/Predictions/Models/apollo_probabilities_c", paste0(grp, ".R")))
    
    apollo_inputs <- apollo_validateInputs()
    
    prediction <- apollo_prediction(
      model = model,
      apollo_probabilities = apollo_probabilities,
      apollo_inputs = apollo_inputs
    )
    
    pred_df <- as.data.frame(prediction)
    no_part_col <- "no_participation"
    pred_df <- pred_df[, names(pred_df) != "chosen"]
    
    alt_names <- setdiff(names(pred_df)[3:ncol(pred_df)], c(no_part_col))
    pred_df$predicted_choice <- NA
    pred_df$predicted_choice[pred_df[[no_part_col]] > 0.5] <- no_part_col
    
    remaining_rows <- which(pred_df[[no_part_col]] <= 0.5)
    if (length(remaining_rows) > 0) {
      probs <- pred_df[remaining_rows, alt_names, drop = FALSE]
      max_alts <- alt_names[max.col(probs, ties.method = "first")]
      pred_df$predicted_choice[remaining_rows] <- max_alts
    }
    
    pred_df <- pred_df %>% dplyr::select(c(predicted_choice))
    
    # Ensure both have a row identifier
    database$row_id <- seq_len(nrow(database))
    pred_df$row_id <- seq_len(nrow(pred_df))
    
    # Merge
    pred_df <- dplyr::left_join(pred_df, database, by = "row_id")
    
    # Clean up
    pred_df$row_id <- NULL
    
    # Save to object name
    assign(paste0("pred_c", grp, "_", scenario), pred_df)
    
  }
}

### Monthly participation 
pred_c4_GFDL <- pred_c4_GFDL %>% mutate(scenario == "GFDL") %>% 
  rename(predicted_choice_GFDL = predicted_choice) 
pred_c4_IPSL <- pred_c4_IPSL %>% mutate(scenario == "IPSL") %>% 
  rename(predicted_choice_IPSL = predicted_choice)  %>% 
  dplyr::select(c(predicted_choice_IPSL))
pred_c4_HADL <- pred_c4_HADL  %>% 
  rename(predicted_choice_HADL = predicted_choice) %>%  
  dplyr::select(c(predicted_choice_HADL))

pred_c4 <- cbind(pred_c4_GFDL, pred_c4_IPSL)
pred_c4 <- cbind(pred_c4, pred_c4_HADL)



### Monthly participation data to estimate landings ####
monthly_part_c4_GFDL <- pred_c4 %>% 
  filter(predicted_choice_GFDL != "no_participation") %>%
  mutate(n_count = 1,
         group_all = 4) %>%
  group_by(fished_vessel_anon, month, set_year, predicted_choice_GFDL) %>%
  summarize(n_days_part = sum(n_count)) %>% 
  filter(n_days_part > 9) %>%
  mutate(
    PORT_AREA_CODE = toupper(substr(predicted_choice_GFDL, 1, 3)),
    SPECIES = toupper(substr(predicted_choice_GFDL, 5, 8)))

 

# --- Predict Monthly Landings (MSQD, PSDN, NANC) ---

fit_MSQD <- readRDS("Landings/Estimations/fit_qMSQD_boost_GAM.rds")
fit_NANC <- readRDS("Landings/Estimations/fit_qNANC_boost_GAM.rds")

# Predict MSQD
monthly_part_c4_GFDL_MSQD <- monthly_part_c4_GFDL %>% 
  filter(SPECIES == "MSQD")

monthly_part_c4_GFDL_MSQD$pred_log_MSQD <- 
  predict(fit_MSQD, newdata = monthly_part_c4_GFDL_MSQD, resp = "logMSQDLandings") %>% 
  as.data.frame() %>% 
  pull(Estimate)

  FALTA AGREGAR DATOS!!!
  

# 
# # Optional: convert from log-scale
# monthly_data <- monthly_data %>%
#   mutate(
#     MSQD_Landings = exp(pred_log_MSQD),
#     NANC_Landings = exp(pred_log_NANC)
#     # Add PSDN_Landings when available
#   )
# 

  CALCULAR LANDINGS POR AÑO ESPECIE!!!
  

