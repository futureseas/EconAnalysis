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