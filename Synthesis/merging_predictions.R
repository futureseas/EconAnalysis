######################################################################
## Simulate aggregate landings -- (Merging DCM with Landings model) ##
######################################################################

rm(list = ls(all.names = TRUE)) 
gc()


## Read packages 
library("dplyr") 
library("tidyr")
library("zoo")
library('ggplot2')


## Load historical data
historical_data <- read.csv(file ="C:\\Data\\PacFIN data\\dataset_estimation.csv") %>%
  select(c("VESSEL_NUM", "LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE",
           "MSQD_Price_z", "PSDN_Price_z", "NANC_Price_z", "Price.Fishmeal.AFI_z",
           "MSQD_SPAWN_SDM_90", "NANC_SDM_20", "Length_z", "PSDN_SDM_60", 
           "PSDN.Open", "MSQD.Open","group_all", "diesel.price.AFI_z")) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::mutate(WA.Restriction = ifelse(LANDING_MONTH <= 3, 1, 0)) %>%
  mutate(cluster_port = paste(group_all, PORT_AREA_CODE, sep = "-", collapse = NULL)) %>%
  mutate(diesel.price.AFI_z = -1*diesel.price.AFI_z) %>%
  drop_na()
historical_data$port_ID            <- factor(historical_data$PORT_AREA_CODE)


## Average by vessel
vessel_data <- historical_data %>%  select(c("VESSEL_NUM", "group_all", "Length_z")) %>% unique()

## Average by port area per month
area_data <- historical_data %>% group_by(LANDING_YEAR, LANDING_MONTH, PORT_AREA_CODE, port_ID) %>%
  summarize(MSQD_Price_z = mean(MSQD_Price_z),
            PSDN_Price_z = mean(PSDN_Price_z),
            NANC_Price_z = mean(NANC_Price_z),
            Price.Fishmeal.AFI_z = mean(Price.Fishmeal.AFI_z),
            MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90),
            NANC_SDM_20 = mean(NANC_SDM_20),
            PSDN_SDM_60 = mean(PSDN_SDM_60), 
            PSDN.Open = mean(PSDN.Open),
            diesel.price.AFI_z = mean(diesel.price.AFI_z),
            PSDN.Total.Closure = mean(PSDN.Total.Closure),
            WA.Restriction = mean(WA.Restriction),
            MSQD.Open = mean(MSQD.Open))
            

## Load predicted participation
part_pred <- read.csv(here::here("Participation", "R", "monthly_participation_pred.csv")) %>%
  merge(vessel_data, by = "VESSEL_NUM", all.x = TRUE, all.y = FALSE) %>%
  merge(area_data, by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE, all.y = FALSE)
historical_data$part_pred <- factor(historical_data$cluster_port)


# Load landings estimations
fit_qMSQD <- readRDS(here::here("Landings", "Estimations", "fit_qMSQD.RDS"))
fit_qPSDN <- readRDS(here::here("Landings", "Estimations", "fit_qPSDN.RDS"))
fit_qNANC <- readRDS(here::here("Landings", "Estimations", "fit_qNANC.RDS"))

#############
## Predict ##
#############
set.seed(123)

# ## Squid landings
# part_pred_MSQD <- part_pred %>% filter(PACFIN_SPECIES_CODE == "MSQD")
# prediction_MSQD <- predict(fit_qMSQD, newdata = part_pred_MSQD)


## Sardine landings
part_pred_PSDN <- part_pred %>% filter(PACFIN_SPECIES_CODE == "PSDN")
prediction_PSDN <- cbind(predict(fit_qPSDN, newdata = part_pred_PSDN, allow_new_levels = TRUE), part_pred_PSDN)


## Anchovy landings
part_pred_NANC <- part_pred %>% filter(PACFIN_SPECIES_CODE == "NANC")
prediction_NANC <- cbind(predict(fit_qNANC, newdata = part_pred_NANC, allow_new_levels = TRUE), part_pred_NANC)




