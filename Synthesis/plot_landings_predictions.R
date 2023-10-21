######################################################################
## Simulate aggregate landings -- (Merging DCM with Landings model) ##
######################################################################

rm(list = ls(all.names = TRUE)) 
gc()

## Read packages 
library(tidyr)
library(zoo)
library(data.table)
library(packcircles)
library(tidyverse)
library(data.table)
library(ggplot2)
library(viridis)
library(patchwork)
library(dplyr)
library(maps)
library(sf)
library(rnaturalearth)
library(ggspatial)
library(tidygeocoder)
library(ggrepel)
library(scatterpie)


## Load hist prediction
prediction_NANC_h <- readRDS(file = here::here("Landings", "Predictions", "prediction_NANC.rds")) %>% 
  filter(LANDING_YEAR >= 2013, LANDING_YEAR <=2017) %>%
  mutate(NANC_Landings = exp(Estimate.logNANCLandings)) %>%
  group_by(PORT_AREA_CODE, LANDING_YEAR) %>%
  summarise(NANC_Landings = sum(NANC_Landings, na.rm = TRUE)) %>%
  group_by(PORT_AREA_CODE) %>%
  summarise(NANC_Landings_hist = mean(NANC_Landings, na.rm = TRUE)) 

prediction_PSDN_h <- readRDS(file = here::here("Landings", "Predictions", "prediction_PSDN.rds")) %>% 
  filter(LANDING_YEAR >= 2013, LANDING_YEAR <=2017) %>%  
  mutate(PSDN_Landings = exp(Estimate.logPSDNLandings)) %>%
  group_by(PORT_AREA_CODE, LANDING_YEAR) %>%
  summarise(PSDN_Landings = sum(PSDN_Landings, na.rm = TRUE)) %>%
  group_by(PORT_AREA_CODE) %>%
  summarise(PSDN_Landings_hist = mean(PSDN_Landings, na.rm = TRUE)) 

prediction_MSQD_h <- readRDS(file = here::here("Landings", "Predictions", "prediction_MSQD.rds")) %>% 
  filter(LANDING_YEAR >= 2013, LANDING_YEAR <=2017) %>%  
  mutate(MSQD_Landings = exp(Estimate.logMSQDLandings)) %>%
  group_by(PORT_AREA_CODE, LANDING_YEAR) %>%
  summarise(MSQD_Landings = sum(MSQD_Landings, na.rm = TRUE)) %>%
  group_by(PORT_AREA_CODE) %>%
  summarise(MSQD_Landings_hist = mean(MSQD_Landings, na.rm = TRUE)) 


## Load predicted participation
prediction_PSDN <- readRDS("prediction_PSDN.rds") %>%  
  mutate(PSDN_Landings = exp(Estimate.logPSDNLandings)) %>%
  group_by(PORT_AREA_CODE, LANDING_YEAR) %>%
  summarise(PSDN_Landings = sum(PSDN_Landings, na.rm = TRUE)) %>%
  group_by(PORT_AREA_CODE) %>%
  summarise(PSDN_Landings_pred = mean(PSDN_Landings, na.rm = TRUE)) 

prediction_NANC <- readRDS("prediction_NANC.rds") %>%  
  mutate(NANC_Landings = exp(Estimate.logNANCLandings)) %>%
  group_by(PORT_AREA_CODE, LANDING_YEAR) %>%
  summarise(NANC_Landings = sum(NANC_Landings, na.rm = TRUE)) %>%
  group_by(PORT_AREA_CODE) %>%
  summarise(NANC_Landings_pred = mean(NANC_Landings, na.rm = TRUE)) 


# Final data
predictions <- merge(prediction_PSDN, prediction_NANC, by = "PORT_AREA_CODE", all = TRUE)
rm(prediction_NANC, prediction_PSDN)
predictions_h <- merge(prediction_PSDN_h, prediction_NANC_h, by = "PORT_AREA_CODE", all = TRUE)
predictions_h <- merge(predictions_h, prediction_MSQD_h, by = "PORT_AREA_CODE", all = TRUE)
rm(prediction_NANC_h, prediction_PSDN_h, prediction_MSQD_h)

historical_landings <- readRDS("historical_landings,rds")
historical_landings[historical_landings == 0] <- NA
dataset <- merge(predictions, historical_landings, by = "PORT_AREA_CODE", all.y = TRUE)
dataset <- merge(dataset, predictions_h, by = "PORT_AREA_CODE", all.y = TRUE)
rm(historical_landings, predictions, predictions_h)

#################
## Create plot ##
#################

## Get coordinates for port area, and add label
ports <- read_csv("Data/Ports/port_areas.csv") %>% 
  drop_na() %>%
  rename(PORT_AREA_CODE = port_group_code) %>%
  rename(lat_port = lat) %>%
  rename(lon_port = lon) %>%
  dplyr::select(-c(port_group_name))
dataset <- dataset %>% merge(ports, by = "PORT_AREA_CODE", all.x = TRUE)
rm(ports)
dataset <- dataset %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "NPS", "N. Puget Sound", NA)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "SPS", "S. Puget Sound", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "CWA", "Coastal Washington\nPorts", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "CLW", "Columbia River (WA)", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "CLO", "Columbia River (OR)", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "TLA", "Tillamook", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "NPA", "Newport", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "CBA", "Coos Bay", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "BRA", "Brookings", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "CCA", "Crescent City", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "ERA", "Eureka", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "BGA", "Fort Bragg", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "BDA", "Bodega Bay", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "SFA", "San Francisco", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "MNA", "Monterey", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "MRA", "Morro Bay", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "SBA", "Santa Barbara", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "LAA", "Los Angeles", port_group_name)) %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "SDA", "San Diego", port_group_name))

## Get Map 
world_map_data <- ne_countries(scale = "medium", returnclass = "sf")
state_map_data <- maps::map('state', fill = TRUE, plot = FALSE) %>% st_as_sf()
land_color <- c('antiquewhite1')
mytheme <- theme(panel.grid.major = element_line(color = '#cccccc',linetype = 'dashed', size = .3)
                 ,panel.background = element_rect(fill = 'aliceblue')
                 ,plot.title = element_text(size = 12)
                 ,plot.subtitle = element_text(size = 11)
                 ,axis.title = element_blank()
                 ,axis.text = element_text(size = 10))

group.colors <- c("PSDN_Landings" = "#fde725",
                  "NANC_Landings" = "#25858e", 
                  "MSQD_Landings" = "#440154")  

species_label <- as_labeller(c("MSQD_Landings" = "Market squid",
                               "NANC_Landings" = "Northern Anchovy",
                               "PSDN_Landings" = "Pacific sardine"))

#---------------------------
# ADD OFFSETS FOR PORT NAMES
#---------------------------
dataset <- dataset %>% 
  mutate(x_nudge = case_when(port_group_name == 'Newport' ~ 1,
                             port_group_name == 'Coos Bay' ~ 1,
                             port_group_name == 'Monterey' ~ -4,
                             port_group_name == 'Bodega Bay' ~ -2.5,
                             port_group_name == 'Santa Barbara' ~ -3,
                             port_group_name == 'Los Angeles' ~ -3,
                             port_group_name == 'Morro Bay' ~ -5,
                             port_group_name == 'San Francisco' ~ -3,
                             TRUE ~ -1),
         y_nudge = case_when(port_group_name == 'San Diego' ~ -.5,
                             port_group_name == 'Bodega Bay' ~ 0.5,
                             port_group_name == 'Los Angeles' ~ -1,
                             TRUE ~ 0)) 


##########################################################################
dataset1 <- dataset %>% 
  select(c("PORT_AREA_CODE", "PSDN_Landings_hist", "NANC_Landings_hist",
           "lat_port", "lon_port", "port_group_name", "x_nudge", "y_nudge")) %>%
  replace(is.na(.), 0) %>%
  rename(PSDN_Landings = PSDN_Landings_hist) %>%
  rename(NANC_Landings = NANC_Landings_hist)
max_obs = ncol(dataset1) - 5
dataset1 <- dataset1 %>%
  mutate(sum_part = rowSums(.[2:max_obs])) %>%
  filter(lat_port <= 42)

coef = 1/1500
gg1 <- ggplot() +
  geom_sf(data = world_map_data, fill = land_color, size = .4) +
  geom_sf(data = state_map_data, fill = NA, size = .4) +
  coord_sf(xlim = c(-133, -116), ylim = c(32,42)) + mytheme +
  geom_scatterpie(aes(x=lon_port, y=lat_port, group = PORT_AREA_CODE, r = sum_part*coef), 
                  data = dataset1, legend_name = "Landings",
                  cols = colnames(dataset1[,c(2:max_obs)])) +
  theme(legend.position = "left") + 
  geom_scatterpie_legend(dataset1$sum_part*coef, x=-131, y=40, 
                         labeller = scales::label_number(scale =1/coef, big.mark = ",")) +
  geom_text_repel(aes(x=lon_port, y=lat_port, group = PORT_AREA_CODE, label = port_group_name), 
                  data = dataset1, segment.color = "#333333", size = 3,
                  nudge_x = dataset1$x_nudge,
                  nudge_y = dataset1$y_nudge) + 
  ggtitle('(a) Historical SDM') + 
  scale_fill_manual(values = group.colors, labels=species_label)



#-------------------------------------------------------------------

dataset2 <- dataset %>% 
  select(c("PORT_AREA_CODE", "PSDN_Landings_pred", "NANC_Landings_pred",
           "lat_port", "lon_port", "port_group_name", "x_nudge", "y_nudge")) %>%
  replace(is.na(.), 0) %>%
  rename(PSDN_Landings = PSDN_Landings_pred) %>%
  rename(NANC_Landings = NANC_Landings_pred)
max_obs = ncol(dataset2) - 5
dataset2 <- dataset2 %>%
  mutate(sum_part = rowSums(.[2:max_obs])) %>%
  filter(lat_port <= 42)

gg2 <- ggplot() +
  geom_sf(data = world_map_data, fill = land_color, size = .4) +
  geom_sf(data = state_map_data, fill = NA, size = .4) +
  coord_sf(xlim = c(-133, -116), ylim = c(32,42)) + mytheme +
  geom_scatterpie(aes(x=lon_port, y=lat_port, group = PORT_AREA_CODE, r = sum_part*coef), 
                  data = dataset2, legend_name = "Landings",
                  cols = colnames(dataset2[,c(2:max_obs)])) +
  theme(legend.position = "none") + 
  geom_text_repel(aes(x=lon_port, y=lat_port, group = PORT_AREA_CODE, label = port_group_name), 
                  data = dataset2, segment.color = "#333333", size = 3,
                  nudge_x = dataset2$x_nudge,
                  nudge_y = dataset2$y_nudge) + 
  ggtitle('(b) Squid SDM = 0') + 
  scale_fill_manual(values = group.colors, labels=species_label)

#################################################################
## Create Map! ##
gg1 + gg2








