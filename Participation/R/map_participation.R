########################################
##### Work to do: maybe show map picture [ like WK4 figure] of  Participation x Landing results ,
##### (a dummy example) –  and this allows you to cite your Landings paper. WORKING ON THIS!

rm(list = ls(all.names = TRUE)) 
gc()

# libraries
library(packcircles)
library(data.table)
library(ggplot2)
library(viridis)
library(patchwork)
library(dplyr)
library(maps)
library(sf)
library(rnaturalearth)
library(tidyverse)
library(ggspatial)
library(tidygeocoder)
library(ggrepel)
library(scatterpie)

setwd("D:/GitHub/EconAnalysis/")
## Load datase (from Stata work on predicting shares)
Simulated_shares <- read.csv("D:/GitHub/EconAnalysis/Participation/R/Simulated_shares_fig.csv") %>%
  complete(selection, set_year) %>% mutate(perc = ifelse(is.na(perc), 0, perc),
                                           perc3 = ifelse(is.na(perc3), 0, perc3)) %>%
  mutate(port = ifelse(selection != "No-Participation",  substr(selection, 1, 3), NA)) %>%  
  group_by(selection, port) %>%
  summarize(perc = mean(perc, na.rm = TRUE), perc3 = mean(perc3, na.rm = TRUE)) %>% 
  ungroup() %>%
  drop_na()


## Get coordinates for port area
ports <- read_csv("Data/Ports/port_areas.csv") %>% 
  drop_na() %>%
  rename(port = port_group_code) %>%
  rename(lat_port = lat) %>%
  rename(lon_port = lon) %>%
  dplyr::select(-c(port_group_name))

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

Simulated_shares <- Simulated_shares %>%
  mutate(port_group_name = ifelse(port == "NPS", "N. Puget Sound", NA)) %>%
  mutate(port_group_name = ifelse(port == "SPS", "S. Puget Sound", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "CWA", "Coastal Washington\nPorts", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "CLW", "Columbia River (WA)", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "CLO", "Columbia River (OR)", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "TLA", "Tillamook", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "NPA", "Newport", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "CBA", "Coos Bay", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "BRA", "Brookings", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "CCA", "Crescent City", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "ERA", "Eureka", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "BGA", "Fort Bragg", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "BDA", "Bodega Bay", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "SFA", "San Francisco", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "MNA", "Monterey", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "MRA", "Morro Bay", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "SBA", "Santa Barbara", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "LAA", "Los Angeles", port_group_name)) %>%
  mutate(port_group_name = ifelse(port == "SDA", "San Diego", port_group_name))


group.colors <- c("BTNA" = "#38588c",
                  "CHNK" = "#c2df23", 
                  "CMCK" = "#86d549",
                  "DCRB" = "#52c569", 
                  "JMCK" = "#2ab07f", 
                  "NANC" = "#25858e", 
                  "PBNT" = "#2d708e",
                  "PSDN" = "#fde725",
                  "STNA" = "#433e85",
                  "UDAB" = "#482173",
                  "YTNA" = "#1e9b8a",
                  "MSQD" = "#440154")  


species_label <- as_labeller(c("BTNA" = "Bluefin Tuna",
                               "CHNK" = "Chinook Salmon",
                               "CMCK" = "Chub Mackerel",
                               "DCRB" = "Dungeness Crab",
                               "JMCK" = "Jack Mackerel",
                               "MSQD" = "Market squid",
                               "NANC" = "Northern Anchovy",
                               "PBNT" = "Pacific Bonito",
                               "PSDN" = "Pacific sardine",
                               "STNA" = "Skipjack Tuna",
                               "UDAB" = "Sanddabs",
                               "YTNA" = "Yellowfin Tuna"))

#---------------------------
# ADD OFFSETS FOR PORT NAMES
#---------------------------
Simulated_shares <- Simulated_shares %>% 
  mutate(x_nudge = case_when(port_group_name == 'Newport' ~ 1,
                             port_group_name == 'Coos Bay' ~ 1,
                             port_group_name == 'Monterey' ~ -2,
                             port_group_name == 'Bodega Bay' ~ -2.5,
                             port_group_name == 'Santa Barbara' ~ -3,
                             port_group_name == 'Los Angeles' ~ -3,
                             TRUE ~ -1),
         y_nudge = case_when(port_group_name == 'San Diego' ~ -.5,
                             port_group_name == 'Bodega Bay' ~ 0.5,
                             port_group_name == 'Los Angeles' ~ -1,
                             TRUE ~ 0)) 


##########################################################################
Simulated_shares1 <- Simulated_shares %>% mutate(perc = perc)  %>% 
  mutate(Species = ifelse(selection != "No-Participation",  substr(selection, 5, 8), NA)) %>% 
  select(-c('perc3', 'selection')) %>%
  spread(Species, perc) %>%
  replace(is.na(.), 0) 

max_obs = ncol(Simulated_shares1)
Simulated_shares1 <- Simulated_shares1 %>%
  mutate(sum_part = rowSums(.[5:max_obs])) %>% 
  merge(ports, by = c("port"), all.x = TRUE)
max_obs = ncol(Simulated_shares1) - 3
coef = 4.5

gg1 <- ggplot() +
  geom_sf(data = world_map_data, fill = land_color, size = .4) +
  geom_sf(data = state_map_data, fill = NA, size = .4) +
  coord_sf(xlim = c(-133, -116), ylim = c(32,46)) + mytheme +
  geom_scatterpie(aes(x=lon_port, y=lat_port, group = port, r = sum_part*coef), 
                  data = Simulated_shares1, legend_name = "Species",
                  cols = colnames(Simulated_shares1[,c(5:max_obs)])) +
  theme(legend.position = "left") + 
  geom_scatterpie_legend(Simulated_shares1$sum_part*coef, x=-131, y=44, 
                         labeller = scales::label_percent(scale = 100/coef)) +
  geom_text_repel(aes(x=lon_port, y=lat_port, group = port, label = port_group_name), 
                  data = Simulated_shares1, segment.color = "#333333", size = 3,
                  nudge_x = Simulated_shares1$x_nudge,
                  nudge_y = Simulated_shares1$y_nudge) + 
  ggtitle('(a) Historical SDM') + 
  scale_fill_manual(values = group.colors, labels=species_label)
  


#-------------------------------------------------------------------

Simulated_shares1 <- Simulated_shares %>% mutate(perc = perc3) %>%
  mutate(port = ifelse(selection != "No-Participation",  substr(selection, 1, 3), NA)) %>%  
  drop_na() %>% 
  mutate(Species = ifelse(selection != "No-Participation",  substr(selection, 5, 8), NA)) %>% 
  select(-c('perc3', 'selection')) %>%
  spread(Species, perc) %>%
  replace(is.na(.), 0)
max_obs = ncol(Simulated_shares1)
Simulated_shares1 <- Simulated_shares1 %>%
  mutate(sum_part = rowSums(.[5:max_obs])) %>% 
  merge(ports, by = c("port"), all.x = TRUE)
max_obs = ncol(Simulated_shares1) - 3
coef = 4.5

gg2 <- ggplot() +
  geom_sf(data = world_map_data, fill = land_color, size = .4) +
  geom_sf(data = state_map_data, fill = NA, size = .4) +
  coord_sf(xlim = c(-133, -116), ylim = c(32,46)) + mytheme +
  geom_scatterpie(aes(x=lon_port, y=lat_port, group = port, r = sum_part*coef), 
                  data = Simulated_shares1, legend_name = "Species",
                  cols = colnames(Simulated_shares1[,c(5:max_obs)])) +
  theme(legend.position = "none") + 
  geom_scatterpie_legend(Simulated_shares1$sum_part*coef, x=-131, y=44, 
                         labeller = scales::label_percent(scale = 100/coef)) +
  geom_text_repel(aes(x=lon_port, y=lat_port, group = port, label = port_group_name), 
                  data = Simulated_shares1, segment.color = "#333333", size = 3,
                  nudge_x = Simulated_shares1$x_nudge,
                  nudge_y = Simulated_shares1$y_nudge) +
  ggtitle('(b) Squid SDM = 0') + 
  scale_fill_manual(values = group.colors, labels=species_label)

#################################################################
## Create Map! ##
gg1 + gg2
