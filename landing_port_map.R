### Create map for landing paper ###

# Load packages #
library(tidyverse)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(tidygeocoder)
library(maps)
library(ggplot2)
library(ggrepel)
library(extrafont)
library(dplyr)

# Delete old data #
rm(list=ls())
gc()

# Get map data #
world_map_data <- ne_countries(scale = "medium", returnclass = "sf")
state_map_data <- map('state', fill = TRUE, plot = FALSE) %>% st_as_sf()

# Get port coordinates #
port_data <- read.csv(here::here("Data", "Ports", "port_areas.csv")) %>%
  rename(PORT_AREA_CODE = ï..port_group_code)

port_data <- port_data %>%
  mutate(port_group_name = ifelse(PORT_AREA_CODE == "NPS", "N. Puget Sound", port_group_name)) %>%
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


fleet_label <- as_labeller(c("1" = "Southern CCS small-scale\nsquid-specialists",
                            "2" = "Southern CCS small-scale\nCPS-opportunists",
                            "3" = "PNW sardine\nopportunists",
                            "4" = "Southern CCS industrial\nsquid-specialists",
                            "5" = "Roving industrial\nsardine-squid generalists",
                            "6" = "PNW sardine\nspecialists",
                            "7" = "Southern CCS\nforage fish diverse",
                            "8" = "PNW albacore-crab\ngeneralists"))


#------------------------
# Add nudge to port data
#------------------------
port_data <- port_data %>%                              
  mutate(x_nudge = case_when( 
    PORT_AREA_CODE == "NPS" ~ -4,
    PORT_AREA_CODE == "SPS" ~ -4,
    PORT_AREA_CODE == "CWA" ~ -4,
    PORT_AREA_CODE == "CLW" ~ -3.5,
    PORT_AREA_CODE == "CLO" ~ -4,
    PORT_AREA_CODE == "TLA" ~ -1.3,
    PORT_AREA_CODE == "NPA" ~ -1.3,
    PORT_AREA_CODE == "CBA" ~ -1.3,
    PORT_AREA_CODE == "BRA" ~ -1.3,
    PORT_AREA_CODE == "CCA" ~ -1.3,
    PORT_AREA_CODE == "ERA" ~ -1.3,
    PORT_AREA_CODE == "BGA" ~ -1.3,
    PORT_AREA_CODE == "BDA" ~ -1.3,
    PORT_AREA_CODE == "SFA" ~ -1.3,
    PORT_AREA_CODE == "MNA" ~ -1.8,
    PORT_AREA_CODE == "MRA" ~ -1.3,
    PORT_AREA_CODE == "SBA" ~ -3,
    PORT_AREA_CODE == "LAA" ~ -3,
    PORT_AREA_CODE == "SDA" ~ -1.8,
    TRUE ~ 0),
    y_nudge = case_when( 
      PORT_AREA_CODE == "NPS" ~ 0,
      PORT_AREA_CODE == "SPS" ~ 0.8,
      PORT_AREA_CODE == "CWA" ~ 0.6,
      PORT_AREA_CODE == "CLW" ~ 0.5,
      PORT_AREA_CODE == "CLO" ~ -0.5,
      PORT_AREA_CODE == "TLA" ~ 0,
      PORT_AREA_CODE == "NPA" ~ 0,
      PORT_AREA_CODE == "CBA" ~ 0,
      PORT_AREA_CODE == "BRA" ~ 0,
      PORT_AREA_CODE == "CCA" ~ 0,
      PORT_AREA_CODE == "ERA" ~ 0,
      PORT_AREA_CODE == "BGA" ~ 0,
      PORT_AREA_CODE == "BDA" ~ 0,
      PORT_AREA_CODE == "SFA" ~ 0,
      PORT_AREA_CODE == "MNA" ~ 0,
      PORT_AREA_CODE == "MRA" ~ 0,
      PORT_AREA_CODE == "SBA" ~ 0,
      PORT_AREA_CODE == "LAA" ~ -0.5,
      PORT_AREA_CODE == "SDA" ~ 0,
      TRUE ~ 0)
  )

#----------------------------------------------------------
# Select ports and fleet segment by each species equations
#----------------------------------------------------------
port_data_MSQD <- read.csv("C:/Data/PacFIN data/dataset_estimation_MSQD.csv") %>%
  dplyr::select(c('PORT_AREA_CODE','group_all')) %>% unique() %>%
  merge(port_data, by = c('PORT_AREA_CODE'), all.x = TRUE, all.y = FALSE)
port_data_PSDN <- read.csv("C:/Data/PacFIN data/dataset_estimation_PSDN.csv") %>%
  dplyr::select(c('PORT_AREA_CODE','group_all')) %>% unique() %>%
  merge(port_data, by = c('PORT_AREA_CODE'), all.x = TRUE, all.y = FALSE)
port_data_NANC <- read.csv("C:/Data/PacFIN data/dataset_estimation_NANC.csv") %>%
  dplyr::select(c('PORT_AREA_CODE','group_all')) %>% unique() %>%
  merge(port_data, by = c('PORT_AREA_CODE'), all.x = TRUE, all.y = FALSE)


#---------------------
# Create theme for map
#---------------------
mytheme <- theme(text = element_text(family = 'sans')
                 ,panel.grid.major = element_line(color = '#cccccc' 
                                                    ,linetype = 'dashed'
                                                  ,size = .3
                 )
                 ,panel.background = element_rect(fill = 'aliceblue')
                 ,plot.title = element_text(size = 12)
                 ,plot.subtitle = element_text(size = 10)
                 ,axis.title = element_blank()
                 ,axis.text = element_text(size = 8)
)

land_color <- c('antiquewhite1')


#----------------------
# CHANGE STATE NAME
# change to "title case"
#----------------------
state_map_data %>% 
  mutate(ID = stringr::str_to_title(ID)) ->
  state_map_data

names(state_map_data)

#--------------------
# ADD STATE CENTROIDS
#--------------------
sf_use_s2(FALSE)
state_map_data %>% 
  mutate(centroid = st_centroid(geom)) ->
  state_map_data

#------------------------
# ADD X AND Y COORDINATES
#------------------------
statename_coords <- state_map_data %>% 
  st_centroid() %>% 
  st_coordinates() %>%
  as_tibble()

state_map_data %>%  
  bind_cols(statename_coords) %>% 
  select(ID, X, Y, centroid, geom) ->
  state_map_data


#----------------------------
# ADD OFFSETS FOR STATE NAMES
#----------------------------
state_map_data %>% 
  mutate(x_nudge = case_when( ID == 'Washington' ~ .7
                              ,ID == 'Nevada' ~ -.7
                              ,ID == 'California' ~ -.3
                              ,TRUE ~ 0
  )
  ,y_nudge = case_when( ID == 'Nevada' ~ .3
                        ,TRUE ~ 0
  )
  ) -> 
  state_map_data


#----------
# ADD NAMES
#----------
state_names <- geom_text(data = state_map_data
                         ,aes(x = X, y = Y, label = ID)
                         ,color = "#333333"
                           ,size = 3
                         ,fontface = 'bold'
                         ,nudge_x = state_map_data$x_nudge
                         ,nudge_y = state_map_data$y_nudge
)

#------
# Plot 
#------


group.colors <- c("1" = "#fde725",
                  '3' = "#35b779", 
                  "4" = "#0d0887", 
                  "5" = "#31688e", 
                  "6" = "#fdb42f", 
                  "7" = "#440154")

### Market squid ###
port_data_plot <- port_data_MSQD %>% select(c(PORT_AREA_CODE, lon,lat, port_group_name, x_nudge, y_nudge)) %>% unique()
MSQD_plot <- ggplot() +
  geom_sf(data = world_map_data, fill = land_color, size = .4) +
  geom_sf(data = state_map_data, fill = NA, size = .4) +
  geom_point(data = port_data_MSQD, 
             aes(x = lon, y = lat, color = factor(group_all)), 
             position = position_dodge(width = 1.15), 
             size = 10, 
             shape = 20, alpha = 0.75) +
  coord_sf(xlim = c(-129, -116), ylim = c(33, 49)) +
  mytheme + 
  state_names +
  geom_text_repel(data = port_data_plot
                  ,aes(x = lon, y = lat, label = port_group_name)
                  ,size = 3
                  ,nudge_x = port_data_plot$x_nudge
                  ,nudge_y = port_data_plot$y_nudge
                  ,segment.color = "#333333") + 
  guides(colour=guide_legend(title="Fleet segments: ")) +
  scale_color_manual(values=group.colors, labels=fleet_label) +
  labs(title = '(a) Market squid model') +
  theme(legend.position="none")

### Pacific sardine ###
port_data_plot <- port_data_PSDN %>% select(c(PORT_AREA_CODE, lon,lat, port_group_name, x_nudge, y_nudge)) %>% unique()
PSDN_plot <- ggplot() +
  geom_sf(data = world_map_data, fill = land_color, size = .4) +
  geom_sf(data = state_map_data, fill = NA, size = .4) +
  geom_point(data = port_data_PSDN, 
             aes(x = lon, y = lat, color = factor(group_all)), 
             position = position_dodge(width = 1.15), 
             size = 8, 
             shape = 20, alpha = 0.75) +
  coord_sf(xlim = c(-129, -116), ylim = c(33, 49)) +
  mytheme + 
  state_names +
  geom_text_repel(data = port_data_plot
                  ,aes(x = lon, y = lat, label = port_group_name)
                  ,size = 3
                  ,nudge_x = port_data_plot$x_nudge
                  ,nudge_y = port_data_plot$y_nudge
                  ,segment.color = "#333333") + 
  guides(colour=guide_legend(title="Fleet segments: ")) +
  labs(title = '(b) Pacific sardine model') +
  scale_color_manual(values=group.colors, labels=fleet_label) +
  theme(legend.position="none")

### Northern anchovy ###
port_data_plot <- port_data_NANC %>% select(c(PORT_AREA_CODE, lon,lat, port_group_name, x_nudge, y_nudge)) %>% unique()
NANC_plot_legend <- ggplot() +
  geom_sf(data = world_map_data, fill = land_color, size = .4) +
  geom_sf(data = state_map_data, fill = NA, size = .4) +
  geom_point(data = port_data_NANC, 
             aes(x = lon, y = lat, colour = factor(group_all)), 
             size = 10, 
             shape = 20, alpha = 0.75) +
  coord_sf(xlim = c(-129, -116), ylim = c(33, 49)) +
  mytheme + 
  state_names +
  geom_text_repel(data = port_data_plot
                  ,aes(x = lon, y = lat, label = port_group_name)
                  ,size = 3
                  ,nudge_x = port_data_plot$x_nudge
                  ,nudge_y = port_data_plot$y_nudge
                  ,segment.color = "#333333") + 
  labs(title = '(c) Northern anchovy model') +
  guides(colour=guide_legend(title="Fleet segments: ")) +
  scale_color_manual(values=group.colors, labels=fleet_label) +
  theme(legend.position="right")

library(ggpubr)
leg <- get_legend(NANC_plot_legend)
legend <- as_ggplot(leg)
legend

NANC_plot <- NANC_plot_legend +
  theme(legend.position="none")

gridExtra::grid.arrange(MSQD_plot, PSDN_plot, NANC_plot, legend, nrow = 1)



