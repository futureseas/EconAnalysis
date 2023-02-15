### Create map for landing paper ###

# Load packages #
library(tidyverse)
library(sf)
library(ggspatial)
library(rnaturalearth)
library(tidygeocoder)
library(maps)
library(ggplot2)

# Delete old data #
rm(list=ls())
gc()

# Get map data #
world_map_data <- ne_countries(scale = "medium", returnclass = "sf")
state_map_data <- map('state', fill = TRUE, plot = FALSE) %>% st_as_sf()

# Get port coordinates #
port_data <- read.csv(here::here("Data", "Ports", "port_areas.csv"))


#---------------------
# Create theme for map
#---------------------

mytheme <- theme(text = element_text(family = 'Avenir')
                 ,panel.grid.major = element_line(color = '#cccccc' 
                                                    ,linetype = 'dashed'
                                                  ,size = .3
                 )
                 ,panel.background = element_rect(fill = 'aliceblue')
                 ,plot.title = element_text(size = 32)
                 ,plot.subtitle = element_text(size = 14)
                 ,axis.title = element_blank()
                 ,axis.text = element_text(size = 10)
)

land_color <- c('antiquewhite1')

#--------------------------
# Plot U.S. West Coast map 
#--------------------------
base_plot <- ggplot() +
  geom_sf(data = world_map_data, fill = land_color, size = .4) +
  geom_sf(data = state_map_data, fill = NA, size = .4) +
  geom_point(data = port_data, aes(x = lon, y = lat), size = 4, color = 'red', alpha = .15) +
  geom_point(data = port_data, aes(x = lon, y = lat), size  = 4, shape = 1,  color = 'red') +
  coord_sf(xlim = c(-126.5, -117.5), ylim = c(34,48.5)) +
  mytheme

# SHOW MAP
print(base_plot)






