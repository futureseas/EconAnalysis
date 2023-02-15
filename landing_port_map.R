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


# Plot U.S. West Coast map #
ggplot() +
  geom_sf(data = world_map_data) +
  geom_sf(data = state_map_data) +
  geom_point(data = port_data, aes(x = lon, y = lat), color = 'red') +
  coord_sf(xlim = c(-126, -117), ylim = c(32,49))