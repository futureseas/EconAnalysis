########################################
##### Work to do: maybe show map picture [ like WK4 figure] of  Participation x Landing results ,
##### (a dummy example) –  and this allows you to cite your Landings paper. WORKING ON THIS!



rm(list = ls(all.names = TRUE)) 
gc()
## Load datase (from Stata work on predicting shares)
Simulated_shares <- read.csv("C:/GitHub/EconAnalysis/Participation/R/Simulated_shares.csv")


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





## Get coordinates for port area
Simulated_shares <- Simulated_shares %>% mutate(perc = perc) %>%
  mutate(port = ifelse(selection != "No-Participation",  substr(selection, 1, 3), NA)) %>%  
  drop_na() %>% 
  mutate(Species = ifelse(selection != "No-Participation",  substr(selection, 5, 8), NA)) %>% 
  select(-c('perc1', 'perc2', 'perc3', 'selection')) %>%
  spread(Species, perc) %>%
  replace(is.na(.), 0) %>%
  mutate(sum_part = rowSums(across(where(is.numeric))))


ports <- read_csv("Data/Ports/port_areas.csv") %>% 
  drop_na() %>%
  rename(port = port_group_code) %>%
  rename(lat_port = lat) %>%
  rename(lon_port = lon) %>%
  dplyr::select(-c(port_group_name))

Simulated_shares <- Simulated_shares %>% merge(ports, by = c("port"), all.x = TRUE)
rm(ports)


world_map_data <- ne_countries(scale = "medium", returnclass = "sf")
state_map_data <- maps::map('state', fill = TRUE, plot = FALSE) %>% st_as_sf()


### Theme
land_color <- c('antiquewhite1')
mytheme <- theme(panel.grid.major = element_line(color = '#cccccc' 
                                                 ,linetype = 'dashed'
                                                 ,size = .3)
                        ,panel.background = element_rect(fill = 'aliceblue')
                        ,plot.title = element_text(size = 12)
                        ,plot.subtitle = element_text(size = 11)
                        ,axis.title = element_blank()
                        ,axis.text = element_text(size = 10)
                        )



# = function(x) {m = round(x/coef*100, digits=0), return(paste0()}


###################
## Create Map! ##
###################

library(scatterpie)
max_obs = ncol(Simulated_shares) - 3

coef = 4.5
ggplot() +
  geom_sf(data = world_map_data, fill = land_color, size = .4) +
  geom_sf(data = state_map_data, fill = NA, size = .4) +
  coord_sf(xlim = c(-133, -116), ylim = c(32,45)) + mytheme +
  geom_scatterpie(aes(x=lon_port, y=lat_port, group = port, r = sum_part*coef), 
                  data = Simulated_shares, legend_name = "Species",
                  cols = colnames(Simulated_shares[,c(2:max_obs)])) +
  theme(legend.position = "none") + 
  geom_scatterpie_legend(Simulated_shares$sum_part*coef, x=-131, y=44, labeller = scales::label_percent(scale = 1/coef)) +
  geom_text_repel(aes(x=lon_port, y=lat_port, group = port, label = port), 
                  data = Simulated_shares, segment.color = "#333333")

