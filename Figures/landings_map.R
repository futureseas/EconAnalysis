#' Load PacFIN landings data by year and port code
#'
#'

library(tidyverse)
library(ggmap)
library(maps)
library(ggthemes)
library(gganimate)
library(gifski)

# load data files in two batches landed and round weight
#

# landed weight files
lwt_dat_CA <- read_csv("data/ALL005-California-1980-2019_landedwt.csv") %>%
  select(-contains("TOTAL")) %>%
  gather(var, val, -LANDING_YEAR, -AGENCY_CODE, -MANAGEMENT_GROUP_CODE,
         -COMPLEX, -PACFIN_SPECIES_COMMON_NAME, -PACFIN_SPECIES_CODE) %>%
  extract(var, into = c("port", "info"), 
          regex = ("(.{3})_(.*)")) %>%
  spread(info, val)

lwt_dat_OR <- read_csv("data/ALL005-Oregon-1980-2019_landedwt.csv") %>%
  select(-contains("TOTAL")) %>%
  gather(var, val, -LANDING_YEAR, -AGENCY_CODE, -MANAGEMENT_GROUP_CODE,
         -COMPLEX, -PACFIN_SPECIES_COMMON_NAME, -PACFIN_SPECIES_CODE) %>%
  extract(var, into = c("port", "info"), 
          regex = ("(.{3})_(.*)")) %>%
  spread(info, val) %>%
  select(-contains("ROUND"))

lwt_dat_WA <- read_csv("data/ALL005-Washington-1980-2019_landedwt.csv") %>%
  select(-contains("TOTAL")) %>%
  gather(var, val, -LANDING_YEAR, -AGENCY_CODE, -MANAGEMENT_GROUP_CODE,
         -COMPLEX, -PACFIN_SPECIES_COMMON_NAME, -PACFIN_SPECIES_CODE) %>%
  extract(var, into = c("port", "info"), 
          regex = ("(.{3})_(.*)")) %>%
  spread(info, val)

lwt_dat <- lwt_dat_CA %>% 
  bind_rows(lwt_dat_OR,
            lwt_dat_WA)

# round weight files
rwt_dat_CA <- read_csv("data/ALL005-California-1980-2019.csv") %>%
  select(-contains("TOTAL")) %>%
  gather(var, val, -LANDING_YEAR, -AGENCY_CODE, -MANAGEMENT_GROUP_CODE,
         -COMPLEX, -PACFIN_SPECIES_COMMON_NAME, -PACFIN_SPECIES_CODE) %>%
  extract(var, into = c("port", "info"), 
          regex = ("(.{3})_(.*)")) %>%
  spread(info, val)

rwt_dat_OR <- read_csv("data/ALL005-Oregon-1980-2019.csv") %>%
  select(-contains("TOTAL")) %>%
  gather(var, val, -LANDING_YEAR, -AGENCY_CODE, -MANAGEMENT_GROUP_CODE,
         -COMPLEX, -PACFIN_SPECIES_COMMON_NAME, -PACFIN_SPECIES_CODE) %>%
  extract(var, into = c("port", "info"), 
          regex = ("(.{3})_(.*)")) %>%
  spread(info, val)

rwt_dat_WA <- read_csv("data/ALL005-Washington-1980-2019.csv") %>%
  select(-contains("TOTAL")) %>%
  gather(var, val, -LANDING_YEAR, -AGENCY_CODE, -MANAGEMENT_GROUP_CODE,
         -COMPLEX, -PACFIN_SPECIES_COMMON_NAME, -PACFIN_SPECIES_CODE) %>%
  extract(var, into = c("port", "info"), 
          regex = ("(.{3})_(.*)")) %>%
  spread(info, val)

rwt_dat <- rwt_dat_CA %>% 
  bind_rows(rwt_dat_OR,
            rwt_dat_WA) %>%
  select(LANDING_YEAR,
         PACFIN_SPECIES_CODE,
         port,
         ROUND_WEIGHT_MTONS,
         ROUND_WEIGHT_PPP)

# Load port info
ports <- read_csv("data/port_groups.csv") %>%
  rename(port = port_group_code)

# Join round and landed weight files and port info file
dat <- lwt_dat %>%
  inner_join(rwt_dat, by = c("LANDING_YEAR", "PACFIN_SPECIES_CODE", "port")) %>%
  left_join(ports, by = c("port"))

# Generate map for sardines
dat_join <- dat %>%
  group_by(LANDING_YEAR, port) %>%
  summarize(port_group_name = first(port_group_name),
            lat = first(lat),
            lon = first(lon))

sard_dat <- dat %>%
  filter(PACFIN_SPECIES_COMMON_NAME == "PACIFIC SARDINE") %>%
  select(-lat, -lon, -port_group_name) %>%
  right_join(dat_join, by = c("LANDING_YEAR", "port")) %>%
  mutate(LANDED_WEIGHT_MTONS = as.numeric(LANDED_WEIGHT_MTONS))

write_csv(sard_dat, "data/sard_dat.csv")

map <- get_stamenmap(bbox = c(left = -134.61, 
                                  bottom = 26.26, 
                                  right = -110.48,
                                  top = 52.56),
                     zoom = 6,
                     maptype = "toner-lite")

sard_map_ani <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = LANDED_WEIGHT_MTONS),
             data = sard_dat, 
             colour = "darkgreen") +
  labs(size = "Landed Weight (MTons)") +
  transition_time(LANDING_YEAR) +
  labs(title = 'Year: {frame_time}    Sardine')

animate(sard_map_ani, fps = 10, duration = 30)
anim_save("figures/sardine_landings_ani.gif")


# Go ahead and build maps for the other other future seas species
# Albacore
alb_dat <- dat %>%
  filter(PACFIN_SPECIES_COMMON_NAME == "ALBACORE") %>%
  select(-lat, -lon, -port_group_name) %>%
  right_join(dat_join, by = c("LANDING_YEAR", "port")) %>%
  mutate(LANDED_WEIGHT_MTONS = as.numeric(LANDED_WEIGHT_MTONS))

alb_map_ani <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = LANDED_WEIGHT_MTONS),
             data = alb_dat, 
             colour = "darkorange4") +
  labs(size = "Landed Weight (MTons)") +
  transition_time(LANDING_YEAR) +
  labs(title = 'Year: {frame_time}    Albacore')

animate(alb_map_ani, fps = 10, duration = 30)
anim_save("figures/albacore_landings_ani.gif")


# Swordfish
swd_dat <- dat %>%
  filter(PACFIN_SPECIES_COMMON_NAME == "SWORDFISH") %>%
  select(-lat, -lon, -port_group_name) %>%
  right_join(dat_join, by = c("LANDING_YEAR", "port")) %>%
  mutate(LANDED_WEIGHT_MTONS = as.numeric(LANDED_WEIGHT_MTONS))

swd_map_ani <- ggmap(map) +
  geom_point(aes(x = lon, y = lat, size = LANDED_WEIGHT_MTONS),
             data = swd_dat, 
             colour = "dodgerblue4") +
  labs(size = "Landed Weight (MTons)") +
  transition_time(LANDING_YEAR) +
  labs(title = 'Year: {frame_time}    Swordfish')

animate(swd_map_ani, fps = 10, duration = 30)
anim_save("figures/swordfish_landings_ani.gif")

# Load Barb's SDM output for sardine.
sdm_dat <- read_csv("data/sardineSDMPredsROMS5x5.csv") %>%
  gather(key, prob, -year) %>%
  extract(key, c("lon", "lat"), "(.*),(.*)") %>%
  mutate(lon = as.numeric(lon),
         lat = as.numeric(lat))

sdm_land_dat <- sard_dat %>%
  rename(year = LANDING_YEAR)

sdm_sard_map <- ggmap(map) +
  geom_tile(data = sdm_dat, aes(x = lon, y = lat, fill = prob),
            alpha = 0.5) +
  scale_fill_gradientn(limits = c(0, 1), colours = rev(rainbow(5))) +
  geom_point(aes(x = lon, y = lat, size = LANDED_WEIGHT_MTONS),
             data = sdm_land_dat, 
             colour = "darkgreen") +
  transition_time(year) +
  labs(title = 'Year: {frame_time}    Sardine')

animate(sdm_sard_map, fps = 10, duration = 30)
anim_save("figures/sardine_sdm_ani.gif")