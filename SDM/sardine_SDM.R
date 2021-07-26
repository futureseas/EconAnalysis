# Retrieve SDM outputs in .nc format to use them in landing analysis #
# Thanks Jon for this code #
library(tidyverse)
library(ncdf4)
library(geosphere)
library(here)

# Build empty data frame to deposit results
SDM_port <- tibble(year = integer(),
                   port = character(),
                   SDM_mean = numeric())

# Recode using sapply and a function specification

for (i in 2000:2016) {
  for (j in 1:22) {
    # Begin by calulating the expected value at each port for each year.
    dat <- nc_open(paste0("C:/Users/felip/Google Drive (UCSC)/Project/Data/SDM/PredSard", paste0(as.character(i), ".nc")))
    lat <- ncvar_get(dat, varid = "latitude")
    lon <- ncvar_get(dat, varid = "longitude")
    time <- ncvar_get(dat, varid = "time")
    prob_array <- ncvar_get(dat, varid = "Probability")
    
    # Load port info
    ports <- read_csv(here::here("Data","port_areas.csv")) %>%
      dplyr::rename(port = port_group_code)
    
    dat_prob <- as_tibble(expand.grid(lon = lon, lat = lat, time = time)) %>%
      add_column(prob = as.vector(prob_array)) %>%
      group_by(lat, lon) %>%
      summarize(exp_prob = mean(prob, na.rm = T))  %>%
      ungroup(.) %>%
      mutate(dist = by(., 1:nrow(.), function(row) {
        distHaversine(c(row$lon, row$lat), c(ports[j,]$lon, ports[j,]$lat))
        })) %>%
      mutate(dist = dist / 1000) %>%
      # Assumes 20 km/hr and 22 hours for traveling there and back and 2 hours for fishing (Rose et al)
      filter(dist <= 220) 
    
    SDM_mean <- mean(dat_prob$exp_prob, na.rm = T)
    
    SDM_port <- SDM_port %>%
      add_row(year = i, port = as.character(ports[j, 1]), SDM_mean = SDM_mean)
    
    print(i)
    print(j)
  }
}

write_csv(SDM_port, path = "Data/SDM_port.csv")

