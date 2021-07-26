#' Prepare Barbs sardine SDM output for landings analysis at the port month level
#' This is the file to be used for the project with Barb, James, Desiree, and Jon.
#'

library(tidyverse)
library(ncdf4)
library(geosphere)
library(lubridate)

# Build empty data frame to deposit results
SDM_port <- tibble(year = integer(),
                   month = integer(),
                   port = character(),
                   SDM_60 = numeric(),
                   SDM_120 = numeric(),
                   SDM_200 = numeric())

# Load port georeferenced data
ports <- read_csv("data/port_names.csv")

# Compare port distances
ports_dist <- ports %>%
  mutate(k = 1)

ports_dist <- ports_dist %>%
  full_join(ports_dist, by = "k") %>%
  mutate(dist = by(., 1:nrow(.), function(row) { 
    distHaversine(c(row$lon.x, row$lat.x), c(row$lon.y, row$lat.y)) / 1000}))

# Calculate SDM values and build output data frame.
for (i in 2000:2016) {
  for(j in 1:12) {
    for(k in 1:nrow(ports)) {
      # Begin by calulating the expected value at each port for each month.
      dat <- nc_open(paste0("data/PredSard", paste0(as.character(i), ".nc")))
      
      lat <- ncvar_get(dat, varid = "latitude")
      lon <- ncvar_get(dat, varid = "longitude")
      time <- ncvar_get(dat, varid = "time")
      prob_array <- ncvar_get(dat, varid = "Probability")
      
      prob_slice <- prob_array[,,1]
      
      image(lon,lat,prob_slice)
      
      dat_prob <- as_tibble(expand.grid(lon = lon, lat = lat, time = time)) %>%
        add_column(prob = as.vector(prob_array)) %>%
        mutate(date = as.Date(time, origin = "1970-01-01")) %>%
        mutate(month = month(date)) %>%
        filter(month == j) %>%
        group_by(lat, lon) %>%
        summarize(exp_prob = mean(prob, na.rm = T)) %>%
        ungroup(.) %>%
        mutate(dist = by(., 1:nrow(.), function(row) { 
          distHaversine(c(row$lon, row$lat), c(ports[k,]$lon, ports[k,]$lat))
        })) %>%
        mutate(dist = dist / 1000)
        
      # Filter three different distance bands
      dat_prob_60 <- dat_prob %>%
        filter(dist <= 60)
      
      dat_prob_120 <- dat_prob %>%
        filter(dist > 60 & dist <= 120)
      
      dat_prob_200 <- dat_prob %>%
        filter(dist > 120 & dist <= 200)
      
      SDM_mean_60 <- mean(dat_prob_60$exp_prob, na.rm = T)
      SDM_mean_120 <- mean(dat_prob_120$exp_prob, na.rm = T)
      SDM_mean_200 <- mean(dat_prob_200$exp_prob, na.rm = T)
      
      SDM_port <- SDM_port %>%
        add_row(year = i, month = j, port = as.character(ports[k, 1]), 
                SDM_60 = SDM_mean_60,
                SDM_120 = SDM_mean_120,
                SDM_200 = SDM_mean_200)
      print(i)
      print(j)
      print(k)
    }
  }
}

write_csv(SDM_port, path = "data/SDM_port_month.csv")