library(tidyverse)
library(ncdf4)
library(geosphere)
library(lubridate)
library(here)
library(readxl)
library(dplyr)

rm(list=ls())
gc()

SDM_port <- tibble(LANDING_YEAR = integer(),
                   LANDING_MONTH = integer(),
                   PORT_NAME = character(),
                   AGENCY_CODE = character(),
                   SDM_SPAWN_90 = numeric(),
                   SDM_SPAWN_100 = numeric(),
                   SDM_SPAWN_200 = numeric(),
                   SDM_SPAWN_300 = numeric(),
                   SDM_SPAWN_5_100 = numeric())

# Obtain port names used to land species of interest
port_codes <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\Ports.landing.FF.csv")

# Load port coordinates
ports_coord <- read.csv(here::here("Data", "Ports", "port_names.csv"))

# Merge coordinates with selected ports
ports <- merge(x=port_codes,y=ports_coord, by=c("PORT_NAME", "AGENCY_CODE"),all.x=TRUE, all.y=FALSE) %>%
  drop_na()

for (y in 2000:2019) {
  for (m in 1:12) {
    for (j in 1:nrow(ports)) {
	  		# Read netcdf
	  		dat <- nc_open(paste0("G:/My Drive/Project/Data/SDM/squid_spawn/squidSpawn_", paste0(as.character(m), paste0("_", paste0(as.character(y),"_envelope.nc")))))
	  		lon <- ncvar_get(dat, "lon")
		  	lat <- ncvar_get(dat, "lat")
		  	tim <- ncvar_get(dat, "time")
		  	predSDM <- ncvar_get(dat, "pred")

			# Close the netcdf
			nc_close(dat)			

			# Reshape the 3D array so we can map it, change the time field to be date
			dimnames(predSDM) <- list(lon = lon, lat = lat, tim = tim)
			sdmMelt <- reshape2::melt(predSDM, value.name = "predSDM")
			sdmMelt$tim <- as.integer(sdmMelt$tim)
			sdmMelt$dt <- as.Date("1900-01-01") + days(sdmMelt$tim)			

			# Optional (but recommended): trim predictions to within 300-500km of the coast
			if(!exists("distLand")) {
			  distLand <- (read.csv(here::here("SDM", "DistLandROMSPoints.csv"), head=TRUE, sep=","))[c("lon","lat","distLand")]
			}
			sdmMelt <- dplyr::full_join(sdmMelt, distLand, by = c("lon", "lat"))
			sdmMelt <- subset(sdmMelt, sdmMelt$distLand < 500000)			
			

			sdmMelt <- sdmMelt %>%
			  group_by(lat, lon) %>%
			  summarize(exp_prob = mean(predSDM, na.rm = T))	%>%
			  ungroup(.) %>%
			  mutate(dist = by(., 1:nrow(.), function(row) {
			    distHaversine(c(row$lon, row$lat), c(ports[j,]$Longitude, ports[j,]$Latitude))
			    })) %>%
			  mutate(dist = dist / 1000) 
			
			
			# Filter three different distance bands
			dat_prob_90 <- sdmMelt %>%
			  dplyr::filter(dist <= 90)
			dat_prob_100 <- sdmMelt %>%
			  dplyr::filter(dist <= 100)
			dat_prob_200 <- sdmMelt %>%
			  dplyr::filter(dist <= 200)
			dat_prob_300 <- sdmMelt %>%
			  dplyr::filter(dist <= 300)
			dat_prob_5_100 <- sdmMelt %>%
			  dplyr::filter(dist > 5)	%>%
			  dplyr::filter(dist <= 100)
			
			SDM_SPAWN_mean_90 <- mean(dat_prob_90$exp_prob, na.rm = T)
			SDM_SPAWN_mean_100 <- mean(dat_prob_100$exp_prob, na.rm = T)
			SDM_SPAWN_mean_200 <- mean(dat_prob_200$exp_prob, na.rm = T)
			SDM_SPAWN_mean_300 <- mean(dat_prob_300$exp_prob, na.rm = T)
			SDM_SPAWN_mean_5_100 <- mean(dat_prob_5_100$exp_prob, na.rm = T)
			
			SDM_port <- SDM_port %>%
			  add_row(LANDING_YEAR = y, LANDING_MONTH = m, PORT_NAME = as.character(ports[j, 1]),
			          SDM_SPAWN_90 = SDM_SPAWN_mean_90, SDM_SPAWN_100 = SDM_SPAWN_mean_100,
			          SDM_SPAWN_200 = SDM_SPAWN_mean_200, SDM_SPAWN_300 = SDM_SPAWN_mean_300,
			          SDM_SPAWN_5_100 = SDM_SPAWN_mean_5_100, 
			          AGENCY_CODE = as.character(ports[j, 2]))
	    
	    print(y)
	    print(m)
	    print(j)
	    	
	  }
  }
  # Save database each year
  write_csv(SDM_port, file = "data/SDM/MSQD_Spawn_SDM_port_month.csv")
}

