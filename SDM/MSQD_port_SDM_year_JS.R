### Processing of Justin Suca squid outputs ###

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
                   SDM_90 = numeric())

# Obtain port names used to land species of interest
port_codes <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\Ports.landing.FF.csv")

# Load port coordinates
ports_coord <- read.csv(here::here("Data", "Ports", "port_names.csv"))

# Merge coordinates with selected ports
ports <- merge(x=port_codes,y=ports_coord, by=c("PORT_NAME", "AGENCY_CODE"),all.x=TRUE, all.y=FALSE) %>%
  drop_na()

dat <- read.csv("G:\\My Drive\\Project\\Data\\SDM\\squid\\JS outputs\\Monthly_Mean_Values_Squid_Abund.csv")

dat_long <- gather(dat, condition, lnCPUE, X1998.7:X2019.8, factor_key=TRUE)
  dat_long$YEAR <- as.data.frame(str_extract_all(dat_long$condition, "\\d{4}", simplify = T))
  dat_long <- dat_long %>% group_by(Longitude, Latitude, YEAR) %>% 
    summarise(mean.lnCPUE = mean(lnCPUE, na.rm = TRUE))

#---------------------------------------------------
# Run loop to associate each SDM output to a port

for (y in 2000:2019) {
  for (j in 1:nrow(ports)) {
    # Read netcdf
# 			dimnames(predSDM) <- list(lon = lon, lat = lat, tim = tim)
# 			sdmMelt <- reshape2::melt(predSDM, value.name = "predSDM")
# 			sdmMelt$dt <- as.Date("1900-01-01") + days(sdmMelt$tim)			
# 
# 			# Optional (but recommended): trim predictions to within 300-500km of the coast
# 			if(!exists("distLand")) {
# 			  distLand <- (read.csv(here::here("SDM", "DistLandROMSPoints.csv"), head=TRUE, sep=","))[c("lon","lat","distLand")]
# 			}
# 			sdmMelt <- dplyr::full_join(sdmMelt, distLand, by = c("lon", "lat"))
# 			sdmMelt <- subset(sdmMelt, sdmMelt$distLand < 500000)			
# 			
# 
# 			sdmMelt <- sdmMelt %>%
# 			  group_by(lat, lon) %>%
# 			  summarize(exp_prob = mean(predSDM, na.rm = T))	%>%
# 			  ungroup(.) %>%
# 			  mutate(dist = by(., 1:nrow(.), function(row) {
# 			    distHaversine(c(row$lon, row$lat), c(ports[j,]$Longitude, ports[j,]$Latitude))
# 			    })) %>%
# 			  mutate(dist = dist / 1000) 
# 			
# 			
# 			# Filter three different distance bands
# 			
# 			dat_prob_90 <- sdmMelt %>%
# 			  dplyr::filter(dist <= 90)
# 			
# 			SDM_mean_90 <- mean(dat_prob_90$exp_prob, na.rm = T)
# 			
# 			SDM_port <- SDM_port %>%
# 			  add_row(LANDING_YEAR = y, LANDING_MONTH = m, PORT_NAME = as.character(ports[j, 1]),
# 			          SDM_90 = SDM_mean_90)
# 	    
 	    print(y)
 	    print(j)
 }
}