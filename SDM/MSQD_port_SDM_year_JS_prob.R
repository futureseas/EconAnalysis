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
                   PORT_NAME = character(),
                   SDM_90_JS_prob = numeric())

# Obtain port names used to land species of interest
port_codes <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\Ports.landing.FF.csv")

# Load port coordinates
ports_coord <- read.csv(here::here("Data", "Ports", "port_names.csv"))

# Merge coordinates with selected ports
ports <- merge(x=port_codes,y=ports_coord, by=c("PORT_NAME", "AGENCY_CODE"),all.x=TRUE, all.y=FALSE) %>%
  drop_na()

dat <- read.csv("G:\\My Drive\\Project\\Data\\SDM\\squid\\JS outputs\\Monthly_Mean_Values_Squid_PA.csv")

dat_long <- gather(dat, condition, prob, X1998.7:X2019.8, factor_key=TRUE)
  dat_long$YEAR <- as.data.frame(str_extract_all(dat_long$condition, "\\d{4}", simplify = T))
  dat_long <- dat_long %>% group_by(Longitude, Latitude, YEAR) %>% 
    summarise(mean.prob = mean(prob, na.rm = TRUE))

#---------------------------------------------------
# Run loop to associate each SDM output to a port

for (y in 2000:2019) {
  for (j in 1:nrow(ports)) {
    dat_long_year <- dat_long %>% filter(YEAR == y)
    
    dat_with_dist <- dat_long_year %>%
			  group_by(Latitude,Longitude) %>%
			  summarize(exp_prob = mean(mean.prob, na.rm = T))	%>%
			  ungroup(.) %>%
			  mutate(dist = by(., 1:nrow(.), function(row) {
			    distHaversine(c(row$Longitude, row$Latitude), c(ports[j,]$Longitude, ports[j,]$Latitude))
			    })) %>%
			  mutate(dist = dist / 1000)

			# Filter distance bands
			dat_prob_90 <- dat_with_dist %>%
			  dplyr::filter(dist <= 90)

			SDM_mean_90 <- mean(dat_prob_90$exp_prob, na.rm = T)

			SDM_port <- SDM_port %>%
			  add_row(LANDING_YEAR = y, PORT_NAME = as.character(ports[j, 1]),
			          SDM_90_JS_prob = SDM_mean_90)
			
 	    print(y)
 	    print(j)
 }
}

write_csv(SDM_port, file = "data/SDM/MSQD_SDM_port_year_JS_prob.csv")
  
  