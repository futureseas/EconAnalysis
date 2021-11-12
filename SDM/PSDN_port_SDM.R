library(tidyverse)
library(ncdf4)
library(geosphere)
library(lubridate)
library(here)
library(readxl)
library(dplyr)

SDM_port <- tibble(LANDING_YEAR = integer(),
                   LANDING_MONTH = integer(),
                   PORT_NAME = character(),
                   SDM_60 = numeric())

# Load base to choose ports
PacFIN_2010_2020 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")

# Obtain port names used to land species of interest
port_codes_PSDN <- PacFIN_2010_2020 %>% filter(PACFIN_SPECIES_CODE %in% c("PSDN")) %>%
  select("PORT_NAME") %>% unique()
port_codes <- PacFIN_2010_2020 %>% filter(PACFIN_SPECIES_CODE %in% c("PSDN", "MSQD", "NANC")) %>%
  select("PORT_NAME", "PACFIN_PORT_CODE") %>% unique() %>% anti_join(port_codes_PSDN, by = "PORT_NAME")

# Load port coordinates
ports_coord <- read_excel(here::here("Data", "Ports", "port_coord.xlsx"), "port_names") %>%
  dplyr::rename(PORT_NAME = port_name)

# Merge coordinates with selected ports
ports <- merge(x=port_codes,y=ports_coord, by="PORT_NAME",all.x=TRUE, all.y=FALSE)
  ports <- ports %>% filter(PACFIN_PORT_CODE != "OWA")
 

# Run iteration to obtain SDM by ports
for (y in 1998:2018) {
  for (m in 1:12) {
    for (j in 1:nrow(ports)) {
	  		# Read netcdf
	  		dat <- nc_open(paste0("G:/My Drive/Project/Data/SDM/sardine/sard_", paste0(as.character(m), paste0("_", paste0(as.character(y),"_GAM.nc")))))
		  	lon <- ncvar_get(dat, "lon")
		  	lat <- ncvar_get(dat, "lat")
		  	tim <- ncvar_get(dat, "time")
		  	predSDM <- ncvar_get(dat, "predGAM")

			# Close the netcdf
			nc_close(dat)			

			# Reshape the 3D array so we can map it, change the time field to be date
			dimnames(predSDM) <- list(lon = lon, lat = lat, tim = tim)
			sdmMelt <- reshape2::melt(predSDM, value.name = "predSDM")
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
			    distHaversine(c(row$lon, row$lat), c(ports[j,]$lon, ports[j,]$lat))
			    })) %>%
			  mutate(dist = dist / 1000) 
			
			
			# Filter three different distance bands
			
			dat_prob_60 <- sdmMelt %>%
			  dplyr::filter(dist <= 60)
			
			SDM_mean_60 <- mean(dat_prob_60$exp_prob, na.rm = T)
			
			SDM_port <- SDM_port %>%
			  add_row(LANDING_YEAR = y, LANDING_MONTH = m, PORT_NAME = as.character(ports[j, 1]),
			          SDM_60 = SDM_mean_60)
	    
	    print(y)
	    print(m)
	    print(j)
	    	
	  }
  }
  
  # Save database each year
  write_csv(SDM_port, file = "data/SDM_port_month.csv")
}
 

##################
  # Year 2019 #
##################
  
# Obtain port names used to land species of interest
port_codes <- PacFIN_2010_2020 %>% filter(PACFIN_SPECIES_CODE %in% c("PSDN", "MSQD", "NANC")) %>%
    select("PORT_NAME", "PACFIN_PORT_CODE") %>% unique() 
  
# Merge coordinates with selected ports
ports <- merge(x=port_codes,y=ports_coord, by="PORT_NAME",all.x=TRUE, all.y=FALSE)
  ports <- ports %>% filter(PACFIN_PORT_CODE != "OWA")
  
# Run iteration to obtain SDM by ports for year 2019
   for (m in 1:8) {
     for (j in 1:nrow(ports)) {
       # Read netcdf
       dat <- nc_open(paste0("G:/My Drive/Project/Data/SDM/sardine/sard_", paste0(as.character(m), paste0("_2019_GAM.nc"))))
       lon <- ncvar_get(dat, "lon")
       lat <- ncvar_get(dat, "lat")
       tim <- ncvar_get(dat, "time")
       predSDM <- ncvar_get(dat, "predGAM")
       
       # Close the netcdf
       nc_close(dat)			
       
       # Reshape the 3D array so we can map it, change the time field to be date
       dimnames(predSDM) <- list(lon = lon, lat = lat, tim = tim)
       sdmMelt <- reshape2::melt(predSDM, value.name = "predSDM")
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
           distHaversine(c(row$lon, row$lat), c(ports[j,]$lon, ports[j,]$lat))
         })) %>%
         mutate(dist = dist / 1000) 
       
       
       # Filter three different distance bands
       
       dat_prob_60 <- sdmMelt %>%
         dplyr::filter(dist <= 60)
       
       SDM_mean_60 <- mean(dat_prob_60$exp_prob, na.rm = T)
       
       SDM_port <- SDM_port %>%
         add_row(LANDING_YEAR = 2019, LANDING_MONTH = m, PORT_NAME = as.character(ports[j, 1]),
                 SDM_60 = SDM_mean_60)
       
       print(m)
       print(j)
       
     }
   }
   
   # Save database each year
   write_csv(SDM_port, file = "data/SDM_port_2019_month.csv")
 
 