library(tidyverse)
library(ncdf4)
library(geosphere)
library(lubridate)
library(here)


SDM_pred <- tibble(year = integer(),
                   month = integer(),
                   lon = numeric(),
                   lat = numeric(),
                   pSDM = numeric())

for (y in 2010:2012) {
  for (m in 1:12) {
    
      # Read netcdf
      dat <- nc_open(paste0("G:/My Drive/Project/Data/SDM/sardine/sard_", 
                            paste0(as.character(m), paste0("_", paste0(as.character(y),"_GAM.nc")))))
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
      
      sdmMelt <- sdmMelt %>%
        group_by(lat, lon) %>%
        summarize(exp_prob = mean(predSDM, na.rm = T))	%>%
        ungroup(.)  

      SDM_pred <- SDM_pred %>%
        add_row(year = y, month = m, lon = sdmMelt$lon , lat = sdmMelt$lat, pSDM = sdmMelt$exp_prob)
      
      print(y)
      print(m)
    }
}

write_csv(SDM_pred, file = "data/SDM_pred_PSDN_bymonth.csv")
