########################################################################################
# Importing and mapping SDM predictions
########################################################################################

library(reshape2)
library(ggplot2)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(dplyr)

# Open the netcdf containing the predictions you want
# Filename structure is species_m_yyyy_SDM.nc
testnc <- nc_open("sard_4_2015_GAM.nc")
# Look at the file structure
print(testnc)
# Extract the spatiotemporal variables
lon <- ncvar_get(testnc, "lon")
lat <- ncvar_get(testnc, "lat")
tim <- ncvar_get(testnc, "time")
# Select the appropriate line below, depending on which SDM the netcdf refers to
predSDM <- ncvar_get(testnc, "predGAM")
predSDM <- ncvar_get(testnc, "predBART")
predSDM <- ncvar_get(testnc, "predBRT")
# Close the netcdf
nc_close(testnc)

# Reshape the 3D array so we can map it, change the time field to be date
dimnames(predSDM) <- list(lon = lon, lat = lat, tim = tim)
sdmMelt <- reshape2::melt(predSDM, value.name = "predSDM")
sdmMelt$dt <- as.Date("1900-01-01") + days(sdmMelt$tim)

# Optional (but recommended): trim predictions to within 300-500km of the coast
if(!exists("distLand")) {
  distLand <- (read.csv("DistLandROMSPoints.csv", head=TRUE, sep=","))[c("lon","lat","distLand")]
}
sdmMelt <- dplyr::full_join(sdmMelt, distLand, by = c("lon", "lat"))
sdmMelt <- subset(sdmMelt, sdmMelt$distLand < 500000)

# Map prep
pac.coast <- borders("world", colour="gray50", fill="gray50", xlim = c(-134, -110),ylim = c(30, 48))
mycols <- colors()[c(473,562,71,610,655,653,621,34)]
mypalette <- colorRampPalette(mycols)(255)

# Select a date to map. This example just uses the median value
myDate <- round(median(sdmMelt$dt), 0) 
p1 <- ggplot(data = subset(sdmMelt, sdmMelt$dt == myDate)) +
  geom_tile(aes(x = lon, y = lat, fill = predSDM)) +
  scale_fill_gradientn(colours = mypalette, limits = c(0, 0.8), na.value = NA) +
  guides(fill = guide_colorbar(barwidth=0.5, barheight=5)) +
  pac.coast + coord_quickmap(xlim = c(-134, -115), ylim = c(30, 48)) 
p1
