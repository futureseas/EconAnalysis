rm(list = ls())
gc()

library(ncdf4)
library(data.table)
library(dplyr)
library(geosphere)
library(furrr)
library(here)
library(reshape2)
library(lubridate)
library(progressr)
library(tidyverse)

# Setup parallel plan
plan(multisession, workers = parallel::detectCores() - 2)
handlers(global = TRUE)

# Load static data
ports <- fread("C:/GitHub/EconAnalysis/Data/Ports/port_areas.csv") %>% drop_na()
port_locs <- ports[, .(port_group_code, lat, lon)]
distLand <- fread(here("FuturePredictions", "DistLandROMSPoints.csv"))[, .(lon, lat, distLand)]
grid <- unique(distLand[, .(lon, lat)])

# 📍 Precompute distance to ports
dist_list <- lapply(1:nrow(port_locs), function(i) {
  port <- port_locs[i]
  dists <- distHaversine(matrix(c(grid$lon, grid$lat), ncol = 2),
                         c(port$lon, port$lat)) / 1000
  data.table(
    lon = grid$lon,
    lat = grid$lat,
    PORT_AREA_CODE = port$port_group_code,
    dist = dists
  )
})
port_dists <- rbindlist(dist_list)
near_ports <- port_dists[dist <= 60]

# Merge port distances into grid
setkey(near_ports, lon, lat)
setkey(distLand, lon, lat)
grid <- merge(distLand, near_ports, by = c("lon", "lat"))

# 🔁 Process each (year, month) in parallel
ym_grid <- expand.grid(year = 2070:2090, month = 1:12)

with_progress({
  results_list <- future_map(
    split(ym_grid, seq(nrow(ym_grid))),
    function(ym) {
      y <- ym$year
      m <- ym$month
      nc_path <- sprintf("C:/Data/Future Projections CPS SC-GAMs ROMS Domain/sardine/sardSDMs_proj_%d_%d.nc", m, y)
      if (!file.exists(nc_path)) return(NULL)
      
      dat <- tryCatch(nc_open(nc_path), error = function(e) return(NULL))
      if (is.null(dat)) return(NULL)
      
      lon <- ncvar_get(dat, "lon")
      lat <- ncvar_get(dat, "lat")
      tim <- ncvar_get(dat, "time")
      predGFDL <- ncvar_get(dat, "predGAMBOOST_GFDL")
      predIPSL <- ncvar_get(dat, "predGAMBOOST_IPSL")
      predHADL <- ncvar_get(dat, "predGAMBOOST_HADL")
      nc_close(dat)
      
      dimnames(predGFDL) <- list(lon = lon, lat = lat, time = tim)
      dimnames(predIPSL) <- list(lon = lon, lat = lat, time = tim)
      dimnames(predHADL) <- list(lon = lon, lat = lat, time = tim)
      
      gfdl_dt <- melt(predGFDL, value.name = "GFDL")
      ipsl_dt <- melt(predIPSL, value.name = "IPSL")
      hadl_dt <- melt(predHADL, value.name = "HADL")
      
      sdm_dt <- Reduce(function(x, y) merge(x, y, by = c("lon", "lat", "time")),
                       list(as.data.table(gfdl_dt), as.data.table(ipsl_dt), as.data.table(hadl_dt)))
      
      sdm_dt[, date := as.Date("1900-01-01") + as.numeric(time)]
      sdm_dt[, time := NULL]
      
      sdm_dt <- merge(sdm_dt, grid, by = c("lon", "lat"))
      sdm_dt <- sdm_dt[distLand < 500000]
      
      result <- sdm_dt[, .(
        SDM_60_GFDL = mean(GFDL, na.rm = TRUE),
        SDM_60_IPSL = mean(IPSL, na.rm = TRUE),
        SDM_60_HADL = mean(HADL, na.rm = TRUE)
      ), by = .(date, PORT_AREA_CODE)]
      
      result[, `:=`(
        LANDING_YEAR = year(date),
        LANDING_MONTH = month(date),
        LANDING_DAY = day(date)
      )]
      
      return(result)
    },
    .progress = TRUE
  )
})

# 💾 Combine and write to file
results_clean <- results_list[!sapply(results_list, is.null)]
SDM_all <- rbindlist(results_clean, fill = TRUE)
fwrite(SDM_all, "C:/Data/PSDN_FutureSDM_port_day.csv")
