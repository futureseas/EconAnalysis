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
distLand <- fread(here("FuturePredictions", "SDM", "DistLandROMSPoints.csv"))[, .(lon, lat, distLand)]
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
ym_grid <- expand.grid(year = 2000:2019, month = 1:12)

with_progress({
  results_list <- future_map(
    split(ym_grid, seq(nrow(ym_grid))),
    function(ym) {
      y <- ym$year
      m <- ym$month
      nc_path <- sprintf("C:/Data/Historical CPS SC-GAMs ROMS Domain/sardine/sard_%d_%d_mboost_roms.nc", m, y)
      if (!file.exists(nc_path)) return(NULL)
      
      dat <- tryCatch(nc_open(nc_path), error = function(e) return(NULL))
      if (is.null(dat)) return(NULL)
      
      lon <- ncvar_get(dat, "lon")
      lat <- ncvar_get(dat, "lat")
      tim <- ncvar_get(dat, "time")
      pred <- ncvar_get(dat, "predGAMBOOST")
      nc_close(dat)
      dimnames(pred) <- list(lon = lon, lat = lat, time = tim)
      sdm_dt <- as.data.table(data.table::melt(pred, value.name = "pred"))
      setnames(sdm_dt, c("lon", "lat", "time", "pred"))
      sdm_dt[, date := as.Date("1900-01-01") + as.numeric(time)]
      sdm_dt[, time := NULL]
      sdm_dt <- merge(sdm_dt, grid, by = c("lon", "lat"))
      sdm_dt <- sdm_dt[distLand < 500000]
      
      result <- sdm_dt[, .(SDM_60 = mean(pred, na.rm = TRUE)), by = .(date, PORT_AREA_CODE)]
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
fwrite(SDM_all, here("FuturePredictions", "SDM", "Historical", "PSDN_SDM_port_day.csv"))
