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
testnc <- nc_open("C://Data//v-20210917T202955Z-001//ROMS u-v//wcnrt_sv_daily_20110102_20170419.nc")
predSDM <- ncvar_get(testnc, "sv")