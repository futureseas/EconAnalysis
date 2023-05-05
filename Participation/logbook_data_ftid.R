###############################################################
### OBTAIN LOGBOOK DATA WITH FTID TO MERGE WITH PACFIN DATA ###
###############################################################

library(dplyr)
library(tidyverse) 
library(readxl)

###-------------------------------------------
# Link to logbook -- MARKET SQUID

## Oregon Squid
sqd.logbook.OR.2016 <- readxl::read_excel("C:\\Data\\Logbooks\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2016") %>%
  dplyr::rename(set_number = `Set #`) %>%
  mutate(Lat = Lat + min...9/60) %>%
  mutate(Long = Long + min...11/60) %>%
  mutate(AGENCY_CODE="O") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VESSEL_NUM = FedDoc) %>%
  mutate(set_date = as.Date(Date,format="%Y-%m-%d")) %>%
  dplyr::rename(FTID = `Ticket #`) %>%
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'set_date', 'FTID', 'set_number')) %>% 
  drop_na(set_number) %>% 
  mutate(ind = ifelse(set_number <= lag(set_number), row_number(), NA)) %>%
  fill(ind) %>% 
  mutate(ind = ifelse(is.na(ind), 1, ind)) %>% 
  group_by(ind) %>%
  fill(FTID, .direction = "downup") %>% 
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'set_date', 'FTID', 'set_number')) %>%
  ungroup()

sqd.logbook.OR.2018 <- read_excel("C:\\Data\\Logbooks\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2018") %>%
  mutate(Lat = Lat + Min/60) %>%
  mutate(Long = Long + mins/60) %>%
  mutate(AGENCY_CODE="O") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VESSEL_NUM = `Doc #`) %>%
  tidyr::fill(VESSEL_NUM) %>%
  mutate(set_date = as.Date(`Log Date`,format="%Y-%m-%d")) %>%
  dplyr::rename(FTID = `Fish Ticket #`) %>%
  dplyr::rename(set_number = `Set No.`) %>%
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'set_date', 'FTID', 'set_number')) %>%
  drop_na(set_number) %>%  
  mutate(ind = ifelse(set_number <= lag(set_number), row_number(), NA)) %>%
  fill(ind) %>% 
  mutate(ind = ifelse(is.na(ind), 1, ind)) %>% 
  group_by(ind) %>%
  fill(FTID, .direction = "downup") %>% 
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'set_date', 'FTID', 'set_number')) %>%
  ungroup()

sqd.logbook.OR.2019 <- read_excel("C:\\Data\\Logbooks\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2019") %>%
  dplyr::rename(set_number = Set) %>%
  mutate(lat = Lat + lat/60) %>%
  mutate(lon = Long + long/60) %>%
  mutate(AGENCY_CODE="O") %>%
  dplyr::rename(VESSEL_NUM = `doc number`) %>%
  mutate(set_date = as.Date(`Fishing date`,format="%Y-%m-%d")) %>%
  dplyr::rename(FTID = `Fish ticket`) %>%
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'set_date', 'FTID', 'set_number')) %>%
  drop_na(set_number) %>% 
  mutate(ind = ifelse(set_number <= lag(set_number), row_number(), NA)) %>%
  fill(ind) %>% 
  mutate(ind = ifelse(is.na(ind), 1, ind)) %>% 
  group_by(ind) %>%
  fill(FTID, .direction = "downup") %>% 
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'set_date', 'FTID', 'set_number')) %>%
  ungroup()

sqd.logbook.OR.2020 <- read_excel("C:\\Data\\Logbooks\\ODFW CPS logbooks\\Squid logbooks.xlsx", sheet = "Squid 2020") %>%
  dplyr::rename(set_number = `Set No.`) %>%
  mutate(Lat = Lat + Min/60) %>%
  mutate(Long = Long + mins/60) %>%
  mutate(AGENCY_CODE="O") %>%
  dplyr::rename(lat = Lat) %>%
  dplyr::rename(lon = Long) %>%
  dplyr::rename(VESSEL_NUM = `Doc #`) %>%
  mutate(set_date = as.Date(`log date`,format="%Y-%m-%d")) %>%
  dplyr::rename(FTID = `Fish Ticket #`) %>%
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'set_date', 'FTID', 'set_number')) %>%
  drop_na(set_number) %>%  
  mutate(ind = ifelse(set_number <= lag(set_number), row_number(), NA)) %>%
  fill(ind) %>% 
  mutate(ind = ifelse(is.na(ind), 1, ind)) %>% 
  group_by(ind) %>%
  fill(FTID, .direction = "downup") %>% 
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'set_date', 'FTID', 'set_number')) %>%
  ungroup()

sqd.logbook.OR <- rbind(sqd.logbook.OR.2016, sqd.logbook.OR.2018, sqd.logbook.OR.2019, sqd.logbook.OR.2020) %>%
  drop_na(FTID) %>%
  mutate(lat = ifelse(lat == 0, NA, lat)) %>%
  mutate(lon = ifelse(lon == 0, NA, lon)) %>%
  drop_na(lon) %>%
  drop_na(lat) %>%
  filter(FTID != "No landing") %>%
  filter(FTID != "no landing")

sqd.logbook.OR <-sqd.logbook.OR[ -c(1) ]

rm(sqd.logbook.OR.2016, sqd.logbook.OR.2018, sqd.logbook.OR.2019, sqd.logbook.OR.2020)


## California Squid
sqd.logbook.vessel <- read_csv("C:\\Data\\Logbooks\\CDFW CPS logbooks\\MarketSquidVesselDataExtract.csv") %>%
  dplyr::rename(lat = SetLatitude) %>%
  dplyr::rename(lon = SetLongitude) %>%
  dplyr::rename(VESSEL_NUM = VesselID) %>%
  dplyr::rename(FTID = LandingReceipts) %>%
  dplyr::rename(set_number = SetNumber) %>%
  mutate(AGENCY_CODE="C") %>%
  mutate(set_date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'set_date', 'FTID', 'set_number')) %>%
  drop_na(FTID) %>%
  mutate(lat = ifelse(lat == 0, NA, lat)) %>%
  mutate(lon = ifelse(lon == 0, NA, lon)) %>%
  drop_na(lon) %>%
  drop_na(lat)

sqd.logbook.light.brail <- read_csv("C:\\Data\\Logbooks\\CDFW CPS logbooks\\MarketSquidLightBrailLogDataExtract.csv") %>%
  dplyr::rename(lat = Lat_DD) %>%
  dplyr::rename(lon = Long_DD) %>%
  dplyr::rename(VESSEL_NUM = VesselID) %>%
  dplyr::rename(FTID = LandingReceipt) %>%
  mutate(AGENCY_CODE="C") %>%
  mutate(set_date = as.Date(LogDateString,format="%m/%d/%Y")) %>%
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'set_date', 'FTID')) %>%
  drop_na(FTID) %>%
  mutate(lat = ifelse(lat == 0, NA, lat)) %>%
  mutate(lon = ifelse(lon == 0, NA, lon)) %>%
  drop_na(lon) %>%
  drop_na(lat) %>% 
  group_by(FTID) %>% 
  mutate(set_number = 1:n()) %>% 
  ungroup()

sqd.logbook <- rbind(sqd.logbook.OR, sqd.logbook.vessel,sqd.logbook.light.brail) %>% mutate(PACFIN_SPECIES_CODE = "MSQD")
rm(sqd.logbook.OR, sqd.logbook.vessel,sqd.logbook.light.brail)

###-------------------------------------------
# Link to logbook -- NORTHERN ANCHOVY
nanc.logbook.OR <- readxl::read_excel("C:\\Data\\Logbooks\\ODFW CPS logbooks\\Anchovy logbooks.xlsx", sheet = "2016") %>%
  dplyr::rename("lat_D" = "Lat (D)") %>% 
  dplyr::rename("lat_min" = "Lat (DM)") %>%
  mutate(lat = lat_D + lat_min/60) %>%
  dplyr::rename("lon_D" = "Long (D)") %>% 
  dplyr::rename("lon_min" = "Long (Decimal Min)")  %>% 
  mutate(lon = lon_D + lon_min/60) %>%
  mutate(AGENCY_CODE ="O") %>%
  dplyr::rename(VESSEL_NUM = FedDoc) %>%
  mutate(set_date = as.Date(Date,format="%Y-%m-%d")) %>%
  dplyr::rename(FTID = Ticket) %>%
  dplyr::rename(set_number = Set) %>%
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'set_date', 'FTID', 'set_number')) %>%
  mutate(lat = ifelse(lat == 0, NA, lat)) %>%
  mutate(lon = ifelse(lon == 0, NA, lon)) %>%
  drop_na(lon) %>%
  drop_na(lat) %>%
  filter(FTID != "No ticket") 

nanc.logbook <- nanc.logbook.OR %>% mutate(PACFIN_SPECIES_CODE = "NANC")
rm(nanc.logbook.OR)


# PACIFIC SARDINE
## Oregon
psdn.logbook.OR <- read_excel("C:\\Data\\Logbooks\\ODFW CPS logbooks\\Sardine logbooks.xlsx", sheet = "Sardine") %>%
  mutate(lat = Lat + LatMin/60) %>%
  mutate(lon = Long + LongMin/60) %>%
  mutate(AGENCY_CODE = "O") %>%
  dplyr::rename(VESSEL_NUM = FedDoc) %>%
  dplyr::rename(set_number = Set) %>%
  dplyr::rename(FTID = Ticket) %>%
  mutate(set_date = as.Date(Date,format="%Y-%m-%d")) %>%
  dplyr::rename(effort = Sard) %>%
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'set_date', 'set_number', 'FTID')) %>%
  mutate(lat = ifelse(lat == 0, NA, lat)) %>%
  mutate(lon = ifelse(lon == 0, NA, lon)) %>%
  drop_na(lon) %>%
  drop_na(lat) %>%
  filter(FTID != "No ticket") 

psdn.logbook <- psdn.logbook.OR %>% mutate(PACFIN_SPECIES_CODE = "PSDN")

psdn.logbook.WA.NO_FTID <- read_excel("C:\\Data\\Logbooks\\WDFW CPS logbooks\\WA Sardine Logbook Flatfile Data Reques 20-15485.xlsx") %>%
  mutate(lat = `Latitude Degrees` + `Latitude Minutes`/60) %>%
  mutate(lon = `Longitude Degrees` + `Longitude Minutes`/60) %>%
  mutate(AGENCY_CODE = "W") %>%
  dplyr::rename(VESSEL_NUM = `Vessel`) %>%
  dplyr::rename(set_number = `Set Number`) %>%
  mutate(set_date = as.Date(`Fishing Date`,format="%Y-%m-%d")) %>%
  dplyr::select(c('lat', 'lon', 'VESSEL_NUM', 'AGENCY_CODE', 'set_date', 'set_number')) %>%
  mutate(FTID = NA) %>%
  mutate(lat = ifelse(lat == 0, NA, lat)) %>%
  mutate(lon = ifelse(lon == 0, NA, lon)) %>%
  drop_na(lon) %>%
  drop_na(lat)


### Merge all logbooks with FTID ### 
logbooks.FTID <- rbind(sqd.logbook, nanc.logbook, psdn.logbook) %>% 
  filter(lat >= 30) %>% filter(lat <= 50) %>% 
  mutate(lon = ifelse(lon > 0, lon*(-1), lon)) %>%
  filter(lon <= -115) %>% filter(lon >= -130)


logbooks.coord <- logbooks.FTID %>%
  group_by(FTID) %>% summarize(lat_logbook = mean(lat), lon_loogbook = mean(lon))

saveRDS(logbooks.coord, "C:\\Data\\Logbooks\\logbooks_FTID.rds")




