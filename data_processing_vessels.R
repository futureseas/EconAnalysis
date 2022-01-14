########################################
##                                    ##
##### Public PacFIN data processing #### 
##                                    ##
########################################

library(bookdown)
library(doBy)
library(dplyr)
library(fastDummies)
library(ggplot2)
library(here)
library(hrbrthemes)
library(lmtest)
library(lubridate)
library(magrittr)
library(plm)
library(reshape)
library(reshape2)
library(rstan)
library(scales)
library(sjlabelled)
library(summarytools)
library(texreg)
library(tidyr)
library(tidyverse)
library(tinytex)
library(viridis)
library(xtable)
library(xlsx)
library(zoo)


### Functions ###
meanfun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...)) #, v=var(x, na.rm=TRUE, ...), l=length(x))
}

sumfun <- function(x, ...){
  c(sum=sum(x, na.rm=TRUE, ...)) #, v=var(x, na.rm=TRUE, ...), l=length(x))
}

sum_mean_fun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...), sum=sum(x, na.rm=TRUE, ...)) #, l=length(x))
}


###################
### PacFIN data ###
###################

# Read PacFIN data by vessels for different decades #
PacFIN_2010_2020 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
  PacFIN_2000_2009 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
  PacFIN_1990_1999 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_1990_1999.csv")
  PacFIN_1981_1989 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_1981_1989.csv")

# Merge different decades of data #
PacFIN <- rbind.data.frame(PacFIN_1981_1989, PacFIN_1990_1999, PacFIN_2000_2009, PacFIN_2010_2020)
  rm(PacFIN_1981_1989, PacFIN_1990_1999, PacFIN_2000_2009, PacFIN_2010_2020)
  gc()

# Include price per kilogram
  PacFIN$AFI_PRICE_PER_KG <- PacFIN$AFI_PRICE_PER_POUND * 2.20462


# Create monthly data
  PacFIN.month <- summaryBy(LANDED_WEIGHT_MTONS + AFI_PRICE_PER_KG + AFI_EXVESSEL_REVENUE
                            + VESSEL_LENGTH + VESSEL_WEIGHT + VESSEL_HORSEPOWER + NUM_OF_DAYS_FISHED
                          ~ PACFIN_SPECIES_CODE + PACFIN_GEAR_CODE + PORT_NAME + PACFIN_PORT_CODE
                          + LANDING_YEAR + LANDING_MONTH + VESSEL_NUM + AGENCY_CODE + REMOVAL_TYPE_CODE,
                          FUN=sum_mean_fun, data=PacFIN)

# PacFIN.month <- read.csv("C:\\Data\\PacFIN data\\PacFIN_month.csv")



#######################
## Merge SDM dataset ##
#######################

# Merge data with SDM Pacific Sardine #
SDM_port_PSDN <- read.csv(file = here::here("Data", "SDM", "PSDN_SDM_port_month.csv"))
  PacFIN.month <- merge(PacFIN.month, SDM_port_PSDN, 
                        by = c("PORT_NAME", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

# Merge data with SDM Market Squid #
SDM_port_MSQD <- read.csv(file = here::here("Data", "SDM", "MSQD_SDM_port_month.csv"))
  PacFIN.month <- merge(PacFIN.month, SDM_port_MSQD, 
                        by = c("PORT_NAME", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

# Merge data with SDM Northern Anchovy #
SDM_port_NANC <- read.csv(file = here::here("Data", "SDM", "NANC_SDM_port_month.csv"))
  PacFIN.month <- merge(PacFIN.month, SDM_port_NANC, 
                        by = c("PORT_NAME", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

  
# Merge data with SDM Market Squid #
SDM_port_MSQD_Spawn <- read.csv(file = here::here("Data", "SDM", "MSQD_Spawn_SDM_port_month.csv"))
  PacFIN.month <- merge(PacFIN.month, SDM_port_MSQD_Spawn, 
                        by = c("PORT_NAME", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)  
  
  PacFIN.month <- PacFIN.month %>%
    dplyr::rename(PSDN_SDM_60 = SDM_60) %>%
    dplyr::rename(MSQD_SDM_90 = SDM_90) %>%
    dplyr::rename(MSQD_SPAWN_SDM_90 = SDM_SPAWN_90) %>%
    dplyr::rename(NANC_SDM_20 = SDM_20)


#######################
## Merge TAC dataset ##
#######################

# Open CSV file with TAC information and landings #
  TAC.PSDN <- read.csv(file ="C:\\GitHub\\EconAnalysis\\Data\\ACL_data\\historical_TAC.csv") %>%
    dplyr::rename(LANDING_YEAR = Year) %>% dplyr::rename(LANDING_MONTH = Month) %>% 
    mutate(QuotaAllocated = 1) %>% mutate(QuotaAllocationNumber = 1) %>%
    mutate(TAC_N = ifelse(LANDING_YEAR > 2005, 0, TAC_N)) %>% 
    mutate(TAC_S = ifelse(LANDING_YEAR > 2005, 100, TAC_S)) %>%
    mutate(Alloc_lat = ifelse(LANDING_YEAR > 2005, 49, Alloc_lat))
  
  ## Create data base ##
  years <- as.data.frame(2000:2019) %>% dplyr::rename("LANDING_YEAR" = "2000:2019")
  month <- as.data.frame(1:12) %>% dplyr::rename("LANDING_MONTH" = "1:12")
  dates <- merge(month, years,  all.x = T, all.y = T) 
  rm(years, month)
  
  ### Calculate total CATCH of PSDN that reduce the TAC ##
  landings.psdn <- PacFIN.month %>% filter(LANDING_YEAR >= 2000) %>% 
    filter(PACFIN_SPECIES_CODE == "PSDN") %>% 
    filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D") %>%
    group_by(LANDING_YEAR, LANDING_MONTH) %>% summarise(landings_psdn = sum(LANDED_WEIGHT_MTONS.sum))

  # Incorporate total landings in data base #
  landings.TAC.psdn <- dates %>% merge(landings.psdn,  by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = T)
  landings.TAC.psdn$landings_psdn[is.na(landings.TAC.psdn$landings_psdn)] = 0
  landings.TAC.psdn <- landings.TAC.psdn %>% mutate(no_psdn_land = ifelse(landings_psdn == 0, 1, 0)) 
  
  # Merge TAC into database # 
  landings.TAC.psdn <- landings.TAC.psdn %>%
    merge(TAC.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>%
    mutate(QuotaAllocated = ifelse(is.na(QuotaAllocated), 0, QuotaAllocated))
  landings.TAC.psdn$Alloc_lat <- na.locf(landings.TAC.psdn$Alloc_lat)
  landings.TAC.psdn$TAC_N     <- na.locf(landings.TAC.psdn$TAC_N)
  landings.TAC.psdn$TAC_S     <- na.locf(landings.TAC.psdn$TAC_S)
  landings.TAC.psdn$TAC_mt    <- na.locf(landings.TAC.psdn$TAC_mt) 
  
  
  # Substract accumulative landings to TAC #
  landings.TAC.psdn$n <- (1:nrow(landings.TAC.psdn)) 
  landings.TAC.psdn <- landings.TAC.psdn %>% mutate(QuotaAllocationNumber = QuotaAllocationNumber * n)
  landings.TAC.psdn$QuotaAllocationNumber <- na.locf(landings.TAC.psdn$QuotaAllocationNumber)
  landings.TAC.psdn$QuotaAllocationNumber <- as.factor(
    udpipe::unique_identifier(landings.TAC.psdn, fields = "QuotaAllocationNumber", start_from = 1))
  landings.TAC.psdn <- landings.TAC.psdn %>% group_by(QuotaAllocationNumber) %>% 
    mutate(csum = cumsum(landings_psdn)) 
  landings.TAC.psdn <- landings.TAC.psdn %>% group_by(QuotaAllocationNumber) %>%
    arrange(n) %>%  mutate(TAC_mt_v2 = TAC_mt - shift(csum, fill = first(0))) %>% ungroup()
  
  # Replace negative values by zero (fishery should be closed) #
  landings.TAC.psdn <- landings.TAC.psdn %>% mutate(TAC_mt = ifelse(TAC_mt_v2 <= 0, 0, TAC_mt_v2)) %>%
    select(-c("n", "csum", "TAC_mt_v2", "landings_psdn", 'no_psdn_land'))
  
  rm(dates, landings.psdn, TAC.PSDN)
  
  
  # Still landings after select just commercial fishery.
  
  # YEAR 2014
  # 6966
  # 23293
  
  # PERIODO 2015-2020
  # 7000
  # 8000
  # 8000
  # 7000
  # 4000
  # 4000
  


# Merge data with ACL #
PacFIN.month <- merge(PacFIN.month, landings.TAC.psdn,
                          by=c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

### Merge latitude by ports and calculate actual quota by port ###
port_coord <- read.csv(file = here::here("C:\\GitHub\\EconAnalysis\\Data\\Ports\\port_names.csv")) %>%
  dplyr::rename(lat_port = lat) %>%  dplyr::rename(lon_port = lon) %>%  dplyr::rename(PORT_NAME = port_name)

PacFIN.month <- merge(PacFIN.month, port_coord, by=c("PORT_NAME"), all.x = TRUE)

### Assign quota according to port latitude ##
PacFIN.month <- PacFIN.month %>% mutate(TAC = ifelse(lat_port <= Alloc_lat, (TAC_S * TAC_mt), (TAC_N * TAC_mt)))




...

####################
### Save DATASET ###
####################
  
sapply(PacFIN.month, class)
write.csv(PacFIN.month.ACL,"C:\\Data\\PacFIN data\\PacFIN_month.csv", row.names = FALSE)
