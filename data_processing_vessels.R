################################
#### PacFIN data processing #### 
################################

library(data.table)
library(dplyr)
library(distances)
library(forcats)
library(cluster)
library(ggplot2)

#-----------------------
### Read PacFIN database

rm(list=ls())
gc()

## Read PacFIN data by vessels for different decades #
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)

## Number of fish tickets and vessels resporting it.
nrow(as.data.frame(unique(Tickets$FTID))) 
nrow(as.data.frame(unique(Tickets$VESSEL_NUM))) 

## Include price per kilogram
Tickets$AFI_PRICE_PER_MTON <- Tickets$AFI_PRICE_PER_POUND * 2.20462 * 1000

## Create subsample excluding bycatch and vessels that never target a FF species
  
## Subset the data to get remove columns not relevant to this analysis. This will speed things up.
Tickets <- dplyr::select(Tickets, c(FTID, VESSEL_NUM, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, 
                             AFI_PRICE_PER_MTON, LANDED_WEIGHT_MTONS, AFI_EXVESSEL_REVENUE, 
                             VESSEL_LENGTH, VESSEL_WEIGHT, VESSEL_HORSEPOWER, NUM_OF_DAYS_FISHED,
                             PACFIN_GEAR_CODE, PORT_NAME, PACFIN_PORT_CODE, LANDING_YEAR, LANDING_MONTH,  
                             AGENCY_CODE, REMOVAL_TYPE_CODE, PRODUCT_USE_CODE, DISPOSITION_CODE))
               
## Remove records associated with landings of zero value; this is likely bycatch 
Tickets <- Tickets[which(Tickets$AFI_EXVESSEL_REVENUE>0),] 
  # 80% of the total number of tickets, from 4,596,193 tickets to 3,709,040.
  nrow(as.data.frame(unique(Tickets$FTID))) 

  
## Creating a filter here to only retain vessels with more than 3 forage fish landings 
#  (tickets where FF is the dominant species) over the time period. 
#  I think 3 is appropriate if you are clustering several years together, 
#  but if you are just clustering a single year than maybe you should drop it down to 1. 

FF_Vessels <- read.csv(file = here::here("Clustering", "FF_Vessels.csv")) 

  #Subset from the complete data set to only retain records associated with these Vessels       
  Tickets<-setDT(Tickets)[VESSEL_NUM %chin% FF_Vessels$VESSEL_NUM]            
  Tickets<-as.data.frame(Tickets)
  
  rm(FF_Vessels)

  #removal
  nrow_tickets <- nrow(Tickets)
  removal.df <- Tickets %>% select("REMOVAL_TYPE_CODE") %>% mutate(n = 1) %>% 
    group_by(REMOVAL_TYPE_CODE) %>% summarise(n_group = sum(n)/nrow_tickets*100) 
  
  Tickets <- Tickets %>% filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") 
  nrow(as.data.frame(unique(Tickets$FTID))) 
  nrow(as.data.frame(unique(Tickets$VESSEL_NUM))) 

rm(removal.df, nrow_tickets)

#----------------------
### Create monthly data
  
  sum_mean_fun <- function(x, ...){
    c(mean=mean(x, na.rm=TRUE, ...), sum=sum(x, na.rm=TRUE, ...)) }
  
  PacFIN.month <- doBy::summaryBy(AFI_PRICE_PER_MTON + LANDED_WEIGHT_MTONS + AFI_EXVESSEL_REVENUE + 
                              VESSEL_LENGTH + VESSEL_WEIGHT + VESSEL_HORSEPOWER + NUM_OF_DAYS_FISHED 
                            ~ VESSEL_NUM + PACFIN_SPECIES_CODE + PACFIN_SPECIES_COMMON_NAME + 
                              PACFIN_GEAR_CODE + PORT_NAME + 
                              PACFIN_PORT_CODE + LANDING_YEAR + LANDING_MONTH + AGENCY_CODE + 
                              PRODUCT_USE_CODE + DISPOSITION_CODE,
                          FUN=sum_mean_fun, data=Tickets)
rm(Tickets)
  
port_area <- read.csv(file = here::here("Data", "Ports", "ports_area_and_name_codes.csv"))
PacFIN.month <- PacFIN.month %>% merge(port_area, by = c("PACFIN_PORT_CODE"), all.x = TRUE)
ports_area_codes <- PacFIN.month %>% dplyr::select('PORT_NAME', 'AGENCY_CODE', 'PORT_AREA_CODE') %>% unique()


#---------------------
### Merge SDM data set 
  
# Create database with ports that land FF species
Ports.landing.FF <- PacFIN.month %>%
    dplyr::filter(PACFIN_SPECIES_CODE %in%
                    c("CMCK", "JMCK", "MSQD", "NANC", "PSDN", "UMCK")) %>%
    select("PORT_NAME", "AGENCY_CODE") %>% unique()

  write.csv(Ports.landing.FF,"C:\\GitHub\\EconAnalysis\\Data\\Ports\\Ports.landing.FF.csv", row.names = FALSE)

#### Merge data with SDM Pacific Sardine
SDM_port_PSDN <- read.csv(file = here::here("Data", "SDM", "PSDN_SDM_port_month.csv")) %>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_MONTH, LANDING_YEAR) %>% summarize(PSDN_SDM_60 = mean(SDM_60))
PacFIN.month <- merge(PacFIN.month, SDM_port_PSDN,
                              by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

#### Merge data with SDM Market Squid (spawn aggregation) #
SDM_port_MSQD_Spawn <- read.csv(file = here::here("Data", "SDM", "MSQD_Spawn_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  dplyr::filter(LANDING_MONTH == 8 | LANDING_MONTH == 9 | LANDING_MONTH == 10) %>% 
  group_by(PORT_AREA_CODE, LANDING_YEAR) %>% summarize(MSQD_SPAWN_SDM_90_v2 = mean(SDM_SPAWN_90))
PacFIN.month <- merge(PacFIN.month, SDM_port_MSQD_Spawn, 
                              by = c("PORT_AREA_CODE", "LANDING_YEAR"), all.x = TRUE) 

#### Merge data with SDM Market Squid (spawn aggregation) #
SDM_port_MSQD_Spawn <- read.csv(file = here::here("Data", "SDM", "MSQD_Spawn_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_YEAR, LANDING_MONTH) %>% summarize(MSQD_SPAWN_SDM_90 = mean(SDM_SPAWN_90))
PacFIN.month <- merge(PacFIN.month, SDM_port_MSQD_Spawn, 
                      by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) 

#### Merge data with SDM Northern Anchovy #
SDM_port_NANC <- read.csv(file = here::here("Data", "SDM", "NANC_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_MONTH, LANDING_YEAR) %>% summarize(NANC_SDM_20 = mean(SDM_20))
PacFIN.month <- merge(PacFIN.month, SDM_port_NANC, 
                              by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

#### Merge data with SDM Market Squid (JS Abundance model) #
SDM_port_MSQD_JS_cpue <- read.csv(file = here::here("Data", "SDM", "MSQD_SDM_port_year_JS_abund_V2.csv")) %>%
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_YEAR) %>% summarize(MSQD_SDM_90_JS_CPUE = mean(SDM_90_JS_CPUE))
PacFIN.month <- merge(PacFIN.month, SDM_port_MSQD_JS_cpue, 
                 by = c("PORT_AREA_CODE", "LANDING_YEAR"), all.x = TRUE)

#### Merge data with SDM Market Squid (JS Abundance model) #
Recruitment_port_MSQD <- read.csv(file = here::here("Data", "SDM", "MSQD_recruitmen_index.csv")) %>%
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_YEAR) %>% summarize(MSQD_recruitment = mean(Model_Predictions))
PacFIN.month.SDM <- merge(PacFIN.month, Recruitment_port_MSQD, 
                      by = c("PORT_AREA_CODE", "LANDING_YEAR"), all.x = TRUE)

#----------------------
### Merge TAC data set 
#
# # Open CSV file with TAC information and landings #
#   TAC.PSDN <- read.csv(file ="C:\\GitHub\\EconAnalysis\\Data\\ACL_data\\historical_TAC.csv") %>%
#     dplyr::rename(LANDING_YEAR = Year) %>% dplyr::rename(LANDING_MONTH = Month) %>% 
#     mutate(QuotaAllocated = 1) %>% mutate(QuotaAllocationNumber = 1) %>%
#     mutate(TAC_N = ifelse(LANDING_YEAR > 2005, 0, TAC_N)) %>% 
#     mutate(TAC_S = ifelse(LANDING_YEAR > 2005, 100, TAC_S)) %>%
#     mutate(Alloc_lat = ifelse(LANDING_YEAR > 2005, 49, Alloc_lat))
#   
#   ## Create data base ##
#   years <- as.data.frame(2000:2019) %>% dplyr::rename("LANDING_YEAR" = "2000:2019")
#   month <- as.data.frame(1:12) %>% dplyr::rename("LANDING_MONTH" = "1:12")
#   dates <- merge(month, years,  all.x = T, all.y = T) 
#   rm(years, month)
#   
#   ### Calculate total CATCH of PSDN that reduce the TAC ##
#   landings.psdn <- PacFIN.month %>% filter(LANDING_YEAR >= 2000) %>% 
#     filter(PACFIN_SPECIES_CODE == "PSDN") %>% 
#     filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D") %>%
#     group_by(LANDING_YEAR, LANDING_MONTH) %>% summarise(landings_psdn = sum(LANDED_WEIGHT_MTONS.sum))
# 
#   # Incorporate total landings in data base #
#   landings.TAC.psdn <- dates %>% merge(landings.psdn,  by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = T)
#   landings.TAC.psdn$landings_psdn[is.na(landings.TAC.psdn$landings_psdn)] = 0
#   landings.TAC.psdn <- landings.TAC.psdn %>% mutate(no_psdn_land = ifelse(landings_psdn == 0, 1, 0)) 
#   
#   # Merge TAC into database # 
#   landings.TAC.psdn <- landings.TAC.psdn %>%
#     merge(TAC.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>%
#     mutate(QuotaAllocated = ifelse(is.na(QuotaAllocated), 0, QuotaAllocated))
#   landings.TAC.psdn$Alloc_lat <- na.locf(landings.TAC.psdn$Alloc_lat)
#   landings.TAC.psdn$TAC_N     <- na.locf(landings.TAC.psdn$TAC_N)
#   landings.TAC.psdn$TAC_S     <- na.locf(landings.TAC.psdn$TAC_S)
#   landings.TAC.psdn$TAC_mt    <- na.locf(landings.TAC.psdn$TAC_mt) 
#   
#   
#   # Substract accumulative landings to TAC #
#   landings.TAC.psdn$n <- (1:nrow(landings.TAC.psdn)) 
#   landings.TAC.psdn <- landings.TAC.psdn %>% mutate(QuotaAllocationNumber = QuotaAllocationNumber * n)
#   landings.TAC.psdn$QuotaAllocationNumber <- na.locf(landings.TAC.psdn$QuotaAllocationNumber)
#   landings.TAC.psdn$QuotaAllocationNumber <- as.factor(
#     udpipe::unique_identifier(landings.TAC.psdn, fields = "QuotaAllocationNumber", start_from = 1))
#   landings.TAC.psdn <- landings.TAC.psdn %>% group_by(QuotaAllocationNumber) %>% 
#     mutate(csum = cumsum(landings_psdn)) 
#   landings.TAC.psdn <- landings.TAC.psdn %>% group_by(QuotaAllocationNumber) %>%
#     arrange(n) %>%  mutate(TAC_mt_v2 = TAC_mt - shift(csum, fill = first(0))) %>% ungroup()
#   
#   # Replace negative values by zero (fishery should be closed) #
#   landings.TAC.psdn <- landings.TAC.psdn %>% mutate(TAC_mt = ifelse(TAC_mt_v2 <= 0, 0, TAC_mt_v2)) %>%
#     select(-c("n", "csum", "TAC_mt_v2", "landings_psdn", 'no_psdn_land'))
#   
#   rm(dates, landings.psdn, TAC.PSDN)
#   #   
#   # Still landings after select just commercial fishery.
#   
#   # YEAR 2014
#   # 6966
#   # 23293
#   
#   # PERIODO 2015-2020
#   # 7000
#   # 8000
#   # 8000
#   # 7000
#   # 4000
#   # 4000
#   
# # Merge data with ACL #
# PacFIN.month <- merge(PacFIN.month, landings.TAC.psdn,
#                           by=c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)
# 
# ### Merge latitude by ports and calculate actual quota by port ###
# port_coord <- read.csv(file = here::here("C:\\GitHub\\EconAnalysis\\Data\\Ports\\Port_names.csv")) %>% 
#   select("Latitude", "Longitude", "PORT_NAME", "AGENCY_CODE") %>%
#   dplyr::rename(lat_port = Latitude) %>% 
#   dplyr::rename(lon_port = Longitude)
# 
# PacFIN.month.ACL <- merge(PacFIN.month, port_coord, by=c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE)
# 
# ### Assign quota according to port latitude ##
# PacFIN.month.ACL <- PacFIN.month.ACL %>% mutate(TAC = ifelse(lat_port <= Alloc_lat, (TAC_S * TAC_mt), (TAC_N * TAC_mt)))


# --------------------------------------------------------------------------------------
### Merge PacFIN data with results from clustering (~\Clustering\Clustering_Code_2.1.22)
PAM_Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\PAM_Vessel_Groups.csv")
PacFIN.month.cluster <- merge(PacFIN.month.SDM, PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)


# --------------------------------------------------------------------------------------
### Save DATASET


write.csv(PacFIN.month.cluster,"C:\\Data\\PacFIN data\\PacFIN_month.csv", row.names = FALSE)
