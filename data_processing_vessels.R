################################
#### PacFIN data processing #### 
################################

library(data.table)
library(dplyr)
library(distances)
library(forcats)
library(cluster)
library(ggplot2)



###################
### PacFIN data ###
###################

##---------------------------------
## Create PacFIN database ##

rm(list=ls())
gc()

# Read PacFIN data by vessels for different decades #
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)

nrow(as.data.frame(unique(Tickets$FTID))) 
nrow(as.data.frame(unique(Tickets$VESSEL_NUM))) 

# Include price per kilogram
  Tickets$AFI_PRICE_PER_MTON <- Tickets$AFI_PRICE_PER_POUND * 2.20462 * 1000
  
## --------------------------------  
## Create subsample excluding bycatch and vessels that never target a FF species
  
#Subset the data to get remove columns not relevant to this analysis. This will speed things up.
Tickets <- select(Tickets, c(FTID, VESSEL_NUM, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, 
                             AFI_PRICE_PER_MTON, LANDED_WEIGHT_MTONS, AFI_EXVESSEL_REVENUE, 
                             VESSEL_LENGTH, VESSEL_WEIGHT, VESSEL_HORSEPOWER, NUM_OF_DAYS_FISHED,
                             PACFIN_GEAR_CODE, PORT_NAME, PACFIN_PORT_CODE, LANDING_YEAR, LANDING_MONTH,  
                             AGENCY_CODE, REMOVAL_TYPE_CODE))
               
# Remove records associated with landings of zero value; this is likely bycatch 
Tickets <- Tickets[which(Tickets$AFI_EXVESSEL_REVENUE>0),]
 # 80% of the total number of tickets, from 4,596,193 tickets to 3,709,040.
  
# Find the dominant species by value of each fishing trip, presumably this is the target species. 
  ## Using the logic of metiers, all landings of each fishing trip should be tagged with a single identifier.
  Boats<-dcast(Tickets, FTID ~ PACFIN_SPECIES_COMMON_NAME, fun.aggregate=sum, value.var="AFI_EXVESSEL_REVENUE", fill=0)
  row.names(Boats) <- Boats$FTID
  FTID<-Boats$FTID
  Boats<-Boats[,-(1)]
  X<-as.data.frame(colnames(Boats)[apply(Boats,1,which.max)])
  colnames(X)<-"Dominant"
  Trip_Dominant<-as.data.frame(cbind(FTID,X))
  Tickets<-merge(Tickets, Trip_Dominant, by='FTID')
  
  ###Subset to select only records where one of the forage fish species of interest was the target species
  FF_Tickets<-Tickets[which(Tickets$Dominant=="PACIFIC SARDINE" | Tickets$Dominant=="MARKET SQUID"| 
                              Tickets$Dominant=="NORTHERN ANCHOVY"| Tickets$Dominant=="CHUB MACKEREL" |
                              Tickets$Dominant=="JACK MACKEREL"),]
  FF_Tickets<-as.data.frame(FF_Tickets)
  
  
  ###Find the list of unique vessels in the subset, these are the vessels we will cluster                           
  FF_Vessels<-as.data.frame(unique(FF_Tickets$VESSEL_NUM)) 
  names(FF_Vessels)[1]<-"VESSEL_NUM"
  FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM==""),])
  names(FF_Vessels)[1]<-"VESSEL_NUM"
  FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM=="UNKNOWN"),])
  names(FF_Vessels)[1]<-"VESSEL_NUM"
  FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM=="MISSING"),])
  names(FF_Vessels)[1]<-"VESSEL_NUM"
  
  ###Subset from the complete data set to only retain records associated with these Vessels       
  Tickets<-setDT(Tickets)[VESSEL_NUM %chin% FF_Vessels$VESSEL_NUM]            
  Tickets<-as.data.frame(Tickets)

  rm(Boats, FF_Tickets, FF_Vessels, Trip_Dominant, X, FTID)
  
  nrow(as.data.frame(unique(Tickets$FTID))) 
  nrow(as.data.frame(unique(Tickets$VESSEL_NUM))) 
  
  #removal
  removal.df <- Tickets %>% select("REMOVAL_TYPE_CODE") %>% mutate(n = 1) %>% 
    group_by(REMOVAL_TYPE_CODE) %>% summarise(n_group = sum(n))

  
  head(as.data.frame(unique(removal.df$REMOVAL_TYPE_CODE))) 


# Create monthly data
  
  sum_mean_fun <- function(x, ...){
    c(mean=mean(x, na.rm=TRUE, ...), sum=sum(x, na.rm=TRUE, ...)) }
  
  PacFIN.month <- doBy::summaryBy(AFI_PRICE_PER_MTON + LANDED_WEIGHT_MTONS + AFI_EXVESSEL_REVENUE + 
                              VESSEL_LENGTH + VESSEL_WEIGHT + VESSEL_HORSEPOWER + NUM_OF_DAYS_FISHED 
                            ~ VESSEL_NUM + PACFIN_SPECIES_CODE + PACFIN_GEAR_CODE + 
                              PORT_NAME + PACFIN_PORT_CODE + LANDING_YEAR + LANDING_MONTH +  
                              AGENCY_CODE + REMOVAL_TYPE_CODE + PARTICIPATION_GROUP_CODE,
                          FUN=sum_mean_fun, data=Tickets)
rm(Tickets)
  
#######################
## Merge SDM dataset ##
#######################
  
# # Create database with ports that land FF species
# Ports.landing.FF <- PacFIN.month %>%
#     dplyr::filter(PACFIN_SPECIES_CODE %in%
#                     c("CMCK", "JMCK", "MSQD", "NANC", "PSDN", "UMCK")) %>%
#     select("PORT_NAME", "AGENCY_CODE") %>% unique()
# 
#   write.csv(Ports.landing.FF,"C:\\GitHub\\EconAnalysis\\Data\\Ports\\Ports.landing.FF.csv", row.names = FALSE)

# Merge data with SDM Pacific Sardine #
SDM_port_PSDN <- read.csv(file = here::here("Data", "SDM", "PSDN_SDM_port_month.csv"))
  PacFIN.month <- merge(PacFIN.month, SDM_port_PSDN, 
                        by = c("PORT_NAME", "AGENCY_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

# Merge data with SDM Market Squid (spawn aggregation) #
SDM_port_MSQD_Spawn <- read.csv(file = here::here("Data", "SDM", "MSQD_Spawn_SDM_port_month.csv"))
  PacFIN.month <- merge(PacFIN.month, SDM_port_MSQD_Spawn, 
                        by = c("PORT_NAME", "AGENCY_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) 

# Merge data with SDM Market Squid #
SDM_port_MSQD <- read.csv(file = here::here("Data", "SDM", "MSQD_SDM_port_month.csv"))
  PacFIN.month <- merge(PacFIN.month, SDM_port_MSQD, 
                        by = c("PORT_NAME", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

# Merge data with SDM Northern Anchovy #
SDM_port_NANC <- read.csv(file = here::here("Data", "SDM", "NANC_SDM_port_month.csv"))
  PacFIN.month <- merge(PacFIN.month, SDM_port_NANC, 
                        by = c("PORT_NAME", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

  
PacFIN.month.SDM <- PacFIN.month %>%
    dplyr::rename(PSDN_SDM_60 = SDM_60) %>%
    dplyr::rename(MSQD_SDM_90 = SDM_90) %>%
    dplyr::rename(MSQD_SPAWN_SDM_90 = SDM_SPAWN_90) %>%
    dplyr::rename(MSQD_SPAWN_SDM_100 = SDM_SPAWN_100) %>%
    dplyr::rename(MSQD_SPAWN_SDM_200 = SDM_SPAWN_200) %>%
    dplyr::rename(MSQD_SPAWN_SDM_300 = SDM_SPAWN_300) %>%
    dplyr::rename(MSQD_SPAWN_SDM_5_100 = SDM_SPAWN_5_100) %>%
    dplyr::rename(NANC_SDM_20 = SDM_20)


# #######################
# ## Merge TAC dataset ##
# #######################
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
#   
#   
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
# 
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




####################
### Save DATASET ###
####################
  
write.csv(PacFIN.month.SDM,"C:\\Data\\PacFIN data\\PacFIN_month.csv", row.names = FALSE)
