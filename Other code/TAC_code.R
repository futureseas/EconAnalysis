
library(dplyr)
library(zoo)
library(data.table)

# Open CSV file with TAC information and landings #
TAC.PSDN <- read.csv(file ="C:\\GitHub\\EconAnalysis\\Data\\ACL_data\\historical_TAC.csv") %>%
  dplyr::rename(LANDING_YEAR = Year) %>% dplyr::rename(LANDING_MONTH = Month) %>% 
  mutate(QuotaAllocated = 1) %>% mutate(QuotaAllocationNumber = 1) %>%
  mutate(TAC_N = ifelse(LANDING_YEAR > 2005, 0, TAC_N)) %>% 
  mutate(TAC_S = ifelse(LANDING_YEAR > 2005, 100, TAC_S)) %>%
  mutate(Alloc_lat = ifelse(LANDING_YEAR > 2005, 49, Alloc_lat))

PacFIN.month <- read.csv(file ="C:\\Data\\PacFIN data\\PacFIN_month.csv") 

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
  rm(PacFIN.month)

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
landings.TAC.psdn <- landings.TAC.psdn %>% mutate(TAC_mt_accum = ifelse(TAC_mt_v2 <= 0, 0, TAC_mt_v2)) %>%
  select(-c("n", "csum", "TAC_mt_v2", "landings_psdn", 'no_psdn_land'))

write.csv(landings.TAC.psdn,"C:\\GitHub\\EconAnalysis\\Other code\\TAC_month.csv", row.names = FALSE)
  
 
# Still landings after select just commercial fishery.
 
# AÑO 2014
# 6966
# 23293
 
# PERIODO 2015-2020
 # 7000
 # 8000
 # 8000
 # 7000
 # 4000
 # 4000
 
 


