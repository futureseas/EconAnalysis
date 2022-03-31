#####################
### Landing model ###
#####################

#----------------------------
# Setup #
rm(list = ls(all.names = TRUE)) 
gc()

## Read packages 

library("tidyr")
library("dplyr") 
library("data.table") 
library("reshape2")


## Read PacFIN database 
PacFIN.month <- read.csv(file ="C:\\Data\\PacFIN data\\PacFIN_month.csv")


# Database of port area code and port names
ports_area_codes <- PacFIN.month %>% dplyr::select('PORT_NAME', 'AGENCY_CODE', 'PORT_AREA_CODE') %>% unique()

# ## Select port that have land CPS at least one
# Ports.landing.FF <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\Ports\\Ports.landing.FF.csv") %>% mutate(CPS.landing = 1)
#   
# PacFIN.month <- merge(PacFIN.month, Ports.landing.FF, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) 
#   PacFIN.month <- PacFIN.month %>% filter(CPS.landing == 1)
# 
# port.areas <-  PacFIN.month %>% dplyr::select('PORT_AREA_CODE') %>% unique()
# rm(port.areas, Ports.landing.FF)

#---------------------------------------------------
## Construct monthly database
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "CMCK"] <- "OMCK")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "JMCK"] <- "OMCK")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "UMCK"] <- "OMCK")
PacFIN.month <- PacFIN.month %>% mutate(
  PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "OMCK",PACFIN_SPECIES_CODE, 
                               ifelse(PACFIN_SPECIES_CODE == "PSDN",PACFIN_SPECIES_CODE, 
                                      ifelse(PACFIN_SPECIES_CODE == "MSQD", PACFIN_SPECIES_CODE, 
                                             ifelse(PACFIN_SPECIES_CODE == "NANC", PACFIN_SPECIES_CODE, "OTHER")))))

sum_mean_fun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...), sum=sum(x, na.rm=TRUE, ...)) }

library(doBy)
PacFIN.month.aggregate <- doBy::summaryBy(LANDED_WEIGHT_MTONS.sum + AFI_PRICE_PER_MTON.mean ~ LANDING_YEAR + 
                                  LANDING_MONTH + VESSEL_NUM + PORT_AREA_CODE + PACFIN_SPECIES_CODE +
                                    AGENCY_CODE + group_all, FUN=sum_mean_fun, data=PacFIN.month)

PacFIN.month.aggregate <- PacFIN.month.aggregate %>% 
  dplyr::select(LANDING_YEAR, LANDING_MONTH, VESSEL_NUM,
                LANDED_WEIGHT_MTONS.sum.sum, AFI_PRICE_PER_MTON.mean.mean, 
                PACFIN_SPECIES_CODE, AGENCY_CODE, group_all, PORT_AREA_CODE) %>%
  dplyr::rename(AFI_PRICE_PER_MTON.mean = AFI_PRICE_PER_MTON.mean.mean) %>%
  dplyr::rename(LANDED_WEIGHT_MTONS.sum = LANDED_WEIGHT_MTONS.sum.sum) %>%
  mutate(AFI_PRICE_PER_MTON.mean = na_if(AFI_PRICE_PER_MTON.mean, 0)) %>% 
  filter(group_all != is.na(group_all)) 

PacFIN.month.dataset <- PacFIN.month.aggregate %>%
  reshape2::melt(id.vars=c("LANDING_YEAR", "LANDING_MONTH", "VESSEL_NUM", 
                 "PACFIN_SPECIES_CODE", "AGENCY_CODE", 'PORT_AREA_CODE', "group_all")) %>% 
  reshape2::dcast(LANDING_YEAR + LANDING_MONTH + VESSEL_NUM + AGENCY_CODE + PORT_AREA_CODE + group_all ~
          PACFIN_SPECIES_CODE + variable, fun.aggregate=mean, rm.na = T) %>%
  dplyr::select('LANDING_YEAR', 'LANDING_MONTH', 'VESSEL_NUM', 
                'PSDN_LANDED_WEIGHT_MTONS.sum', 'MSQD_LANDED_WEIGHT_MTONS.sum', 'NANC_LANDED_WEIGHT_MTONS.sum',  
                'PSDN_AFI_PRICE_PER_MTON.mean', 'MSQD_AFI_PRICE_PER_MTON.mean', 'NANC_AFI_PRICE_PER_MTON.mean', 
                'AGENCY_CODE', 'PORT_AREA_CODE', 'group_all')

rm(PacFIN.month.aggregate)

### Select port areas ###
# selected.ports <- PacFIN.month.dataset %>% 
#   group_by(PORT_AREA_CODE) %>% 
#   summarise(landings_PSDN = sum(PSDN_LANDED_WEIGHT_MTONS.sum, na.rm = TRUE),
#             landings_MSQD = sum(MSQD_LANDED_WEIGHT_MTONS.sum, na.rm = TRUE),
#             landings_NANC = sum(NANC_LANDED_WEIGHT_MTONS.sum, na.rm = TRUE)) %>%
#   rowwise() %>% mutate(total = min(landings_PSDN, landings_MSQD)) %>%
#   filter(total > 0) %>% dplyr::select(PORT_AREA_CODE) %>% mutate(port_included = 1)
# PacFIN.month.dataset <- PacFIN.month.dataset %>% 
#   merge(selected.ports, by = c("PORT_AREA_CODE"), all.x = TRUE) %>%
#   filter(port_included == 1) 


### Selecting port using results from cluster analysis (15% of the revenue for at least one cluster)
PacFIN.month.dataset <- PacFIN.month.dataset %>%
  dplyr::filter(PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "MNA" |
         PORT_AREA_CODE == "CLO" | PORT_AREA_CODE == "CLW" | PORT_AREA_CODE == "CWA" | 
           PORT_AREA_CODE == "SFA" | PORT_AREA_CODE == "SDA")


### Create ID data and change NaN to NA ###
PacFIN.month.dataset <- PacFIN.month.dataset %>%
  mutate(PORT_AREA_ID = as.numeric(as.factor(PORT_AREA_CODE))) 
PacFIN.month.dataset[PacFIN.month.dataset == "NaN"] <- NA


### Merge SDM by year/month/port ###


#### Merge data with SDM Pacific Sardine
SDM_port_PSDN <- read.csv(file = here::here("Data", "SDM", "PSDN_SDM_port_month.csv")) %>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_MONTH, LANDING_YEAR) %>% summarize(PSDN_SDM_60 = mean(SDM_60))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_PSDN,
                      by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

#### Merge data with SDM Market Squid (spawn aggregation) #
SDM_port_MSQD_Spawn <- read.csv(file = here::here("Data", "SDM", "MSQD_Spawn_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  dplyr::filter(LANDING_MONTH == 8 | LANDING_MONTH == 9 | LANDING_MONTH == 10) %>% 
  group_by(PORT_AREA_CODE, LANDING_YEAR) %>% summarize(MSQD_SPAWN_SDM_90_v2 = mean(SDM_SPAWN_90))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_MSQD_Spawn, 
                      by = c("PORT_AREA_CODE", "LANDING_YEAR"), all.x = TRUE) 

#### Merge data with SDM Market Squid (spawn aggregation) #
SDM_port_MSQD_Spawn <- read.csv(file = here::here("Data", "SDM", "MSQD_Spawn_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_YEAR, LANDING_MONTH) %>% summarize(MSQD_SPAWN_SDM_90 = mean(SDM_SPAWN_90))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_MSQD_Spawn, 
                      by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) 

#### Merge data with SDM Northern Anchovy #
SDM_port_NANC <- read.csv(file = here::here("Data", "SDM", "NANC_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_MONTH, LANDING_YEAR) %>% summarize(NANC_SDM_20 = mean(SDM_20))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_NANC, 
                      by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

#### Merge data with SDM Market Squid (JS Abundance model) #
SDM_port_MSQD_JS_cpue <- read.csv(file = here::here("Data", "SDM", "MSQD_SDM_port_year_JS_abund_V2.csv")) %>%
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_YEAR) %>% summarize(MSQD_SDM_90_JS_CPUE = mean(SDM_90_JS_CPUE))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_MSQD_JS_cpue, 
                      by = c("PORT_AREA_CODE", "LANDING_YEAR"), all.x = TRUE)

#### Merge data with Market Squid recruitment #
Recruitment_port_MSQD <- read.csv(file = here::here("Data", "SDM", "MSQD_recruitmen_index.csv")) %>%
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_YEAR) %>% summarize(MSQD_recruitment = mean(Model_Predictions))
dataset <- merge(PacFIN.month.dataset, Recruitment_port_MSQD, 
                          by = c("PORT_AREA_CODE", "LANDING_YEAR"), all.x = TRUE)

rm(PacFIN.month.dataset, 
   SDM_port_PSDN, SDM_port_NANC, SDM_port_MSQD_Spawn, SDM_port_MSQD_JS_cpue, Recruitment_port_MSQD,
   ports_area_codes)


#-----------------------------------------------------------------
## Calculate SDM for N/A's using PacFIN port code and port area code.

### Pacific sardine

# #### (a) Using PacFIN port code
# SDM.port.code.PSDN <- aggregate(x=dataset$PSDN_SDM_60, 
#                                 by = list(dataset$LANDING_YEAR, 
#                                           dataset$LANDING_MONTH, 
#                                           dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
# SDM.port.code.PSDN <- SDM.port.code.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
#   dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
#   dplyr::rename(PSDN.SDM.port.code = x)
# SDM.port.code.PSDN[SDM.port.code.PSDN == "NaN"] <- NA
# dataset <- dataset %>%
#   merge(SDM.port.code.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>% 
#   mutate(PSDN_SDM_60 = ifelse(is.na(PSDN_SDM_60), PSDN.SDM.port.code, PSDN_SDM_60))
# rm(SDM.port.code.PSDN)

#### (b) Using port area code
SDM.port.area.PSDN <- aggregate(x=dataset$PSDN_SDM_60,
                                by = list(dataset$LANDING_YEAR,
                                          dataset$LANDING_MONTH,
                                          dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.PSDN <- SDM.port.area.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(PSDN.SDM.port.area = x)
SDM.port.area.PSDN[SDM.port.area.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(PSDN_SDM_60 = ifelse(is.na(PSDN_SDM_60), PSDN.SDM.port.area, PSDN_SDM_60))
rm(SDM.port.area.PSDN)

dataset = subset(dataset, select = -c(PSDN.SDM.port.area))


### Northern anchovy

# #### (a) Using PacFIN port code
# SDM.port.code.NANC <- aggregate(x=dataset$NANC_SDM_20, 
#                                 by = list(dataset$LANDING_YEAR, 
#                                           dataset$LANDING_MONTH, 
#                                           dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
# SDM.port.code.NANC <- SDM.port.code.NANC %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
#   dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
#   dplyr::rename(NANC.SDM.port.code = x)
# SDM.port.code.NANC[SDM.port.code.NANC == "NaN"] <- NA
# dataset <- dataset %>%
#   merge(SDM.port.code.NANC, by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>% 
#   mutate(NANC_SDM_20 = ifelse(is.na(NANC_SDM_20), NANC.SDM.port.code, NANC_SDM_20))
# rm(SDM.port.code.NANC)

#### (b) Using port area code
SDM.port.area.NANC <- aggregate(x=dataset$NANC_SDM_20,
                                by = list(dataset$LANDING_YEAR,
                                          dataset$LANDING_MONTH,
                                          dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.NANC <- SDM.port.area.NANC %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(NANC.SDM.port.area = x)
SDM.port.area.NANC[SDM.port.area.NANC == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.NANC, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(NANC_SDM_20 = ifelse(is.na(NANC_SDM_20), NANC.SDM.port.area, NANC_SDM_20))
rm(SDM.port.area.NANC)

dataset = subset(dataset, select = -c(NANC.SDM.port.area))


### Market squid (SPAWN)

# #### (a) Using PacFIN port code
# SDM.port.code.MSQD_SPAWN <- aggregate(x=dataset$MSQD_SPAWN_SDM_90, 
#                                       by = list(dataset$LANDING_YEAR,
#                                                 dataset$LANDING_MONTH, 
#                                                 dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
# SDM.port.code.MSQD_SPAWN <- SDM.port.code.MSQD_SPAWN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
#   dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
#   dplyr::rename(MSQD_SPAWN.SDM.port.code = x)
# SDM.port.code.MSQD_SPAWN[SDM.port.code.MSQD_SPAWN == "NaN"] <- NA
# dataset <- dataset %>%
#   merge(SDM.port.code.MSQD_SPAWN, by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>%
#   mutate(MSQD_SPAWN_SDM_90 = ifelse(is.na(MSQD_SPAWN_SDM_90), MSQD_SPAWN.SDM.port.code, MSQD_SPAWN_SDM_90))
# rm(SDM.port.code.MSQD_SPAWN)

#### (b) Using port area code
SDM.port.area.MSQD_SPAWN <- aggregate(x=dataset$MSQD_SPAWN_SDM_90, 
                                      by = list(dataset$LANDING_YEAR,
                                                dataset$LANDING_MONTH, 
                                                dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.MSQD_SPAWN <- SDM.port.area.MSQD_SPAWN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(MSQD_SPAWN.SDM.port.area = x)
SDM.port.area.MSQD_SPAWN[SDM.port.area.MSQD_SPAWN == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.MSQD_SPAWN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(MSQD_SPAWN_SDM_90 = ifelse(is.na(MSQD_SPAWN_SDM_90), MSQD_SPAWN.SDM.port.area, MSQD_SPAWN_SDM_90))
rm(SDM.port.area.MSQD_SPAWN)
dataset = subset(dataset, select = -c(MSQD_SPAWN.SDM.port.area))


### Market squid (JS abundance)

# #### (a) Using PacFIN port code
# SDM.port.code.MSQD_JS_cpue <- aggregate(x=dataset$MSQD_SDM_90_JS_cpue, 
#                                         by = list(dataset$LANDING_YEAR,
#                                                   dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
# SDM.port.code.MSQD_JS_cpue <- SDM.port.code.MSQD_JS_cpue %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
#   dplyr::rename(PACFIN_PORT_CODE = Group.2) %>%  dplyr::rename(MSQD_JS_cpue.SDM.port.code = x)
# SDM.port.code.MSQD_JS_cpue[SDM.port.code.MSQD_JS_cpue == "NaN"] <- NA
# dataset <- dataset %>%
#   merge(SDM.port.code.MSQD_JS_cpue, by = c("LANDING_YEAR", "PACFIN_PORT_CODE"), all.x = TRUE) %>%
#   mutate(MSQD_SDM_90_JS_cpue = ifelse(is.na(MSQD_SDM_90_JS_cpue), MSQD_JS_cpue.SDM.port.code, MSQD_SDM_90_JS_cpue))
# rm(SDM.port.code.MSQD_JS_cpue)

#### (b) Using port area code
SDM.port.area.MSQD_JS_cpue <- aggregate(x=dataset$MSQD_SDM_90_JS_CPUE, 
                                        by = list(dataset$LANDING_YEAR, 
                                                  dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.MSQD_JS_cpue <- SDM.port.area.MSQD_JS_cpue %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(PORT_AREA_CODE = Group.2) %>% dplyr::rename(MSQD_JS_cpue.SDM.port.area = x)
SDM.port.area.MSQD_JS_cpue[SDM.port.area.MSQD_JS_cpue == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.MSQD_JS_cpue, by = c("LANDING_YEAR", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(MSQD_SDM_90_JS_cpue = ifelse(is.na(MSQD_SDM_90_JS_CPUE), MSQD_JS_cpue.SDM.port.area, MSQD_SDM_90_JS_CPUE))
rm(SDM.port.area.MSQD_JS_cpue)
dataset = subset(dataset, select = -c(MSQD_JS_cpue.SDM.port.area))



#----------------------------------------------------------------------------------------------
## Calculate year price for N/A's using PacFIN port code, port area code, state and year/month.

### Pacific sardine

# #### (a) Using port name
# price.port.name.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_MTON.mean, 
#                          by = list(dataset$LANDING_YEAR, dataset$LANDING_MONTH, dataset$PORT_NAME), FUN = mean, na.rm=T)
# price.port.name.PSDN <- price.port.name.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
#   dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_NAME = Group.3) %>%
#   dplyr::rename(PSDN.price.port.name = x)
# price.port.name.PSDN[price.port.name.PSDN == "NaN"] <- NA
# dataset <- dataset %>% 
#   merge(price.port.name.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_NAME"), all.x = TRUE) %>%
#   mutate(PSDN_AFI_PRICE_PER_MTON.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_MTON.mean), 
#                                              PSDN.price.port.name, PSDN_AFI_PRICE_PER_MTON.mean))
# rm(price.port.name.PSDN)
# 
# #### (b) Using PacFIN port code
# price.port.code.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_MTON.mean, 
#                          by = list(dataset$LANDING_YEAR, dataset$LANDING_MONTH, dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
# price.port.code.PSDN <- price.port.code.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
#   dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
#   dplyr::rename(PSDN.price.port.code = x)
# price.port.code.PSDN[price.port.code.PSDN == "NaN"] <- NA
# dataset <- dataset %>%
#   merge(price.port.code.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>% 
#   mutate(PSDN_AFI_PRICE_PER_MTON.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_MTON.mean), 
#                                              PSDN.price.port.code, PSDN_AFI_PRICE_PER_MTON.mean))
# rm(price.port.code.PSDN)

#### (c) Using port area code
price.port.area.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_MTON.mean, 
                                  by = list(dataset$LANDING_YEAR, dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
price.port.area.PSDN <- price.port.area.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(PSDN.price.port.area = x)
price.port.area.PSDN[price.port.area.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.area.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_MTON.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_MTON.mean), 
                                             PSDN.price.port.area, PSDN_AFI_PRICE_PER_MTON.mean))
rm(price.port.area.PSDN)

#### (d) Using state
price.state.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                              dataset$LANDING_MONTH, dataset$AGENCY_CODE), FUN = mean, na.rm=T)
price.state.PSDN <- price.state.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(AGENCY_CODE = Group.3) %>%
  dplyr::rename(PSDN.price.state = x)
price.state.PSDN[price.state.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.state.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "AGENCY_CODE"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_MTON.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_MTON.mean), 
                                             PSDN.price.state, PSDN_AFI_PRICE_PER_MTON.mean))
rm(price.state.PSDN)

#### (e) Using year/month
price.year.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                             dataset$LANDING_MONTH), FUN = mean, na.rm=T)
price.year.PSDN <- price.year.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PSDN.price.year.month = x)
price.year.PSDN[price.year.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.year.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_MTON.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_MTON.mean), 
                                             PSDN.price.year.month, PSDN_AFI_PRICE_PER_MTON.mean))
rm(price.year.PSDN)

dataset = subset(dataset, select = 
                   -c(PSDN.price.port.area, PSDN.price.state, PSDN.price.year.month))


### Market squid

# #### (a) Using port name
# price.port.name.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
#                                                                                   dataset$LANDING_MONTH, dataset$PORT_NAME), FUN = mean, na.rm=T)
# price.port.name.MSQD <- price.port.name.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
#   dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_NAME = Group.3) %>%
#   dplyr::rename(MSQD.price.port.name = x)
# price.port.name.MSQD[price.port.name.MSQD == "NaN"] <- NA
# dataset <- dataset %>% 
#   merge(price.port.name.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_NAME"), all.x = TRUE) %>%
#   mutate(MSQD_AFI_PRICE_PER_MTON.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_MTON.mean), 
#                                              MSQD.price.port.name, MSQD_AFI_PRICE_PER_MTON.mean))
# rm(price.port.name.MSQD)
# 
# #### (b) Using PacFIN port code
# price.port.code.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
#                                                                                   dataset$LANDING_MONTH, dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
# price.port.code.MSQD <- price.port.code.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
#   dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
#   dplyr::rename(MSQD.price.port.code = x)
# price.port.code.MSQD[price.port.code.MSQD == "NaN"] <- NA
# dataset <- dataset %>%
#   merge(price.port.code.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>% 
#   mutate(MSQD_AFI_PRICE_PER_MTON.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_MTON.mean), 
#                                              MSQD.price.port.code, MSQD_AFI_PRICE_PER_MTON.mean))
# rm(price.port.code.MSQD)

#### (c) Using port area code
price.port.area.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
price.port.area.MSQD <- price.port.area.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(MSQD.price.port.area = x)
price.port.area.MSQD[price.port.area.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.area.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_MTON.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_MTON.mean), 
                                             MSQD.price.port.area, MSQD_AFI_PRICE_PER_MTON.mean))
rm(price.port.area.MSQD)

#### (d) Using state
price.state.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                              dataset$LANDING_MONTH, dataset$AGENCY_CODE), FUN = mean, na.rm=T)
price.state.MSQD <- price.state.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(AGENCY_CODE = Group.3) %>%
  dplyr::rename(MSQD.price.state = x)
price.state.MSQD[price.state.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.state.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "AGENCY_CODE"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_MTON.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_MTON.mean), 
                                             MSQD.price.state, MSQD_AFI_PRICE_PER_MTON.mean))
rm(price.state.MSQD)

#### (e) Using year/month
price.year.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                             dataset$LANDING_MONTH), FUN = mean, na.rm=T)
price.year.MSQD <- price.year.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(MSQD.price.year.month = x)
price.year.MSQD[price.year.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.year.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_MTON.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_MTON.mean), 
                                             MSQD.price.year.month, MSQD_AFI_PRICE_PER_MTON.mean))
rm(price.year.MSQD)

dataset = subset(dataset, select = 
                   -c(MSQD.price.port.area, MSQD.price.state, MSQD.price.year.month))


### Northern anchovy

# #### (a) Using port name
# price.port.name.NANC <- aggregate(x=dataset$NANC_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
#                                                                                   dataset$LANDING_MONTH, dataset$PORT_NAME), FUN = mean, na.rm=T)
# price.port.name.NANC <- price.port.name.NANC %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
#   dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_NAME = Group.3) %>%
#   dplyr::rename(NANC.price.port.name = x)
# price.port.name.NANC[price.port.name.NANC == "NaN"] <- NA
# dataset <- dataset %>% 
#   merge(price.port.name.NANC, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_NAME"), all.x = TRUE) %>%
#   mutate(NANC_AFI_PRICE_PER_MTON.mean = ifelse(is.na(NANC_AFI_PRICE_PER_MTON.mean), 
#                                              NANC.price.port.name, NANC_AFI_PRICE_PER_MTON.mean))
# rm(price.port.name.NANC)
# 
# #### (b) Using PacFIN port code
# price.port.code.NANC <- aggregate(x=dataset$NANC_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
#                                                                                   dataset$LANDING_MONTH, dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
# price.port.code.NANC <- price.port.code.NANC %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
#   dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
#   dplyr::rename(NANC.price.port.code = x)
# price.port.code.NANC[price.port.code.NANC == "NaN"] <- NA
# dataset <- dataset %>%
#   merge(price.port.code.NANC, by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>% 
#   mutate(NANC_AFI_PRICE_PER_MTON.mean = ifelse(is.na(NANC_AFI_PRICE_PER_MTON.mean), 
#                                              NANC.price.port.code, NANC_AFI_PRICE_PER_MTON.mean))
# rm(price.port.code.NANC)

#### (c) Using port area code
price.port.area.NANC <- aggregate(x=dataset$NANC_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
price.port.area.NANC <- price.port.area.NANC %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(NANC.price.port.area = x)
price.port.area.NANC[price.port.area.NANC == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.area.NANC, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>% 
  mutate(NANC_AFI_PRICE_PER_MTON.mean = ifelse(is.na(NANC_AFI_PRICE_PER_MTON.mean), 
                                             NANC.price.port.area, NANC_AFI_PRICE_PER_MTON.mean))
rm(price.port.area.NANC)

#### (d) Using state
price.state.NANC <- aggregate(x=dataset$NANC_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                              dataset$LANDING_MONTH, dataset$AGENCY_CODE), FUN = mean, na.rm=T)
price.state.NANC <- price.state.NANC %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(AGENCY_CODE = Group.3) %>%
  dplyr::rename(NANC.price.state = x)
price.state.NANC[price.state.NANC == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.state.NANC, by = c("LANDING_YEAR", "LANDING_MONTH", "AGENCY_CODE"), all.x = TRUE) %>% 
  mutate(NANC_AFI_PRICE_PER_MTON.mean = ifelse(is.na(NANC_AFI_PRICE_PER_MTON.mean), 
                                             NANC.price.state, NANC_AFI_PRICE_PER_MTON.mean))
rm(price.state.NANC)

#### (e) Using year/month
price.year.NANC <- aggregate(x=dataset$NANC_AFI_PRICE_PER_MTON.mean, by = list(dataset$LANDING_YEAR, 
                                                                             dataset$LANDING_MONTH), FUN = mean, na.rm=T)
price.year.NANC <- price.year.NANC %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(NANC.price.year.month = x)
price.year.NANC[price.year.NANC == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.year.NANC, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>% 
  mutate(NANC_AFI_PRICE_PER_MTON.mean = ifelse(is.na(NANC_AFI_PRICE_PER_MTON.mean), 
                                             NANC.price.year.month, NANC_AFI_PRICE_PER_MTON.mean))
rm(price.year.NANC)

dataset = subset(dataset, select = 
                   -c(NANC.price.port.area, NANC.price.state, NANC.price.year.month))


#---------------------------------------
## Summary statistics ##
dataset <- dataset %>% 
  dplyr::rename(PSDN_Price = PSDN_AFI_PRICE_PER_MTON.mean) %>%
  dplyr::rename(MSQD_Price = MSQD_AFI_PRICE_PER_MTON.mean) %>%
  dplyr::rename(NANC_Price = NANC_AFI_PRICE_PER_MTON.mean) %>%
  dplyr::rename(PSDN_Landings = PSDN_LANDED_WEIGHT_MTONS.sum) %>%
  dplyr::rename(MSQD_Landings = MSQD_LANDED_WEIGHT_MTONS.sum) %>%
  dplyr::rename(NANC_Landings = NANC_LANDED_WEIGHT_MTONS.sum)

### Label dataset ###
sjlabelled::set_label(dataset$MSQD_SDM_90_JS_cpue) <- "Abundance: MSQD (CPUE)"
sjlabelled::set_label(dataset$MSQD_SPAWN_SDM_90)   <- "Prob(presence): MSQD (Spawning aggregations)"
sjlabelled::set_label(dataset$NANC_SDM_20)         <- "Prob(presence): NANC"
sjlabelled::set_label(dataset$PSDN_SDM_60)         <- "Prob(presence): PSDN"
sjlabelled::set_label(dataset$LANDING_YEAR)     <- "Year"
sjlabelled::set_label(dataset$LANDING_MONTH)    <- "Month"
sjlabelled::set_label(dataset$PORT_AREA_CODE)   <- "Port area code"
sjlabelled::set_label(dataset$AGENCY_CODE)      <- "State"
sjlabelled::set_label(dataset$VESSEL_NUM)       <- "Vessel ID"
sjlabelled::set_label(dataset$MSQD_Landings) <- "Landings: MSQD"
sjlabelled::set_label(dataset$MSQD_Price)    <- "Price: MSQD"
sjlabelled::set_label(dataset$PSDN_Landings) <- "Landings: PSDN"
sjlabelled::set_label(dataset$PSDN_Price)    <- "Price: PSDN"
sjlabelled::set_label(dataset$NANC_Landings) <- "Landings: NANC"
sjlabelled::set_label(dataset$NANC_Price)    <- "Price: NANC"
sjlabelled::set_label(dataset$PORT_ID)       <- "Port ID"


### Monthly data ###
desc_data <- dataset %>%
  subset(select = -c(PORT_AREA_ID, VESSEL_NUM, group_all, LANDING_YEAR, LANDING_MONTH, AGENCY_CODE, PORT_AREA_CODE))

# sjlabelled::set_label(desc_data$MSQD_SDM_90_JS_cpue) <- "Prob(presence): MSQD (CPUE)"
# sjlabelled::set_label(desc_data$MSQD_SPAWN_SDM_90)   <- "Prob(presence): MSQD (Spawning aggregations)"
# sjlabelled::set_label(desc_data$PSDN_SDM_60)   <- "Prob(presence): PSDN"
# sjlabelled::set_label(desc_data$NANC_SDM_20)   <- "Prob(presence): NANC"
# sjlabelled::set_label(desc_data$MSQD_Landings) <- "Landings: MSQD"
# sjlabelled::set_label(desc_data$MSQD_Price)    <- "Price: MSQD"
# sjlabelled::set_label(desc_data$PSDN_Landings) <- "Landings: PSDN"
# sjlabelled::set_label(desc_data$PSDN_Price)    <- "Price: PSDN"
# sjlabelled::set_label(desc_data$NANC_Landings) <- "Landings: NANC"
# sjlabelled::set_label(desc_data$NANC_Price)    <- "Price: NANC"
# vtable::st(desc_data, labels = TRUE, title='Summary Statistics')

table <- psych::describe(desc_data, fast=TRUE) %>%
  mutate(vars = ifelse(vars == 1, "Landings: PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 2, "Landings: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 3, "Landings: NANC", vars)) %>%
  mutate(vars = ifelse(vars == 4, "Price: PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 5, "Price: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 6, "Price: NANC", vars)) %>%
  mutate(vars = ifelse(vars == 7, "Prob(presence): PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 8, "Prob(presence): MSQD -- Aug-Oct", vars)) %>%
  mutate(vars = ifelse(vars == 9, "Prob(presence): MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 10, "Prob(presence): NANC", vars)) %>%
  mutate(vars = ifelse(vars == 11, "Abundance: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 12, "Recruitment: MSQD", vars))


# library("googlesheets4")
# gs4_auth(
#   email = gs4_auth(),
#   path = NULL,
#   scopes = "https://www.googleapis.com/auth/spreadsheets",
#   cache = gargle::gargle_oauth_cache(),
#   use_oob = gargle::gargle_oob_default(),
#   token = NULL)
# 
# gs4_create("SummaryMonthly", sheets = table)
rm(desc_data, table)
  

# ### Quarterly data ###
# library("zoo")
# dataset$MonthYear <- as.yearmon(paste(dataset$LANDING_YEAR, dataset$LANDING_MONTH), "%Y %m")
# dataset$QuarterYear <- as.yearqtr(dataset$MonthYear, format = "%Y-%m")
# 
# dataset_quarter <- dataset %>% 
#   group_by(QuarterYear, LANDING_YEAR, VESSEL_NUM, group_all, PORT_AREA_ID, PORT_AREA_CODE) %>%
#   summarise(PSDN_Landings = sum(PSDN_Landings, na.rm=T), 
#             MSQD_Landings = sum(MSQD_Landings, na.rm=T), 
#             NANC_Landings = sum(NANC_Landings, na.rm=T), 
#             PSDN_Price = mean(PSDN_Price, na.rm=T), 
#             MSQD_Price = mean(MSQD_Price, na.rm=T),
#             NANC_Price = mean(NANC_Price, na.rm=T),
#             PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm=T), 
#             MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm=T),
#             NANC_SDM_20 = mean(NANC_SDM_20, na.rm=T),
#             MSQD_SDM_90_JS_cpue = mean(MSQD_SDM_90_JS_cpue, na.rm=T))
#   dataset_quarter[dataset_quarter == "NaN"] <- NA
# 
# desc_data <- dataset_quarter %>%
#   subset(select = -c(PORT_AREA_ID, PORT_AREA_CODE, VESSEL_NUM, group_all, LANDING_YEAR, QuarterYear))
# 
# table <- psych::describe(desc_data, fast=TRUE) %>%
#   mutate(vars = ifelse(vars == 1, "Landings: PSDN", vars)) %>%
#   mutate(vars = ifelse(vars == 2, "Landings: MSQD", vars)) %>%
#   mutate(vars = ifelse(vars == 3, "Landings: NANC", vars)) %>%
#   mutate(vars = ifelse(vars == 4, "Price: PSDN", vars)) %>%
#   mutate(vars = ifelse(vars == 5, "Price: MSQD", vars)) %>%
#   mutate(vars = ifelse(vars == 6, "Price: NANC", vars)) %>%
#   mutate(vars = ifelse(vars == 7, "Prob(presence): PSDN", vars)) %>%
#   mutate(vars = ifelse(vars == 8, "Prob(presence): MSQD", vars)) %>%
#   mutate(vars = ifelse(vars == 9, "Prob(presence): NANC", vars)) %>%
#   mutate(vars = ifelse(vars == 10, "Abundance: MSQD", vars))
# 
# # gs4_create("SummaryQuarter", sheets = table)
# rm(desc_data, table)
# 
# 
### Annual data ###
dataset_annual <- dataset %>%
  group_by(LANDING_YEAR, VESSEL_NUM, group_all, PORT_AREA_CODE, PORT_AREA_ID) %>%
  summarise(PSDN_Landings = sum(PSDN_Landings, na.rm=T),
            MSQD_Landings = sum(MSQD_Landings, na.rm=T),
            NANC_Landings = sum(NANC_Landings, na.rm=T),
            PSDN_Price = mean(PSDN_Price, na.rm=T),
            MSQD_Price = mean(MSQD_Price, na.rm=T),
            NANC_Price = mean(NANC_Price, na.rm=T),
            PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm=T),
            MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm=T),
            NANC_SDM_20 = mean(NANC_SDM_20, na.rm=T),
            MSQD_SDM_90_JS_cpue = mean(MSQD_SDM_90_JS_CPUE, na.rm=T),
            MSQD_SPAWN_SDM_90_v2 = mean(MSQD_SPAWN_SDM_90_v2, na.rm=T),
            MSQD_recruitment = mean(MSQD_recruitment, na.rm=T)) %>% ungroup()
  dataset_annual[dataset_annual == "NaN"] <- NA

# desc_data <- dataset_annual %>%
#   subset(select = -c(PORT_AREA_ID, PORT_AREA_CODE, VESSEL_NUM, group_all, LANDING_YEAR))
# 
# table <- psych::describe(desc_data, fast=TRUE) %>%
#   mutate(vars = ifelse(vars == 1, "Landings: PSDN", vars)) %>%
#   mutate(vars = ifelse(vars == 2, "Landings: MSQD", vars)) %>%
#   mutate(vars = ifelse(vars == 3, "Landings: NANC", vars)) %>%
#   mutate(vars = ifelse(vars == 4, "Price: PSDN", vars)) %>%
#   mutate(vars = ifelse(vars == 5, "Price: MSQD", vars)) %>%
#   mutate(vars = ifelse(vars == 6, "Price: NANC", vars)) %>%
#   mutate(vars = ifelse(vars == 7, "Prob(presence): PSDN", vars)) %>%
#   mutate(vars = ifelse(vars == 8, "Prob(presence): MSQD", vars)) %>%
#   mutate(vars = ifelse(vars == 9, "Prob(presence): NANC", vars)) %>%
#   mutate(vars = ifelse(vars == 10, "Abundance: MSQD", vars))
# 
# # gs4_create("SummaryAnnual", sheets = table)
# rm(desc_data, table)

#-----------------------------------------------
## Create dataset for estiumation and run models 


### Market squid ###

#### Select data for estimation, replace N/A landings to zero 
dataset_msqd <- dataset_annual %>%
  dplyr::select(PORT_AREA_ID, PORT_AREA_CODE, VESSEL_NUM, group_all, LANDING_YEAR,
                MSQD_SPAWN_SDM_90, MSQD_SPAWN_SDM_90_v2, 
                MSQD_recruitment, MSQD_SDM_90_JS_cpue,
                MSQD_Landings, MSQD_Price, 
                PSDN_Landings, NANC_Landings, PSDN_Price, NANC_Price, 
                PSDN_SDM_60, NANC_SDM_20) %>% 
  dplyr::mutate(MSQD_Landings = coalesce(MSQD_Landings, 0)) %>%
  dplyr::mutate(PSDN_Landings = coalesce(PSDN_Landings, 0)) %>%
  dplyr::mutate(NANC_Landings = coalesce(NANC_Landings, 0)) %>%
  mutate(MSQD_Landings = ifelse(MSQD_Landings<= 0, 0, MSQD_Landings)) %>%
  mutate(MSQD_Landings = ifelse(MSQD_Landings< 0.01, 0, MSQD_Landings)) %>%
  mutate(PSDN_Landings = ifelse(PSDN_Landings<= 0, 0, MSQD_Landings)) %>%
  mutate(PSDN_Landings = ifelse(PSDN_Landings< 0.01, 0, MSQD_Landings)) %>%
  mutate(NANC_Landings = ifelse(NANC_Landings<= 0, 0, MSQD_Landings)) %>%
  mutate(NANC_Landings = ifelse(NANC_Landings< 0.01, 0, MSQD_Landings)) %>%
  dplyr::mutate(PSDN.Closure = ifelse(LANDING_YEAR >= 2015,1,0)) %>%
  dplyr::mutate(PSDN.Open = ifelse(LANDING_YEAR < 2015,1,0)) %>%
  dplyr::mutate(PSDN.Participation = ifelse(PSDN_Landings > 0, 1, 0)) %>%
  dplyr::mutate(NANC.Participation = ifelse(NANC_Landings > 0, 1, 0)) %>%
  dplyr::mutate(MSQD_Price_c = (MSQD_Price - mean(MSQD_Price, na.rm = TRUE)) / sd(MSQD_Price, na.rm = TRUE)) %>%
  drop_na() 

#### Create new port ID and cluster variable 
dataset_msqd$port_ID <- udpipe::unique_identifier(dataset_msqd, fields = "PORT_AREA_CODE", start_from = 1) 
dataset_msqd$cluster <- udpipe::unique_identifier(dataset_msqd, fields = "group_all", start_from = 1) 
MSQD_port_area <- dataset_msqd %>%
  dplyr::select('PORT_AREA_CODE', 'port_ID') %>%
  unique()
MSQD_clusters <- dataset_msqd %>%
  dplyr::select(group_all, cluster) %>%
  unique()

#### Convert variables to factor
dataset_msqd$port_ID      <- factor(dataset_msqd$port_ID)
dataset_msqd$cluster      <- factor(dataset_msqd$cluster)
dataset_msqd$LANDING_YEAR <- factor(dataset_msqd$LANDING_YEAR)
class(dataset_msqd$PSDN.Open)
class(dataset_msqd$port_ID)
class(dataset_msqd$cluster)


# ## Check stationarity in the panel dataset
# library("plm")
# 
# # dataset_msqd$Date <- zoo::as.yearmon(paste(dataset_msqd$LANDING_YEAR, dataset_msqd$LANDING_MONTH), "%Y %m")
# 
# pDataset <- dataset_msqd %>% mutate(Unique_ID = paste(VESSEL_NUM, PORT_AREA_CODE, sep = " ")) %>%
#   group_by(Unique_ID) %>% mutate(n_obs_group = n()) %>% ungroup() %>% filter(n_obs_group > 11) %>% drop_na()
# pDataset <- pdata.frame(pDataset, index = c('Unique_ID', 'LANDING_YEAR'))
# 
# purtest(pDataset$MSQD_Price, pmax = 4, exo = "intercept", test = "madwu")
# purtest(pDataset$PSDN_Price, pmax = 4, exo = "intercept", test = "madwu")
# purtest(pDataset$NANC_Price, pmax = 4, exo = "intercept", test = "madwu")
# purtest(pDataset$PSDN_SDM_60, pmax = 4, exo = "intercept", test = "madwu")
# purtest(pDataset$NANC_SDM_20, pmax = 4, exo = "intercept", test = "madwu")
# purtest(pDataset$MSQD_SPAWN_SDM_90, pmax = 4, exo = "intercept", test = "madwu")
# 
# 
# pDatasetV2 <- dataset_msqd %>% mutate(Unique_ID = paste(VESSEL_NUM, PORT_AREA_CODE, sep = " ")) %>% 
#   filter(MSQD_Landings > 0)  %>%  
#   group_by(Unique_ID) %>% mutate(n_obs_group = n()) %>% ungroup() %>% filter(n_obs_group > 11) %>% drop_na()
# pDatasetV2 <- pdata.frame(pDatasetV2, index = c('Unique_ID', 'LANDING_YEAR'))
# 
# purtest(pDatasetV2$MSQD_Landings, pmax = 4, exo = "intercept", test = "madwu")
# purtest(pDatasetV2$MSQD_Price, pmax = 4, exo = "intercept", test = "madwu")
# purtest(pDatasetV2$PSDN_Price, pmax = 4, exo = "intercept", test = "madwu")
# purtest(pDatasetV2$NANC_Price, pmax = 4, exo = "intercept", test = "madwu")
# purtest(pDatasetV2$PSDN_SDM_60, pmax = 4, exo = "intercept", test = "madwu")
# purtest(pDatasetV2$NANC_SDM_20, pmax = 4, exo = "intercept", test = "madwu")
# purtest(pDatasetV2$MSQD_SPAWN_SDM_90, pmax = 4, exo = "intercept", test = "madwu")
# 
# rm(pDataset, pDatasetV2)
# 
# # duplicate_indexes <- dataset_msqd %>% 
# #   group_by(PORT_AREA_CODE, Date, VESSEL_NUM) %>% mutate(dupe = n()>1)



#### Estimate models ####
library(brms)

fit_qMSQD_SpawningV1 <- brm(bf(
  MSQD_Landings ~ 1 + MSQD_SPAWN_SDM_90 + MSQD_SPAWN_SDM_90:PSDN_SDM_60:PSDN.Open
    + (1 + MSQD_SPAWN_SDM_90:PSDN_SDM_60:PSDN.Open + MSQD_SPAWN_SDM_90 | cluster + port_ID)
    + (1 | LANDING_YEAR),
  hu ~ 1 + PSDN.Open
    + (1 + PSDN.Open | cluster + port_ID)
    + (1 | LANDING_YEAR)), data = dataset_msqd,
  family = hurdle_gamma(),
  control = list(adapt_delta = 0.95, max_treedepth = 20),
  chains = 2,
  cores = 4)
  saveRDS(fit_qMSQD_SpawningV1, 
          file = here::here("Estimations", "fit_qMSQD_SpawningV1.RDS"))

# fit_qMSQD_SpawningV2 <- brm(bf(
#   MSQD_Landings ~ 1 + MSQD_SPAWN_SDM_90_v2 + MSQD_SPAWN_SDM_90_v2:PSDN_SDM_60:PSDN.Open 
#     + (1 + MSQD_SPAWN_SDM_90_v2:PSDN_SDM_60:PSDN.Open + MSQD_SPAWN_SDM_90_v2 | cluster + port_ID) 
#     + (1 | LANDING_YEAR),
#   hu ~ 1 + PSDN.Open 
#     + (1 + PSDN.Open | cluster + port_ID)
#     + (1 | LANDING_YEAR)), data = dataset_msqd,
#   family = hurdle_gamma(),
#   control = list(adapt_delta = 0.95, max_treedepth = 20),
#   chains = 2, 
#   cores = 4)
# 
# saveRDS(fit_qMSQD_SpawningV2, file = here::here("Estimations", "fit_qMSQD_SpawningV2.RDS"))

# fit_qMSQD_recruit <- brm(bf(
#   MSQD_Landings ~ 1 +  MSQD_recruitment +  MSQD_recruitment:PSDN_SDM_60:PSDN.Open 
#   + (1 +  MSQD_recruitment:PSDN_SDM_60:PSDN.Open +  MSQD_recruitment | cluster + port_ID) 
#   + (1 | LANDING_YEAR),
#   hu ~ 1 + PSDN.Open 
#   + (1 + PSDN.Open | cluster + port_ID)
#   + (1 | LANDING_YEAR)), data = dataset_msqd,
#   family = hurdle_gamma(),
#   control = list(adapt_delta = 0.95, max_treedepth = 20),
#   chains = 2, 
#   cores = 4)
# 
# saveRDS(fit_qMSQD_recruit, file = here::here("Estimations", "fit_qMSQD_recruit.RDS"))

# fit_qMSQD_abund <- brm(bf(
#   MSQD_Landings ~ 1 +  MSQD_SDM_90_JS_cpue +  MSQD_SDM_90_JS_cpue:PSDN_SDM_60:PSDN.Open 
#   + (1 +  MSQD_SDM_90_JS_cpue:PSDN_SDM_60:PSDN.Open +  MSQD_SDM_90_JS_cpue | cluster + port_ID) 
#   + (1 | LANDING_YEAR),
#   hu ~ 1 + PSDN.Open 
#   + (1 + PSDN.Open | cluster + port_ID)
#   + (1 | LANDING_YEAR)), data = dataset_msqd,
#   family = hurdle_gamma(),
#   control = list(adapt_delta = 0.95, max_treedepth = 20),
#   chains = 2, 
#   cores = 4)
# 
# saveRDS(fit_qMSQD_abund, file = here::here("Estimations", "fit_qMSQD_abund.RDS"))

##### Model Comparision
fit_qMSQD_SpawningV1 <- readRDS(here::here("Estimations", "fit_qMSQD_SpawningV1.RDS"))
fit_qMSQD_SpawningV2 <- readRDS(here::here("Estimations", "fit_qMSQD_SpawningV2.RDS"))
fit_qMSQD_abund      <- readRDS(here::here("Estimations", "fit_qMSQD_abund.RDS"))
fit_qMSQD_recruit    <- readRDS(here::here("Estimations", "fit_qMSQD_recruit.RDS"))

loo(fit_qMSQD_SpawningV1, fit_qMSQD_SpawningV2)
loo(fit_qMSQD_SpawningV1, fit_qMSQD_abund)
loo(fit_qMSQD_SpawningV1, fit_qMSQD_recruit)
fit_qMSQD <- fit_qMSQD_SpawningV1



# Include dummies that the vessel also enter Sardine
# Include year fixed-effects
# Include anchovy SDM



##### Model summary ####                       
stanplot(fit_qMSQD)
summary(fit_qMSQD)

library(ggplot2)
library(ggthemes)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
theme_set(theme_sjplot())

plot_model(fit_qMSQD) + theme(axis.text.y = element_text(hjust = 0))

library(patchwork)

pp_check(fit_qMSQD) + ggtitle('(a) Market Squid (SDM: Spawning aggregation model)') +
  scale_color_manual(name = "", values = c("y" = "royalblue4", "yrep" = "azure3"),
                     labels = c("y" = "Observed", "yrep" = "Replicated")) + 
  theme(legend.position = "none", plot.title = element_text(size=12, face="bold.italic"))  + 
  xlim(0, 2000) + xlab("Landing (tons)")


# Compare multilevel effects
as_draws_df(fit_qMSQD, add_chain = T) %>%
  ggplot(aes(x = sd_cluster__MSQD_SPAWN_SDM_90)) +
  geom_density(size = 0, fill = "orange1", alpha = 3/4) +
  geom_density(aes(x = sd_port_ID__MSQD_SPAWN_SDM_90),
               size = 0, fill = "orange4", alpha = 3/4) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(title = expression(sigma), subtitle = "Market squid SDM") +
  annotate("text", x = 10, y = 1/10, label = "Port area", color = "orange4") +
  annotate("text", x = 4, y = 1/5, label = "Cluster", color = "orange1") +
  theme_fivethirtyeight()


##### Conditional effects #####

# Conditional effects by port area
library(tibble)
conditions <- data.frame(cluster = unique(dataset_msqd$port_ID))
rownames(conditions) <- unique(dataset_msqd$PORT_AREA_CODE)

conditional_effects_msqd_sdm <-
  conditional_effects(
    fit_qMSQD, 
    "MSQD_SPAWN_SDM_90_v2",                
    surface=TRUE, 
    conditions = conditions, 
    re_formula = NULL)#, transform = log, method = "posterior_predict"))

library(ggplot2)

plot(conditional_effects_msqd_sdm, plot = FALSE, nrow = 3, ncol = 2)[[1]] + 
  ggtitle('Market squid availability effect on squid landings') +
  theme(plot.title = element_text(size=9, face="bold.italic"),
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_continuous(name = "Prob(Presence)") +
  scale_y_continuous(name = element_blank())
                  
conditional_effects_psdn_sdm <-
  conditional_effects(
    fit_qMSQD, 
    "PSDN_SDM_60",                
    surface=TRUE, 
    conditions = conditions, 
    re_formula = NULL)#, transform = log, method = "posterior_predict"))

plot(conditional_effects_psdn_sdm, plot = FALSE, nrow = 3, ncol = 2)[[1]] + 
  ggtitle('Pacific sardine availability effect on squid landings') +
  theme(plot.title = element_text(size=9, face="bold.italic"),
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_continuous(name = "Prob(Presence)") +
  scale_y_continuous(name = element_blank())

ranef(fit_qMSQD)   

rm(conditional_effects_msqd_sdm, conditional_effects_psdn.open,
   conditional_effects_psdn_sdm)
     


### Interaction effects ###


c_eff_int_psdn_msqd <- (conditional_effects(
  fit_qMSQD_Spawning, "PSDN_SDM_60:MSQD_SPAWN_SDM_90", 
  surface=TRUE, 
  conditions = conditions, re_formula = NULL))

plot(c_eff_int_psdn_msqd, plot = FALSE)[[1]] + 
  ggtitle('(a) Pacific sardine x Market squid') +
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: PSDN")) +
  scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): MSQD")



##### Hypothesis test
hypothesis(fit_qMSQD, "MSQD_SPAWN_SDM_90 = 0")

##### Predictions from the model using data to estimate the model

#   #   <!-- # ### Predictions ### -->
#   <!-- # pred_data <- data.frame(PSDN_SDM = c(0.5, 0.25), MSQD_SDM = c(0.5), Port_ID = 1) -->
#   <!-- # predict(fit_qPSDN, newdata = pred_data, re_formula = NA) -->

set.seed(123)
prediction <- cbind(predict(fit_qMSQD), dataset_msqd)
prediction$LANDING_YEAR <- as.numeric(as.character(prediction$LANDING_YEAR))

meltdf <- prediction %>% 
  dplyr::select(Estimate, MSQD_Landings, LANDING_YEAR, PORT_AREA_CODE) %>%
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarise(Est_landings = sum(Estimate), Landings = sum(MSQD_Landings)) %>%
  gather(key = Variable, value = value,
         c("Est_landings", "Landings"))

ggplot(meltdf, aes(x=LANDING_YEAR, y = value, colour = Variable)) + 
  geom_line(size=1) + 
  facet_wrap(~PORT_AREA_CODE)


meltdf <- prediction %>% 
  dplyr::select(Estimate, MSQD_Landings, LANDING_YEAR, group_all) %>%
  group_by(LANDING_YEAR, group_all) %>% 
  summarise(Est_landings = sum(Estimate), Landings = sum(MSQD_Landings)) %>%
  gather(key = Variable, value = value,
         c("Est_landings", "Landings"))
ggplot(meltdf, aes(x=LANDING_YEAR, y = value, colour = Variable)) + 
  geom_line(size=1) + 
  facet_wrap(~group_all)


# d2 %>%
#   ggplot(aes(x = weight)) +
#   geom_ribbon(data = pred_height, 
#               aes(ymin = Q2.5, ymax = Q97.5),
#               fill = "grey83") +
#   geom_smooth(data = mu_summary,
#               aes(y = Estimate, ymin = Q2.5, ymax = Q97.5),
#               stat = "identity",
#               fill = "grey70", color = "black", alpha = 1, size = 1/2) +
#   geom_point(aes(y = height),
#              color = "navyblue", shape = 1, size = 1.5, alpha = 2/3) +
#   coord_cartesian(xlim = range(d2$weight),
#                   ylim = range(d2$height)) +
#   theme(text = element_text(family = "Times"),
#         panel.grid = element_blank())

### Predict if sardine would have been open
# prediction_mod <- cbind(predict(fit_qMSQD_Spawning, newdata = data.frame(MSQD_Landings = dataset_msqd$MSQD_Landings,
#                                            MSQD_SPAWN_SDM_90 = dataset_msqd$MSQD_SPAWN_SDM_90,
#                                            PSDN_SDM.Open = dataset_msqd$PSDN_SDM.Open,
#                                            cluster = dataset_msqd$cluster,
#                                            port_ID = dataset_msqd$port_ID, 
#                                            PSDN.Closure = 0), dataset_msqd) 
# 

# # NOT RUN {
# ## fit a model
# fit <- brm(rating ~ treat + period + carry + (1|subject), 
#            data = inhaler)
# 
# ## compute expected predictions
# fitted_values <- fitted(fit)
# head(fitted_values)
# 
# ## plot expected predictions against actual response
# dat <- as.data.frame(cbind(Y = standata(fit)$Y, fitted_values))
# ggplot(dat) + geom_point(aes(x = Estimate, y = Y))
# # }
# # NOT RUN {
# # }


# ##### Model Comparision
# loo(fit_qMSQD_Spawning, fit_qMSQD_SpawningV2)
#
# fit_qMSQD <- add_criterion(fit_qMSQD, "loo")
# fit_qMSQD_90 <- add_criterion(fit_qMSQD_90, "loo")
# 
# comp <- loo_compare(loo(fit_qMSQD), loo(fit_qMSQD_90)) %>%
#   as.data.frame() %>%
#   select(elpd_loo, elpd_diff, se_diff, p_loo, looic) %>%
#   dplyr::rename(
#     "ELPD-Diff" = elpd_diff, "SE-Diff" = se_diff, 
#     "ELPD-LOO" = elpd_loo, "P-LOO" = p_loo, "LOOIC" = looic
#   )
# 
# mw1 <- model_weights(fit_qMSQD, fit_qMSQD_90, weights = "loo")[rownames(comp)]
# 
# comp %>%
#   cbind("Akaike-Weight" = mw1) %>%
#   apa_table(
#     format = "latex", booktabs = TRUE, digits = 2,
#     caption = "Comparison of models fit1 to fit3 based on approximate leave-one-out cross-validation. Market squid landigns model.",
#     note =  "ELPD-LOO = expected log posterior predictive density (higher is better); ELPD-DIFF = difference in ELPD values compared to the best model. SE-DIFF = standard error of the ELPD difference. P-LOO = effective number of model parameters (lower is better); LOOIC: leave-one-out information criterion (lower is better); Akaike-Weight = Model weight based on the LOOIC values (higher is better).",
#     align = c("l", rep("r", 6))
#   )



#---------------------------------------------------------------
## Pacific Sardine ##

# ### Select data for estimation, replace N/A landings to zero #
# 
# dataset_psdn <- dataset %>% 
#   dplyr::select(VESSEL_NUM, PSDN_SDM_60, PSDN_Landings, PSDN_Price, MSQD_Price,
#                 PORT_AREA_ID, LANDING_YEAR, MSQD_SDM_90, MSQD_SPAWN_SDM_90, group) %>%
#   filter(LANDING_YEAR >= 2000) %>% 
#   dplyr::mutate(PSDN_Landings = coalesce(PSDN_Landings, 0)) %>% #filter(PSDN_Landings > 0) %>% 
#   mutate(PSDN_Landings = ifelse(PSDN_Landings<=0, 0 ,PSDN_Landings)) %>% 
#   mutate(Closure = ifelse(LANDING_YEAR >= 2015,1,0)) %>%
#   mutate(RelPrice = PSDN_Price / MSQD_Price) %>% drop_na() 
# dataset_psdn$port_ID <- as.factor(
#   udpipe::unique_identifier(dataset_psdn, fields = "PORT_AREA_ID", start_from = 1))
# dataset_psdn$cluster <- as.factor(
#   udpipe::unique_identifier(dataset_psdn, fields = "group", start_from = 1))
# # dataset_psdn_port_names <- dataset_psdn %>% dplyr::select(PORT_AREA_CODE, port_ID) %>% unique()
# 
# 
# fit_qPSDN <- brm(bf(PSDN_Landings ~ PSDN_SDM_60 + MSQD_SPAWN_SDM_90 + (1 | cluster) + (1 | port_ID), 
#                     hu ~ (1 | cluster) + (1 | port_ID)),
#                  data = dataset_psdn, 
#                  prior = c(set_prior("cauchy(0,2)", class = "sd")),
#                  family = hurdle_gamma(), 
#                  chains = 4, cores = 4, warmup = "1000", iter = "2000",
#                  control = list(adapt_delta = 0.95))
# 
# plot(fit_qPSDN)
# plot(fit_qPSDN, pars = c("PSDN_SDM_60"))
# coef(fit_qPSDN)
# 
#
#------------------------------------------------------------------------ 
# Results


### Own species distribution effect
# 
# ```{r by_port_sdm, eval=FALSE, fig.cap=, include=FALSE}
# # PSDN plots
# conditions <- data.frame(port_ID = unique(est_data_psdn$port_ID))
# rownames(conditions) <- unique(est_data_psdn$Port)
# conditions_psdn <- conditions %>% 
#   rownames_to_column('port_name') %>%
#   filter(port_ID == 1 | port_ID == 2 | port_ID == 3) %>%
#   column_to_rownames('port_name')
# 
# c_eff_psdn <- (conditional_effects
#                (fit_qPSDN_price, "PSDN_SDM_60", surface=TRUE, conditions = conditions_psdn, re_formula = NULL))
# #, transform = log, method = "posterior_predict"))
# g1 <- plot(c_eff_psdn, plot = FALSE, nrow = 3, ncol = 1)[[1]] + ggtitle('(a) Pacific sardine')+ 
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
#   scale_x_continuous(name = element_blank()) +
#   scale_y_continuous(name = "Landings (tons)")
# 
 
# 
# # NANC plots
# conditions3 <- data.frame(port_ID = unique(est_data_nanc$port_ID))
# rownames(conditions3) <- unique(est_data_nanc$Port)
# conditions_nanc <- conditions3 %>% 
#   rownames_to_column('port_name') %>%
#   filter(port_ID == 3  | port_ID == 4  | port_ID == 5) %>%
#   column_to_rownames('port_name')
# c_eff_nanc <- (conditional_effects
#                (fit_qNANC_price, "NANC_SDM_20", surface=TRUE, conditions = conditions_nanc, re_formula = NULL))
# #, transform = log, method = "posterior_predict"))
# g3 <- plot(c_eff_nanc, plot = FALSE, nrow = 3, ncol = 1)[[1]] + ggtitle('(c) Northern anchovy') + 
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
#   scale_x_continuous(name = element_blank()) +
#   scale_y_continuous(name = element_blank()) 
# 
# # Merge plots
# g1 + g2 + g3


#--------------------------------------------------------- 
## Interaction effects
# 
# ```{r int_effect_PSDN_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
# c_eff_int_psdn_msqd <- (conditional_effects(fit_qPSDN_price, "PSDN_SDM_60:MSQD_SDM_90", surface=TRUE, 
#                                             conditions = conditions_psdn, re_formula = NULL))
# c_eff_int_psdn_nanc <- (conditional_effects(fit_qPSDN_price, "PSDN_SDM_60:NANC_SDM_20", surface=TRUE, 
#                                             conditions = conditions_psdn, re_formula = NULL))
# c_eff_int_msqd_nanc <- (conditional_effects(fit_qPSDN_price, "MSQD_SDM_90:NANC_SDM_20", surface=TRUE, 
#                                             conditions = conditions_psdn, re_formula = NULL))
# 
# g1_PSDN <-  plot(c_eff_int_psdn_msqd, plot = FALSE)[[1]] + ggtitle('(a) Pacific sardine x Market squid') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: PSDN")) +
#   scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): MSQD") 
# 
# g2_PSDN <-  plot(c_eff_int_psdn_nanc, plot = FALSE)[[1]] + ggtitle('(b) Pacific sardine x Northern anchovy') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
#   scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): NANC") 
# 
# g3_PSDN <-  plot(c_eff_int_msqd_nanc, plot = FALSE)[[1]] + ggtitle('(c) Northern anchovy x Market squid') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
#   scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): NANC") 
# save.image (file = "stan_fit.RData")
# ```
# 
# ```{r int_effect_PSDN_by_port_PLOT, eval=FALSE, fig.cap=, message=FALSE, warning=FALSE, include=FALSE}
# g1_PSDN / g2_PSDN
# ```
# 
# ```{r int_effect_MSQD_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
# c_eff_int_msqd_psdn <- (conditional_effects(fit_qMSQD_price, "MSQD_SDM_90:PSDN_SDM_60_dOpen", surface=TRUE, 
#                                             conditions = conditions_msqd, re_formula = NULL))
# c_eff_int_msqd_nanc <- (conditional_effects(fit_qMSQD_price, "MSQD_SDM_90:NANC_SDM_20", surface=TRUE, 
#                                             conditions = conditions_msqd, re_formula = NULL))
# c_eff_int_psdn_nanc <- (conditional_effects(fit_qMSQD_price, "PSDN_SDM_60_dOpen:NANC_SDM_20", surface=TRUE, 
#                                             conditions = conditions_msqd, re_formula = NULL))
# 
# g1_MSQD <-  plot(c_eff_int_msqd_psdn, plot = FALSE)[[1]] + ggtitle('(a) Market squid x Pacific sardine') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: MSQD")) +
#   scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): PSDN") 
# 
# g2_MSQD <-  plot(c_eff_int_msqd_nanc, plot = FALSE)[[1]] + ggtitle('(b) Market squid x Northern anchovy') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: MSQD")) + 
#   scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): NANC") 
# 
# g3_MSQD <-  plot(c_eff_int_psdn_nanc, plot = FALSE)[[1]] + ggtitle('(c) Pacific sardine x Northern anchovy') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: MSQD")) + 
#   scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): NANC") 
# save.image (file = "stan_fit.RData")
# ```
# 
# ```{r int_effect_MSQD_by_port_PLOT, eval=FALSE, fig.cap=, message=FALSE, warning=FALSE, include=FALSE}
# g1_MSQD / g2_MSQD 
# ```
# 
# ```{r int_effect_NANC_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
# conditions_nanc <- conditions_nanc %>% filter(port_ID == 3  | port_ID == 5)
# 
# c_eff_int_nanc_psdn <- (conditional_effects(fit_qNANC_price, "NANC_SDM_20:PSDN_SDM_60_dOpen", surface=TRUE, 
#                                             conditions = conditions_nanc, re_formula = NULL))
# c_eff_int_nanc_msqd <- (conditional_effects(fit_qNANC_price, "NANC_SDM_20:MSQD_SDM_90", surface=TRUE, 
#                                             conditions = conditions_nanc, re_formula = NULL))
# c_eff_int_psdn_msqd <- (conditional_effects(fit_qNANC_price, "PSDN_SDM_60_dOpen:MSQD_SDM_90", surface=TRUE, 
#                                             conditions = conditions_nanc, re_formula = NULL))
# 
# g1_NANC <-  plot(c_eff_int_nanc_psdn, plot = FALSE)[[1]] + ggtitle('(a) Northern anchovy x Pacific sardine') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: PSDN")) +
#   scale_x_continuous(name = "P(Pres): NANC") + scale_y_continuous(name = "P(Pres): PSDN") 
# 
# g2_NANC <-  plot(c_eff_int_nanc_msqd, plot = FALSE)[[1]] + ggtitle('(b) Northern anchovy x Market squid') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
#   scale_x_continuous(name = "P(Pres): NANC") + scale_y_continuous(name = "P(Pres): MSQD") 
# 
# g3_NANC <-  plot(c_eff_int_psdn_msqd, plot = FALSE)[[1]] + ggtitle('(c) Pacific sardine x Market squid') + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"), 
#     axis.text = element_text(size = 7), 
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9), 
#     legend.text = element_text(size=8)
#   ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
#   scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): MSQD") 
# save.image (file = "stan_fit.RData")
# ```
# 
# ```{r int_effect_NANC_by_port_PLOT, eval=FALSE, fig.cap=, message=FALSE, warning=FALSE, include=FALSE}
# g1_NANC / g2_NANC 
# ```
# 
# ### Pacific sardine closure
# 
# ```{r by_port_msqd_dclose, eval=FALSE, fig.cap=, include=FALSE}
# # conditions_dClose <- data.frame(port_ID = unique(est_data_msqd$dClose))
# # rownames(conditions_dClose) <- unique(est_data_msqd$dClose)
# c_eff_close_msqd <- (conditional_effects(fit_qMSQD_price, "dClose", conditions = conditions_msqd, re_formula = NULL))
# g1 <-  plot(c_eff_close_msqd, plot = FALSE)[[1]] + ggtitle('(a) Market squid') + 
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
#   scale_x_discrete(name = "") +
#   scale_y_continuous(name = "Landings (tons)") 
# 
# c_eff_close_nanc <- (conditional_effects(fit_qNANC_price, "dClose", conditions = conditions_nanc, re_formula = NULL))
# g2 <- plot(c_eff_close_nanc, plot = FALSE)[[1]] + ggtitle('(b) Northern anchovy') + 
#   theme(plot.title = element_text(size=9, face="bold.italic"), 
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
#   scale_x_discrete(name = "Closure? (1 = True; 0 = False)") +
#   scale_y_continuous(name = "Landings (tons)") 
# 
# g1 / g2
# ```
# 
# # OTHER CODE
# 
# <!-- ```{r int_effect_sep, eval=FALSE, include=FALSE} -->
#   
#   <!-- hu <- plot(conditional_effects(fit_qPSDN, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="hu"), -->
#                     <!--            plot = FALSE, ask = FALSE) -->
#   <!-- mu <- plot(conditional_effects(fit_qPSDN, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="mu"), -->
#                     <!--            plot = FALSE, ask = FALSE) -->
#   
#   <!-- mu[[1]] + hu[[1]] + plot_layout(nrow = 2) -->
#   
#   <!-- ``` -->
#   
#   <!-- ```{r int_effect_sep_msqd, eval=FALSE, fig.cap=, include=FALSE} -->
#   
#   <!-- hu <- plot(conditional_effects(fit_qMSQD_gamma, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="hu"), -->
#                     <!--            plot = FALSE, ask = FALSE) -->
#   <!-- mu <- plot(conditional_effects(fit_qMSQD_gamma, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="mu"), -->
#                     <!--            plot = FALSE, ask = FALSE) -->
#   
#   <!-- mu[[1]] + hu[[1]] + plot_layout(nrow = 2) -->
#   
