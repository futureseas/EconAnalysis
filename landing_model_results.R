#####################
### Landing model ###
#####################

#----------------------------
# Setup #
rm(list = ls(all.names = TRUE)) 
gc()

library("googlesheets4")
gs4_auth(
  email = "fequezad@ucsc.edu",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)

## Read packages 

library("tidyr")
library("dplyr") 
library("data.table") 
library("reshape2")

## Read PacFIN database 
PacFIN.month <- read.csv(file ="C:\\Data\\PacFIN data\\PacFIN_month.csv") %>% 
  dplyr::filter(AFI_EXVESSEL_REVENUE.sum > 0)

## Create database of port area code and port names
ports_area_codes <- PacFIN.month %>% dplyr::select('PORT_NAME', 'AGENCY_CODE', 'PORT_AREA_CODE') %>% unique()

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
PacFIN.month.aggregate <- doBy::summaryBy(
  LANDED_WEIGHT_MTONS.sum + AFI_PRICE_PER_MTON.mean + AFI_EXVESSEL_REVENUE.sum + Length.mean 
    ~ LANDING_YEAR + LANDING_MONTH + VESSEL_NUM + PORT_AREA_CODE + PACFIN_SPECIES_CODE + AGENCY_CODE + group_all,
  FUN=sum_mean_fun, data=PacFIN.month) %>%
  dplyr::filter(AFI_EXVESSEL_REVENUE.sum.sum > 0) %>%
  dplyr::select(-c('LANDED_WEIGHT_MTONS.sum.mean', 'AFI_PRICE_PER_MTON.mean.sum',
                   'AFI_EXVESSEL_REVENUE.sum.mean', 'Length.mean.sum'))
rm(PacFIN.month)

########################################################

PacFIN.month.dataset <- PacFIN.month.aggregate %>% 
  dplyr::rename(AFI_PRICE_PER_MTON.mean = AFI_PRICE_PER_MTON.mean.mean) %>%
  dplyr::rename(LANDED_WEIGHT_MTONS.sum = LANDED_WEIGHT_MTONS.sum.sum) %>%
  dplyr::rename(AFI_EXVESSEL_REVENUE.sum = AFI_EXVESSEL_REVENUE.sum.sum) %>%
  dplyr::rename(Length = Length.mean.mean) %>%
  mutate(AFI_PRICE_PER_MTON.mean = na_if(AFI_PRICE_PER_MTON.mean, 0)) %>% 
  filter(group_all != is.na(group_all)) %>%
  reshape2::melt(id.vars=c("LANDING_YEAR", "LANDING_MONTH", "VESSEL_NUM", 'PORT_AREA_CODE',
                           "PACFIN_SPECIES_CODE", "AGENCY_CODE", "Length", "group_all")) %>% 
  reshape2::dcast(LANDING_YEAR + LANDING_MONTH + VESSEL_NUM + PORT_AREA_CODE + AGENCY_CODE + group_all + Length ~
          PACFIN_SPECIES_CODE + variable, fun.aggregate=mean, rm.na = T) %>%
  dplyr::select('LANDING_YEAR', 'LANDING_MONTH', 'VESSEL_NUM', 'PORT_AREA_CODE', 'AGENCY_CODE', 'group_all',
                'Length', 'PSDN_LANDED_WEIGHT_MTONS.sum', 'MSQD_LANDED_WEIGHT_MTONS.sum', 'NANC_LANDED_WEIGHT_MTONS.sum',  
                'PSDN_AFI_PRICE_PER_MTON.mean', 'MSQD_AFI_PRICE_PER_MTON.mean', 'NANC_AFI_PRICE_PER_MTON.mean')

rm(PacFIN.month.aggregate)

                
  
### Selecting port using results from cluster analysis (10% of the revenue for at least one cluster)
PacFIN.month.dataset <- PacFIN.month.dataset %>%
  dplyr::filter(PORT_AREA_CODE == "SDA" | PORT_AREA_CODE == "LAA" |
                  PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "MNA" | 
                  PORT_AREA_CODE == "CLO" | PORT_AREA_CODE == "CLW" | 
                  PORT_AREA_CODE == "CWA" | PORT_AREA_CODE == "NPS")


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
SDM_port_MSQD <- read.csv(file = here::here("Data", "SDM", "MSQD_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_YEAR, LANDING_MONTH) %>% summarize(MSQD_SDM_90 = mean(SDM_90))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_MSQD, 
                              by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) 

#### Merge data with SDM Market Squid (spawn aggregation) #
SDM_port_MSQD_Spawn <- read.csv(file = here::here("Data", "SDM", "MSQD_Spawn_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
# dplyr::filter(LANDING_MONTH == 8 | LANDING_MONTH == 9 | LANDING_MONTH == 10) %>% 
  group_by(PORT_AREA_CODE, LANDING_YEAR, LANDING_MONTH) %>% summarize(MSQD_SPAWN_SDM_90 = mean(SDM_SPAWN_90))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_MSQD_Spawn, 
                      by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) 

#### Merge data with SDM Northern Anchovy #
SDM_port_NANC <- read.csv(file = here::here("Data", "SDM", "NANC_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_MONTH, LANDING_YEAR) %>% summarize(NANC_SDM_20 = mean(SDM_20))
dataset <- merge(PacFIN.month.dataset, SDM_port_NANC, 
                      by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

# #### Merge data with SDM Market Squid (JS Abundance model) #
# SDM_port_MSQD_JS_cpue <- read.csv(file = here::here("Data", "SDM", "MSQD_SDM_port_year_JS_abund_V2.csv")) %>%
#   merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
#   group_by(PORT_AREA_CODE, LANDING_YEAR) %>% summarize(MSQD_SDM_90_JS_CPUE = mean(SDM_90_JS_CPUE))
# PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_MSQD_JS_cpue, 
#                       by = c("PORT_AREA_CODE", "LANDING_YEAR"), all.x = TRUE)

rm(PacFIN.month.dataset, 
   SDM_port_PSDN, SDM_port_NANC, SDM_port_MSQD, SDM_port_MSQD_Spawn, ports_area_codes, vessel.monthly.dataset)


#-----------------------------------------------------------------
## Calculate SDM for N/A's using PacFIN port code and port area code.

### Pacific sardine

#### Using port area code
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

#### Using port area code
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


### Market squid

#### Using port area code
SDM.port.area.MSQD <- aggregate(x=dataset$MSQD_SDM_90,
                                by = list(dataset$LANDING_YEAR,
                                          dataset$LANDING_MONTH, 
                                          dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.MSQD <- SDM.port.area.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(MSQD.SDM.port.area = x)
SDM.port.area.MSQD[SDM.port.area.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(MSQD_SDM_90 = ifelse(is.na(MSQD_SDM_90), MSQD.SDM.port.area, MSQD_SDM_90))
rm(SDM.port.area.MSQD)
dataset = subset(dataset, select = -c(MSQD.SDM.port.area))


### Market squid (SPAWN)

#### Using port area code
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


# ### Market squid (JS abundance)
# 
# #### Using port area code
# SDM.port.area.MSQD_JS_cpue <- aggregate(x=dataset$MSQD_SDM_90_JS_CPUE, 
#                                         by = list(dataset$LANDING_YEAR, 
#                                                   dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
# SDM.port.area.MSQD_JS_cpue <- SDM.port.area.MSQD_JS_cpue %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
#   dplyr::rename(PORT_AREA_CODE = Group.2) %>% dplyr::rename(MSQD_JS_cpue.SDM.port.area = x)
# SDM.port.area.MSQD_JS_cpue[SDM.port.area.MSQD_JS_cpue == "NaN"] <- NA
# dataset <- dataset %>%
#   merge(SDM.port.area.MSQD_JS_cpue, by = c("LANDING_YEAR", "PORT_AREA_CODE"), all.x = TRUE) %>%
#   mutate(MSQD_SDM_90_JS_CPUE = ifelse(is.na(MSQD_SDM_90_JS_CPUE), MSQD_JS_cpue.SDM.port.area, MSQD_SDM_90_JS_CPUE))
# rm(SDM.port.area.MSQD_JS_cpue)
# dataset = subset(dataset, select = -c(MSQD_JS_cpue.SDM.port.area))



#----------------------------------------------------------------------------------------------
## Calculate year price for N/A's using PacFIN port code, port area code, state and year/month.

### Pacific sardine

#### (a) Using port area code
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

#### (b) Using state
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

#### (c) Using year/month
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

#### (a) Using port area code
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

#### (b) Using state
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

#### (c) Using year/month
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

#### (a) Using port area code
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

#### (b) Using state
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

#### (c) Using year/month
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


# --------------------------------------------------------------------------------------
### Include closure data
PSDN_closure <- read.csv("C:\\Data\\Closures\\PSDN_closures.csv")
MSQD_closure <- read.csv("C:\\Data\\Closures\\MSQD_closures.csv") 
dataset <- merge(dataset, PSDN_closure, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE, all.y = FALSE)
dataset <- merge(dataset, MSQD_closure, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE, all.y = FALSE)
rm(PSDN_closure, MSQD_closure)


#---------------------------------------------------------------------------------------
### Include Consumer Price Index to use as a deflactor
CPI <- read.csv(here::here("Data", "CPI", "CPIAUCSL.csv"))
CPI$DATE <- as.Date(CPI$DATE, format = "%Y-%m-%d") 
CPI$LANDING_YEAR  <- lubridate::year(CPI$DATE)
CPI$LANDING_MONTH <- lubridate::month(CPI$DATE)
CPI <- CPI %>% dplyr::select(-c('DATE')) %>% dplyr::rename(CPI = CPIAUCSL)
dataset <- merge(dataset, CPI, by = c('LANDING_YEAR', 'LANDING_MONTH'), all.x = TRUE, all.y = FALSE)
dataset$deflactor <- dataset$CPI/100 
rm(CPI)

#---------------------------------------------------------------------------------------
### Include world's fish meal price as instrument
fish.meal <- read.csv(here::here("Data", "Instruments", "PFISHUSDM.csv"), header = TRUE, stringsAsFactors = FALSE)
fish.meal$DATE <- as.Date(fish.meal$DATE, format = "%m/%d/%Y") 
fish.meal$LANDING_YEAR  <- lubridate::year(fish.meal$DATE)
fish.meal$LANDING_MONTH <- lubridate::month(fish.meal$DATE)
fish.meal <- fish.meal %>% dplyr::select(-c('DATE')) %>% dplyr::rename(Price.Fishmeal = PFISHUSDM)
dataset <- merge(dataset, fish.meal, by = c('LANDING_YEAR', 'LANDING_MONTH'), all.x = TRUE, all.y = FALSE)
dataset$Price.Fishmeal.AFI <- dataset$Price.Fishmeal/dataset$deflactor
rm(fish.meal)

#---------------------------------------------------------------------------------------
### Include fuel prices
fuel.prices.CA <- readxl::read_excel(here::here("Data", "Fuel_prices", "fuelca.xls"), sheet = "fuelca")
fuel.prices.WA <- readxl::read_excel(here::here("Data", "Fuel_prices", "fuelwa.xls"), sheet = "fuelwa")
fuel.prices.OR <- readxl::read_excel(here::here("Data", "Fuel_prices", "fuelor.xls"), sheet = "fuelor")

fuel.prices <- rbind(fuel.prices.CA,fuel.prices.OR,fuel.prices.WA) %>% 
  dplyr::select(-c('portname', 'DOCKCODE', 'notes', 'pricettl', 'pxquoted')) %>%
  dplyr::rename(PACFIN_PORT_CODE = port) %>%
  dplyr::rename(LANDING_YEAR = YEAR) %>%
  dplyr::rename(LANDING_MONTH = MONTH) 

## Replace zero to NA
fuel.prices[fuel.prices == 0] <- NA

## Merge port area code
port_area_codes <- read.csv(here::here('Data', 'Ports', 'ports_area_and_name_codes.csv'), 
                            header = TRUE, stringsAsFactors = FALSE)

fuel.prices <- merge(fuel.prices, port_area_codes, 
                     by = ("PACFIN_PORT_CODE"), all.x = TRUE, all.y = FALSE)

fuel.prices <- fuel.prices %>% group_by(LANDING_YEAR, LANDING_MONTH, PORT_AREA_CODE) %>% 
  summarize(diesel.price = mean(pricegal, na.rm = TRUE))

## Merge price to dataset
dataset <- merge(dataset, fuel.prices, by = c('LANDING_YEAR', 'LANDING_MONTH', 'PORT_AREA_CODE'), all.x = TRUE, all.y = FALSE)

#### replace NA using state price
fuel.prices.state <- readxl::read_excel(here::here("Data", "Fuel_prices", "state_averages.xls"), sheet = "state_averages") %>%
  dplyr::rename(LANDING_YEAR = YEAR) %>%
  dplyr::rename(LANDING_MONTH = MONTH) %>%
  dplyr::rename(AGENCY_CODE = STATE) %>%
  dplyr::rename(diesel.price.state = avgpricegal) %>%
  dplyr::select(-c('avgpricettl')) %>%
  mutate(AGENCY_CODE = ifelse(AGENCY_CODE == 'WA', 'W', 
                              ifelse(AGENCY_CODE == 'CA', 'C', 
                                     ifelse(AGENCY_CODE == 'OR', 'O', 'A'))))

dataset <- dataset %>%
  merge(fuel.prices.state, by = c("LANDING_YEAR", "LANDING_MONTH", "AGENCY_CODE"), all.x = TRUE) %>% 
  mutate(diesel.price = ifelse(is.na(diesel.price), diesel.price.state, diesel.price))

## Deflact prices
dataset$diesel.price.AFI <- dataset$diesel.price/dataset$deflactor

rm(fuel.prices, fuel.prices.CA, fuel.prices.OR, fuel.prices.WA, fuel.prices.state)

#---------------------------------------
## Summary statistics ##
dataset <- dataset %>% 
  dplyr::rename(PSDN_Price = PSDN_AFI_PRICE_PER_MTON.mean) %>%
  dplyr::rename(MSQD_Price = MSQD_AFI_PRICE_PER_MTON.mean) %>%
  dplyr::rename(NANC_Price = NANC_AFI_PRICE_PER_MTON.mean) %>%
  dplyr::rename(PSDN_Landings = PSDN_LANDED_WEIGHT_MTONS.sum) %>%
  dplyr::rename(MSQD_Landings = MSQD_LANDED_WEIGHT_MTONS.sum) %>%
  dplyr::rename(NANC_Landings = NANC_LANDED_WEIGHT_MTONS.sum)

# Create centerized variables and z-values 

dataset <- dataset %>%
  dplyr::mutate(MSQD_Price_z = ((MSQD_Price - mean(MSQD_Price, na.rm = TRUE)) / sd(MSQD_Price, na.rm = TRUE))) %>%
  dplyr::mutate(PSDN_Price_z = ((PSDN_Price - mean(PSDN_Price, na.rm = TRUE)) / sd(PSDN_Price, na.rm = TRUE))) %>%
  dplyr::mutate(MSQD_SPAWN_SDM_90_z = ((MSQD_SPAWN_SDM_90 - mean(MSQD_SPAWN_SDM_90, na.rm = TRUE))/sd(MSQD_SPAWN_SDM_90, na.rm = TRUE))) %>%
  dplyr::mutate(MSQD_SDM_90_z = ((MSQD_SDM_90 - mean(MSQD_SDM_90, na.rm = TRUE))/sd(MSQD_SDM_90, na.rm = TRUE))) %>%
  dplyr::mutate(PSDN_SDM_60_z = ((PSDN_SDM_60 - mean(PSDN_SDM_60, na.rm = TRUE))/sd(PSDN_SDM_60, na.rm = TRUE))) %>%
  dplyr::mutate(MSQD_Price_c = MSQD_Price - mean(MSQD_Price, na.rm = TRUE)) %>%
  dplyr::mutate(MSQD_SPAWN_SDM_90_c = MSQD_SPAWN_SDM_90 - mean(MSQD_SPAWN_SDM_90, na.rm = TRUE)) %>%
  dplyr::mutate(MSQD_SDM_90_c = MSQD_SDM_90 - mean(MSQD_SDM_90, na.rm = TRUE)) %>%
  dplyr::mutate(PSDN_SDM_60_c = PSDN_SDM_60 - mean(PSDN_SDM_60, na.rm = TRUE)) %>%
  dplyr::mutate(Price.Fishmeal_z = ((Price.Fishmeal - mean(Price.Fishmeal, na.rm = TRUE))/sd(Price.Fishmeal, na.rm = TRUE))) %>%
  dplyr::mutate(diesel.price_z = ((diesel.price - mean(diesel.price, na.rm = TRUE))/sd(diesel.price, na.rm = TRUE))) %>%
  dplyr::mutate(Price.Fishmeal.AFI_z = ((Price.Fishmeal.AFI - mean(Price.Fishmeal.AFI, na.rm = TRUE))/sd(Price.Fishmeal.AFI, na.rm = TRUE))) %>%
  dplyr::mutate(diesel.price.AFI_z = ((diesel.price.AFI - mean(diesel.price.AFI, na.rm = TRUE))/sd(diesel.price.AFI, na.rm = TRUE))) %>%
  dplyr::mutate(Length_z = ((Length - mean(Length, na.rm = TRUE))/sd(Length, na.rm = TRUE)))



# ### Label dataset ###
# sjlabelled::set_label(dataset$MSQD_SPAWN_SDM_90) <- "Prob(presence): MSQD (Spawning aggregation model)"
# sjlabelled::set_label(dataset$MSQD_SDM_90)       <- "Prob(presence): MSQD"
# sjlabelled::set_label(dataset$NANC_SDM_20)       <- "Prob(presence): NANC"
# sjlabelled::set_label(dataset$PSDN_SDM_60)       <- "Prob(presence): PSDN"
# sjlabelled::set_label(dataset$LANDING_YEAR)     <- "Year"
# sjlabelled::set_label(dataset$LANDING_MONTH)    <- "Month"
# sjlabelled::set_label(dataset$PORT_AREA_CODE)   <- "Port area code"
# sjlabelled::set_label(dataset$AGENCY_CODE)      <- "State"
# sjlabelled::set_label(dataset$VESSEL_NUM)       <- "Vessel ID"
# sjlabelled::set_label(dataset$MSQD_Landings) <- "Landings: MSQD"
# sjlabelled::set_label(dataset$MSQD_Price)    <- "Price: MSQD"
# sjlabelled::set_label(dataset$PSDN_Landings) <- "Landings: PSDN"
# sjlabelled::set_label(dataset$PSDN_Price)    <- "Price: PSDN"
# sjlabelled::set_label(dataset$NANC_Landings) <- "Landings: NANC"
# sjlabelled::set_label(dataset$NANC_Price)    <- "Price: NANC"
# sjlabelled::set_label(dataset$PORT_ID)       <- "Port ID"


### Monthly data ###
desc_data <- dataset %>%
  subset(select = -c(PORT_AREA_ID, VESSEL_NUM, group_all, LANDING_YEAR, LANDING_MONTH, AGENCY_CODE, PORT_AREA_CODE, 
                     MSQD_Price_z, MSQD_SPAWN_SDM_90_z, MSQD_SDM_90_z, PSDN_SDM_60_z, 
                     MSQD_Price_c, MSQD_SPAWN_SDM_90_c, MSQD_SDM_90_c, PSDN_SDM_60_c,
                     length_month_PSDN, length_month_MSQD, days_open_month_PSDN, days_open_month_MSQD, deflactor,
                     CPI, Price.Fishmeal_z, diesel.price_z, Price.Fishmeal.AFI_z, diesel.price.AFI_z, Length_z))

table <- psych::describe(desc_data, fast=TRUE) %>%
  mutate(vars = ifelse(vars == 1, "Vessel Length", vars))%>%
  mutate(vars = ifelse(vars == 2, "Landings: PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 3, "Landings: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 4, "Landings: NANC", vars)) %>%
  mutate(vars = ifelse(vars == 5, "Price: PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 6, "Price: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 7, "Price: NANC", vars)) %>%
  mutate(vars = ifelse(vars == 8, "Prob(presence): PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 9, "Prob(presence): MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 10, "Prob(presence): MSQD (Spawning model)", vars)) %>%
  mutate(vars = ifelse(vars == 11, "Prob(presence): NANC", vars)) %>%
  mutate(vars = ifelse(vars == 12, "Fraction of month open: PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 13, "Fraction of month open: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 14, "Fishmeal price", vars)) %>%
  mutate(vars = ifelse(vars == 15, "Fishmeal price (AFI)", vars)) %>%
  mutate(vars = ifelse(vars == 16, "Diesel price", vars)) %>%
  mutate(vars = ifelse(vars == 17, "Diesel price (AFI)", vars))

# gs4_create("SummaryMonthly", sheets = table)
rm(desc_data, table)
  

### Quarterly data ###
# library("zoo")
# dataset$MonthYear <- as.yearmon(paste(dataset$LANDING_YEAR, dataset$LANDING_MONTH), "%Y %m")
# dataset$QuarterYear <- as.yearqtr(dataset$MonthYear, format = "%Y-%m")
  
# ### Annual data ###
# dataset_annual <- dataset %>%
#   group_by(LANDING_YEAR, VESSEL_NUM, group_all, PORT_AREA_CODE, PORT_AREA_ID) %>%
#   summarise(PSDN_Landings = sum(PSDN_Landings, na.rm=T),
#             MSQD_Landings = sum(MSQD_Landings, na.rm=T),
#             NANC_Landings = sum(NANC_Landings, na.rm=T),
#             PSDN_Price = mean(PSDN_Price, na.rm=T),
#             MSQD_Price = mean(MSQD_Price, na.rm=T),
#             NANC_Price = mean(NANC_Price, na.rm=T),
#             PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm=T),
#             MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm=T),
#             NANC_SDM_20 = mean(NANC_SDM_20, na.rm=T))
#   dataset_annual[dataset_annual == "NaN"] <- NA

#-----------------------------------------------
## Create dataset for estiumation and run models 


### Market squid ###

#### Select data for estimation, replace N/A landings to zero 
dataset_msqd <- dataset %>%
  dplyr::select(PORT_AREA_ID, PORT_AREA_CODE, VESSEL_NUM, group_all, 
                LANDING_YEAR, LANDING_MONTH,
                MSQD_SPAWN_SDM_90, MSQD_Landings, MSQD_Price, 
                PSDN_Landings, NANC_Landings, PSDN_Price, NANC_Price, 
                PSDN_SDM_60, NANC_SDM_20,
                MSQD_Price_z, PSDN_Price_z, MSQD_SPAWN_SDM_90_z, MSQD_SDM_90_z, PSDN_SDM_60_z,
                MSQD_Price_c, MSQD_SPAWN_SDM_90_c, MSQD_SDM_90_c, PSDN_SDM_60_c,
                PSDN.Open, MSQD.Open, Price.Fishmeal, Price.Fishmeal_z, 
                Price.Fishmeal.AFI, Price.Fishmeal.AFI_z,
                diesel.price, diesel.price.AFI, diesel.price_z, diesel.price.AFI_z,
                Length, Length_z) %>% 
  dplyr::mutate(MSQD_Landings = coalesce(MSQD_Landings, 0)) %>%
  dplyr::mutate(PSDN_Landings = coalesce(PSDN_Landings, 0)) %>%
  dplyr::mutate(NANC_Landings = coalesce(NANC_Landings, 0)) %>%
  mutate(MSQD_Landings = ifelse(MSQD_Landings<= 0, 0, MSQD_Landings)) %>%
  mutate(MSQD_Landings = ifelse(MSQD_Landings< 0.0001, 0, MSQD_Landings)) %>%
  mutate(PSDN_Landings = ifelse(PSDN_Landings<= 0, 0, MSQD_Landings)) %>%
  mutate(PSDN_Landings = ifelse(PSDN_Landings< 0.0001, 0, MSQD_Landings)) %>%
  mutate(NANC_Landings = ifelse(NANC_Landings<= 0, 0, MSQD_Landings)) %>%
  mutate(NANC_Landings = ifelse(NANC_Landings< 0.0001, 0, MSQD_Landings)) %>%
  dplyr::mutate(PSDN.Participation = ifelse(PSDN_Landings > 0, 1, 0)) %>% 
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::mutate(ln_MSQD_Landings = log(MSQD_Landings)) %>%
    filter(group_all == 1 | group_all == 2 | group_all == 4 | group_all == 5 | group_all == 7) %>%
    filter(PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "MNA") %>% drop_na()


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
class(dataset_msqd$LANDING_YEAR)

# # install.packages(c("fastDummies", "recipes"))
# library('fastDummies')
# dataset_msqd <- dummy_cols(dataset_msqd, select_columns = 'cluster')

#-------------------------------------------------------------------
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


#-------------------------------------------------------------------
#### Estimate models ####
library(brms)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(insight)
library(httr)

dataset_msqd_landing <- dataset_msqd %>%
  dplyr::filter(MSQD_Landings > 0) %>%
  dplyr::filter(MSQD.Open == 1) 

# fit_qMSQD <-
#   brm(data = dataset_msqd_landing,
#       formula = log(MSQD_Landings) ~
#         1 + MSQD_SPAWN_SDM_90_z  + MSQD_Price_z + Length_z +
#         (1 | port_ID) + (1 | cluster),
#       prior = c(
#         prior(normal(0, 1), class = b),
#         prior(exponential(1), class = sigma)),
#       control = list(adapt_delta = 0.90, max_treedepth = 12),
#       chains = 2,
#       family = gaussian,
#       cores = 4,
#       file = "Estimations/fit_qMSQD")
# 
# fit_qMSQD_b <-
#   brm(data = dataset_msqd_landing,
#       formula = log(MSQD_Landings) ~
#         1 + MSQD_SPAWN_SDM_90  + MSQD_Price + Length +
#         (1 | port_ID) + (1 | cluster),
#       prior = c(
#         prior(normal(0, 1), class = b),
#         prior(exponential(1), class = sigma)),
#       control = list(adapt_delta = 0.90, max_treedepth = 12),
#       chains = 2,
#       family = gaussian,
#       cores = 4,
#       file = "Estimations/fit_qMSQD_b")

### Model Comparision ###
# tab_model(fit_qMSQD, fit_qMSQD_b)

# fit_qMSQD   <- add_criterion(fit_qMSQD,   "loo")
# fit_qMSQD_b <- add_criterion(fit_qMSQD_b, "loo")


# # w <- as.data.frame(
# loo_compare(fit_qMSQD,
#             fit_qMSQD_b,
#             criterion = "loo")
# )
# gs4_create("LOO", sheets = w)


## -------------------------------------------------------------------
### Problem using price, as is endogenous. Solve price endogeneity ### 

#### Problems to solve: 
## - Other marginal costs?
## - Fish meal as instrument for squid?
## - Include other species


### Base model (z-value) ###
price_model   <- bf(MSQD_Price_z ~ 1 + Price.Fishmeal.AFI_z + (1 | port_ID))
landing_model <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z +
                        (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z || cluster))

fit_qMSQD_endog <- readRDS(here::here("Estimations", "fit_qMSQD_endog.RDS"))

### Add sardine SDM  ###
landing_model_b <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z +
                        PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open +
                          (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z +
                        PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open || cluster))

fit_qMSQD_endog_b <- readRDS(here::here("Estimations", "fit_qMSQD_endog_b.RDS"))


### Only interaction ###
landing_model_c <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z +
                        PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z +
                          (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z +
                        PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z || cluster))

fit_qMSQD_endog_c <- readRDS(here::here("Estimations", "fit_qMSQD_endog_c.RDS"))

### Without interaction ###
landing_model_d <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z
                      + PSDN_SDM_60_z:PSDN.Open
                      + (1 | port_ID)
                      + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z
                      + PSDN_SDM_60_z:PSDN.Open || cluster))

fit_qMSQD_endog_d <- readRDS(here::here("Estimations", "fit_qMSQD_endog_d.RDS"))

## Try also including PSDN_open separate
landing_model_b_2 <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z
                        + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open + PSDN.Open
                        + (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z
                        + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open + PSDN.Open || cluster))

fit_qMSQD_endog_b_2 <- readRDS(here::here("Estimations", "fit_qMSQD_endog_b_2.RDS"))


### Only interaction ###
landing_model_c_2 <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z
                        + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN.Open 
                        + (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
                                           + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN.Open || cluster))

fit_qMSQD_endog_c_2 <- readRDS(here::here("Estimations", "fit_qMSQD_endog_c_2.RDS"))


### Without interaction ###
landing_model_d_2 <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
                      + PSDN_SDM_60_z:PSDN.Open + PSDN.Open
                      + (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
                      + PSDN_SDM_60_z:PSDN.Open + PSDN.Open || cluster))

fit_qMSQD_endog_d_2 <- readRDS(here::here("Estimations", "fit_qMSQD_endog_d_2.RDS"))



#####################################################################################################
### Preferred model with diesel price by port
landing_model_b_3 <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
                       + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open + PSDN.Open
                       + diesel.price.AFI_z
                       + (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
                       + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open + PSDN.Open
                       + diesel.price.AFI_z || cluster))

fit_qMSQD_endog_b_3 <-
  brm(data = dataset_msqd_landing,
      family = gaussian,
      price_model + landing_model_b_3 + set_rescor(TRUE),
      prior = c(# E model
        prior(normal(0, 1), class = b, resp = MSQDPricez),
        prior(exponential(1), class = sigma, resp = MSQDPricez),
        # W model
        prior(normal(0, 1), class = b, resp = logMSQDLandings),
        prior(exponential(1), class = sigma, resp = logMSQDLandings),
        # rho
        prior(lkj(2), class = rescor)),
      iter = 2000, warmup = 1000, chains = 2, cores = 4,
      file = "Estimations/fit_qMSQD_endog_b_3")



# LOO comparision between models ###
# fit_qMSQD_endog     <- add_criterion(fit_qMSQD_endog, "loo")
# fit_qMSQD_endog_b   <- add_criterion(fit_qMSQD_endog_b, "loo")
# fit_qMSQD_endog_c   <- add_criterion(fit_qMSQD_endog_c, "loo")
# fit_qMSQD_endog_d   <- add_criterion(fit_qMSQD_endog_d, "loo")
# fit_qMSQD_endog_b_2 <- add_criterion(fit_qMSQD_endog_b_2, "loo", overwrite = TRUE)
# fit_qMSQD_endog_c_2 <- add_criterion(fit_qMSQD_endog_c_2, "loo", overwrite = TRUE)
# fit_qMSQD_endog_d_2 <- add_criterion(fit_qMSQD_endog_d_2, "loo", overwrite = TRUE)
fit_qMSQD_endog_b_3 <- add_criterion(fit_qMSQD_endog_b_3, "loo", overwrite = TRUE)



loo_compare(fit_qMSQD_endog,
            fit_qMSQD_endog_b,
            fit_qMSQD_endog_c,
            fit_qMSQD_endog_d,
            fit_qMSQD_endog_b_2,
            fit_qMSQD_endog_c_2,
            fit_qMSQD_endog_d_2,
            fit_qMSQD_endog_b_3,
            criterion = "loo")

tab_model(fit_qMSQD_endog_b_3)


## Add new fuel prices to the best model ##




#----------------------------------------------------
## Model summary ##
fit_qMSQD <- fit_qMSQD_endog_b_2
coef(fit_qMSQD)

library(patchwork)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tibble)
theme_set(theme_sjplot())


### Posterior predictive check ###
pp_check(fit_qMSQD, resp = "logMSQDLandings") + ggtitle('Market Squid (SDM: Spawning aggregation model)') +
  scale_color_manual(name = "", values = c("y" = "royalblue4", "yrep" = "azure3"),
                     labels = c("y" = "Observed", "yrep" = "Replicated")) + 
  theme(legend.position = "none", plot.title = element_text(size=12, face="bold.italic"))  + 
  xlim(-5, 11) + xlab("Landing (tons)")


### Population parameters ###
summary(fit_qMSQD)
mcmc_plot(fit_qMSQD, regex = TRUE, variable = 
            c("b_logMSQDLandings_MSQD_SPAWN_SDM_90_z", 
              "b_logMSQDLandings_MSQD_Price_z",
              "b_logMSQDLandings_Length_z",
              "b_logMSQDLandings_Intercept")) +
theme(axis.text.y = element_text(hjust = 0)) + scale_y_discrete(
labels = c(
  "b_logMSQDLandings_MSQD_Price_z" = "MSQD price",
  "b_logMSQDLandings_MSQD_SPAWN_SDM_90_z" = "MSQD availability (SDM)",
  "b_logMSQDLandings_Length_z" = "Vessel length",
  "b_logMSQDLandings_Intercept" = "Intercept"))

#------------------------------------------------------
### Group parameters ###
#### By port ID

# brmstools::coefplot(fit_qMSQD,
#                     pars = "MSQD_SPAWN_SDM_90_z",
#                     grouping = "port_ID",
#                     r_intervals = TRUE,
#                     r_col = "firebrick")

# coeff_port_sdm <- coef(fit_qMSQD)$port_ID[, c(1, 3:4), 2] %>%
#   as_tibble() %>% round(digits = 2) %>% mutate(port_ID = as.factor(1:n()))
#   ggplot(coeff_port_sdm, aes(x=port_ID, y=Estimate)) +
#   geom_point() +  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5),
#     width=.2, position=position_dodge(0.05)) + coord_flip() + ggtitle("Squid SDM estimates") +
#   ylab("Coefficient") + xlab("") + 
#     scale_x_discrete(labels=c("1" = "Los Angeles", 
#                               "4" = "Monterey",
#                               "2" = "Santa Barbara", 
#                               "3" = "San Diego"))
#   
# 
#   coeff_port_int <- coef(fit_qMSQD)$port_ID[, c(1, 3:4), 3] %>%
#     as_tibble() %>% round(digits = 2) %>% mutate(port_ID = as.factor(1:n()))
#   ggplot(coeff_port_int, aes(x=port_ID, y=Estimate)) + geom_point() +
#     geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5),
#                   width=.2, position=position_dodge(0.05)) + coord_flip() +
#     ggtitle("Sardine SDM") +  ylab("Coefficient") + xlab("") +
#     scale_x_discrete(labels=c("1" = "Los Angeles",
#                               "4" = "Monterey",
#                               "2" = "Santa Barbara",
#                               "3" = "San Diego"))
#   
# coeff_port_int <- coef(fit_qMSQD)$port_ID[, c(1, 3:4), 4] %>%
#   as_tibble() %>% round(digits = 2) %>% mutate(port_ID = as.factor(1:n()))
#   ggplot(coeff_port_int, aes(x=port_ID, y=Estimate)) + geom_point() +
#   geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5),
#                 width=.2, position=position_dodge(0.05)) + coord_flip() +
#   ggtitle("Squid SDM x Sardine SDM") +  ylab("Coefficient") + xlab("") +
#   scale_x_discrete(labels=c("1" = "Los Angeles",
#                             "4" = "Monterey",
#                             "2" = "Santa Barbara",
#                             "3" = "San Diego"))


coef(fit_qMSQD)$cluster
#### By clusters 
coeff_cluster_sdm <- coef(fit_qMSQD)$cluster[, c(1, 3:4), 2] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
  ggplot(coeff_cluster_sdm, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5), 
  width=.2, position=position_dodge(0.05)) + ggtitle("Squid SDM") +  
  xlab("Coefficient") + ylab("Cluster") 
  
coeff_cluster_osdm <- coef(fit_qMSQD)$cluster[, c(1, 3:4), 3] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
  ggplot(coeff_cluster_osdm, aes(y=cluster, x=Estimate)) + geom_point() +  
  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5), 
  width=.2, position=position_dodge(0.05)) + ggtitle("MSQD Price") + 
    xlab("Coefficient") + ylab("Cluster")  
  
coeff_cluster_int <- coef(fit_qMSQD)$cluster[, c(1, 3:4), 4] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
  ggplot(coeff_cluster_int, aes(y=cluster, x=Estimate)) + geom_point() +  
  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5), 
                  width=.2, position=position_dodge(0.05)) + ggtitle("Lenght") + 
    xlab("Coefficient") + ylab("Cluster")  
  

  

# ### Hypothesis test ###
# hypothesis(fit_qMSQD, "MSQD_SPAWN_SDM_90 = 0") 
  
# ### Compare multilevel effects ###
# as_draws_df(fit_qMSQD, add_chain = T) %>%
#   ggplot(aes(x = sd_cluster__MSQD_SPAWN_SDM_90)) +
#   geom_density(size = 0, fill = "orange1", alpha = 3/4) +
#   geom_density(aes(x = sd_port_ID__MSQD_SPAWN_SDM_90),
#                size = 0, fill = "orange4", alpha = 3/4) +
#   scale_y_continuous(NULL, breaks = NULL) +
#   labs(title = expression(sigma), subtitle = "Market squid SDM") +
#   annotate("text", x = 10, y = 1/10, label = "Port area", color = "orange4") +
#   annotate("text", x = 4, y = 1/5, label = "Cluster", color = "orange1") +
#   theme_fivethirtyeight()


### Conditional effects ###

  
  #### By port_area
  conditions <- data.frame(cluster = unique(dataset_msqd$PORT_AREA_ID))
  rownames(conditions) <- unique(dataset_msqd$PORT_AREA_CODE)
  
  conditional_effects_msqd_sdm <-
    conditional_effects(
      fit_qMSQD_endog_b_2, 
      "MSQD_SPAWN_SDM_90_z",                
      surface=TRUE, 
      conditions = conditions, 
      re_formula = NULL)#, transform = log, method = "posterior_predict"))
  
  plot(conditional_effects_msqd_sdm, plot = FALSE, nrow = 3, ncol = 2)[[2]] + 
    ggtitle('Market squid availability effect on squid landings') +
    theme(plot.title = element_text(size=9, face="bold.italic"),
          axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
    scale_x_continuous(name = "Prob(Presence): MSQD") +
    scale_y_continuous(name = element_blank())
  
  conditional_effects_psdn_sdm <-
    conditional_effects(
      fit_qMSQD, 
      "PSDN_SDM_60_z",                
      surface=TRUE, 
      conditions = conditions, 
      re_formula = NULL)#, transform = log, method = "posterior_predict"))
  
  plot(conditional_effects_psdn_sdm, plot = FALSE, nrow = 3, ncol = 2)[[2]] + 
    ggtitle('Pacific sardine availability effect on squid landings') +
    theme(plot.title = element_text(size=9, face="bold.italic"),
          axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
    scale_x_continuous(name = "Prob(Presence)") +
    scale_y_continuous(name = element_blank())
  
  rm(conditional_effects_msqd_sdm, conditional_effects_psdn.open, conditional_effects_psdn_sdm)
  
  
  
#### By cluster
conditions <- data.frame(cluster = unique(dataset_msqd$cluster))
rownames(conditions) <- unique(dataset_msqd$group_all)

conditional_effects_msqd_sdm <-
  conditional_effects(
    fit_qMSQD_endog_b_2, 
    "MSQD_SPAWN_SDM_90_z",                
    surface=TRUE, 
    conditions = conditions, 
    re_formula = NULL)#, transform = log, method = "posterior_predict"))

plot(conditional_effects_msqd_sdm, plot = FALSE, nrow = 3, ncol = 2)[[2]] + 
  ggtitle('Market squid availability effect on squid landings') +
  theme(plot.title = element_text(size=9, face="bold.italic"),
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_continuous(name = "Prob(Presence)") +
  scale_y_continuous(name = element_blank())
                  
conditional_effects_psdn_sdm <-
  conditional_effects(
    fit_qMSQD_endog_b_2, 
    "PSDN_SDM_60_z",                
    surface=TRUE, 
    conditions = conditions, 
    re_formula = NULL)#, transform = log, method = "posterior_predict"))

plot(conditional_effects_psdn_sdm, plot = FALSE, nrow = 3, ncol = 2)[[2]] + 
  ggtitle('Pacific sardine availability effect on squid landings') +
  theme(plot.title = element_text(size=9, face="bold.italic"),
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_continuous(name = "Prob(Presence)") +
  scale_y_continuous(name = element_blank())

rm(conditional_effects_msqd_sdm, conditional_effects_psdn.open, conditional_effects_psdn_sdm)
     

### Interaction effects ###
c_eff_int_psdn_msqd_b <- (conditional_effects(
  fit_qMSQD_endog_b, "PSDN_SDM_60_z:MSQD_SPAWN_SDM_90_z", 
  surface=TRUE, 
  conditions = conditions, re_formula = NULL))

plot(c_eff_int_psdn_msqd, plot = FALSE)[[2]] + 
  ggtitle('(a) Pacific sardine x Market squid') +
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: MSQD")) +
  scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): MSQD")


### Predictions ###

#### Using the data estimation
set.seed(123)
prediction <- cbind(predict(fit_qMSQD), dataset_msqd_landing)
prediction$LANDING_YEAR <- as.numeric(as.character(prediction$LANDING_YEAR))

meltdf <- prediction %>% 
  dplyr::select(Estimate, ln_MSQD_Landings, LANDING_YEAR, PORT_AREA_CODE) %>%
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
  summarise(Est_landings = sum(Estimate), Landings = sum(ln_MSQD_Landings)) %>%
  gather(key = Variable, value = value,
         c("Est_landings", "Landings"))

ggplot(meltdf, aes(x=LANDING_YEAR, y = value, colour = Variable)) + 
  geom_line(size=1) + 
  facet_wrap(~PORT_AREA_CODE)

meltdf <- prediction %>% 
  dplyr::select(Estimate, ln_MSQD_Landings, LANDING_YEAR, group_all) %>%
  group_by(LANDING_YEAR, group_all) %>% 
  summarise(Est_landings = sum(Estimate), Landings = sum(ln_MSQD_Landings)) %>%
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
