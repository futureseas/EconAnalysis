#######################################
### Landing database for estimation ###
#######################################

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

## Read PacFIN database and deflactor
PacFIN.month <- read.csv(file = "C:\\Data\\PacFIN data\\PacFIN_month.csv") %>% 
  dplyr::filter(AFI_EXVESSEL_REVENUE.sum > 0)

Deflactor <- read.csv(file = "C:\\Data\\PacFIN data\\deflactor.csv")


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
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_NANC, 
                 by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

# #### Merge data with SDM Market Squid (JS Abundance model) #
# SDM_port_MSQD_JS_cpue <- read.csv(file = here::here("Data", "SDM", "MSQD_SDM_port_year_JS_abund_V2.csv")) %>%
#   merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
#   group_by(PORT_AREA_CODE, LANDING_YEAR) %>% summarize(MSQD_SDM_90_JS_CPUE = mean(SDM_90_JS_CPUE))
# PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_MSQD_JS_cpue, 
#                       by = c("PORT_AREA_CODE", "LANDING_YEAR"), all.x = TRUE)

#### Merge data with Total Crab Landings by port and month #
Landing_port_DCRB <- read.csv(file = "C:\\GitHub\\EconAnalysis\\Data\\Port landings\\DCRB_landings.csv") %>%
  group_by(PORT_AREA_CODE, LANDING_MONTH, LANDING_YEAR) %>% summarize(DCRB_LANDING = mean(ROUND_WEIGHT_MTONS, na.rm = TRUE))
dataset <- merge(PacFIN.month.dataset, Landing_port_DCRB, 
                          by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)


rm(PacFIN.month.dataset, 
   SDM_port_PSDN, SDM_port_NANC, SDM_port_MSQD, SDM_port_MSQD_Spawn, Landing_port_DCRB, ports_area_codes)


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


#### Using port area code
LANDING.port.area.DCRB <- aggregate(x=dataset$DCRB_LANDING,
                        by = list(dataset$LANDING_YEAR,
                                  dataset$LANDING_MONTH,
                                  dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
LANDING.port.area.DCRB <- LANDING.port.area.DCRB %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(DCRB.LANDING.port.area = x)
LANDING.port.area.DCRB[LANDING.port.area.DCRB == "NaN"] <- NA
dataset <- dataset %>%
  merge(LANDING.port.area.DCRB, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(DCRB_LANDING = ifelse(is.na(DCRB_LANDING), DCRB.LANDING.port.area, DCRB_LANDING))
rm(LANDING.port.area.DCRB)

dataset = subset(dataset, select = -c(DCRB.LANDING.port.area))


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
### Include deflactor
# CPI <- read.csv(here::here("Data", "CPI", "CPIAUCSL.csv"))
# CPI$DATE <- as.Date(CPI$DATE, format = "%Y-%m-%d") 
# CPI$LANDING_YEAR  <- lubridate::year(CPI$DATE)
# CPI$LANDING_MONTH <- lubridate::month(CPI$DATE)
# CPI <- CPI %>% dplyr::select(-c('DATE')) %>% dplyr::rename(CPI = CPIAUCSL)
dataset <- merge(dataset, Deflactor, by = c('LANDING_YEAR', 'LANDING_MONTH'), all.x = TRUE, all.y = FALSE)
rm(Deflactor)

#---------------------------------------------------------------------------------------
### Include world's fish meal price as instrument
fish.meal <- read.csv(here::here("Data", "Instruments", "PFISHUSDM.csv"), header = TRUE, stringsAsFactors = FALSE)
fish.meal$DATE <- as.Date(fish.meal$DATE, format = "%m/%d/%Y") 
fish.meal$LANDING_YEAR  <- lubridate::year(fish.meal$DATE)
fish.meal$LANDING_MONTH <- lubridate::month(fish.meal$DATE)
fish.meal <- fish.meal %>% dplyr::select(-c('DATE')) %>% dplyr::rename(Price.Fishmeal = PFISHUSDM)
dataset <- merge(dataset, fish.meal, by = c('LANDING_YEAR', 'LANDING_MONTH'), all.x = TRUE, all.y = FALSE)
dataset$Price.Fishmeal.AFI <- dataset$Price.Fishmeal*dataset$defl
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
dataset$diesel.price.AFI <- dataset$diesel.price*dataset$defl
rm(fuel.prices, fuel.prices.CA, fuel.prices.OR, fuel.prices.WA, fuel.prices.state)




#---------------------------------------------------------------------------------------
## Include trend and average number of set for MSQD
avg.number.set <- read.csv(here::here("Data", "Logbook_output", "max_n_set.by.trip.csv"), header = TRUE, stringsAsFactors = FALSE)
dataset <- merge(dataset, avg.number.set, by = ('LANDING_YEAR'), all.x = TRUE, all.y = FALSE)


#---------------------------------------
## Summary statistics ##
dataset <- dataset %>% 
  dplyr::rename(PSDN_Price = PSDN_AFI_PRICE_PER_MTON.mean) %>%
  dplyr::rename(MSQD_Price = MSQD_AFI_PRICE_PER_MTON.mean) %>%
  dplyr::rename(NANC_Price = NANC_AFI_PRICE_PER_MTON.mean) %>%
  dplyr::rename(PSDN_Landings = PSDN_LANDED_WEIGHT_MTONS.sum) %>%
  dplyr::rename(MSQD_Landings = MSQD_LANDED_WEIGHT_MTONS.sum) %>%
  dplyr::rename(NANC_Landings = NANC_LANDED_WEIGHT_MTONS.sum)

## NA crab landings to zero
dataset <- dataset %>%
  dplyr::mutate(DCRB_LANDING = ifelse(is.na(DCRB_LANDING), 0, DCRB_LANDING))
# Create centerized variables and z-values 

dataset <- dataset %>%
  dplyr::mutate(MSQD_Price_z = ((MSQD_Price - mean(MSQD_Price, na.rm = TRUE)) / sd(MSQD_Price, na.rm = TRUE))) %>%
  dplyr::mutate(PSDN_Price_z = ((PSDN_Price - mean(PSDN_Price, na.rm = TRUE)) / sd(PSDN_Price, na.rm = TRUE))) %>%
  dplyr::mutate(NANC_Price_z = ((NANC_Price - mean(NANC_Price, na.rm = TRUE)) / sd(NANC_Price, na.rm = TRUE))) %>%
  dplyr::mutate(MSQD_SPAWN_SDM_90_z = ((MSQD_SPAWN_SDM_90 - mean(MSQD_SPAWN_SDM_90, na.rm = TRUE))/sd(MSQD_SPAWN_SDM_90, na.rm = TRUE))) %>%
  dplyr::mutate(MSQD_SDM_90_z = ((MSQD_SDM_90 - mean(MSQD_SDM_90, na.rm = TRUE))/sd(MSQD_SDM_90, na.rm = TRUE))) %>%
  dplyr::mutate(PSDN_SDM_60_z = ((PSDN_SDM_60 - mean(PSDN_SDM_60, na.rm = TRUE))/sd(PSDN_SDM_60, na.rm = TRUE))) %>%
  dplyr::mutate(DCRB_LANDING_z = ((DCRB_LANDING - mean(DCRB_LANDING, na.rm = TRUE))/sd(DCRB_LANDING, na.rm = TRUE))) %>%
  dplyr::mutate(NANC_SDM_20_z = ((NANC_SDM_20 - mean(NANC_SDM_20, na.rm = TRUE))/sd(NANC_SDM_20, na.rm = TRUE))) %>%
  dplyr::mutate(Price.Fishmeal_z = ((Price.Fishmeal - mean(Price.Fishmeal, na.rm = TRUE))/sd(Price.Fishmeal, na.rm = TRUE))) %>%
  dplyr::mutate(diesel.price_z = ((diesel.price - mean(diesel.price, na.rm = TRUE))/sd(diesel.price, na.rm = TRUE))) %>%
  dplyr::mutate(Price.Fishmeal.AFI_z = ((Price.Fishmeal.AFI - mean(Price.Fishmeal.AFI, na.rm = TRUE))/sd(Price.Fishmeal.AFI, na.rm = TRUE))) %>%
  dplyr::mutate(diesel.price.AFI_z = ((diesel.price.AFI - mean(diesel.price.AFI, na.rm = TRUE))/sd(diesel.price.AFI, na.rm = TRUE))) %>%
  dplyr::mutate(Length_z = ((Length - mean(Length, na.rm = TRUE))/sd(Length, na.rm = TRUE))) %>%
  dplyr::mutate(avg_set_MSQD_z = ((avg_set_MSQD - mean(avg_set_MSQD, na.rm = TRUE))/sd(avg_set_MSQD, na.rm = TRUE)))


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

write.csv(dataset,"C:\\Data\\PacFIN data\\dataset_estimation.csv", row.names = FALSE)