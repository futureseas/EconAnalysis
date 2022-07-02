###########################
### Participation model ###
###########################

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

#----------------------------------------
# Market squid vessels and expansion grid

squid.ports <- PacFIN.month %>% 
  dplyr::filter(PACFIN_SPECIES_CODE == "MSQD") %>%
  group_by(VESSEL_NUM, LANDING_YEAR, PORT_AREA_CODE) %>%
  summarize(anual_rev = sum(AFI_EXVESSEL_REVENUE.sum, na.rm = TRUE)) %>%
  group_by(VESSEL_NUM, PORT_AREA_CODE) %>%
  summarize(anual_rev_port = mean(anual_rev, na.rm = TRUE)) %>%
  filter(anual_rev_port >= 1000) %>% drop_na() %>% dplyr::select(-c('anual_rev_port'))

squid.vessels <- PacFIN.month %>% 
  dplyr::filter(PACFIN_SPECIES_CODE == "MSQD") %>%
  group_by(VESSEL_NUM, LANDING_YEAR) %>%
  summarize(anual_rev = sum(AFI_EXVESSEL_REVENUE.sum, na.rm = TRUE)) %>% 
  group_by(VESSEL_NUM) %>%
  summarize(avg_anual_rev = mean(anual_rev, na.rm = TRUE)) %>% 
  filter(avg_anual_rev >= 5000)


### Create grid with all the years and month by port where a vessel participate
vessels = expand.grid(VESSEL_NUM = unique(squid.vessels$VESSEL_NUM), LANDING_YEAR = 2005:2020)
months = expand.grid(LANDING_YEAR = unique(vessels$LANDING_YEAR), LANDING_MONTH = 1:12)
squid.database <- left_join(vessels, months, by = "LANDING_YEAR")
squid.database <- left_join(squid.database, squid.ports, by = "VESSEL_NUM")


#------------------------------------------------------------------------------------------

### Change dataset to wide

sum_mean_fun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...), sum=sum(x, na.rm=TRUE, ...)) }

library(doBy)
PacFIN.month.dataset <- doBy::summaryBy(
  LANDED_WEIGHT_MTONS.sum + AFI_PRICE_PER_MTON.mean + AFI_EXVESSEL_REVENUE.sum + Length.mean 
  ~ LANDING_YEAR + LANDING_MONTH + VESSEL_NUM + PORT_AREA_CODE + PACFIN_SPECIES_CODE + AGENCY_CODE + group_all,
  FUN=sum_mean_fun, data=PacFIN.month) %>%
  dplyr::filter(AFI_EXVESSEL_REVENUE.sum.sum > 0) %>%
  dplyr::select(-c('LANDED_WEIGHT_MTONS.sum.mean', 'AFI_PRICE_PER_MTON.mean.sum',
                   'AFI_EXVESSEL_REVENUE.sum.mean', 'Length.mean.sum')) %>% 
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
  dplyr::select('LANDING_YEAR', 'LANDING_MONTH', 'VESSEL_NUM', 'PORT_AREA_CODE', 'group_all', 'AGENCY_CODE',
                'PSDN_LANDED_WEIGHT_MTONS.sum', 'MSQD_LANDED_WEIGHT_MTONS.sum', 'NANC_LANDED_WEIGHT_MTONS.sum',  
                'PSDN_AFI_PRICE_PER_MTON.mean', 'MSQD_AFI_PRICE_PER_MTON.mean', 'NANC_AFI_PRICE_PER_MTON.mean')
  


## Squid database 

squid.full.data <- left_join(squid.database, PacFIN.month.dataset,
  by = c("VESSEL_NUM", "PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"))
  squid.full.data[squid.full.data == "NaN"] <- NA
  squid.full.data <- squid.full.data %>% 
    group_by(VESSEL_NUM) %>%
    fill(group_all, .direction = "downup")
  squid.full.data <- squid.full.data %>% 
    group_by(PORT_AREA_CODE) %>%
    fill(AGENCY_CODE, .direction = "downup")
  

#-------------------------------------------------------------------------------------------------------



### Merge SDM by year/month/port ###

#### Merge data with SDM Pacific Sardine
SDM_port_PSDN <- read.csv(file = here::here("Data", "SDM", "PSDN_SDM_port_month.csv")) %>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_MONTH, LANDING_YEAR) %>% summarize(PSDN_SDM_60 = mean(SDM_60))
squid.full.data <- merge(squid.full.data, SDM_port_PSDN,
                              by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

#### Merge data with SDM Market Squid (spawn aggregation) #
SDM_port_MSQD_Spawn <- read.csv(file = here::here("Data", "SDM", "MSQD_Spawn_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_YEAR, LANDING_MONTH) %>% summarize(MSQD_SPAWN_SDM_90 = mean(SDM_SPAWN_90))
squid.full.data <- merge(squid.full.data, SDM_port_MSQD_Spawn, 
                              by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) 

#### Merge data with SDM Northern Anchovy #
SDM_port_NANC <- read.csv(file = here::here("Data", "SDM", "NANC_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_MONTH, LANDING_YEAR) %>% summarize(NANC_SDM_20 = mean(SDM_20))
dataset <- merge(squid.full.data, SDM_port_NANC, 
                 by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

rm(squid.full.data, 
   SDM_port_PSDN, SDM_port_NANC, SDM_port_MSQD_Spawn, ports_area_codes)


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
price.state.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_MTON.mean, 
                              by = list(dataset$LANDING_YEAR, dataset$LANDING_MONTH, dataset$AGENCY_CODE), FUN = mean, na.rm=T)
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


#---------------------------------------------------------------------------------------
## Include trend and average number of set for MSQD
avg.number.set <- read.csv(here::here("Data", "Logbook_output", "max_n_set.by.trip.csv"), header = TRUE, stringsAsFactors = FALSE)
dataset <- merge(dataset, avg.number.set, by = ('LANDING_YEAR'), all.x = TRUE, all.y = FALSE)





####################################################
####################################################

### Average by VESSEL_NUM, month and year ###

dataset_avg <- dataset %>% group_by(VESSEL_NUM, LANDING_YEAR, LANDING_MONTH, group_all) %>% 
  summarize(MSQD.Landings  = sum(MSQD_LANDED_WEIGHT_MTONS.sum , na.rm = TRUE),
            MSQD.SDM       = mean(MSQD_SPAWN_SDM_90           , na.rm = TRUE),
            PSDN.Open      = mean(PSDN.Open                   , na.rm = TRUE),
            MSQD.Open      = mean(MSQD.Open                   , na.rm = TRUE),
            Fishmeal.price = mean(Price.Fishmeal.AFI          , na.rm = TRUE),
            Diesel.price   = mean(diesel.price.AFI            , na.rm = TRUE))


#----------------------------------------------------------------------------------------
# Include participation variables
participation_variables <- read.csv(file ="C:\\GitHub\\EconAnalysis\\Data\\participation_variables.csv")
dataset_avg_part <- left_join(dataset_avg, participation_variables, by=c("VESSEL_NUM", "LANDING_MONTH", "LANDING_YEAR"))


### Calculate moving average ###
library(zoo)

df = dataset_avg_part %>%
  group_by(VESSEL_NUM) %>%
  arrange(VESSEL_NUM, LANDING_YEAR, LANDING_MONTH) %>%
  mutate(LAT.lag1        = lag(LAT,                  n = 1)) %>%
  mutate(REV.lag1        = lag(AFI_EXVESSEL_REVENUE, n = 1)) %>%
  mutate(Inertia.lag1    = lag(DISTANCE_A,           n = 1)) %>%
  mutate(MSQD.SDM.lag1   = lag(MSQD.SDM,             n = 1)) %>%
  mutate(Percentage.lag1 = lag(Percentage,           n = 1)) %>%
  mutate(diversity.lag1  = lag(diversity,            n = 1)) %>%
  mutate(landings.lag12  = lag(MSQD.Landings,        n = 12)) %>%
  mutate(LCG_MA               = rollapply(data = LAT.lag1       , width = 11, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(Average.Revenue_MA   = rollapply(data = REV.lag1       , width = 11, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(Inertia_MA           = rollapply(data = Inertia.lag1   , width = 11, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(MSQD.SDM_MA          = rollapply(data = MSQD.SDM.lag1  , width = 11, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(CPS.revenue_MA       = rollapply(data = Percentage.lag1, width = 11, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(Diversity_MA         = rollapply(data = diversity.lag1 , width = 11, FUN = mean, align = "right", fill = NA, na.rm = T))




#-----------------------------------------------
## Create dataset for estimation and run landing models 

df <- df %>%
  mutate(MSQD.Participation = ifelse(MSQD.Landings > 0, 1, 0)) %>%
  mutate(Past.MSQD.Participation = ifelse(landings.lag12 > 0, 1, 0))

df <- df %>%
  dplyr::select('VESSEL_NUM', 'LANDING_YEAR', 'LANDING_MONTH', 'group_all', 
    'MSQD.Participation', 'PSDN.Open', 'MSQD.Open', 'Fishmeal.price', 'Diesel.price', 
    'LCG_MA', 'Average.Revenue_MA', 'Inertia_MA', 'MSQD.SDM_MA', 'CPS.revenue_MA', 'Diversity_MA',
    'Past.MSQD.Participation') %>%
     drop_na()
                
df$group_all     <- factor(df$group_all)
df$LANDING_MONTH <- factor(df$LANDING_MONTH)




