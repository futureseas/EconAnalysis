######################################################################
## Simulate aggregate landings -- (Merging DCM with Landings model) ##
######################################################################

rm(list = ls(all.names = TRUE)) 
gc()

## Read packages 
library("dplyr") 
library("tidyr")
library("zoo")
library('ggplot2')
library("reshape2")
library(data.table)

## Load predicted participation
part_pred <- read.csv(here::here("Participation", "R", "monthly_participation_pred.csv")) 
max_year <- max(part_pred$LANDING_YEAR)
min_year <- min(part_pred$LANDING_YEAR)

#########################
## Historical landings ##
#########################
historical_landings <- read.csv(file ="C:\\Data\\PacFIN data\\dataset_estimation.csv") %>%
  select(c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE", 
           "PSDN_Landings", "MSQD_Landings", "NANC_Landings")) %>%
  dplyr::filter(LANDING_YEAR >= min_year & LANDING_YEAR <= max_year) %>%
  group_by(LANDING_YEAR, PORT_AREA_CODE) %>%
  summarize(PSDN_Landings = sum(PSDN_Landings, na.rm = TRUE),
            MSQD_Landings = sum(MSQD_Landings, na.rm = TRUE),
            NANC_Landings = sum(NANC_Landings, na.rm = TRUE)) %>%
  group_by(PORT_AREA_CODE) %>%
  summarize(PSDN_Landings = sum(PSDN_Landings, na.rm = TRUE),
            MSQD_Landings = sum(MSQD_Landings, na.rm = TRUE),
            NANC_Landings = sum(NANC_Landings, na.rm = TRUE))

##########################
## Load historical data ##
##########################
historical_data <- read.csv(file ="C:\\Data\\PacFIN data\\dataset_estimation.csv") 
historical_data <- historical_data %>%
  select(c("VESSEL_NUM", "LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE",
           "MSQD_Price_z", "PSDN_Price_z", "NANC_Price_z", "Price.Fishmeal.AFI_z",
           "MSQD_SPAWN_SDM_90", "NANC_SDM_20", "Length_z", "PSDN_SDM_60", 
           "PSDN.Open", "MSQD.Open","group_all", "diesel.price.AFI_z", 
           "PSDN_Landings", "MSQD_Landings", "NANC_Landings")) 

#######################
## Average by vessel ##
#######################
vessel_data <- historical_data %>%  select(c("VESSEL_NUM", "group_all", "Length_z")) %>% unique()


####################################
## Average by port area per month ##
####################################
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets_raw<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)
port_area <- read.csv(file = here::here("Data", "Ports", "ports_area_and_name_codes.csv"))
Tickets_raw <- Tickets_raw %>% merge(port_area, by = c("PACFIN_PORT_CODE"), all.x = TRUE)
rm(port_area)

### get all combinations
all_ports <- Tickets_raw %>% select(c("PORT_AREA_CODE")) %>% unique() %>% 
  mutate(join = 1) %>% drop_na()
all_years <- as.data.frame(c(2000:2020)) 
all_months <- expand.grid(LANDING_YEAR = unique(all_years$`c(2000:2020)`), LANDING_MONTH = 1:12) %>%
  mutate(join = 1)
data_ports <- merge(all_ports, all_months, by = "join", all = TRUE, allow.cartesian=TRUE)
rm(all_months, all_ports, all_years)



agency_codes <- read.csv(file = "C:\\Data\\PacFIN data\\PacFIN_month.csv") %>% 
  dplyr::select('AGENCY_CODE', 'PORT_AREA_CODE') %>% 
  unique() %>% drop_na()
data_ports <- merge(data_ports, agency_codes,
                    by = "PORT_AREA_CODE", all = TRUE, 
                    allow.cartesian=TRUE)


### Merge SDM by year/month/port
ports_area_codes <- read.csv(file = "C:\\Data\\PacFIN data\\PacFIN_month.csv") %>% 
  dplyr::select('PORT_NAME', 'AGENCY_CODE', 'PORT_AREA_CODE') %>% 
  unique()

SDM_port_PSDN <- read.csv(file = here::here("Landings", "SDM", "PSDN_SDM_port_month.csv")) %>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_MONTH, LANDING_YEAR) %>% 
  summarize(PSDN_SDM_60 = mean(SDM_60))

SDM_port_MSQD_Spawn <- read.csv(file = here::here("Landings", "SDM", "MSQD_Spawn_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_YEAR, LANDING_MONTH) %>% 
  summarize(MSQD_SPAWN_SDM_90 = mean(SDM_SPAWN_90))

SDM_port_NANC <- read.csv(file = here::here("Landings", "SDM", "NANC_SDM_port_month.csv"))%>% 
  merge(ports_area_codes, by = c("PORT_NAME", "AGENCY_CODE"), all.x = TRUE) %>% 
  group_by(PORT_AREA_CODE, LANDING_MONTH, LANDING_YEAR) %>% 
  summarize(NANC_SDM_20 = mean(SDM_20))

data_ports <- merge(data_ports, SDM_port_PSDN,
                    by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)
data_ports <- merge(data_ports, SDM_port_MSQD_Spawn, 
                              by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) 
data_ports <- merge(data_ports, SDM_port_NANC, 
                              by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)
rm(SDM_port_PSDN, SDM_port_NANC, SDM_port_MSQD_Spawn, ports_area_codes)


#----------------------------------------------------------------------------------------------
## Calculate year price for N/A's using PacFIN port code, port area code, state and year/month.

Tickets_raw$AFI_PRICE_PER_MTON <- Tickets_raw$AFI_PRICE_PER_POUND * 2.20462 * 1000


## Sardine
price.data <- Tickets_raw %>% 
  dplyr::filter(PACFIN_SPECIES_CODE == "PSDN") %>%
  group_by(PORT_AREA_CODE, LANDING_YEAR, LANDING_MONTH) %>%
  summarise(PSDN_Price = mean(AFI_PRICE_PER_MTON))

data_ports <- merge(data_ports, price.data, 
                    by = c('PORT_AREA_CODE', 'LANDING_YEAR', 'LANDING_MONTH'),
                    all.x = TRUE, all.y = FALSE)

#### (b) Using state
price.data <- Tickets_raw %>% 
  dplyr::filter(PACFIN_SPECIES_CODE == "PSDN") %>%
  group_by(AGENCY_CODE, LANDING_YEAR, LANDING_MONTH) %>%
  summarise(PSDN_Price_State = mean(AFI_PRICE_PER_MTON))
data_ports <- merge(data_ports, price.data, 
                    by = c('AGENCY_CODE', 'LANDING_YEAR', 'LANDING_MONTH'),
                    all.x = TRUE, all.y = FALSE)

#### (c) month/year
price.data <- Tickets_raw %>% 
  dplyr::filter(PACFIN_SPECIES_CODE == "PSDN") %>%
  group_by(LANDING_YEAR, LANDING_MONTH) %>%
  summarise(PSDN_Price_Date = mean(AFI_PRICE_PER_MTON))
data_ports <- merge(data_ports, price.data, 
                    by = c('LANDING_YEAR', 'LANDING_MONTH'),
                    all.x = TRUE, all.y = FALSE)
data_ports <- data_ports %>%
 mutate(PSDN_Price = ifelse(is.na(PSDN_Price), PSDN_Price_State, PSDN_Price)) %>%
  mutate(PSDN_Price = ifelse(is.na(PSDN_Price), PSDN_Price_Date, PSDN_Price))
rm(price.data)

## Squid
price.data <- Tickets_raw %>% 
  dplyr::filter(PACFIN_SPECIES_CODE == "MSQD") %>%
  group_by(PORT_AREA_CODE, LANDING_YEAR, LANDING_MONTH) %>%
  summarise(MSQD_Price = mean(AFI_PRICE_PER_MTON))

data_ports <- merge(data_ports, price.data, 
                    by = c('PORT_AREA_CODE', 'LANDING_YEAR', 'LANDING_MONTH'),
                    all.x = TRUE, all.y = FALSE)

#### (b) Using state
price.data <- Tickets_raw %>% 
  dplyr::filter(PACFIN_SPECIES_CODE == "MSQD") %>%
  group_by(AGENCY_CODE, LANDING_YEAR, LANDING_MONTH) %>%
  summarise(MSQD_Price_State = mean(AFI_PRICE_PER_MTON))
data_ports <- merge(data_ports, price.data, 
                    by = c('AGENCY_CODE', 'LANDING_YEAR', 'LANDING_MONTH'),
                    all.x = TRUE, all.y = FALSE)

#### (c) month/year
price.data <- Tickets_raw %>% 
  dplyr::filter(PACFIN_SPECIES_CODE == "MSQD") %>%
  group_by(LANDING_YEAR, LANDING_MONTH) %>%
  summarise(MSQD_Price_Date = mean(AFI_PRICE_PER_MTON))
data_ports <- merge(data_ports, price.data, 
                    by = c('LANDING_YEAR', 'LANDING_MONTH'),
                    all.x = TRUE, all.y = FALSE)
data_ports <- data_ports %>%
  mutate(MSQD_Price = ifelse(is.na(MSQD_Price), MSQD_Price_State, MSQD_Price)) %>%
  mutate(MSQD_Price = ifelse(is.na(MSQD_Price), MSQD_Price_Date, MSQD_Price))
rm(price.data)

## Anchovy
price.data <- Tickets_raw %>% 
  dplyr::filter(PACFIN_SPECIES_CODE == "NANC") %>%
  group_by(PORT_AREA_CODE, LANDING_YEAR, LANDING_MONTH) %>%
  summarise(NANC_Price = mean(AFI_PRICE_PER_MTON))

data_ports <- merge(data_ports, price.data, 
                    by = c('PORT_AREA_CODE', 'LANDING_YEAR', 'LANDING_MONTH'),
                    all.x = TRUE, all.y = FALSE)

#### (b) Using state
price.data <- Tickets_raw %>% 
  dplyr::filter(PACFIN_SPECIES_CODE == "NANC") %>%
  group_by(AGENCY_CODE, LANDING_YEAR, LANDING_MONTH) %>%
  summarise(NANC_Price_State = mean(AFI_PRICE_PER_MTON))
data_ports <- merge(data_ports, price.data, 
                    by = c('AGENCY_CODE', 'LANDING_YEAR', 'LANDING_MONTH'),
                    all.x = TRUE, all.y = FALSE)

#### (c) month/year
price.data <- Tickets_raw %>% 
  dplyr::filter(PACFIN_SPECIES_CODE == "NANC") %>%
  group_by(LANDING_YEAR, LANDING_MONTH) %>%
  summarise(NANC_Price_Date = mean(AFI_PRICE_PER_MTON))
data_ports <- merge(data_ports, price.data, 
                    by = c('LANDING_YEAR', 'LANDING_MONTH'),
                    all.x = TRUE, all.y = FALSE)
data_ports <- data_ports %>%
  mutate(NANC_Price = ifelse(is.na(NANC_Price), NANC_Price_State, NANC_Price)) %>%
  mutate(NANC_Price = ifelse(is.na(NANC_Price), NANC_Price_Date, NANC_Price))
rm(price.data)

### Include closure data
PSDN_closure <- read.csv("C:\\Data\\Closures\\PSDN_closures.csv")
MSQD_closure <- read.csv("C:\\Data\\Closures\\MSQD_closures.csv") 
data_ports <- merge(data_ports, PSDN_closure, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE, all.y = FALSE)
data_ports <- merge(data_ports, MSQD_closure, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE, all.y = FALSE)
rm(PSDN_closure, MSQD_closure)

### Include deflactor
Deflactor <- read.csv(file = "C:\\Data\\PacFIN data\\deflactor.csv")
data_ports <- merge(data_ports, Deflactor, by = c('LANDING_YEAR', 'LANDING_MONTH'), all.x = TRUE, all.y = FALSE)
rm(Deflactor)

### Include world's fish meal price as instrument
fish.meal <- read.csv(here::here("Data", "Instruments", "PFISHUSDM.csv"), header = TRUE, stringsAsFactors = FALSE)
fish.meal$DATE <- as.Date(fish.meal$DATE, format = "%m/%d/%Y") 
fish.meal$LANDING_YEAR  <- lubridate::year(fish.meal$DATE)
fish.meal$LANDING_MONTH <- lubridate::month(fish.meal$DATE)
fish.meal <- fish.meal %>% dplyr::select(-c('DATE')) %>% dplyr::rename(Price.Fishmeal = PFISHUSDM)
data_ports <- merge(data_ports, fish.meal, by = c('LANDING_YEAR', 'LANDING_MONTH'), all.x = TRUE, all.y = FALSE)
data_ports$Price.Fishmeal.AFI <- data_ports$Price.Fishmeal*data_ports$defl
rm(fish.meal)

# Create centerized variables and z-values 
data_ports <- data_ports %>%
  dplyr::mutate(MSQD_Price_z = ((MSQD_Price - mean(MSQD_Price, na.rm = TRUE)) / sd(MSQD_Price, na.rm = TRUE))) %>%
  dplyr::mutate(PSDN_Price_z = ((PSDN_Price - mean(PSDN_Price, na.rm = TRUE)) / sd(PSDN_Price, na.rm = TRUE))) %>%
  dplyr::mutate(NANC_Price_z = ((NANC_Price - mean(NANC_Price, na.rm = TRUE)) / sd(NANC_Price, na.rm = TRUE))) %>%
  dplyr::mutate(Price.Fishmeal.AFI_z = ((Price.Fishmeal.AFI - mean(Price.Fishmeal.AFI, na.rm = TRUE))/sd(Price.Fishmeal.AFI, na.rm = TRUE))) 
  
area_data <- data_ports %>% select('LANDING_YEAR', 'LANDING_MONTH', 'PORT_AREA_CODE',
              'MSQD_Price_z', 'PSDN_Price_z', 'NANC_Price_z',
              'Price.Fishmeal.AFI_z', 'MSQD_SPAWN_SDM_90', 'NANC_SDM_20', 'PSDN_SDM_60',
              'PSDN.Open', 'MSQD.Open') %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::mutate(WA.Restriction = ifelse(LANDING_MONTH <= 3, 1, 0))  

area_data$port_ID <- factor(area_data$PORT_AREA_CODE) 


#################################################################################
# Merge #
part_pred2 <- part_pred %>% 
  merge(vessel_data, by = "VESSEL_NUM", all.x = TRUE, all.y = FALSE) %>%
  merge(area_data, 
        by = c("PORT_AREA_CODE", "LANDING_YEAR", "LANDING_MONTH"), 
        all.x = TRUE, all.y = FALSE) %>%
  mutate(cluster_port = paste(group_all, PORT_AREA_CODE, sep = "-", collapse = NULL))

#################################################################################
# Load landings estimations
fit_qMSQD <- readRDS(here::here("Landings", "Estimations", "fit_qMSQD.RDS"))
fit_qPSDN <- readRDS(here::here("Landings", "Estimations", "fit_qPSDN.RDS"))
fit_qNANC <- readRDS(here::here("Landings", "Estimations", "fit_qNANC.RDS"))

#############
## Predict ##
#############
set.seed(123)

# ## Squid landings
# part_pred_MSQD <- part_pred %>% filter(PACFIN_SPECIES_CODE == "MSQD")
# prediction_MSQD <- predict(fit_qMSQD, newdata = part_pred_MSQD)

## Sardine landings
part_pred_PSDN <- part_pred2 %>% filter(PACFIN_SPECIES_CODE == "PSDN")
prediction_PSDN <- cbind(predict(fit_qPSDN, newdata = part_pred_PSDN, allow_new_levels = TRUE), part_pred_PSDN) %>%
  

## Anchovy landings
part_pred_NANC <- part_pred2 %>% filter(PACFIN_SPECIES_CODE == "NANC")
prediction_NANC <- cbind(predict(fit_qNANC, newdata = part_pred_NANC, allow_new_levels = TRUE), part_pred_NANC)




