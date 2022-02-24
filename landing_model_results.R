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
library("googlesheets4")

## Google access
gs4_auth(
  email = "fequezad@ucsc.edu",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)

## Read PacFIN database 
PacFIN.month <- read.csv(file ="C:\\Data\\PacFIN data\\PacFIN_month.csv")

#---------------------------------------------------
# Methods

## Organize dataset

# PacFIN.month.dataset = PacFIN.month.cluster[,!grepl("*_LE_",names(PacFIN.month.cluster))]
PacFIN.month.dataset <- PacFIN.month.dataset %>% filter(LANDING_YEAR >= 2000) %>% # filter(PACFIN_SPECIES_CODE == "PSDN" | PACFIN_SPECIES_CODE == "MSQD") %>%
  dplyr::select(LANDING_YEAR, LANDING_MONTH, PORT_NAME, VESSEL_NUM, PSDN_SDM_60, MSQD_SDM_90,
                MSQD_SPAWN_SDM_90, LANDED_WEIGHT_MTONS.sum, AFI_PRICE_PER_KG.mean, 
                PACFIN_SPECIES_CODE, PACFIN_PORT_CODE, AGENCY_CODE, group) %>% 
  mutate(AFI_PRICE_PER_KG.mean = na_if(AFI_PRICE_PER_KG.mean, 0)) %>% 
  filter(group != is.na(group)) %>% 
  melt(id.vars=c("LANDING_YEAR", "LANDING_MONTH", "VESSEL_NUM",  "PORT_NAME", 
                 "PACFIN_PORT_CODE", "PACFIN_SPECIES_CODE", "AGENCY_CODE", "group")) %>% 
  dcast(LANDING_YEAR + LANDING_MONTH + VESSEL_NUM + PORT_NAME + PACFIN_PORT_CODE + AGENCY_CODE + group ~
          PACFIN_SPECIES_CODE + variable, fun.aggregate=mean, rm.na = T) %>%
  dplyr::select('LANDING_YEAR', 'LANDING_MONTH', 'VESSEL_NUM', 'PORT_NAME', 'PACFIN_PORT_CODE', 
                'AGENCY_CODE', 'group', 'PSDN_LANDED_WEIGHT_MTONS.sum', 'MSQD_LANDED_WEIGHT_MTONS.sum', 
                'PSDN_AFI_PRICE_PER_KG.mean', 'MSQD_AFI_PRICE_PER_KG.mean')

# Add port area code #  
PacFIN.month.dataset[PacFIN.month.dataset == "NaN"] <- NA
port_area <- read.csv(file = here::here("Data", "Ports", "ports_area_and_name_codes.csv"))
PacFIN.month.dataset <- PacFIN.month.dataset %>% 
  merge(port_area, by = c("PACFIN_PORT_CODE"), all.x = TRUE)

# Exclude port areas without landings of MSQD or PSDN #
selected.ports <- PacFIN.month.dataset %>% 
  group_by(PORT_AREA_CODE) %>% 
  summarise(landings_PSDN = sum(PSDN_LANDED_WEIGHT_MTONS.sum, na.rm = TRUE),
            landings_MSQD = sum(MSQD_LANDED_WEIGHT_MTONS.sum, na.rm = TRUE)) %>%
  rowwise() %>% mutate(total = min(landings_PSDN, landings_MSQD)) %>%
  filter(total > 0) %>% select(PORT_AREA_CODE) %>% mutate(port_included = 1)
PacFIN.month.dataset <- PacFIN.month.dataset %>% 
  merge(selected.ports, by = c("PORT_AREA_CODE"), all.x = TRUE) %>%
  filter(port_included == 1) 

# Create ID data and change NaN to NA #

PacFIN.month.dataset <- PacFIN.month.dataset %>%
  mutate(PORT_ID = as.numeric(as.factor(PORT_NAME))) %>%
  mutate(PORT_CODE_ID = as.numeric(as.factor(PACFIN_PORT_CODE))) %>% 
  mutate(PORT_AREA_ID = as.numeric(as.factor(PORT_AREA_CODE))) 
PacFIN.month.dataset[PacFIN.month.dataset == "NaN"] <- NA

# Merge SDM and ACL by year/month/port

## Merge data with SDM Pacific Sardine
SDM_port_PSDN <- read.csv(file = here::here("Data", "SDM", "PSDN_SDM_port_month.csv"))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_PSDN, 
                              by = c("PORT_NAME", "AGENCY_CODE","LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

## Merge data with SDM Market Squid
SDM_port_MSQD <- read.csv(file = here::here("Data", "SDM", "MSQD_SDM_port_month.csv"))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_MSQD, 
                              by = c("PORT_NAME", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

## Merge data with SDM Market Squid 
SDM_port_MSQD_Spawn <- read.csv(file = here::here("Data", "SDM", "MSQD_Spawn_SDM_port_month.csv"))
PacFIN.month.dataset <- merge(PacFIN.month.dataset, SDM_port_MSQD_Spawn, 
                              by = c("PORT_NAME", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)  

dataset <- PacFIN.month.dataset %>%
  dplyr::rename(PSDN_SDM_60 = SDM_60) %>% dplyr::rename(MSQD_SDM_90 = SDM_90) %>%
  dplyr::rename(MSQD_SPAWN_SDM_90 = SDM_SPAWN_90)


rm(PacFIN.month.dataset, port_area, selected.ports, SDM_port_PSDN, SDM_port_MSQD, SDM_port_MSQD_Spawn)

```

```{r calculate-sdm, include=FALSE}
# Calculate year SDM for N/A's using PacFIN port code and port area code.

## Pacific sardine

### (a) Using PacFIN port code
SDM.port.code.PSDN <- aggregate(x=dataset$PSDN_SDM_60, by = list(dataset$LANDING_YEAR, 
                                                                 dataset$LANDING_MONTH, dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
SDM.port.code.PSDN <- SDM.port.code.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
  dplyr::rename(PSDN.SDM.port.code = x)
SDM.port.code.PSDN[SDM.port.code.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.code.PSDN, 
        by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>% 
  mutate(PSDN_SDM_60 = ifelse(is.na(PSDN_SDM_60), PSDN.SDM.port.code, PSDN_SDM_60))
rm(SDM.port.code.PSDN)

### (b) Using port area code
SDM.port.area.PSDN <- aggregate(x=dataset$PSDN_SDM_60, by = list(dataset$LANDING_YEAR,
                                                                 dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.PSDN <- SDM.port.area.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(PSDN.SDM.port.area = x)
SDM.port.area.PSDN[SDM.port.area.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(PSDN_SDM_60 = ifelse(is.na(PSDN_SDM_60), PSDN.SDM.port.area, PSDN_SDM_60))
rm(SDM.port.area.PSDN)

dataset = subset(dataset, select = -c(PSDN.SDM.port.code, PSDN.SDM.port.area))

## Market squid 

### (a) Using PacFIN port code
SDM.port.code.MSQD <- aggregate(x=dataset$MSQD_SDM_90, by = list(dataset$LANDING_YEAR, 
                                                                 dataset$LANDING_MONTH, dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
SDM.port.code.MSQD <- SDM.port.code.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
  dplyr::rename(MSQD.SDM.port.code = x)
SDM.port.code.MSQD[SDM.port.code.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.code.MSQD, 
        by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>% 
  mutate(MSQD_SDM_90 = ifelse(is.na(MSQD_SDM_90), MSQD.SDM.port.code, MSQD_SDM_90))
rm(SDM.port.code.MSQD)

### (b) Using port area code
SDM.port.area.MSQD <- aggregate(x=dataset$MSQD_SDM_90, by = list(dataset$LANDING_YEAR,
                                                                 dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.MSQD <- SDM.port.area.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(MSQD.SDM.port.area = x)
SDM.port.area.MSQD[SDM.port.area.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(MSQD_SDM_90 = ifelse(is.na(MSQD_SDM_90), MSQD.SDM.port.area, MSQD_SDM_90))
rm(SDM.port.area.MSQD)

dataset = subset(dataset, select = -c(MSQD.SDM.port.code, MSQD.SDM.port.area))


# Market squid (SPAWN)
## (a) Using PacFIN port code
SDM.port.code.MSQD_SPAWN <- aggregate(x=dataset$MSQD_SPAWN_SDM_90, by = list(dataset$LANDING_YEAR,
                                                                             dataset$LANDING_MONTH, dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
SDM.port.code.MSQD_SPAWN <- SDM.port.code.MSQD_SPAWN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
  dplyr::rename(MSQD_SPAWN.SDM.port.code = x)
SDM.port.code.MSQD_SPAWN[SDM.port.code.MSQD_SPAWN == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.code.MSQD_SPAWN,
        by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>%
  mutate(MSQD_SPAWN_SDM_90 = ifelse(is.na(MSQD_SPAWN_SDM_90), MSQD_SPAWN.SDM.port.code, MSQD_SPAWN_SDM_90))
rm(SDM.port.code.MSQD_SPAWN)
### (b) Using port area code
SDM.port.area.MSQD_SPAWN <- aggregate(x=dataset$MSQD_SPAWN_SDM_90, by = list(dataset$LANDING_YEAR,
                                                                             dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
SDM.port.area.MSQD_SPAWN <- SDM.port.area.MSQD_SPAWN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(MSQD_SPAWN.SDM.port.area = x)
SDM.port.area.MSQD_SPAWN[SDM.port.area.MSQD_SPAWN == "NaN"] <- NA
dataset <- dataset %>%
  merge(SDM.port.area.MSQD_SPAWN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>%
  mutate(MSQD_SPAWN_SDM_90 = ifelse(is.na(MSQD_SPAWN_SDM_90), MSQD_SPAWN.SDM.port.area, MSQD_SPAWN_SDM_90))
rm(SDM.port.area.MSQD_SPAWN)
dataset = subset(dataset, select = -c(MSQD_SPAWN.SDM.port.code, MSQD_SPAWN.SDM.port.area))

```

```{r calculate-price, include=FALSE}

# Calculate year price for N/A's using PacFIN port code, port area code, state and year/month.

## Pacific sardine

### (a) Using port name
price.port.name.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PORT_NAME), FUN = mean, na.rm=T)
price.port.name.PSDN <- price.port.name.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_NAME = Group.3) %>%
  dplyr::rename(PSDN.price.port.name = x)
price.port.name.PSDN[price.port.name.PSDN == "NaN"] <- NA
dataset <- dataset %>% 
  merge(price.port.name.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_NAME"), all.x = TRUE) %>%
  mutate(PSDN_AFI_PRICE_PER_KG.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_KG.mean), 
                                             PSDN.price.port.name, PSDN_AFI_PRICE_PER_KG.mean))
rm(price.port.name.PSDN)

### (b) Using PacFIN port code
price.port.code.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
price.port.code.PSDN <- price.port.code.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
  dplyr::rename(PSDN.price.port.code = x)
price.port.code.PSDN[price.port.code.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.code.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_KG.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_KG.mean), 
                                             PSDN.price.port.code, PSDN_AFI_PRICE_PER_KG.mean))
rm(price.port.code.PSDN)

### (c) Using port area code
price.port.area.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
price.port.area.PSDN <- price.port.area.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(PSDN.price.port.area = x)
price.port.area.PSDN[price.port.area.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.area.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_KG.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_KG.mean), 
                                             PSDN.price.port.area, PSDN_AFI_PRICE_PER_KG.mean))
rm(price.port.area.PSDN)

### (d) Using state
price.state.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                              dataset$LANDING_MONTH, dataset$AGENCY_CODE), FUN = mean, na.rm=T)
price.state.PSDN <- price.state.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(AGENCY_CODE = Group.3) %>%
  dplyr::rename(PSDN.price.state = x)
price.state.PSDN[price.state.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.state.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH", "AGENCY_CODE"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_KG.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_KG.mean), 
                                             PSDN.price.state, PSDN_AFI_PRICE_PER_KG.mean))
rm(price.state.PSDN)

### (e) Using year/month
price.year.PSDN <- aggregate(x=dataset$PSDN_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                             dataset$LANDING_MONTH), FUN = mean, na.rm=T)
price.year.PSDN <- price.year.PSDN %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PSDN.price.year.month = x)
price.year.PSDN[price.year.PSDN == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.year.PSDN, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>% 
  mutate(PSDN_AFI_PRICE_PER_KG.mean = ifelse(is.na(PSDN_AFI_PRICE_PER_KG.mean), 
                                             PSDN.price.year.month, PSDN_AFI_PRICE_PER_KG.mean))
rm(price.year.PSDN)

dataset = subset(dataset, select = 
                   -c(PSDN.price.port.name, PSDN.price.port.code, 
                      PSDN.price.port.area, PSDN.price.state, PSDN.price.year.month))


## Market squid

### (a) Using port name
price.port.name.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PORT_NAME), FUN = mean, na.rm=T)
price.port.name.MSQD <- price.port.name.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_NAME = Group.3) %>%
  dplyr::rename(MSQD.price.port.name = x)
price.port.name.MSQD[price.port.name.MSQD == "NaN"] <- NA
dataset <- dataset %>% 
  merge(price.port.name.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_NAME"), all.x = TRUE) %>%
  mutate(MSQD_AFI_PRICE_PER_KG.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_KG.mean), 
                                             MSQD.price.port.name, MSQD_AFI_PRICE_PER_KG.mean))
rm(price.port.name.MSQD)

### (b) Using PacFIN port code
price.port.code.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PACFIN_PORT_CODE), FUN = mean, na.rm=T)
price.port.code.MSQD <- price.port.code.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PACFIN_PORT_CODE = Group.3) %>%
  dplyr::rename(MSQD.price.port.code = x)
price.port.code.MSQD[price.port.code.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.code.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "PACFIN_PORT_CODE"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_KG.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_KG.mean), 
                                             MSQD.price.port.code, MSQD_AFI_PRICE_PER_KG.mean))
rm(price.port.code.MSQD)

### (c) Using port area code
price.port.area.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                                  dataset$LANDING_MONTH, dataset$PORT_AREA_CODE), FUN = mean, na.rm=T)
price.port.area.MSQD <- price.port.area.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(PORT_AREA_CODE = Group.3) %>%
  dplyr::rename(MSQD.price.port.area = x)
price.port.area.MSQD[price.port.area.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.port.area.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "PORT_AREA_CODE"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_KG.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_KG.mean), 
                                             MSQD.price.port.area, MSQD_AFI_PRICE_PER_KG.mean))
rm(price.port.area.MSQD)

### (d) Using state
price.state.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                              dataset$LANDING_MONTH, dataset$AGENCY_CODE), FUN = mean, na.rm=T)
price.state.MSQD <- price.state.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(AGENCY_CODE = Group.3) %>%
  dplyr::rename(MSQD.price.state = x)
price.state.MSQD[price.state.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.state.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH", "AGENCY_CODE"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_KG.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_KG.mean), 
                                             MSQD.price.state, MSQD_AFI_PRICE_PER_KG.mean))
rm(price.state.MSQD)

### (e) Using year/month
price.year.MSQD <- aggregate(x=dataset$MSQD_AFI_PRICE_PER_KG.mean, by = list(dataset$LANDING_YEAR, 
                                                                             dataset$LANDING_MONTH), FUN = mean, na.rm=T)
price.year.MSQD <- price.year.MSQD %>% dplyr::rename(LANDING_YEAR = Group.1) %>%
  dplyr::rename(LANDING_MONTH = Group.2) %>% dplyr::rename(MSQD.price.year.month = x)
price.year.MSQD[price.year.MSQD == "NaN"] <- NA
dataset <- dataset %>%
  merge(price.year.MSQD, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE) %>% 
  mutate(MSQD_AFI_PRICE_PER_KG.mean = ifelse(is.na(MSQD_AFI_PRICE_PER_KG.mean), 
                                             MSQD.price.year.month, MSQD_AFI_PRICE_PER_KG.mean))
rm(price.year.MSQD)

dataset = subset(dataset, select = 
                   -c(MSQD.price.port.name, MSQD.price.port.code, 
                      MSQD.price.port.area, MSQD.price.state, MSQD.price.year.month))

```

```{r desc, echo=FALSE, message=FALSE, warning=FALSE, results='asis'}
# Rename Price and landing variables #
dataset <- dataset %>% 
  dplyr::rename(PSDN_Price = PSDN_AFI_PRICE_PER_KG.mean) %>%
  dplyr::rename(MSQD_Price = MSQD_AFI_PRICE_PER_KG.mean) %>%
  dplyr::rename(PSDN_Landings = PSDN_LANDED_WEIGHT_MTONS.sum) %>%
  dplyr::rename(MSQD_Landings = MSQD_LANDED_WEIGHT_MTONS.sum)

# Label dataset
sjlabelled::set_label(dataset$MSQD_SDM_90)         <- "Prob(presence): MSQD"
sjlabelled::set_label(dataset$MSQD_SPAWN_SDM_90)   <- "Prob(presence): MSQD (Spawn)"
sjlabelled::set_label(dataset$PSDN_SDM_60)         <- "Prob(presence): PSDN"
sjlabelled::set_label(dataset$LANDING_YEAR)     <- "Year"
sjlabelled::set_label(dataset$LANDING_MONTH)    <- "Month"
sjlabelled::set_label(dataset$PACFIN_PORT_CODE) <- "Port code"
sjlabelled::set_label(dataset$PORT_AREA_CODE)   <- "Port area code"
sjlabelled::set_label(dataset$PORT_NAME)        <- "Port name"
sjlabelled::set_label(dataset$AGENCY_CODE)      <- "State"
sjlabelled::set_label(dataset$VESSEL_NUM)       <- "Vessel ID"
sjlabelled::set_label(dataset$MSQD_Landings) <- "Landings: MSQD"
sjlabelled::set_label(dataset$MSQD_Price)    <- "Price: MSQD"
sjlabelled::set_label(dataset$PSDN_Landings) <- "Landings: PSDN"
sjlabelled::set_label(dataset$PSDN_Price)    <- "Price: PSDN"
sjlabelled::set_label(dataset$PORT_ID)       <- "Port ID"


### Convert to quarterly??? ###
library("zoo")
dataset$MonthYear <- as.yearmon(paste(dataset$LANDING_YEAR, dataset$LANDING_MONTH), "%Y %m")
dataset$QuarterYear <- as.yearqtr(dataset$MonthYear, format = "%Y-%m")

dataset <- dataset %>% 
  group_by(LANDING_YEAR, VESSEL_NUM, group, PORT_AREA_ID) %>%
  summarise(PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm=T), MSQD_SDM_90 = mean(MSQD_SDM_90, na.rm=T),
            MSQD_SPAWN_SDM_90 = mean(MSQD_SPAWN_SDM_90, na.rm=T), PSDN_Landings = sum(PSDN_Landings, na.rm=T), 
            MSQD_Landings = sum(MSQD_Landings, na.rm=T), PSDN_Price = mean(PSDN_Price, na.rm=T), 
            MSQD_Price = mean(MSQD_Price, na.rm=T))
dataset[dataset == "NaN"] <- NA

desc_data <- dataset %>%
  subset(select = -c(PORT_AREA_ID, VESSEL_NUM, group))

sjlabelled::set_label(desc_data$MSQD_SDM_90)       <- "Prob(presence): MSQD"
sjlabelled::set_label(desc_data$MSQD_SPAWN_SDM_90) <- "Prob(presence): MSQD (Spawn)"
sjlabelled::set_label(desc_data$PSDN_SDM_60)   <- "Prob(presence): PSDN"
sjlabelled::set_label(desc_data$MSQD_Landings) <- "Landings: MSQD"
sjlabelled::set_label(desc_data$MSQD_Price)    <- "Price: MSQD"
sjlabelled::set_label(desc_data$PSDN_Landings) <- "Landings: PSDN"
sjlabelled::set_label(desc_data$PSDN_Price)    <- "Price: PSDN"
sjlabelled::set_label(desc_data$ACL_PSDN)      <- "Annual Catch Limit: PSDN"


# Compute descriptive statistics #

desc_data <- desc_data[c("PSDN_SDM_60", "MSQD_SDM_90", "MSQD_SPAWN_SDM_90", "PSDN_Landings", "MSQD_Landings",
                         "PSDN_Price", "MSQD_Price")]

vtable::st(desc_data, labels = TRUE, title='Summary Statistics \\label{tab:sum_stats}', out='latex')
rm(desc_data, Tot.landings.ports)

```

<!-- ############################################### -->
  <!-- ### CREATE DATASET BY SPECIES AND RUN MODEL ### -->
  <!-- ############################################### -->
  
  <!-- ### Pacific Sardine ### -->
  
  ```{r dataset_psdn, message=FALSE, warning=FALSE, include=FALSE, results='asis'}

## Select data for estimation, replace N/A landings to zero #

dataset_psdn <- dataset %>% 
  dplyr::select(VESSEL_NUM, PSDN_SDM_60, PSDN_Landings, PSDN_Price, MSQD_Price,
                PORT_AREA_ID, LANDING_YEAR, MSQD_SDM_90, MSQD_SPAWN_SDM_90, group) %>%
  filter(LANDING_YEAR >= 2000) %>% 
  dplyr::mutate(PSDN_Landings = coalesce(PSDN_Landings, 0)) %>% #filter(PSDN_Landings > 0) %>% 
  mutate(PSDN_Landings = ifelse(PSDN_Landings<=0, 0 ,PSDN_Landings)) %>% 
  mutate(Closure = ifelse(LANDING_YEAR >= 2015,1,0)) %>%
  mutate(RelPrice = PSDN_Price / MSQD_Price) %>% drop_na() 
dataset_psdn$port_ID <- as.factor(
  udpipe::unique_identifier(dataset_psdn, fields = "PORT_AREA_ID", start_from = 1))
dataset_psdn$cluster <- as.factor(
  udpipe::unique_identifier(dataset_psdn, fields = "group", start_from = 1))
# dataset_psdn_port_names <- dataset_psdn %>% dplyr::select(PORT_AREA_CODE, port_ID) %>% unique()

```

```{r psdn_estimation, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
fit_qPSDN <- brm(bf(PSDN_Landings ~ PSDN_SDM_60 + MSQD_SPAWN_SDM_90 + (1 | cluster) + (1 | port_ID), 
                    hu ~ (1 | cluster) + (1 | port_ID)),
                 data = dataset_psdn, 
                 prior = c(set_prior("cauchy(0,2)", class = "sd")),
                 family = hurdle_gamma(), 
                 chains = 4, cores = 4, warmup = "1000", iter = "2000",
                 control = list(adapt_delta = 0.95))

plot(fit_qPSDN)
plot(fit_qPSDN, pars = c("PSDN_SDM_60"))
coef(fit_qPSDN)

save.image (file = "stan_fit.RData")

# # work better with SDM separated. 
# loo(fit_qPSDN, fit_qPSDNv2)

```

```{r pp_check_psdn, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
g1 <- pp_check(fit_qPSDN) + ggtitle('(a) Pacific sardine') + 
  scale_color_manual(name = "", values = c("y" = "royalblue4", "yrep" = "azure3"),
                     labels = c("y" = "Observed", "yrep" = "Replicated")) +  
  theme(legend.position = "none", plot.title = element_text(size=9, face="bold.italic")) +  
  xlim(0.1, 4000) + ylab("Density")

g1
```

<!-- # print summary -->
  <!-- texreg(list(f2, f3, r2, r3), caption = 'Panel data models for Pacific Sardine landings.\\label{table:sardine_est}', caption.above = TRUE, float.pos = "h", custom.model.names = c("FE: Model 1", "FE: Model 2", "RE: Model 1", "RE: Model 2")) -->
  
  
  <!-- # ##  Extract Grouo-Level estimates ## -->
  <!-- # ranef(fit_qPSDN_gamma) -->
  <!-- #  -->
  <!-- # ## Check divergence and other model check ## -->
  <!-- # shinystan::launch_shinystan(fit_qPSDN_gamma) -->
  
  <!-- # Investigate chain and posterior distributions.  -->
  <!-- # https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html -->
  <!-- # plot(fit_qPSDN_t2nc, pars = c("PSDN_SDM")) -->
  
  <!-- # ### Predictions ### -->
  <!-- # pred_data <- data.frame(PSDN_SDM = c(0.5, 0.25), MSQD_SDM = c(0.5), Port_ID = 1) -->
  <!-- # predict(fit_qPSDN, newdata = pred_data, re_formula = NA) -->
  
  <!-- # ## Compare models ## -->
  <!-- # loo(fit1, fit2) -->
  
  
  <!-- ### Market squid ### -->
  
  ```{r msqd_data, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE, results='asis'}
# Select data for estimation, replace N/A landings to zero #
est_data_msqd <- est_data %>%
  dplyr::select(port_num, Port, MSQD_SDM_90, MSQD_Landings, MSQD_Price, 
                PSDN_SDM_60, NANC_SDM_20, Landing_year) %>%
  dplyr::mutate(MSQD_Landings = coalesce(MSQD_Landings, 0)) %>% 
  dplyr::mutate(dClose = ifelse(Landing_year >= 2015,1,0)) %>%
  dplyr::mutate(dOpen = ifelse(Landing_year < 2015,1,0)) %>%
  dplyr::mutate(PSDN_SDM_60_dOpen = PSDN_SDM_60 * dOpen) %>%
  filter(Landing_year >= 2000)

# Select ports that at least they have landing PSDN once, and drop N/A #
Tot.landings.ports <- as.data.frame(cbind(rowsum(est_data_msqd$MSQD_Landings, est_data_msqd$Port, na.rm = TRUE)))

Tot.landings.ports <- Tot.landings.ports %>%
  mutate(dTotalMSQD = ifelse(V1>0, 1, 0)) %>%
  mutate(Port = rownames(Tot.landings.ports)) %>%
  dplyr::select(Port, dTotalMSQD)

### NOTE: If I use drop_na, some ports without MSQD landings are excluded. ###
est_data_msqd <- est_data_msqd %>%
  merge(Tot.landings.ports, by.x = "Port", by.y = "Port", all.x = TRUE, all.y = FALSE) %>%
  filter(dTotalMSQD==1) %>%
  drop_na() 

# Create matrices and vectors to run STAN model #
year_id <- as.vector(cbind(as.numeric(factor(est_data_msqd$Landing_year))));
est_data_msqd$port_ID <- udpipe::unique_identifier(est_data_msqd, fields = "port_num", start_from = 1) 
portID_names_MSQD <- est_data_msqd %>%
  dplyr::select(Port, port_ID) %>%
  unique()

### Estimate using BRMS package ###
est_data_msqd$dClose    <- factor(est_data_msqd$dClose)
est_data_msqd$port_ID   <- factor(est_data_msqd$port_ID)
class(est_data_msqd$dClose)
class(est_data_msqd$port_ID)
class(est_data_msqd$dOpen)
```


```{r msqd_est_price, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
fit_qMSQD_price <- brm(bf(MSQD_Landings ~ MSQD_SDM_90 + MSQD_Price + PSDN_SDM_60_dOpen + NANC_SDM_20 + dClose 
                          + (1 + MSQD_SDM_90 + MSQD_Price + PSDN_SDM_60_dOpen + NANC_SDM_20 + dClose | port_ID),
                          hu ~ MSQD_SDM_90 + MSQD_Price + PSDN_SDM_60_dOpen + NANC_SDM_20 + dClose 
                          + (1 + MSQD_SDM_90 + MSQD_Price + PSDN_SDM_60_dOpen + NANC_SDM_20 + dClose | port_ID)),
                       data = est_data_msqd,
                       family = hurdle_gamma(),
                       control = list(adapt_delta = 0.999, max_treedepth = 20),
                       chains = 4, cores = 4)
save.image (file = "stan_fit.RData")
```

```{r pp_check_msqd, eval=FALSE, include=FALSE}
g2 <- pp_check(fit_qMSQD_price) + ggtitle('(b) Market Squid') +
  scale_color_manual(name = "", values = c("y" = "royalblue4", "yrep" = "azure3"),
                     labels = c("y" = "Observed", "yrep" = "Replicated")) + 
  theme(legend.position = "none", plot.title = element_text(size=9, face="bold.italic"))  + 
  xlim(0.1, 15000) + xlab("Landing (tons)")

```



# Results

## Landing model


### Graphical posterior predictive

```{r y_rep, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
g1 

```

```{r shinystan, eval=FALSE, include=FALSE}
shinystan::launch_shinystan(fit_qPSDN_price) 
shinystan::launch_shinystan(fit_qMSQD_price) 
shinystan::launch_shinystan(fit_qNANC_price) 
```

```{r y_rep_zero, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
g1 <- pp_check(fit_qPSDN_price) + ggtitle('(a) Pacific sardine')  + theme(legend.position = "none") + xlim(0, 0.1) 
g2 <- pp_check(fit_qMSQD_price) + ggtitle('(b) Market Squid')     + theme(legend.position = "none") + xlim(0, 0.1) 
g3 <- pp_check(fit_qNANC_price) + ggtitle('(c) Northern anchovy') + theme(legend.position = "right", 
                                                                          plot.title = element_text(size=9, face="bold.italic")) + xlim(0, 0.1) 



g1 + g2 + g3
```

```{r model-comparision, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE, results='asis'}
fit_qMSQD <- add_criterion(fit_qMSQD, "loo")
fit_qMSQD_90 <- add_criterion(fit_qMSQD_90, "loo")

comp <- loo_compare(loo(fit_qMSQD), loo(fit_qMSQD_90)) %>%
  as.data.frame() %>%
  select(elpd_loo, elpd_diff, se_diff, p_loo, looic) %>%
  dplyr::rename(
    "ELPD-Diff" = elpd_diff, "SE-Diff" = se_diff, 
    "ELPD-LOO" = elpd_loo, "P-LOO" = p_loo, "LOOIC" = looic
  )

mw1 <- model_weights(fit_qMSQD, fit_qMSQD_90, weights = "loo")[rownames(comp)]

comp %>%
  cbind("Akaike-Weight" = mw1) %>%
  apa_table(
    format = "latex", booktabs = TRUE, digits = 2,
    caption = "Comparison of models fit1 to fit3 based on approximate leave-one-out cross-validation. Market squid landigns model.",
    note =  "ELPD-LOO = expected log posterior predictive density (higher is better); ELPD-DIFF = difference in ELPD values compared to the best model. SE-DIFF = standard error of the ELPD difference. P-LOO = effective number of model parameters (lower is better); LOOIC: leave-one-out information criterion (lower is better); Akaike-Weight = Model weight based on the LOOIC values (higher is better).",
    align = c("l", rep("r", 6))
  )
```


### Own species distribution effect

```{r by_port_sdm, eval=FALSE, fig.cap=, include=FALSE}
# PSDN plots
conditions <- data.frame(port_ID = unique(est_data_psdn$port_ID))
rownames(conditions) <- unique(est_data_psdn$Port)
conditions_psdn <- conditions %>% 
  rownames_to_column('port_name') %>%
  filter(port_ID == 1 | port_ID == 2 | port_ID == 3) %>%
  column_to_rownames('port_name')

c_eff_psdn <- (conditional_effects
               (fit_qPSDN_price, "PSDN_SDM_60", surface=TRUE, conditions = conditions_psdn, re_formula = NULL))
#, transform = log, method = "posterior_predict"))
g1 <- plot(c_eff_psdn, plot = FALSE, nrow = 3, ncol = 1)[[1]] + ggtitle('(a) Pacific sardine')+ 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_continuous(name = element_blank()) +
  scale_y_continuous(name = "Landings (tons)")

# MSQD plots
conditions2 <- data.frame(port_ID = unique(est_data_msqd$port_ID))
rownames(conditions2) <- unique(est_data_msqd$Port)
conditions_msqd <- conditions2 %>% 
  rownames_to_column('port_name') %>%
  filter(port_ID == 4  | port_ID == 5  | port_ID == 8) %>%
  column_to_rownames('port_name')
c_eff_msqd <- (conditional_effects
               (fit_qMSQD_price, "MSQD_SDM_90", surface=TRUE, conditions = conditions_msqd, re_formula = NULL))
#, transform = log, method = "posterior_predict"))
g2 <- plot(c_eff_msqd, plot = FALSE, nrow = 3, ncol = 1)[[1]] + ggtitle('(b) Market squid') + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_continuous(name = "Prob(Presence)") +
  scale_y_continuous(name = element_blank()) 

# NANC plots
conditions3 <- data.frame(port_ID = unique(est_data_nanc$port_ID))
rownames(conditions3) <- unique(est_data_nanc$Port)
conditions_nanc <- conditions3 %>% 
  rownames_to_column('port_name') %>%
  filter(port_ID == 3  | port_ID == 4  | port_ID == 5) %>%
  column_to_rownames('port_name')
c_eff_nanc <- (conditional_effects
               (fit_qNANC_price, "NANC_SDM_20", surface=TRUE, conditions = conditions_nanc, re_formula = NULL))
#, transform = log, method = "posterior_predict"))
g3 <- plot(c_eff_nanc, plot = FALSE, nrow = 3, ncol = 1)[[1]] + ggtitle('(c) Northern anchovy') + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_continuous(name = element_blank()) +
  scale_y_continuous(name = element_blank()) 

# Merge plots
g1 + g2 + g3
```

### Interaction effects

```{r int_effect_PSDN_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
c_eff_int_psdn_msqd <- (conditional_effects(fit_qPSDN_price, "PSDN_SDM_60:MSQD_SDM_90", surface=TRUE, 
                                            conditions = conditions_psdn, re_formula = NULL))
c_eff_int_psdn_nanc <- (conditional_effects(fit_qPSDN_price, "PSDN_SDM_60:NANC_SDM_20", surface=TRUE, 
                                            conditions = conditions_psdn, re_formula = NULL))
c_eff_int_msqd_nanc <- (conditional_effects(fit_qPSDN_price, "MSQD_SDM_90:NANC_SDM_20", surface=TRUE, 
                                            conditions = conditions_psdn, re_formula = NULL))

g1_PSDN <-  plot(c_eff_int_psdn_msqd, plot = FALSE)[[1]] + ggtitle('(a) Pacific sardine x Market squid') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: PSDN")) +
  scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): MSQD") 

g2_PSDN <-  plot(c_eff_int_psdn_nanc, plot = FALSE)[[1]] + ggtitle('(b) Pacific sardine x Northern anchovy') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
  scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): NANC") 

g3_PSDN <-  plot(c_eff_int_msqd_nanc, plot = FALSE)[[1]] + ggtitle('(c) Northern anchovy x Market squid') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
  scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): NANC") 
save.image (file = "stan_fit.RData")
```

```{r int_effect_PSDN_by_port_PLOT, eval=FALSE, fig.cap=, message=FALSE, warning=FALSE, include=FALSE}
g1_PSDN / g2_PSDN
```

```{r int_effect_MSQD_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
c_eff_int_msqd_psdn <- (conditional_effects(fit_qMSQD_price, "MSQD_SDM_90:PSDN_SDM_60_dOpen", surface=TRUE, 
                                            conditions = conditions_msqd, re_formula = NULL))
c_eff_int_msqd_nanc <- (conditional_effects(fit_qMSQD_price, "MSQD_SDM_90:NANC_SDM_20", surface=TRUE, 
                                            conditions = conditions_msqd, re_formula = NULL))
c_eff_int_psdn_nanc <- (conditional_effects(fit_qMSQD_price, "PSDN_SDM_60_dOpen:NANC_SDM_20", surface=TRUE, 
                                            conditions = conditions_msqd, re_formula = NULL))

g1_MSQD <-  plot(c_eff_int_msqd_psdn, plot = FALSE)[[1]] + ggtitle('(a) Market squid x Pacific sardine') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: MSQD")) +
  scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): PSDN") 

g2_MSQD <-  plot(c_eff_int_msqd_nanc, plot = FALSE)[[1]] + ggtitle('(b) Market squid x Northern anchovy') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: MSQD")) + 
  scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): NANC") 

g3_MSQD <-  plot(c_eff_int_psdn_nanc, plot = FALSE)[[1]] + ggtitle('(c) Pacific sardine x Northern anchovy') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: MSQD")) + 
  scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): NANC") 
save.image (file = "stan_fit.RData")
```

```{r int_effect_MSQD_by_port_PLOT, eval=FALSE, fig.cap=, message=FALSE, warning=FALSE, include=FALSE}
g1_MSQD / g2_MSQD 
```

```{r int_effect_NANC_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE}
conditions_nanc <- conditions_nanc %>% filter(port_ID == 3  | port_ID == 5)

c_eff_int_nanc_psdn <- (conditional_effects(fit_qNANC_price, "NANC_SDM_20:PSDN_SDM_60_dOpen", surface=TRUE, 
                                            conditions = conditions_nanc, re_formula = NULL))
c_eff_int_nanc_msqd <- (conditional_effects(fit_qNANC_price, "NANC_SDM_20:MSQD_SDM_90", surface=TRUE, 
                                            conditions = conditions_nanc, re_formula = NULL))
c_eff_int_psdn_msqd <- (conditional_effects(fit_qNANC_price, "PSDN_SDM_60_dOpen:MSQD_SDM_90", surface=TRUE, 
                                            conditions = conditions_nanc, re_formula = NULL))

g1_NANC <-  plot(c_eff_int_nanc_psdn, plot = FALSE)[[1]] + ggtitle('(a) Northern anchovy x Pacific sardine') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: PSDN")) +
  scale_x_continuous(name = "P(Pres): NANC") + scale_y_continuous(name = "P(Pres): PSDN") 

g2_NANC <-  plot(c_eff_int_nanc_msqd, plot = FALSE)[[1]] + ggtitle('(b) Northern anchovy x Market squid') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
  scale_x_continuous(name = "P(Pres): NANC") + scale_y_continuous(name = "P(Pres): MSQD") 

g3_NANC <-  plot(c_eff_int_psdn_msqd, plot = FALSE)[[1]] + ggtitle('(c) Pacific sardine x Market squid') + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"), 
    axis.text = element_text(size = 7), 
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9), 
    legend.text = element_text(size=8)
  ) + guides(colour=guide_legend(title="Landings: PSDN")) + 
  scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): MSQD") 
save.image (file = "stan_fit.RData")
```

```{r int_effect_NANC_by_port_PLOT, eval=FALSE, fig.cap=, message=FALSE, warning=FALSE, include=FALSE}
g1_NANC / g2_NANC 
```

### Pacific sardine closure

```{r by_port_msqd_dclose, eval=FALSE, fig.cap=, include=FALSE}
# conditions_dClose <- data.frame(port_ID = unique(est_data_msqd$dClose))
# rownames(conditions_dClose) <- unique(est_data_msqd$dClose)
c_eff_close_msqd <- (conditional_effects(fit_qMSQD_price, "dClose", conditions = conditions_msqd, re_formula = NULL))
g1 <-  plot(c_eff_close_msqd, plot = FALSE)[[1]] + ggtitle('(a) Market squid') + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Landings (tons)") 

c_eff_close_nanc <- (conditional_effects(fit_qNANC_price, "dClose", conditions = conditions_nanc, re_formula = NULL))
g2 <- plot(c_eff_close_nanc, plot = FALSE)[[1]] + ggtitle('(b) Northern anchovy') + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  scale_x_discrete(name = "Closure? (1 = True; 0 = False)") +
  scale_y_continuous(name = "Landings (tons)") 

g1 / g2
```

# OTHER CODE

<!-- ```{r int_effect_sep, eval=FALSE, include=FALSE} -->
  
  <!-- hu <- plot(conditional_effects(fit_qPSDN, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="hu"), -->
                    <!--            plot = FALSE, ask = FALSE) -->
  <!-- mu <- plot(conditional_effects(fit_qPSDN, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="mu"), -->
                    <!--            plot = FALSE, ask = FALSE) -->
  
  <!-- mu[[1]] + hu[[1]] + plot_layout(nrow = 2) -->
  
  <!-- ``` -->
  
  <!-- ```{r int_effect_sep_msqd, eval=FALSE, fig.cap=, include=FALSE} -->
  
  <!-- hu <- plot(conditional_effects(fit_qMSQD_gamma, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="hu"), -->
                    <!--            plot = FALSE, ask = FALSE) -->
  <!-- mu <- plot(conditional_effects(fit_qMSQD_gamma, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="mu"), -->
                    <!--            plot = FALSE, ask = FALSE) -->
  
  <!-- mu[[1]] + hu[[1]] + plot_layout(nrow = 2) -->
  
  <!-- ``` -->
  
  
  <!-- ## Effort susbsitution ##  -->
  
  <!-- * Time series of number of trips (as a proxy for effort) for all the species -->
  <!-- * @richerson2017 use a method identify the nature of the outliers in an ARMA time series model (read more if interested)  -->
  <!--     + I propose to estimate a system of simultaneous equations (VECM model) to study equilibrium of effort and short-run and long-run effects of the closure (structural breaks) -->
  <!--     + Have a long-run equation for each species (simultaneously estimated) and test for structural break in this long-run relationship. -->
  
  
  <!-- ## Seasonality changes ## -->
  
  <!-- * Seasonality can be studied calculating monthly share of total trips by species, regress it using month dummiues and see any is there any structural change after the closure [@richerson2017].  -->
  
  <!-- <!-- Shall we study also effort as number of trips, and seasonality using time series -->
  