########################################
### Participation model -- database ###
########################################

rm(list=ls())
gc()

###Load packages and set working directory

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(geosphere)
library(tidyverse)  


#-----------------------------------------------------
### Load in the data
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets_raw<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)

#-----------------------------------------------------
### Add port area
port_area <- read.csv(file = here::here("Data", "Ports", "ports_area_and_name_codes.csv"))
Tickets_raw <- Tickets_raw %>% merge(port_area, by = c("PACFIN_PORT_CODE"), all.x = TRUE) %>% 
  filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") %>% 
  drop_na(PORT_AREA_CODE)
rm(port_area)



#-----------------------------------------------------
###Subset the data to remove columns not relevant to this analysis. This will speed things up.
Tickets <- select(Tickets_raw, c(AGENCY_CODE, FTID, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, PORT_NAME, PORT_AREA_CODE,
                                 VESSEL_NUM, VESSEL_NAME, VESSEL_LENGTH, VESSEL_WEIGHT, 
                                 LANDED_WEIGHT_MTONS, LANDED_WEIGHT_LBS, AFI_EXVESSEL_REVENUE,
                                 PACFIN_GEAR_CODE, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, VESSEL_OWNER_NAME,
                                 FISHER_LICENSE_NUM, AFI_PRICE_PER_POUND, NUM_OF_DAYS_FISHED, CATCH_AREA_CODE)) %>%
  mutate(AFI_PRICE_PER_MTONS = AFI_PRICE_PER_POUND/0.000453592)


Tickets$FTID_unique <- udpipe::unique_identifier(Tickets, fields = c("FTID", "VESSEL_NUM", "LANDING_YEAR"))

#-----------------------------------------------------
### Find the dominant species by value of each fishing trip (i.e., target species). 

Boats <- dcast(Tickets, FTID_unique ~ PACFIN_SPECIES_CODE, fun.aggregate=sum, value.var="AFI_EXVESSEL_REVENUE", fill=0)
row.names(Boats) <- Boats$FTID_unique
FTID_unique<-Boats$FTID_unique
Boats<-Boats[,-(1)]
X<-as.data.frame(colnames(Boats)[apply(Boats,1,which.max)]) # Indicate the name of the column (PACFIN_SPECIES_CODE) with the highest "AFI_EXVESSEL_REVENUE" 
colnames(X)<-"Species_Dominant"
Trip_Species_Dominant<-as.data.frame(cbind(FTID_unique,X))
Tickets<-merge(Tickets, Trip_Species_Dominant, by='FTID_unique')
rm(Trip_Species_Dominant, X, Boats)


#-----------------------------------------------------
### Aggregate species in a FTID 
Tickets <- Tickets %>% group_by(AGENCY_CODE, FTID_unique, FTID, LANDING_YEAR, LANDING_MONTH, LANDING_DAY, PORT_AREA_CODE,
                                VESSEL_NUM, PACFIN_SPECIES_CODE, Species_Dominant
                                #PORT_NAME, VESSEL_NAME, VESSEL_LENGTH, VESSEL_WEIGHT, VESSEL_OWNER_NAME, PACFIN_GEAR_CODE, 
                                #FISHER_LICENSE_NUM, CATCH_AREA_CODE, PACFIN_SPECIES_COMMON_NAME
                                ) %>% 
  summarize(Landings_mtons = sum(LANDED_WEIGHT_MTONS),
            Landings_lbs = sum(LANDED_WEIGHT_LBS),
            Revenue  = sum(AFI_EXVESSEL_REVENUE),
            Price_lbs = mean(AFI_PRICE_PER_POUND),
            Price_mtons = mean(AFI_PRICE_PER_MTONS),
            max_days_sea = max(NUM_OF_DAYS_FISHED)) %>%
  mutate(dDelete = ifelse(FTID == "142301E" & LANDING_YEAR == 2020, 1, 0)) %>%
  filter(dDelete == 0) %>% select(-c(dDelete))


#-------------------------------------------------------------------------------------
### Subset to select only records where one of the CPS was the target species
### (squid, sardine, mackerel, anchovy, or albacore using anchovy as bait)

FF_Tickets<-Tickets[which((Tickets$Species_Dominant == "PSDN" & Tickets$Revenue > 0) |
                          (Tickets$Species_Dominant == "MSQD" & Tickets$Revenue > 0) |
                          (Tickets$Species_Dominant == "NANC" & Tickets$Revenue > 0) |
                          (Tickets$Species_Dominant == "CMCK" & Tickets$Revenue > 0) |
                          (Tickets$Species_Dominant == "JMCK" & Tickets$Revenue > 0) |
                          (Tickets$Species_Dominant == "UMCK" & Tickets$Revenue > 0) |
                          (Tickets$Species_Dominant == "ALBC" &
                           Tickets$PACFIN_SPECIES_CODE == "NANC")),]

## Aggregate mackerels in one category
FF_Tickets<- within(FF_Tickets, Species_Dominant[Species_Dominant == "CMCK"] <- "OMCK")
FF_Tickets<- within(FF_Tickets, Species_Dominant[Species_Dominant == "JMCK"] <- "OMCK")
FF_Tickets<- within(FF_Tickets, Species_Dominant[Species_Dominant == "UMCK"] <- "OMCK")


###Creating a filter here to only retain vessels with more than 10 forage fish landings (tickets where FF is the dominant species)
FTID_Value<-aggregate(Revenue ~ FTID_unique + VESSEL_NUM, FUN=sum, data=FF_Tickets)
FTID_Value<-FTID_Value[FTID_Value$VESSEL_NUM %in% names(which(table(FTID_Value$VESSEL_NUM) > 10)), ]
FF_Tickets<-setDT(FF_Tickets)[VESSEL_NUM %chin% FTID_Value$VESSEL_NUM]
FF_Tickets<-as.data.frame(FF_Tickets)

# FF_Tickets indicate tickets where FF are dominant in the trip, but still have landings for other species.
FF_Vessels<-as.data.frame(unique(FTID_Value$VESSEL_NUM))
names(FF_Vessels)[1]<-"VESSEL_NUM"
FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM==""),])
names(FF_Vessels)[1]<-"VESSEL_NUM"
FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM=="UNKNOWN"),])
names(FF_Vessels)[1]<-"VESSEL_NUM"
FF_Vessels<-as.data.frame(FF_Vessels[which(!FF_Vessels$VESSEL_NUM=="MISSING"),])
names(FF_Vessels)[1]<-"VESSEL_NUM"


#----------------------------------------------------------------------------------------
###Subset from the complete data set to only retain records associated with these Vessels
###Remove records associated with landings of zero value; this is likely bycatch

Tickets<-Tickets[which(Tickets$Revenue > 0),] # 137,971 row deleted
Tickets<-setDT(Tickets)[VESSEL_NUM %chin% FF_Vessels$VESSEL_NUM] # 1,927,235 row deleted
Tickets<-as.data.frame(Tickets)
rm(FF_Vessels, FTID_Value)

saveRDS(Tickets, "C:/Data/PacFIN data/Tickets_filtered.rds")

#-----------------------------------------------------
### Create port-species choice 
Tickets <- Tickets %>% filter(PACFIN_SPECIES_CODE == Species_Dominant) %>% # 44,035 row deleted 
   mutate(selection = paste(PORT_AREA_CODE, Species_Dominant, sep = "-", collapse = NULL)) 

# Tickets_check <- Tickets %>% group_by(FTID) %>% mutate(n_obs = n()) %>% 
#    ungroup() %>% filter(n_obs==2)

# ### How many vessels?
# Tickets %>% select('VESSEL_NUM') %>% unique() %>% summarize(n_vessels = n())


#---------------------------------------------------------------------------------------
### Expand database to include outside option (when vessel do not have fish ticket.)
Tickets_exp <- complete(Tickets, VESSEL_NUM, LANDING_YEAR, LANDING_MONTH, LANDING_DAY) %>%
  mutate(selection = ifelse(is.na(selection), 'No-Participation', selection)) %>%
  mutate(FTID_unique = ifelse(is.na(FTID_unique), paste('NP-',1:n()), FTID_unique))


#-----------------------------------------------------
### Merge with cluster data
PAM_Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\PAM_Vessel_Groups.csv")
Tickets_clust <- merge(Tickets_exp, PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
rm(PAM_Vessel_Groups)
Tickets_clust <- Tickets_clust[!is.na(Tickets_clust$group_all), ]

# ### How many vessels and fish tickets?
# PAM_Vessel_Groups <- read.csv("C:\\GitHub\\EconAnalysis\\Clustering\\PAM_Vessel_Groups.csv")
# xxx <- merge(Tickets_storm, PAM_Vessel_Groups, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
# xxx <- xxx[!is.na(xxx$group_all), ]
# xxx %>% select('FTID_unique') %>% unique() %>% summarize(n_tickets = n())
# xxx %>% select('VESSEL_NUM') %>% unique() %>% summarize(n_vessels = n())


#-----------------------------------------------
### Merge data to SDM 

psdn.sdm <- read.csv(file = 'Participation/SDM_code/sdm_psdn.csv')
msqd.sdm <- readRDS(file = 'Participation/SDM_code/sdm_msqd.rds')
nanc.sdm <- readRDS(file = 'Participation/SDM_code/sdm_nanc.rds')
phrg.sdm <- readRDS(file = 'Participation/SDM_code/sdm_phrg.rds')
cmck.sdm <- readRDS(file = 'Participation/SDM_code/sdm_cmck.rds')
jmck.sdm <- readRDS(file = 'Participation/SDM_code/sdm_jmck.rds')
msqd_spawn.sdm <- readRDS(file = 'Participation/SDM_code/sdm_msqd_spawn.rds')

psdn.sdm[is.na(psdn.sdm)] <- 0
msqd.sdm[is.na(msqd.sdm)] <- 0
nanc.sdm[is.na(nanc.sdm)] <- 0
phrg.sdm[is.na(phrg.sdm)] <- 0
cmck.sdm[is.na(cmck.sdm)] <- 0
jmck.sdm[is.na(jmck.sdm)] <- 0                     
msqd_spawn.sdm[is.na(msqd_spawn.sdm)] <- 0


Tickets_SDM <- merge(Tickets_clust, psdn.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)
Tickets_SDM <- merge(Tickets_SDM, msqd.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)
Tickets_SDM <- merge(Tickets_SDM, nanc.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)
Tickets_SDM <- merge(Tickets_SDM, phrg.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)
Tickets_SDM <- merge(Tickets_SDM, cmck.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)
Tickets_SDM <- merge(Tickets_SDM, jmck.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)
Tickets_SDM <- merge(Tickets_SDM, msqd_spawn.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)


## lagged SDMs and prices

### Lagged date in database
Tickets_SDM$set_date<-as.Date(with(
  Tickets_SDM,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")
Tickets_SDM$prev_days_date <- Tickets_SDM$set_date - days(1)

### Lagged date in SDM databases
psdn.sdm$set_date<-as.Date(with(
  psdn.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")
msqd.sdm$set_date<-as.Date(with(
  msqd.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")
nanc.sdm$set_date<-as.Date(with(
  nanc.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")
phrg.sdm$set_date<-as.Date(with(
  phrg.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")
cmck.sdm$set_date<-as.Date(with(
  cmck.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")
jmck.sdm$set_date<-as.Date(with(
  jmck.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")
msqd_spawn.sdm$set_date<-as.Date(with(
  msqd_spawn.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")


### Lagged date in price database
prices <- Tickets_SDM %>% 
  dplyr::select(c(set_date, PORT_AREA_CODE, PACFIN_SPECIES_CODE, Price_mtons)) %>% 
  drop_na() %>%
  unique() %>%  
  rename(lag_Price_mtons = Price_mtons) %>%
  rename(prev_days_date = set_date)

Tickets_SDM <- merge(Tickets_SDM, prices, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE', 'PACFIN_SPECIES_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(PSDN)
psdn.sdm <- psdn.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_PSDN_SDM_30 = PSDN_SDM_30) %>%
  rename(lag_PSDN_SDM_60 = PSDN_SDM_60) %>%
  rename(lag_PSDN_SDM_90 = PSDN_SDM_90) %>%
  rename(lag_PSDN_SDM_220 = PSDN_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, psdn.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(MSQD)
msqd.sdm <- msqd.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_MSQD_SDM_30 =  MSQD_SDM_30) %>%
  rename(lag_MSQD_SDM_90 =  MSQD_SDM_90) %>%
  rename(lag_MSQD_SDM_220 = MSQD_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, msqd.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(NANC)
nanc.sdm <- nanc.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_NANC_SDM_20  = NANC_SDM_20) %>%
  rename(lag_NANC_SDM_30  = NANC_SDM_30) %>%
  rename(lag_NANC_SDM_90  = NANC_SDM_90) %>%
  rename(lag_NANC_SDM_220 = NANC_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, nanc.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(PHRG)
phrg.sdm <- phrg.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_PHRG_SDM_30  = PHRG_SDM_30) %>%
  rename(lag_PHRG_SDM_90  = PHRG_SDM_90) %>%
  rename(lag_PHRG_SDM_220 = PHRG_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, phrg.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(CMCK)
cmck.sdm <- cmck.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_CMCK_SDM_30  = CMCK_SDM_30) %>%
  rename(lag_CMCK_SDM_90  = CMCK_SDM_90) %>%
  rename(lag_CMCK_SDM_220 = CMCK_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, cmck.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(JMCK)
jmck.sdm <- jmck.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_JMCK_SDM_30  = JMCK_SDM_30) %>%
  rename(lag_JMCK_SDM_90  = JMCK_SDM_90) %>%
  rename(lag_JMCK_SDM_220 = JMCK_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, jmck.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(MSQD_SPAWN)
msqd_spawn.sdm <- msqd_spawn.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_MSQD_SPAWN_SDM_30  = MSQD_SPAWN_SDM_30) %>%
  rename(lag_MSQD_SPAWN_SDM_90  = MSQD_SPAWN_SDM_90) %>%
  rename(lag_MSQD_SPAWN_SDM_220 = MSQD_SPAWN_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, msqd_spawn.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)






#---------------------------------------------------------------------------------------------------------------------------------------------------------
## Clean dataset for discrete choice model  
## Add unique trip_ID and add set_date, set_year, set_day and set_month 
## (exclude weird period from expanding data)

Tickets_SDM$trip_id <- udpipe::unique_identifier(Tickets_SDM, fields = c("FTID_unique"))
Tickets_SDM <- Tickets_SDM %>% drop_na(set_date) %>% 
  dplyr::rename(set_day = LANDING_DAY) %>%
  dplyr::rename(set_month = LANDING_MONTH) %>%
  dplyr::rename(set_year = LANDING_YEAR)%>%
  dplyr::select("VESSEL_NUM", "trip_id", "set_date", "set_year", "set_month", "set_day", "selection",
                "PORT_AREA_CODE", "Species_Dominant", "Landings_mtons", "Revenue", "Price_mtons", "max_days_sea",
                "group_all", "lag_Price_mtons",
                "PSDN_SDM_30", "PSDN_SDM_60", "PSDN_SDM_90","PSDN_SDM_220",
                "MSQD_SDM_30", "MSQD_SDM_90", "MSQD_SDM_220",
                "NANC_SDM_20", "NANC_SDM_30", "NANC_SDM_90", "NANC_SDM_220",
                "PHRG_SDM_30", "PHRG_SDM_90" , "PHRG_SDM_220",
                "CMCK_SDM_30", "CMCK_SDM_90", "CMCK_SDM_220",
                "JMCK_SDM_30", "JMCK_SDM_90", "JMCK_SDM_220",
                "MSQD_SPAWN_SDM_30", "MSQD_SPAWN_SDM_90", "MSQD_SPAWN_SDM_220",
                "lag_PSDN_SDM_30", "lag_PSDN_SDM_60", "lag_PSDN_SDM_90", "lag_PSDN_SDM_220",
                "lag_MSQD_SDM_30", "lag_MSQD_SDM_90", "lag_MSQD_SDM_220",       
                "lag_NANC_SDM_20", "lag_NANC_SDM_30", "lag_NANC_SDM_90", "lag_NANC_SDM_220",       
                "lag_PHRG_SDM_30", "lag_PHRG_SDM_90", "lag_PHRG_SDM_220",       
                "lag_CMCK_SDM_30", "lag_CMCK_SDM_90", "lag_CMCK_SDM_220",       
                "lag_JMCK_SDM_30", "lag_JMCK_SDM_90", "lag_JMCK_SDM_220",
                "lag_MSQD_SPAWN_SDM_30", "lag_MSQD_SPAWN_SDM_90", "lag_MSQD_SPAWN_SDM_220") 



#---------------------------------------------------------------------------------------
## Create data to filter non-participation

library(fpp2)           # working with time series data
library(zoo)            # working with time series data

n_days_participation = 365

participation_data_all <- Tickets_SDM %>% 
  mutate(CPS_revenue = 
    ifelse(Species_Dominant == "PSDN", Revenue,
    ifelse(Species_Dominant == "NANC", Revenue,
    ifelse(Species_Dominant == "MSQD", Revenue,
    ifelse(Species_Dominant == "PSDN", Revenue,
    ifelse(Species_Dominant == "CMCK", Revenue,
    ifelse(Species_Dominant == "JMCK", Revenue,
    ifelse(Species_Dominant == "UMCK", Revenue, 0)))))))) %>%
  mutate(CPS_revenue = ifelse(selection == "No-Participation", 0, CPS_revenue)) %>% 
  mutate(partDummy = ifelse(selection == "No-Participation", 0, 1)) %>%
  dplyr::select(VESSEL_NUM, set_date, set_year, partDummy, CPS_revenue, Revenue, group_all) %>% unique() %>%
  dplyr::arrange(VESSEL_NUM, set_date) %>%
  group_by(VESSEL_NUM) %>%
  mutate(participation_ndays = rollsum(partDummy, k = n_days_participation, na.rm = TRUE, fill = NA, align = "center")) %>%
  mutate(CPS_revenue_MA = rollsum(CPS_revenue, k = n_days_participation, na.rm = TRUE, fill = NA, align = "center")) %>%
  mutate(Revenue_MA = rollsum(Revenue, k = n_days_participation, na.rm = TRUE, fill = NA, align = "center")) %>%
  ungroup() %>% 
  filter(is.na(participation_ndays) == 0) %>%
  mutate(perc_CPS = CPS_revenue_MA / Revenue_MA) %>%
  mutate(partDummy = as.factor(partDummy))


# ----------
## Plots with filtered data 

# cluster <- 5
# ndays_filter <- 90
# ndays_filter_t <- 30
# 
# participation_filtered <- participation_data_all %>% 
#   #filter(group_all == cluster) %>%
#   mutate(Dfilter = ifelse(partDummy == 0 & participation_ndays < ndays_filter, 0, 1)) %>%
#   mutate(Dfilter = ifelse(partDummy == 1 & participation_ndays < ndays_filter_t, 0, Dfilter)) %>%
#   filter(Dfilter == 1) %>%
#   dplyr::arrange(VESSEL_NUM, set_date)
# 
# perc <- participation_filtered %>% group_by(partDummy) %>% summarize(n_obs = n(), perc = n()/nrow(participation_filtered))
# x1 <- trunc(perc$perc[1]*100*10^2)/10^2
# x2 <- trunc(perc$perc[2]*100*10^2)/10^2
# 
# library(hrbrthemes)
# ggplot(data=participation_filtered, aes(x=participation_ndays, group=partDummy, fill=partDummy)) +
#   geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
#   theme_ipsum()  +
#   ylab("Number of observations") +
#   xlab("Days participating within a year") +
#   guides(fill=guide_legend(title="Participating?")) +
#   scale_fill_discrete(labels = c(paste0("No (", paste0(x1,"%)")), paste0("Yes (", paste0(x2,"%)")))) +
#   ggtitle(paste0("At least ",
#     paste0(ndays_filter,
#     paste0(" participating within a year (",
#     paste0(formatC(nrow(participation_filtered), format="d", big.mark=","), " obs)")))),
#     subtitle = paste0("...and ", paste0(ndays_filter_t, " days participating for rows with tickets")))

# ggplot(data=participation_filtered, aes(x=Revenue_MA, group=partDummy, fill=partDummy)) +
#   geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
#   theme_ipsum()  +
#   xlab("Revenue within a year") +
#   guides(fill=guide_legend(title="Participating?")) +
#   scale_fill_discrete(labels = c(paste0("No (", paste0(x1,"%)")), paste0("Yes (", paste0(x2,"%)")))) +
#   ggtitle("Revenue within year?")
# 
# ggplot(data=participation_filtered, aes(x=perc_CPS, group=partDummy, fill=partDummy)) +
#   geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
#   theme_ipsum()  +
#   xlab("Days participating within a year") +
#   guides(fill=guide_legend(title="Participating?")) +
#   scale_fill_discrete(labels = c(paste0("No (", paste0(x1,"%)")), paste0("Yes (", paste0(x2,"%)")))) +
#   ggtitle("Revenue share from CPS within year?")


#-----------------------------------------------
## filter non-participation

ndays_filter <- 90 ## For rows with no-participation
ndays_filter_t <- 30 ## For row with tickets

participation_filtered <- participation_data_all %>%
  mutate(Dfilter = ifelse(partDummy == 0 & participation_ndays < ndays_filter, 0, 1)) %>%
  mutate(Dfilter = ifelse(partDummy == 1 & participation_ndays < ndays_filter_t, 0, Dfilter)) %>%
  filter(Dfilter == 1) %>%
  dplyr::arrange(VESSEL_NUM, set_date) %>%
  dplyr::select(VESSEL_NUM, set_date) %>% 
  unique() %>% mutate(filter = 1)

Tickets_part <- merge(Tickets_SDM, participation_filtered,
                      by = c("VESSEL_NUM", "set_date"), 
                      all.x = TRUE, all.y = FALSE) %>% 
  filter(filter == 1) %>% 
  dplyr::select(-c("filter"))


#------------------------------------------------------
### Save participation data
saveRDS(Tickets_part, "C:\\Data\\PacFIN data\\participation_data.rds")



