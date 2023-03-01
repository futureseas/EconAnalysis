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
  filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") 
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
                                #FISHER_LICENSE_NUM, NUM_OF_DAYS_FISHED, CATCH_AREA_CODE, PACFIN_SPECIES_COMMON_NAME
                                ) %>% 
  summarize(Landings_mtons = sum(LANDED_WEIGHT_MTONS),
            Landings_lbs = sum(LANDED_WEIGHT_LBS),
            Revenue  = sum(AFI_EXVESSEL_REVENUE),
            Price_lbs = mean(AFI_PRICE_PER_POUND),
            Price_mtons = mean(AFI_PRICE_PER_MTONS)) %>%
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

# saveRDS(Tickets, "C:/Data/PacFIN data/Tickets_filtered.rds")

#-----------------------------------------------------
### Create port-species choice
Tickets <- Tickets %>% filter(PACFIN_SPECIES_CODE == Species_Dominant) %>% # 44,069 row deleted 
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
#... (STILL COMPUTING SDMs Daily for squid (spawn), squid, anchovy, herring, jack mackerel and chub mackerel)

# Pacific sardine
psdn.sdm <- read.csv(file = 'Participation/SDM_code/sdm_psdn.csv')
psdn.sdm[is.na(psdn.sdm)] <- 0

Tickets_SDM <- merge(Tickets_clust, psdn.sdm, 
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)


#---------------------------------------------------------------------------------------------------------------------------------------------------------
## Clean dataset for discrete choice model  
## Add unique trip_ID and add set_date, set_year, set_day and set_month 
## (exclude weird period from expanding data)

Tickets_SDM$trip_id <- udpipe::unique_identifier(Tickets_SDM, fields = c("FTID_unique"))
Tickets_SDM$set_date<-as.Date(with(
  Tickets_SDM,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")

Tickets_SDM <- Tickets_SDM %>% drop_na(set_date) %>% 
  dplyr::rename(set_day = LANDING_DAY) %>%
  dplyr::rename(set_month = LANDING_MONTH) %>%
  dplyr::rename(set_year = LANDING_YEAR)%>%
  dplyr::select("VESSEL_NUM", "trip_id", "set_date", "set_year", "set_month", "set_day", "selection",
                "PORT_AREA_CODE", "Species_Dominant", "Landings_mtons", "Revenue", "Price_mtons", "group_all",
                "PSDN_SDM_30", "PSDN_SDM_60", "PSDN_SDM_90","PSDN_SDM_220")

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
    ifelse(Species_Dominant == "JMCK" , Revenue,
    ifelse(Species_Dominant == "UMCK" , Revenue, 0)))))))) %>%
  mutate(CPS_revenue = ifelse(selection == "No-Participation", 0, CPS_revenue)) %>% 
  mutate(partDummy = ifelse(selection == "No-Participation", 0, 1)) %>%
  dplyr::select(VESSEL_NUM, set_date, set_year, partDummy, CPS_revenue, Revenue) %>% unique() %>%
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

library(hrbrthemes)
participation_data <- participation_data_all %>% filter(group_all == 4)
participation_data %>% group_by(partDummy) %>% summarize(n_obs = n(), perc = n()/nrow(participation_data)) %>%
ggplot(data=participation_data, aes(x=participation_ndays, group=partDummy, fill=partDummy)) +
  geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
  theme_ipsum()  +
  ylab("Number of observations") +
  xlab("Days participating within a year") +
  guides(fill=guide_legend(title="Participating?")) +
  scale_fill_discrete(labels = c("No (92.12%)", "Yes (7.88%)")) +
  ggtitle("No filter  (1,673,427 obs)")

participation_filtered <- participation_data %>%
  filter(participation_ndays >= 30) %>%
  dplyr::arrange(VESSEL_NUM, set_date)
participation_filtered %>% group_by(partDummy) %>% summarize(n_obs = n(), perc = n()/nrow(participation_filtered))
ggplot(data=participation_filtered, aes(x=participation_ndays, group=partDummy, fill=partDummy)) +
  geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
  theme_ipsum()  +
  ylab("Number of observations") +
  xlab("Days participating within a year") +
  guides(fill=guide_legend(title="Participating?")) +
  scale_fill_discrete(labels = c("No (79.7%)", "Yes (20.3%)")) +
  ggtitle("At least 30 days participating within a year (574,756 obs)")

participation_filtered <- participation_data %>%
  filter(participation_ndays >= 90) %>%
  dplyr::arrange(VESSEL_NUM, set_date)
participation_filtered %>% group_by(partDummy) %>% summarize(n_obs = n(), perc = n()/nrow(participation_filtered))
ggplot(data=participation_filtered2, aes(x=participation_ndays, group=partDummy, fill=partDummy)) +
  geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
  theme_ipsum()  +
    ylab("Number of observations") +
    xlab("Days participating within a year") +
  guides(fill=guide_legend(title="Participating?")) +
  scale_fill_discrete(labels = c("No (65.9%)", "Yes (34.1%)")) +
  ggtitle("At least 90 days participating within a year (151,240 obs)")

participation_filtered <- participation_data %>%
  filter(participation_ndays >= 120) %>%
  dplyr::arrange(VESSEL_NUM, set_date)
participation_filtered %>% group_by(partDummy) %>% summarize(n_obs = n(), perc = n()/nrow(participation_filtered))
ggplot(data=participation_filtered, aes(x=participation_ndays, group=partDummy, fill=partDummy)) +
  geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
  theme_ipsum()  +
  ylab("Number of observations") +
  xlab("Days participating within a year") +
  guides(fill=guide_legend(title="Participating?")) +
  scale_fill_discrete(labels = c("No (41.5%)", "Yes (58.5%)")) +
  ggtitle("At least 120 days participating within a year (69,213 obs)")

participation_filtered <- participation_data %>%
  filter(participation_ndays >= 150) %>%
  dplyr::arrange(VESSEL_NUM, set_date)
participation_filtered %>% group_by(partDummy) %>% summarize(n_obs = n(), perc = n()/nrow(participation_filtered))
ggplot(data=participation_filtered, aes(x=participation_ndays, group=partDummy, fill=partDummy)) +
  geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
  theme_ipsum()  +
  ylab("Number of observations") +
  xlab("Days participating within a year") +
  guides(fill=guide_legend(title="Participating?")) +
  scale_fill_discrete(labels = c("No (51.8%)", "Yes (48.2%)")) +
  ggtitle("At least 150 days participating within a year (26,955 obs)")

participation_filtered <- participation_data %>%
  filter(participation_ndays >= 180) %>%
  dplyr::arrange(VESSEL_NUM, set_date)
participation_filtered %>% group_by(partDummy) %>% summarize(n_obs = n(), perc = n()/nrow(participation_filtered))
ggplot(data=participation_filtered, aes(x=participation_ndays, group=partDummy, fill=partDummy)) +
  geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
  theme_ipsum()  +
  ylab("Number of observations") +
  xlab("Days participating within a year") +
  guides(fill=guide_legend(title="Participating?")) +
  scale_fill_discrete(labels = c("No (45.3%)", "Yes (54.7%)")) +
  ggtitle("At least half of a year participating (8,440 obs)")

ggplot(data=participation_filtered, aes(x=Revenue_MA, group=partDummy, fill=partDummy)) +
  geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
  theme_ipsum()  +
  xlab("Revenue previous year") +
  guides(fill=guide_legend(title="Participating?")) +
  scale_fill_discrete(labels = c("No", "Yes")) +
  ggtitle("Revenue within year? (150 days participating)")

ggplot(data=participation_filtered, aes(x=perc_CPS, group=partDummy, fill=partDummy)) +
  geom_density(adjust=1.5, alpha=.4, aes(y = ..count..)) +
  theme_ipsum()  +
  xlab("Previous days participating in a year") +
  guides(fill=guide_legend(title="Participating?")) +
  scale_fill_discrete(labels = c("No", "Yes")) +
  ggtitle("Revenue share from CPS within year?  (150 days participating)")


#-----------------------------------------------
## filter non-participation

participation_filtered <- participation_data_all %>% 
  filter(participation_ndays >= 150) %>%
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



