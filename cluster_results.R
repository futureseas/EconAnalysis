#######################
### Cluster results ###
#######################
library("googlesheets4")
gs4_auth(
  email = "fequezad@ucsc.edu",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)


rm(list = ls(all.names = TRUE)) 
gc()

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(forcats)
library(data.table)

# source("C:/GitHub/EconAnalysis/data_processing_vessels.R")
PacFIN.month <- read.csv(file ="C:\\Data\\PacFIN data\\PacFIN_month.csv")

# ### Vessels from cluster 7 ###
# cluster7 <- PacFIN.month %>% filter(group_all == 7) %>% dplyr::select(VESSEL_NUM) %>% unique()
#   write.csv(cluster7,"C:\\Data\\Cluster7_vessels.csv", row.names = FALSE)


### Descriptive statistics ###

## Number of active vessel by clusters
nvessel.year <- PacFIN.month %>% dplyr::filter(LANDED_WEIGHT_MTONS.sum > 0) %>%
  dplyr::select(LANDING_YEAR, VESSEL_NUM, group_all) %>% unique() %>% 
  mutate(n_vessel = 1) %>% group_by(LANDING_YEAR, group_all) %>%
  summarise(n_vessel = sum(n_vessel))

ggplot(nvessel.year, aes(x=LANDING_YEAR, y = n_vessel)) + 
  geom_line(color = "steelblue", size = 1) + 
  facet_wrap(~ group_all, ncol = 4) + geom_point(color = "steelblue") + 
  scale_y_continuous(name = "Number of active vessels") +
  scale_x_continuous(name = "Year") 

#-----------------------------------------  
## Number and percentage of light brail vessels by cluster
  options(scipen=999)
  VESSEL_NUM_to_CDFW_PLATE <- read.csv(file = "C:\\Data\\VESSEL_NUM_to_CDFW_PLATE.csv")
  Light.Brail.Vessels <- read.csv(file = "C:\\Data\\CDFW CPS Logbooks\\MarketSquidLightBrailLogDataExtract.csv") %>% 
    dplyr::select('VesselID') %>% dplyr::rename(CDFW_PLATE_NUM = VesselID) %>% unique() 
  Light.Brail.Vessels_num <- merge(VESSEL_NUM_to_CDFW_PLATE, Light.Brail.Vessels, 
                                   by = ("CDFW_PLATE_NUM"), all.x = FALSE, all.y = TRUE) %>% 
    dplyr::select('VESSEL_NUM') %>% unique() %>% drop_na() %>% mutate(light.brail = 1)
  Light.Brail_byClusters <- read.csv(file = here::here("Clustering", "PAM_Vessel_Groups.csv")) %>%
    merge(Light.Brail.Vessels_num, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE) %>%
    mutate_all(~replace(., is.na(.), 0)) %>% group_by(group_all) %>% 
    summarize(n_vessel = n(), n_brail_vessels = sum(light.brail)) %>% mutate(percentage = (n_brail_vessels / n_vessel))
  
  colnames(Light.Brail_byClusters) = c("Cluster", "Number of Vessels", "Number of Light Brail Vessels", "Percentage")
  # gs4_create("LightBrail_by_cluster", sheets = clusters_by_vessel_num)
  
  rm(VESSEL_NUM_to_CDFW_PLATE, Light.Brail.Vessels, Light.Brail.Vessels_num, Light.Brail_byClusters)
  
  
  #-----------------------------------------
  ## Number and percentage of vessels with CPS permits by cluster
  options(scipen=999)
  CPS.Vessels <- read.csv(file = "G:\\My Drive\\Data\\Vessels\\CPS_LE_Permits.csv") %>% 
    dplyr::select('Coast.Guard.Number.Vessel.ID') %>% 
    dplyr::rename(VESSEL_NUM = Coast.Guard.Number.Vessel.ID) %>%
    unique() %>% mutate(CPS.permit = 1)
  
  CPS.permits_byCluster <- read.csv(file = here::here("Clustering", "PAM_Vessel_Groups.csv")) %>%
    merge(CPS.Vessels, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE) %>%
    mutate_all(~replace(., is.na(.), 0)) %>% group_by(group_all) %>% 
    summarize(n_vessel = n(), n_vessel_CPS = sum(CPS.permit)) %>% mutate(percentage = (n_vessel_CPS / n_vessel))
  
  colnames(CPS.permits_byCluster) = c("Cluster", "Number of Vessels", "Vessels with CPS permit", "Percentage")
  # gs4_create("CPS.permits_by_cluster", sheets = CPS.permits_byCluster)
  
  Clusters <- read.csv(file = here::here("Clustering", "PAM_Vessel_Groups.csv"))
  CPS.LE.vessels.with.cluster <- read.csv(file = "G:\\My Drive\\Data\\Vessels\\CPS_LE_Permits.csv") %>% 
    dplyr::rename(VESSEL_NUM = Coast.Guard.Number.Vessel.ID) %>% unique() %>% 
    merge(Clusters, by = c('VESSEL_NUM'), all.x = TRUE, all.y = FALSE)  
    # gs4_create("CPS.LE.vessels.with.cluster", sheets = CPS.LE.vessels.with.cluster)
    
  rm(CPS.LE.vessels.with.cluster, CPS.permits_byCluster, CPS.Vessels)
  

#----------------------------------
## Product use
all_product_use <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(PRODUCT_USE_CODE, DISPOSITION_CODE) %>% unique() %>% mutate(merge=1)
  all_product_use <- all_product_use[-c(11), ] 
all_vessels <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge=1)
expand <- merge(all_product_use, all_vessels, by = c('merge'), all.x = TRUE, all.y = TRUE)
rm(all_product_use, all_vessels)

options(scipen=999)
cluster.use <- PacFIN.month %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>%
  group_by(group_all, PRODUCT_USE_CODE, DISPOSITION_CODE, VESSEL_NUM) %>% 
  summarise(revenue = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
  group_by(VESSEL_NUM) %>% mutate(Percentage = revenue / sum(revenue))

cluster.use <- merge(expand, cluster.use, by = c('VESSEL_NUM', 'PRODUCT_USE_CODE', 'DISPOSITION_CODE'), all.x = TRUE) %>%
  mutate(Percentage = ifelse(is.na(Percentage),0,Percentage)) %>% group_by(VESSEL_NUM) %>%
  mutate(group_all = ifelse(is.na(group_all),mean(group_all, na.rm = TRUE),group_all)) 

hist.bait <- cluster.use %>% filter(PRODUCT_USE_CODE == "B" & DISPOSITION_CODE == "B")
  cluster_names <- as_labeller(c(`1` = "Cluster 1",`2` = "Cluster 2",
                               `3` = "Cluster 3",`4` = "Cluster 4",
                               `5` = "Cluster 5",`6` = "Cluster 6",
                               `7` = "Cluster 7", `8` = "Cluster 8"))
  ggplot(data=hist.bait, 
       aes(x=Percentage, group=group_all, fill=group_all)) +
   geom_density() + 
    facet_wrap(~group_all, scales="free_y", labeller = cluster_names) +
    theme(legend.position="none") + labs(x = "Percentage bait", y = "Density") + 
    scale_fill_viridis()


cluster.use  <- cluster.use %>% 
  group_by(group_all, PRODUCT_USE_CODE, DISPOSITION_CODE) %>% summarise(Percentage = mean(Percentage)) %>% 
  unique() %>% filter(group_all != is.na(group_all))


cluster.use <- cluster.use[order(cluster.use$group_all, -cluster.use$Percentage),]
cluster.use.highest <- cluster.use %>%
  group_by(group_all) %>% filter(row_number()==1:3) %>% ungroup() %>% 
  dplyr::select('PRODUCT_USE_CODE', 'DISPOSITION_CODE') %>% unique()

cluster.use<-setDT(cluster.use)[PRODUCT_USE_CODE %chin% cluster.use.highest$PRODUCT_USE_CODE & 
                                    DISPOSITION_CODE %chin% cluster.use.highest$DISPOSITION_CODE] 


table <- as.data.frame(xtabs(Percentage ~  PRODUCT_USE_CODE + DISPOSITION_CODE + group_all, cluster.use))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
colnames(table) = c("Product Use", "Disposition", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8")
#gs4_create("Table6", sheets = table)
rm(table, cluster.use, cluster.use.highest, hist.bait)


# -----------------------------------------------------------------------------------------------
## Catch composition (all species)
all_species <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(PACFIN_SPECIES_CODE) %>% unique() %>% mutate(merge=1)
all_species_POST <- PacFIN.month  %>%   
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::filter(PSDN.Total.Closure == 1) %>% dplyr::select(PACFIN_SPECIES_CODE) %>% unique() %>% mutate(merge=1)
  
all_vessels <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge=1)
all_vessels_POST <- PacFIN.month  %>%   
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::filter(PSDN.Total.Closure == 1) %>% dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge=1)

expand <- merge(all_species, all_vessels, by = c('merge'), all.x = TRUE, all.y = TRUE)
expand_POST <- merge(all_species_POST, all_vessels_POST, by = c('merge'), all.x = TRUE, all.y = TRUE)
rm(all_species, all_vessels)
rm(all_species_POST, all_vessels_POST)

expand_ALL <- merge(expand, expand_POST, by = c('VESSEL_NUM', 'PACFIN_SPECIES_CODE'), all.x = TRUE, all.y = TRUE)


options(scipen=999)
cluster.species <- PacFIN.month %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>%
  group_by(group_all, PACFIN_SPECIES_CODE, VESSEL_NUM) %>% 
  summarise(revenue = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
  group_by(VESSEL_NUM) %>% mutate(Percentage = revenue / sum(revenue))

cluster.species <- merge(expand_ALL, cluster.species, by = c('VESSEL_NUM', 'PACFIN_SPECIES_CODE'), all.x = TRUE) %>%
  mutate(Percentage = ifelse(is.na(Percentage),0,Percentage)) %>% group_by(VESSEL_NUM) %>%
  mutate(group_all = ifelse(is.na(group_all),mean(group_all, na.rm = TRUE),group_all)) %>% 
  group_by(group_all, PACFIN_SPECIES_CODE) %>% summarise(Percentage = mean(Percentage)) %>% 
  unique() %>% filter(group_all != is.na(group_all))

# cluster.species <- cluster.species[order(cluster.species$group_all, -cluster.species$Percentage),]
# cluster.species.highest <- cluster.species %>%
#   group_by(group_all) %>% filter(row_number()==1:5) %>% ungroup() %>% 
#   dplyr::select('PACFIN_SPECIES_CODE') %>% unique()
# cluster.species <- setDT(cluster.species)[PACFIN_SPECIES_CODE %chin% cluster.species.highest$PACFIN_SPECIES_CODE] 

table <- as.data.frame(xtabs(Percentage ~  PACFIN_SPECIES_CODE + group_all, cluster.species))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "CLuster 7", "Cluster 8")
# gs4_create("Table2_all_species", sheets = table)

rm(table, cluster.species, cluster.species.highest)


### After ###
options(scipen=999)
cluster.species_POST <- PacFIN.month %>%   
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::filter(PSDN.Total.Closure == 1) %>%
  group_by(group_all, PACFIN_SPECIES_CODE, VESSEL_NUM) %>% 
  summarise(revenue = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
  group_by(VESSEL_NUM) %>% mutate(Percentage = revenue / sum(revenue))

cluster.species_POST <- merge(expand_ALL, cluster.species_POST, by = c('VESSEL_NUM', 'PACFIN_SPECIES_CODE'), all.x = TRUE) %>%
  mutate(Percentage = ifelse(is.na(Percentage),0,Percentage)) %>% group_by(VESSEL_NUM) %>%
  mutate(group_all = ifelse(is.na(group_all),mean(group_all, na.rm = TRUE),group_all)) %>% 
  group_by(group_all, PACFIN_SPECIES_CODE) %>% summarise(Percentage = mean(Percentage)) %>% 
  unique() %>% filter(group_all != is.na(group_all))

# cluster.species <- cluster.species[order(cluster.species$group_all, -cluster.species$Percentage),]
# cluster.species.highest <- cluster.species %>%
#   group_by(group_all) %>% filter(row_number()==1:5) %>% ungroup() %>% 
#   dplyr::select('PACFIN_SPECIES_CODE') %>% unique()
# cluster.species <- setDT(cluster.species)[PACFIN_SPECIES_CODE %chin% cluster.species.highest$PACFIN_SPECIES_CODE] 

table <- as.data.frame(xtabs(Percentage ~  PACFIN_SPECIES_CODE + group_all, cluster.species_POST))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "CLuster 7", "Cluster 8")
# gs4_create("Table2_POST_all_species", sheets = table)
rm(table, cluster.species_POST, cluster.species.highest_POST)


# -----------------------------------------------------------------------------------------------
## Catch composition 
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "CMCK"] <- "OMCK")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "JMCK"] <- "OMCK")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "UMCK"] <- "OMCK")
PacFIN.month <- PacFIN.month %>% mutate(
  PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "OMCK",PACFIN_SPECIES_CODE,
                               ifelse(PACFIN_SPECIES_CODE == "PSDN",PACFIN_SPECIES_CODE,
                                      ifelse(PACFIN_SPECIES_CODE == "MSQD", PACFIN_SPECIES_CODE,
                                             ifelse(PACFIN_SPECIES_CODE == "NANC", PACFIN_SPECIES_CODE,
                                                    ifelse(PACFIN_SPECIES_CODE == "ALBC", PACFIN_SPECIES_CODE,
                                                           ifelse(PACFIN_SPECIES_CODE == "DCRB", PACFIN_SPECIES_CODE,
                                                                  ifelse(PACFIN_SPECIES_CODE == "LOBS", PACFIN_SPECIES_CODE, "OTHER"))))))))


all_species <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(PACFIN_SPECIES_CODE) %>% unique() %>% mutate(merge=1)
all_species_POST <- PacFIN.month  %>%   
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::filter(PSDN.Total.Closure == 1) %>% dplyr::select(PACFIN_SPECIES_CODE) %>% unique() %>% mutate(merge=1)

all_vessels <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge=1)
all_vessels_POST <- PacFIN.month  %>%   
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::filter(PSDN.Total.Closure == 1) %>% dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge=1)

expand <- merge(all_species, all_vessels, by = c('merge'), all.x = TRUE, all.y = TRUE)
expand_POST <- merge(all_species_POST, all_vessels_POST, by = c('merge'), all.x = TRUE, all.y = TRUE)
rm(all_species, all_vessels)
rm(all_species_POST, all_vessels_POST)

options(scipen=999)
cluster.species <- PacFIN.month %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>%
  group_by(group_all, PACFIN_SPECIES_CODE, VESSEL_NUM) %>% 
  summarise(revenue = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
  group_by(VESSEL_NUM) %>% mutate(Percentage = revenue / sum(revenue))

cluster.species <- merge(expand, cluster.species, by = c('VESSEL_NUM', 'PACFIN_SPECIES_CODE'), all.x = TRUE) %>%
  mutate(Percentage = ifelse(is.na(Percentage),0,Percentage)) %>% group_by(VESSEL_NUM) %>%
  mutate(group_all = ifelse(is.na(group_all),mean(group_all, na.rm = TRUE),group_all)) %>% 
  group_by(group_all, PACFIN_SPECIES_CODE) %>% summarise(Percentage = mean(Percentage)) %>% 
  unique() %>% filter(group_all != is.na(group_all))

# cluster.species <- cluster.species[order(cluster.species$group_all, -cluster.species$Percentage),]
# cluster.species.highest <- cluster.species %>%
#   group_by(group_all) %>% filter(row_number()==1:5) %>% ungroup() %>% 
#   dplyr::select('PACFIN_SPECIES_CODE') %>% unique()
# cluster.species <- setDT(cluster.species)[PACFIN_SPECIES_CODE %chin% cluster.species.highest$PACFIN_SPECIES_CODE] 

table <- as.data.frame(xtabs(Percentage ~  PACFIN_SPECIES_CODE + group_all, cluster.species))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "CLuster 7", "Cluster 8")
# gs4_create("Table2", sheets = table)

rm(table, cluster.species, cluster.species.highest)


### After ###
options(scipen=999)
cluster.species_POST <- PacFIN.month %>%   
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::filter(PSDN.Total.Closure == 1) %>%
  group_by(group_all, PACFIN_SPECIES_CODE, VESSEL_NUM) %>% 
  summarise(revenue = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
  group_by(VESSEL_NUM) %>% mutate(Percentage = revenue / sum(revenue))

cluster.species_POST <- merge(expand_POST, cluster.species_POST, by = c('VESSEL_NUM', 'PACFIN_SPECIES_CODE'), all.x = TRUE) %>%
  mutate(Percentage = ifelse(is.na(Percentage),0,Percentage)) %>% group_by(VESSEL_NUM) %>%
  mutate(group_all = ifelse(is.na(group_all),mean(group_all, na.rm = TRUE),group_all)) %>% 
  group_by(group_all, PACFIN_SPECIES_CODE) %>% summarise(Percentage = mean(Percentage)) %>% 
  unique() %>% filter(group_all != is.na(group_all))

# cluster.species <- cluster.species[order(cluster.species$group_all, -cluster.species$Percentage),]
# cluster.species.highest <- cluster.species %>%
#   group_by(group_all) %>% filter(row_number()==1:5) %>% ungroup() %>% 
#   dplyr::select('PACFIN_SPECIES_CODE') %>% unique()
# cluster.species <- setDT(cluster.species)[PACFIN_SPECIES_CODE %chin% cluster.species.highest$PACFIN_SPECIES_CODE] 

table <- as.data.frame(xtabs(Percentage ~  PACFIN_SPECIES_CODE + group_all, cluster.species_POST))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "CLuster 7", "Cluster 8")
# gs4_create("Table2_POST", sheets = table)

rm(table, cluster.species_POST, cluster.species.highest_POST)



#-----------------------------------
## Gear
all_gear <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(PACFIN_GEAR_CODE) %>% unique() %>% mutate(merge=1)
all_gear_POST <- PacFIN.month  %>%   
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::filter(PSDN.Total.Closure == 1) %>% dplyr::select(PACFIN_GEAR_CODE) %>% unique() %>% mutate(merge=1)

all_vessels <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge=1)
all_vessels_POST <- PacFIN.month  %>%   
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::filter(PSDN.Total.Closure == 1) %>% dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge=1)

expand <- merge(all_gear, all_vessels, by = c('merge'), all.x = TRUE, all.y = TRUE)
expand_POST <- merge(all_gear_POST, all_vessels_POST, by = c('merge'), all.x = TRUE, all.y = TRUE)
rm(all_gear_POST, all_vessels_POST)
rm(all_gear, all_vessels)

expand_ALL <- merge(expand, expand_POST, by = c('VESSEL_NUM', 'PACFIN_GEAR_CODE'), all.x = TRUE, all.y = TRUE)


### How gear are used by clusters??? ###
options(scipen=999)
cluster.gear <- PacFIN.month %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% 
  group_by(group_all, PACFIN_GEAR_CODE, VESSEL_NUM) %>% 
  summarise(revenue = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
  group_by(VESSEL_NUM) %>% mutate(Percentage = revenue / sum(revenue))

cluster.gear <- merge(expand_ALL, cluster.gear, by = c('VESSEL_NUM', 'PACFIN_GEAR_CODE'), all.x = TRUE) %>%
  mutate(Percentage = ifelse(is.na(Percentage),0,Percentage)) %>% group_by(VESSEL_NUM) %>%
  mutate(group_all = ifelse(is.na(group_all),mean(group_all, na.rm = TRUE),group_all)) %>% 
  group_by(group_all, PACFIN_GEAR_CODE) %>% summarise(Percentage = mean(Percentage)) %>% 
  unique() %>% filter(group_all != is.na(group_all))

# cluster.gear <- cluster.gear[order(cluster.gear$group_all, -cluster.gear$Percentage),]
# cluster.gear.highest <- cluster.gear %>%
#   group_by(group_all) %>% filter(row_number()==1:3) %>% ungroup() %>% 
#   dplyr::select('PACFIN_GEAR_CODE') %>% unique()
# cluster.gear<-setDT(cluster.gear)[PACFIN_GEAR_CODE %chin% cluster.gear.highest$PACFIN_GEAR_CODE] 


table <- as.data.frame(xtabs(Percentage ~  PACFIN_GEAR_CODE + group_all, cluster.gear))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
# rownames(table) = c("Crab and Lobster Pot", "Dip Net", "Other Net Gear", "Seine")
colnames(table) = c("Gear", "Cluster 1", "Cluster 2", "Cluster 3", 
                    "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8")
gs4_create("Table3", sheets = table)
# print(xtable(table, caption = 'Percentage of cluster total langings by gear used.\\label{Table:cluster_gear}', type = "latex"), comment=FALSE,  caption.placement = "top")

rm(table, cluster.gear)


## Gear used after closure ##
options(scipen=999)
cluster.gear_POST <- PacFIN.month %>%   
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::filter(PSDN.Total.Closure == 1) %>%
  group_by(group_all, PACFIN_GEAR_CODE, VESSEL_NUM) %>% 
  summarise(revenue = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
  group_by(VESSEL_NUM) %>% mutate(Percentage = revenue / sum(revenue))

cluster.gear_POST <- merge(expand_ALL, cluster.gear_POST, by = c('VESSEL_NUM', 'PACFIN_GEAR_CODE'), all.x = TRUE) %>%
  mutate(Percentage = ifelse(is.na(Percentage),0,Percentage)) %>% group_by(VESSEL_NUM) %>%
  mutate(group_all = ifelse(is.na(group_all),mean(group_all, na.rm = TRUE),group_all)) %>% 
  group_by(group_all, PACFIN_GEAR_CODE) %>% summarise(Percentage = mean(Percentage)) %>% 
  unique() %>% filter(group_all != is.na(group_all))

# cluster.gear_POST <- cluster.gear_POST[order(cluster.gear_POST$group_all, -cluster.gear_POST$Percentage),]
# cluster.gear_POST.highest <- cluster.gear_POST %>%
#   group_by(group_all) %>% filter(row_number()==1:3) %>% ungroup() %>%
#   dplyr::select('PACFIN_GEAR_CODE') %>% unique()
# cluster.gear_POST <- setDT(cluster.gear_POST)[PACFIN_GEAR_CODE %chin% cluster.gear_POST.highest$PACFIN_GEAR_CODE]

table <- as.data.frame(xtabs(Percentage ~  PACFIN_GEAR_CODE + group_all, cluster.gear_POST))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
# rownames(table) = c("Crab and Lobster Pot", "Dip Net", "Other Net Gear", "Seine")
colnames(table) = c("Gear", "Cluster 1", "Cluster 2", "Cluster 3", 
                    "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8")
gs4_create("Table3_POST", sheets = table)
# print(xtable(table, caption = 'Percentage of cluster total langings by gear used.\\label{Table:cluster_gear}', type = "latex"), comment=FALSE,  caption.placement = "top")

rm(table, cluster.gear_POST)



#-----------------------------------
## Port area

all_ports <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(PORT_AREA_CODE) %>% unique() %>% mutate(merge=1)
all_vessels <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge=1)
expand <- merge(all_ports, all_vessels, by = c('merge'), all.x = TRUE, all.y = TRUE)
rm(all_ports, all_vessels)

options(scipen=999)
cluster.port <- PacFIN.month %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% 
  group_by(group_all, PORT_AREA_CODE, VESSEL_NUM) %>% 
  summarise(revenue = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
  group_by(VESSEL_NUM) %>% mutate(Percentage = revenue / sum(revenue))

cluster.port <- merge(expand, cluster.port, by = c('VESSEL_NUM', 'PORT_AREA_CODE'), all.x = TRUE) %>%
  mutate(Percentage = ifelse(is.na(Percentage),0,Percentage)) %>% group_by(VESSEL_NUM) %>%
  mutate(group_all = ifelse(is.na(group_all),mean(group_all, na.rm = TRUE),group_all)) %>% 
  group_by(group_all, PORT_AREA_CODE) %>% summarise(Percentage = mean(Percentage)) %>% 
  unique() %>% filter(group_all != is.na(group_all))

cluster.port <- cluster.port[order(cluster.port$group_all, -cluster.port$Percentage),]
cluster.port.highest <- cluster.port %>%
  group_by(group_all) %>% filter(Percentage >= 0.1) %>% ungroup() %>% 
  dplyr::select('PORT_AREA_CODE') %>% unique()

cluster.port<-setDT(cluster.port)[PORT_AREA_CODE %chin% cluster.port.highest$PORT_AREA_CODE] 


table <- as.data.frame(xtabs(Percentage ~  PORT_AREA_CODE + group_all, cluster.port))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", 
                    "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8")
# gs4_create("Table4", sheets = table)
# print(xtable(table, caption = 'Percentage of cluster total langings by gear used.\\label{Table:cluster_gear}', type = "latex"), comment=FALSE,  caption.placement = "top")

rm(table, cluster.port, cluster.port.highest)



#-----------------------------------
## Port area (percentage of revenue that come from each cluster in a port area)

options(scipen=999)
cluster.port <- PacFIN.month %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% 
  group_by(group_all, PORT_AREA_CODE) %>% 
  summarise(revenue = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
  group_by(PORT_AREA_CODE) %>% mutate(Percentage = revenue / sum(revenue))

table <- as.data.frame(xtabs(Percentage ~  PORT_AREA_CODE + group_all, cluster.port))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
# rownames(table) = c("Crab and Lobster Pot", "Dip Net", "Other Net Gear", "Seine")
colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", 
                    "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")
gs4_create("Table5", sheets = table)
# print(xtable(table, caption = 'Percentage of cluster total langings by gear used.\\label{Table:cluster_gear}', type = "latex"), comment=FALSE,  caption.placement = "top")

rm(table, cluster.port, cluster.port.highest)




#----------------------------------------
### Input histograms by clusters ###
RAW_cluster_inputs <- read.csv(here::here("Clustering", "RAW_cluster_inputs.csv"))
library(ggplot2)

cluster_names <- as_labeller(c(`1` = "Cluster 1",`2` = "Cluster 2",
                               `3` = "Cluster 3",`4` = "Cluster 4",
                               `5` = "Cluster 5",`6` = "Cluster 6",
                               `7` = "Cluster 7",`8` = "Cluster 8"))

## Average Revenue
ggplot(data=RAW_cluster_inputs, aes(x=AVG_REVENUE, fill=group_all))+
  geom_density()+
  facet_wrap(~group_all, scales="free_y") +
  theme(legend.position="none") + labs(x = "Average annual revenue", y = "Density") + 
  scale_fill_viridis()

## LCG
ggplot(data=RAW_cluster_inputs, aes(x=LAT, fill=group_all))+
  geom_density()+
  facet_wrap(~group_all, scales="free_y") +
  theme(legend.position="none") + labs(x = "Latitudinal Center of Gravity (LCG)", y = "Density") + 
  scale_fill_viridis()

## Inertia
ggplot(data=RAW_cluster_inputs, aes(x=DISTANCE_A, fill=group_all))+
  geom_density()+
  facet_wrap(~group_all, scales="free_y") +
  theme(legend.position="none") + labs(x = "Inertia", y = "Density") + 
  scale_fill_viridis()

## Percentage revenue from CPS
ggplot(data=RAW_cluster_inputs, aes(x=Percentage, fill=group_all))+
  geom_density()+
  facet_wrap(~group_all, scales="free_y") +
  theme(legend.position="none") + labs(x = "Percent of revenue from CPS", y = "Density") + 
  scale_fill_viridis()

## CPS diversity index
ggplot(data=RAW_cluster_inputs, aes(x=diversity, fill=group_all))+
  geom_density()+
  facet_wrap(~group_all, scales="free_y") +
  theme(legend.position="none") + labs(x = "CPS diversity index", y = "Density") + 
  scale_fill_viridis()

#----------------------------------------
### Participation (excluded from paper, enougth to use cluster) ###

# total.revenue <- PacFIN.month %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(year_revenue = sum(AFI_EXVESSEL_REVENUE.sum))
# 
# ### Vessel that participate in Pacific sardine fishery ###
# psdn.fleet <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("PSDN")) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(revenue_psdn = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
#   merge(total.revenue, by = c("VESSEL_NUM", "LANDING_YEAR"), all.x = TRUE) %>%
#   filter(revenue_psdn > 0) %>% group_by(VESSEL_NUM) %>%
#   summarise(anual_revenue_psdn = mean(revenue_psdn), 
#             anual_revenue_all = mean(year_revenue), 
#             n_year_fishing = n()) %>%
#   mutate(perc_psdn_rev = anual_revenue_psdn/anual_revenue_all) %>%
#   filter(anual_revenue_psdn >= 1000) %>%
#   filter(perc_psdn_rev >= 0.05) %>%
#   filter(n_year_fishing >= 3) %>%
#   select(VESSEL_NUM) %>% unique() %>% mutate(d_psdn_fleet = 1)
# 
# ### Vessel that participate in Market squid fishery ###
# msqd.fleet <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("MSQD")) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(revenue_msqd = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
#   merge(total.revenue, by = c("VESSEL_NUM", "LANDING_YEAR"), all.x = TRUE) %>%
#   filter(revenue_msqd > 0) %>% group_by(VESSEL_NUM) %>%
#   summarise(anual_revenue_msqd = mean(revenue_msqd), 
#             anual_revenue_all = mean(year_revenue), 
#             n_year_fishing = n()) %>%
#   mutate(perc_msqd_rev = anual_revenue_msqd/anual_revenue_all) %>%
#   filter(anual_revenue_msqd >= 1000) %>%
#   filter(perc_msqd_rev >= 0.05) %>%
#   filter(n_year_fishing >= 3) %>%
#   select(VESSEL_NUM) %>% unique() %>% mutate(d_msqd_fleet = 1)
# 
# ### Vessel that participate in Northen anchovy fishery ###
# nanc.fleet <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("NANC")) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(revenue_nanc = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
#   merge(total.revenue, by = c("VESSEL_NUM", "LANDING_YEAR"), all.x = TRUE) %>%
#   filter(revenue_nanc > 0) %>% group_by(VESSEL_NUM) %>%
#   summarise(anual_revenue_nanc = mean(revenue_nanc), 
#             anual_revenue_all = mean(year_revenue), 
#             n_year_fishing = n()) %>%
#   mutate(perc_nanc_rev = anual_revenue_nanc/anual_revenue_all) %>%
#   filter(anual_revenue_nanc >= 1000) %>%
#   filter(perc_nanc_rev >= 0.05) %>%
#   filter(n_year_fishing >= 3) %>%
#   select(VESSEL_NUM) %>% unique() %>% mutate(d_nanc_fleet = 1)
# 
# ### Vessel that participate in Pacific mackerel fishery ###
# cmck.fleet <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("CMCK")) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(revenue_cmck = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
#   merge(total.revenue, by = c("VESSEL_NUM", "LANDING_YEAR"), all.x = TRUE) %>%
#   filter(revenue_cmck > 0) %>% group_by(VESSEL_NUM) %>%
#   summarise(anual_revenue_cmck = mean(revenue_cmck), 
#             anual_revenue_all = mean(year_revenue), 
#             n_year_fishing = n()) %>%
#   mutate(perc_cmck_rev = anual_revenue_cmck/anual_revenue_all) %>%
#   filter(anual_revenue_cmck >= 1000) %>%
#   filter(perc_cmck_rev >= 0.05) %>%
#   filter(n_year_fishing >= 3) %>%
#   select(VESSEL_NUM) %>% unique() %>% mutate(d_cmck_fleet = 1)
# 
# ### Vessel that participate in Jack mackerel fishery ###
# jmck.fleet <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("JMCK")) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(revenue_jmck = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
#   merge(total.revenue, by = c("VESSEL_NUM", "LANDING_YEAR"), all.x = TRUE) %>%
#   filter(revenue_jmck > 0) %>% group_by(VESSEL_NUM) %>%
#   summarise(anual_revenue_jmck = mean(revenue_jmck), 
#             anual_revenue_all = mean(year_revenue), 
#             n_year_fishing = n()) %>%
#   mutate(perc_jmck_rev = anual_revenue_jmck/anual_revenue_all) %>%
#   filter(anual_revenue_jmck >= 1000) %>%
#   filter(perc_jmck_rev >= 0.05) %>%
#   filter(n_year_fishing >= 3) %>%
#   select(VESSEL_NUM) %>% unique() %>% mutate(d_jmck_fleet = 1)
# 
# ### Vessel that participate in Unknown mackerel fishery ###
# umck.fleet <- PacFIN.month.CPS %>% filter(LANDING_YEAR >= 2000) %>% filter(LANDING_YEAR <= 2014) %>%
#   filter(PACFIN_SPECIES_CODE  %in% c("UMCK")) %>%
#   group_by(VESSEL_NUM, LANDING_YEAR) %>% summarise(revenue_umck = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
#   merge(total.revenue, by = c("VESSEL_NUM", "LANDING_YEAR"), all.x = TRUE) %>%
#   filter(revenue_umck > 0) %>% group_by(VESSEL_NUM) %>%
#   summarise(anual_revenue_umck = mean(revenue_umck), 
#             anual_revenue_all = mean(year_revenue), 
#             n_year_fishing = n()) %>%
#   mutate(perc_umck_rev = anual_revenue_umck/anual_revenue_all) %>%
#   filter(anual_revenue_umck >= 1000) %>%
#   filter(perc_umck_rev >= 0.05) %>%
#   filter(n_year_fishing >= 3) %>%
#   select(VESSEL_NUM) %>% unique() %>% mutate(d_umck_fleet = 1)
# 
# 
# # Select vessel that participate in the CPS fishery. 
# PacFIN.month <- PacFIN.month %>% 
#   merge(psdn.fleet, by = c("VESSEL_NUM"), all.y = FALSE, all.x = TRUE) %>%
#   merge(msqd.fleet, by = c("VESSEL_NUM"), all.y = FALSE, all.x = TRUE) %>%
#   merge(nanc.fleet, by = c("VESSEL_NUM"), all.y = FALSE, all.x = TRUE) %>%
#   merge(cmck.fleet, by = c("VESSEL_NUM"), all.y = FALSE, all.x = TRUE) %>%
#   merge(jmck.fleet, by = c("VESSEL_NUM"), all.y = FALSE, all.x = TRUE) %>% 
#   filter(d_psdn_fleet == 1 | d_msqd_fleet == 1 | d_nanc_fleet == 1 | d_cmck_fleet == 1 | d_jmck_fleet == 1) %>%
#   rowwise() %>% 
#   mutate(d_fleet_CPS = sum(d_psdn_fleet, d_msqd_fleet, d_nanc_fleet, d_cmck_fleet, d_jmck_fleet, na.rm = TRUE))
# 
# rm(total.revenue, psdn.fleet, msqd.fleet, nanc.fleet, cmck.fleet, jmck.fleet, umck.fleet)
# 
# # CREATE NEW PACFIN.MONTH.CPS DATASET!
# PacFIN.month.CPS <- PacFIN.month %>% 
#   dplyr::filter(PACFIN_SPECIES_CODE %in% 
#                   c("CMCK", "JMCK", "MSQD", "NANC", "PSDN", "UMCK"))





#-----------------
