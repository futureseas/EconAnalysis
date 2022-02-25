#######################
### Cluster results ###
#######################
rm(list = ls(all.names = TRUE)) 
gc()

library("googlesheets4")
gs4_auth(
  email = gs4_auth(),
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(viridis)
library(forcats)

PacFIN.month <- read.csv(file ="C:\\Data\\PacFIN data\\PacFIN_month.csv")
n.period = 5


### Descriptive statistics ###

#-----------------------------------
## Catch composition
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "CMCK"] <- "OMCK")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "JMCK"] <- "OMCK")
PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "UMCK"] <- "OMCK")
PacFIN.month <- PacFIN.month %>% mutate(
  PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "OMCK",PACFIN_SPECIES_CODE, 
                               ifelse(PACFIN_SPECIES_CODE == "PSDN",PACFIN_SPECIES_CODE, 
                                      ifelse(PACFIN_SPECIES_CODE == "MSQD", PACFIN_SPECIES_CODE, 
                                             ifelse(PACFIN_SPECIES_CODE == "NANC", PACFIN_SPECIES_CODE, "OTHER")))))


all_species <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(PACFIN_SPECIES_CODE) %>% unique() %>% mutate(merge=1)
all_vessels <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge=1)
expand <- merge(all_species, all_vessels, by = c('merge'), all.x = TRUE, all.y = TRUE)
rm(all_species, all_vessels)

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

table <- as.data.frame(xtabs(Percentage ~  PACFIN_SPECIES_CODE + group_all, cluster.species ))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
# rownames(table) = c("Crab and Lobster Pot", "Dip Net", "Other Net Gear", "Seine")
colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "CLuster 7")
# gs4_create("Table2", sheets = table)
# print(xtable(table, caption = 'Percentage of cluster total langings by gear used.\\label{Table:cluster_gear}', type = "latex"), comment=FALSE,  caption.placement = "top")

rm(table, cluster.species)


#-----------------------------------
## Gear

all_gear <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(PACFIN_GEAR_CODE) %>% unique() %>% mutate(merge=1)
all_vessels <- PacFIN.month  %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge=1)
expand <- merge(all_gear, all_vessels, by = c('merge'), all.x = TRUE, all.y = TRUE)

rm(all_gear, all_vessels)

### How gear are used by clusters??? ###
options(scipen=999)
cluster.gear <- PacFIN.month %>% filter(LANDING_YEAR >= 2005) %>% 
  filter(LANDING_YEAR <= 2014) %>% 
  group_by(group_all, PACFIN_GEAR_CODE, VESSEL_NUM) %>% 
  summarise(revenue = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
  group_by(VESSEL_NUM) %>% mutate(Percentage = revenue / sum(revenue))

cluster.gear <- merge(expand, cluster.gear, by = c('VESSEL_NUM', 'PACFIN_GEAR_CODE'), all.x = TRUE) %>%
  mutate(Percentage = ifelse(is.na(Percentage),0,Percentage)) %>% group_by(VESSEL_NUM) %>%
  mutate(group_all = ifelse(is.na(group_all),mean(group_all, na.rm = TRUE),group_all)) %>% 
  group_by(group_all, PACFIN_GEAR_CODE) %>% summarise(Percentage = mean(Percentage)) %>% 
  unique() %>% filter(group_all != is.na(group_all))

cluster.gear <- cluster.gear[order(cluster.gear$group_all, -cluster.gear$Percentage),]
cluster.gear.highest <- cluster.gear %>%
  group_by(group_all) %>% filter(row_number()==1:3) %>% ungroup() %>% 
  dplyr::select('PACFIN_GEAR_CODE') %>% unique()
cluster.gear<-setDT(cluster.gear)[PACFIN_GEAR_CODE %chin% cluster.gear.highest$PACFIN_GEAR_CODE] 


table <- as.data.frame(xtabs(Percentage ~  PACFIN_GEAR_CODE + group_all, cluster.gear ))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
# rownames(table) = c("Crab and Lobster Pot", "Dip Net", "Other Net Gear", "Seine")
colnames(table) = c("Gear", "Cluster 1", "Cluster 2", "Cluster 3", 
                    "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")
# gs4_create("Table3", sheets = table)
# print(xtable(table, caption = 'Percentage of cluster total langings by gear used.\\label{Table:cluster_gear}', type = "latex"), comment=FALSE,  caption.placement = "top")

rm(table, cluster.gear, cluster.gear.highest)



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
  group_by(group_all) %>% filter(row_number()==1:4) %>% ungroup() %>% 
  dplyr::select('PORT_AREA_CODE') %>% unique()

cluster.port<-setDT(cluster.port)[PORT_AREA_CODE %chin% cluster.port.highest$PORT_AREA_CODE] 


table <- as.data.frame(xtabs(Percentage ~  PORT_AREA_CODE + group_all, cluster.port))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
# rownames(table) = c("Crab and Lobster Pot", "Dip Net", "Other Net Gear", "Seine")
colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", 
                    "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")
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
                               `7` = "Cluster 7"))


## Length
ggplot(data=RAW_cluster_inputs, 
       aes(x=Length, group=group_all, fill=group_all)) +
  geom_density() + 
  facet_wrap(~group_all, scales="free_y", labeller = cluster_names) +
  theme(legend.position="none") + labs(x = "Vessel length", y = "Density") + 
  scale_fill_viridis()

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
