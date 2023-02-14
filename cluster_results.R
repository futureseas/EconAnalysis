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


### Descriptive statistics ###

# ### Figure. Annual revenue by cluster 
# PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "CMCK"] <- "OMCK")
# PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "JMCK"] <- "OMCK")
# PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "UMCK"] <- "OMCK")
# PacFIN.month <- PacFIN.month %>% mutate(
#   PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "OMCK",PACFIN_SPECIES_CODE, 
#                                ifelse(PACFIN_SPECIES_CODE == "PSDN",PACFIN_SPECIES_CODE, 
#                                       ifelse(PACFIN_SPECIES_CODE == "MSQD", PACFIN_SPECIES_CODE, 
#                                              ifelse(PACFIN_SPECIES_CODE == "NANC", PACFIN_SPECIES_CODE, "OTHER")))))
# 
# total.revenue.clusters <- PacFIN.month %>% 
#   group_by(group_all, VESSEL_NUM, LANDING_YEAR, PACFIN_SPECIES_CODE) %>%
#   summarize(sum.vessel.revenue.year = sum(AFI_EXVESSEL_REVENUE.sum)) %>%
#   group_by(group_all, LANDING_YEAR, PACFIN_SPECIES_CODE) %>%
#   summarize(mean.revenue.year = sum(sum.vessel.revenue.year)) %>%
#   group_by(group_all, PACFIN_SPECIES_CODE) %>%
#   summarize(annual.revenue = sum(mean.revenue.year)) 
#   
# 
# cond_label <- as_labeller(c("1" = "Southern CCS small-scale\nsquid-specialists",
#                             "2" = "Southern CCS small-scale\nCPS-opportunists",
#                             "3" = "PNW sardine\nopportunists",
#                             "4" = "Southern CCS industrial\nsquid-specialists",
#                             "5" = "Roving industrial\nsardine-squid generalists",
#                             "6" = "PNW sardine\nspecialists",
#                             "7" = "Southern CCS\nforage fish diverse",
#                             "8" = "PNW albacore-crab\ngeneralists"))
# 
# ggplot(total.revenue.clusters, aes(fill=PACFIN_SPECIES_CODE, 
#                                     y=annual.revenue, x = PACFIN_SPECIES_CODE)) +
#     geom_bar(position="dodge", stat="identity") + 
#     facet_wrap(~ group_all, labeller = cond_label, ncol = 4) +
#     scale_fill_viridis(discrete = T, labels=c("MSQD" = "Market Squid", "NANC" = "Northern Anchovy", 
#                                             "PSDN" = "Pacific Sardine", "OMCK" = "Mackerels",
#                                             "OTHER" = "Non-CPS")) + xlab("") + 
#   theme(axis.title = element_text(size = 9)) +
#   ylab("Average annual revenue (USD)") + theme(legend.position="none") +
#   guides(fill=guide_legend(title="Species: "))  + 
#   scale_x_discrete(labels=c(
#     "MSQD" = "Market\nSquid", "NANC" = "Northern\nAnchovy", 
#     "PSDN" = "Pacific\nSardine", "OMCK" = "Mackerels",
#     "OTHER" = "Non-CPS")) +
#   scale_color_brewer(palette="Set2") 





## Figure 9. Number of active vessel by clusters
nvessel.year <- PacFIN.month %>% dplyr::filter(LANDED_WEIGHT_MTONS.sum > 0) %>%
  dplyr::select(LANDING_YEAR, VESSEL_NUM, group_all) %>% unique() %>% 
  mutate(n_vessel = 1) %>% group_by(LANDING_YEAR, group_all) %>%
  summarise(n_vessel = sum(n_vessel))

cond_label <- as_labeller(c("1" = "Southern CCS small-scale\nsquid-specialists",
                            "2" = "Southern CCS small-scale\nCPS-opportunists",
                            "3" = "PNW sardine\nopportunists",
                            "4" = "Southern CCS industrial\nsquid-specialists",
                            "5" = "Roving industrial\nsardine-squid generalists",
                            "6" = "PNW sardine\nspecialists",
                            "7" = "Southern CCS\nforage fish diverse",
                            "8" = "PNW albacore-crab\ngeneralists"))

ggplot(nvessel.year, aes(x=LANDING_YEAR, y = n_vessel)) + 
  geom_line(color = "steelblue", size = 1) + 
  facet_wrap(~ group_all, labeller = cond_label, ncol = 4) + geom_point(color = "steelblue") + 
  scale_y_continuous(name = "Number of active vessels") +
  scale_x_continuous(name = "Year") 


#-----------------------------------------  
# ## Table W1. Number and percentage of light brail vessels by cluster
#   options(scipen=999)
#   VESSEL_NUM_to_CDFW_PLATE <- read.csv(file = "C:\\Data\\VESSEL_NUM_to_CDFW_PLATE.csv")
#   Light.Brail.Vessels <- read.csv(file = "C:\\Data\\CDFW CPS Logbooks\\MarketSquidLightBrailLogDataExtract.csv") %>% 
#     dplyr::select('VesselID') %>% dplyr::rename(CDFW_PLATE_NUM = VesselID) %>% unique() 
#   Light.Brail.Vessels_num <- merge(VESSEL_NUM_to_CDFW_PLATE, Light.Brail.Vessels, by = ("CDFW_PLATE_NUM"), all.x = FALSE, all.y = TRUE) %>% 
#     dplyr::select('VESSEL_NUM') %>% unique() %>% drop_na() %>% mutate(light.brail = 1)
#   Light.Brail_byClusters <- read.csv(file = here::here("Clustering", "PAM_Vessel_Groups.csv")) %>%
#     merge(Light.Brail.Vessels_num, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE) %>%
#     mutate_all(~replace(., is.na(.), 0)) %>% group_by(group_all) %>% 
#     summarize(n_vessel = n(), n_brail_vessels = sum(light.brail)) %>% mutate(percentage = (n_brail_vessels / n_vessel))
#   
#   colnames(Light.Brail_byClusters) = c("Cluster", "Number of Vessels", "Number of Light Brail Vessels", "Percentage")
#   # gs4_create("LightBrail_by_cluster", sheets = clusters_by_vessel_num)
#   
#   rm(VESSEL_NUM_to_CDFW_PLATE, Light.Brail.Vessels, Light.Brail.Vessels_num, Light.Brail_byClusters)
  
  
#-----------------------------------------
## Table W2. Number and percentage of vessels with CPS permits by cluster
  # options(scipen=999)
  # CPS.Vessels <- read.csv(file = "G:\\My Drive\\Data\\Vessels\\CPS_LE_Permits.csv") %>% 
  #   dplyr::select('Coast.Guard.Number.Vessel.ID') %>% 
  #   dplyr::rename(VESSEL_NUM = Coast.Guard.Number.Vessel.ID) %>%
  #   unique() %>% mutate(CPS.permit = 1)
  # 
  # CPS.permits_byCluster <- read.csv(file = here::here("Clustering", "PAM_Vessel_Groups.csv")) %>%
  #   merge(CPS.Vessels, by = ("VESSEL_NUM"), all.x = TRUE, all.y = FALSE) %>%
  #   mutate_all(~replace(., is.na(.), 0)) %>% group_by(group_all) %>% 
  #   summarize(n_vessel = n(), n_vessel_CPS = sum(CPS.permit)) %>% mutate(percentage = (n_vessel_CPS / n_vessel))
  # 
  # colnames(CPS.permits_byCluster) = c("Cluster", "Number of Vessels", "Vessels with CPS permit", "Percentage")
  # # gs4_create("CPS.permits_by_cluster", sheets = CPS.permits_byCluster)
  # 
  # Clusters <- read.csv(file = here::here("Clustering", "PAM_Vessel_Groups.csv"))
  # CPS.LE.vessels.with.cluster <- read.csv(file = "G:\\My Drive\\Data\\Vessels\\CPS_LE_Permits.csv") %>% 
  #   dplyr::rename(VESSEL_NUM = Coast.Guard.Number.Vessel.ID) %>% unique() %>% 
  #   merge(Clusters, by = c('VESSEL_NUM'), all.x = TRUE, all.y = FALSE)  
  #   # gs4_create("CPS.LE.vessels.with.cluster", sheets = CPS.LE.vessels.with.cluster)
  #   
  # rm(CPS.LE.vessels.with.cluster, CPS.permits_byCluster, CPS.Vessels)
  

#----------------------------------
## Table S6. Product use
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
## Table X. Catch composition over time (stack graph, for each cluster, need to select number of top species)
mat = matrix(ncol = 0, nrow = 0)
cluster.species.all <- as.data.frame(mat)

### GET 10 MORE IMPORTANT SPECIES, THE REST IN OTHERS


for(i in 2005:2020)  {
  for(m in 1:7) {
      PacFIN.month2 <- PacFIN.month %>% dplyr::filter(group_all == m) %>%
      dplyr::filter(LANDING_YEAR == i) %>% dplyr::filter(AFI_EXVESSEL_REVENUE.sum > 0)

  all_species <- PacFIN.month2  %>% dplyr::select(PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME) %>% unique() %>% mutate(merge=1)
  all_vessels <- PacFIN.month2  %>% dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge=1)
  expand <- merge(all_species, all_vessels, by = c('merge'), all.x = TRUE, all.y = TRUE)
  
  options(scipen=999)
  cluster.species <- PacFIN.month2 %>% 
    group_by(PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, VESSEL_NUM) %>% 
    summarise(revenue = sum(AFI_EXVESSEL_REVENUE.sum, na.rm = TRUE)) %>%
    group_by(VESSEL_NUM) %>% 
    mutate(Percentage = revenue / sum(revenue, na.rm = TRUE)) %>% ungroup() %>% drop_na()
  
  cluster.species <- merge(expand, cluster.species, by = c('VESSEL_NUM', 'PACFIN_SPECIES_CODE', 'PACFIN_SPECIES_COMMON_NAME'), all.x = TRUE) %>%
    mutate(Percentage = ifelse(is.na(Percentage),0,Percentage)) %>% 
    group_by(PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME) %>% summarise(Percentage = mean(Percentage, na.rm = TRUE)) %>% drop_na()
  cluster.species$LANDING_YEAR <- i
  cluster.species$group_all <- m
  
  cluster.species.all <- rbind(cluster.species.all, cluster.species)
  rm(cluster.species)
  
  }
}

#-------------------------
## Plot 
cluster = 1
n_species = 5

df <- cluster.species.all %>% filter(Percentage > 0) %>% filter(group_all == cluster)
df2 <- df %>% group_by(group_all, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME) %>% summarize(Percentage = mean(Percentage))
df2 <- df2[order(df2$group_all, -df2$Percentage),]
highest <- df2 %>%
  group_by(group_all) %>% filter(row_number()==1:n_species) %>% ungroup() %>%
  dplyr::select('PACFIN_SPECIES_CODE') %>% unique()
df3 <- df %>% mutate(
  PACFIN_SPECIES_COMMON_NAME = 
                        ifelse(PACFIN_SPECIES_CODE == "PSDN", PACFIN_SPECIES_COMMON_NAME,
                        ifelse(PACFIN_SPECIES_CODE == "CMCK", "MACKEREL (JACK + CHUB)",
                        ifelse(PACFIN_SPECIES_CODE == "JMCK", "MACKEREL (JACK + CHUB)",
                        ifelse(PACFIN_SPECIES_CODE == "UMCK", "MACKEREL (JACK + CHUB)",
                        ifelse(PACFIN_SPECIES_CODE == "MSQD", PACFIN_SPECIES_COMMON_NAME,
                        ifelse(PACFIN_SPECIES_CODE == "NANC", PACFIN_SPECIES_COMMON_NAME, 
                        ifelse(PACFIN_SPECIES_CODE %chin% highest$PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, "OTHER-NON-CPS")))))))) %>%
  group_by(PACFIN_SPECIES_COMMON_NAME, group_all, LANDING_YEAR) %>% summarize(Percentage = sum(Percentage))

ggplot(df3, aes(x=LANDING_YEAR, y=Percentage, fill = PACFIN_SPECIES_COMMON_NAME)) +
  geom_bar(stat="identity", color = "black") + 
  guides(fill = guide_legend(title = "Species")) + 
  xlab("Year") 


# -----------------------------------------------------------------------------------------------
## Table 5 (expanded). Catch composition (all species)
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
## Table 5 and Table S4. Catch composition 
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
## Table S5: Gear
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
## Table S3. Port area

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
## Table X. Port area (percentage of revenue that come from each cluster in a port area) -- FOR ATLANTIS

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

