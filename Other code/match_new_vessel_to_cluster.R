####################################
### Match new vessel to clusters ###
####################################

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


### Read RAW data ###
RAW_cluster_inputs <- read.csv(here::here("Clustering", "RAW_cluster_inputs.csv"))
RAW_cluster_inputs_new_vessels <- read.csv(here::here("Clustering", "RAW_cluster_inputs_new_vessels.csv")) %>%
  mutate(group_all = 9) 
RAW_cluster_inputs_new_vessels <- RAW_cluster_inputs_new_vessels[c(-2)]

# Merge clustered vessels and new vessels #
RAW <- rbind(RAW_cluster_inputs, RAW_cluster_inputs_new_vessels)
Vessel_IDs <- RAW[,1]
Vessels<-as.data.frame(Vessel_IDs)
RAW<-RAW[c(-1)]
rownames(RAW)<-Vessel_IDs
Cluster_IDs <- RAW[,6]
Cluster<-as.data.frame(Cluster_IDs)
RAW<-RAW[c(-6)]

### Standardize RAW data ###
RAW_Scaled<-as.data.frame(RAW %>% scale())

### Calculate distance between vessels 
Distance_matrix<-dist(RAW_Scaled, method='euclidean')
df <- melt(as.matrix(Distance_matrix), varnames = c("Vessel_IDs", "col"))


Vessel_cluster <- cbind(Vessels, Cluster) 
distances <- merge(df, Vessel_cluster, by = c('Vessel_IDs'), all.x = TRUE, all.y = FALSE) %>%
  rename(Vessel_IDs_NEW = Vessel_IDs) %>%
  rename(Vessel_IDs = col) %>%
  merge(Vessel_cluster, by = c('Vessel_IDs'), all.x = TRUE, all.y = FALSE) %>%
  dplyr::filter(Cluster_IDs.x == 9) %>% dplyr::filter(Cluster_IDs.y != 9) %>%
  group_by(Vessel_IDs_NEW) %>% 
  slice(which.min(value)) 

table <- distances %>% group_by(Cluster_IDs.y) %>% summarize(n_vessels = n())
# gs4_create("New_vessels_cluster_match", sheets = table)

new_entrants_ID <- distances %>% dplyr::select(Vessel_IDs_NEW, Cluster_IDs.y) %>% unique() %>%
  rename(VESSEL_NUM = Vessel_IDs_NEW) %>%
  rename(group_all_new = Cluster_IDs.y)

saveRDS(new_entrants_ID, here::here("Clustering", "new_entrants_ID.rds"))




