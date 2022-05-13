########################
### Clustering code ###
########################

###Load packages and set working directory
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(forcats)

rm(list=ls())
gc()
setwd("C:/GitHub/EconAnalysis/Clustering")


#######################
### Data processing ###
#######################

period = "2005-2014"
min.year = 2005 
max.year = 2014

# ----------------------------------------
### Load in the data
library(data.table)
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)

###Subset the data to remove columns not relevant to this analysis. This will speed things up.
Tickets <- select(Tickets, c(AGENCY_CODE, FTID, LANDING_YEAR, LANDING_MONTH, PORT_NAME, PACFIN_PORT_CODE, VESSEL_NUM, 
                             VESSEL_NAME, VESSEL_LENGTH, VESSEL_WEIGHT, LANDED_WEIGHT_LBS, AFI_EXVESSEL_REVENUE, 
                             PACFIN_GEAR_CODE, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, VESSEL_OWNER_NAME, 
                             VESSEL_OWNER_ADDRESS_STATE, VESSEL_OWNER_ADDRESS_STREET, REMOVAL_TYPE_CODE))

####Select the time period you want to cluster over
Tickets<-Tickets[which(Tickets$LANDING_YEAR>=min.year & Tickets$LANDING_YEAR<=max.year),]

#### Use only tickets that the removal type is commercial ####
Tickets <- Tickets %>% filter(REMOVAL_TYPE_CODE == "C" | REMOVAL_TYPE_CODE == "D" | REMOVAL_TYPE_CODE == "E") 


####Find the dominant species by value of each fishing trip, presumably this is the target species. Using the logic of metiers,
###all landings of each fishing trip should be tagged with a single identifier.
Boats<-dcast(Tickets, FTID ~ PACFIN_SPECIES_COMMON_NAME, fun.aggregate=sum, value.var="AFI_EXVESSEL_REVENUE", fill=0)
row.names(Boats) <- Boats$FTID
FTID<-Boats$FTID
Boats<-Boats[,-(1)]
X<-as.data.frame(colnames(Boats)[apply(Boats,1,which.max)])
colnames(X)<-"Dominant"
Trip_Dominant<-as.data.frame(cbind(FTID,X))
Tickets<-merge(Tickets, Trip_Dominant, by='FTID')

rm(Trip_Dominant, X, Boats)

#----------------------------------------
## Merge cluster data

PAM_Vessel_Groups <- fread("PAM_Vessel_Groups.csv")
Tickets <- merge(Tickets, PAM_Vessel_Groups, by = "VESSEL_NUM")

#-----------------------------------------
## Calculate catch composition with and without incidental catch

library("googlesheets4")
gs4_auth(
  email = "fequezad@ucsc.edu",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL
)

Tickets_dominant <- Tickets %>% filter(PACFIN_SPECIES_COMMON_NAME == Dominant)

Tickets<- within(Tickets, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "CMCK"] <- "OMCK")
Tickets<- within(Tickets, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "JMCK"] <- "OMCK")
Tickets<- within(Tickets, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "UMCK"] <- "OMCK")
Tickets <- Tickets %>% mutate(
  PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "OMCK",PACFIN_SPECIES_CODE,
                               ifelse(PACFIN_SPECIES_CODE == "PSDN",PACFIN_SPECIES_CODE,
                                      ifelse(PACFIN_SPECIES_CODE == "MSQD", PACFIN_SPECIES_CODE,
                                             ifelse(PACFIN_SPECIES_CODE == "NANC", PACFIN_SPECIES_CODE,
                                                    ifelse(PACFIN_SPECIES_CODE == "ALBC", PACFIN_SPECIES_CODE,
                                                           ifelse(PACFIN_SPECIES_CODE == "DCRB", PACFIN_SPECIES_CODE,
                                                                  ifelse(PACFIN_SPECIES_CODE == "LOBS", PACFIN_SPECIES_CODE, "OTHER"))))))))

Tickets_dominant <- within(Tickets_dominant, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "CMCK"] <- "OMCK")
Tickets_dominant <- within(Tickets_dominant, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "JMCK"] <- "OMCK")
Tickets_dominant <- within(Tickets_dominant, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "UMCK"] <- "OMCK")
Tickets_dominant <- Tickets_dominant %>% mutate(
  PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "OMCK",PACFIN_SPECIES_CODE,
                               ifelse(PACFIN_SPECIES_CODE == "PSDN",PACFIN_SPECIES_CODE,
                                      ifelse(PACFIN_SPECIES_CODE == "MSQD", PACFIN_SPECIES_CODE,
                                             ifelse(PACFIN_SPECIES_CODE == "NANC", PACFIN_SPECIES_CODE,
                                                    ifelse(PACFIN_SPECIES_CODE == "ALBC", PACFIN_SPECIES_CODE,
                                                           ifelse(PACFIN_SPECIES_CODE == "DCRB", PACFIN_SPECIES_CODE,
                                                                  ifelse(PACFIN_SPECIES_CODE == "LOBS", PACFIN_SPECIES_CODE, "OTHER"))))))))


all_species <- Tickets  %>% 
  dplyr::select(PACFIN_SPECIES_CODE) %>% unique() %>% mutate(merge="1")
all_species_D <- Tickets_dominant  %>% 
  dplyr::select(PACFIN_SPECIES_CODE) %>% unique() %>% mutate(merge="1")

all_vessels   <- Tickets %>% dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge="1")
all_vessels_D <- Tickets_dominant %>% dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge="1")

expand <- base::merge(all_species, all_vessels, by = c('merge'), all.x = TRUE, all.y = TRUE, allow.cartesian=TRUE)
expand_D <- base::merge(all_species_D, all_vessels_D, by = c('merge'), all.x = TRUE, all.y = TRUE, allow.cartesian=TRUE)

rm(all_species, all_vessels)
rm(all_species_D, all_vessels_D)

options(scipen=999)
cluster.species <- Tickets %>%
  group_by(group_all, PACFIN_SPECIES_CODE, VESSEL_NUM) %>% 
  summarise(revenue = sum(AFI_EXVESSEL_REVENUE)) %>%
  group_by(VESSEL_NUM) %>% mutate(Percentage = revenue / sum(revenue))

cluster.species <- merge(expand, cluster.species, by = c('VESSEL_NUM', 'PACFIN_SPECIES_CODE'), all.x = TRUE) %>%
  mutate(Percentage = ifelse(is.na(Percentage),0,Percentage)) %>% group_by(VESSEL_NUM) %>%
  mutate(group_all = ifelse(is.na(group_all),mean(group_all, na.rm = TRUE),group_all)) %>% 
  group_by(group_all, PACFIN_SPECIES_CODE) %>% summarise(Percentage = mean(Percentage)) %>% 
  unique() %>% filter(group_all != is.na(group_all))

table <- as.data.frame(xtabs(Percentage ~  PACFIN_SPECIES_CODE + group_all, cluster.species))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "CLuster 7", "Cluster 8")
gs4_create("Table2_all", sheets = table)

rm(table, cluster.species)


### Only dominant ###
options(scipen=999)
cluster.species_D <- Tickets_dominant %>%   
  group_by(group_all, PACFIN_SPECIES_CODE, VESSEL_NUM) %>% 
  summarise(revenue = sum(AFI_EXVESSEL_REVENUE)) %>%
  group_by(VESSEL_NUM) %>% mutate(Percentage = revenue / sum(revenue))

cluster.species_D <- merge(expand_D, cluster.species_D, by = c('VESSEL_NUM', 'PACFIN_SPECIES_CODE'), all.x = TRUE) %>%
  mutate(Percentage = ifelse(is.na(Percentage),0,Percentage)) %>% group_by(VESSEL_NUM) %>%
  mutate(group_all = ifelse(is.na(group_all),mean(group_all, na.rm = TRUE),group_all)) %>% 
  group_by(group_all, PACFIN_SPECIES_CODE) %>% summarise(Percentage = mean(Percentage)) %>% 
  unique() %>% filter(group_all != is.na(group_all))


table <- as.data.frame(xtabs(Percentage ~  PACFIN_SPECIES_CODE + group_all, cluster.species_D))
table <- table %>%
  spread(key = group_all, value = Freq)

# table = table[,-1]
colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "CLuster 7", "Cluster 8")
gs4_create("Table2_only_dominant", sheets = table)

rm(table, cluster.species_D, cluster.species.highest_D)


