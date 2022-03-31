#######################
#### Atlantis Econ ####
#######################

rm(list = ls(all.names = TRUE)) 
gc()

library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

PacFIN.month <- read.csv(file ="C:\\Data\\PacFIN data\\PacFIN_month.csv")
# PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "CMCK"] <- "OMCK")
# PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "JMCK"] <- "OMCK")
# PacFIN.month<- within(PacFIN.month, PACFIN_SPECIES_CODE[PACFIN_SPECIES_CODE == "UMCK"] <- "OMCK")
# PacFIN.month <- PacFIN.month %>% mutate(
#   PACFIN_SPECIES_CODE = ifelse(PACFIN_SPECIES_CODE == "OMCK",PACFIN_SPECIES_CODE, 
#                                ifelse(PACFIN_SPECIES_CODE == "PSDN",PACFIN_SPECIES_CODE, 
#                                       ifelse(PACFIN_SPECIES_CODE == "MSQD", PACFIN_SPECIES_CODE, 
#                                              ifelse(PACFIN_SPECIES_CODE == "NANC", PACFIN_SPECIES_CODE, "OTHER")))))
# dplyr::filter(PACFIN_SPECIES_CODE!='OTHER') %>%

options(scipen=999)
PacFIN.month.Atlantis <- PacFIN.month %>% 
  dplyr::filter(LANDING_YEAR >= 2011 & LANDING_YEAR <= 2015) %>%  
  dplyr::filter(PORT_AREA_CODE == "SDA" | PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "SBA" | 
                  PORT_AREA_CODE == "MNA" | PORT_AREA_CODE == "SFA" | PORT_AREA_CODE == "CLO" | 
                  PORT_AREA_CODE == "CLW" | PORT_AREA_CODE == "CWA" | PORT_AREA_CODE == "SPS" | 
                  PORT_AREA_CODE == "NPS") %>% 
    group_by(PORT_AREA_CODE, LANDING_YEAR, PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME) %>%
    summarise(LANDINGS_MTONS = sum(LANDED_WEIGHT_MTONS.sum))

write.csv(PacFIN.month.Atlantis, 
          here::here("AtlantisEcon", "PacFIN_year_atlantis_all_species.csv"), 
          row.names = FALSE)


#-----------------------------------
## Table by species of total land by cluster in a port area

options(scipen=999)
cluster.port <- PacFIN.month %>% filter(LANDING_YEAR >= 2011) %>% 
  filter(LANDING_YEAR <= 2015) %>%  
  dplyr::filter(PORT_AREA_CODE == "SDA" | PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "SBA" | 
                  PORT_AREA_CODE == "MNA" | PORT_AREA_CODE == "SFA" | PORT_AREA_CODE == "CLO" | 
                  PORT_AREA_CODE == "CLW" | PORT_AREA_CODE == "CWA" | PORT_AREA_CODE == "SPS" | 
                  PORT_AREA_CODE == "NPS") %>%
  group_by(group_all, PORT_AREA_CODE, PACFIN_SPECIES_CODE) %>% 
  summarise(landing = sum(LANDED_WEIGHT_MTONS.sum))


### PSDN ###
cluster.port.PSDN <- cluster.port %>% dplyr::filter(PACFIN_SPECIES_CODE == "PSDN") %>%
  group_by(PORT_AREA_CODE) %>% mutate(Percentage = landing / sum(landing))

table <- as.data.frame(xtabs(Percentage ~  PORT_AREA_CODE + group_all, cluster.port.PSDN))
library('scales')
table$Freq <- percent(table$Freq, accuracy = 0.1)
table <- table %>%
  spread(key = group_all, value = Freq)

colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", 
                    "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")

write.csv(table, 
          here::here("AtlantisEcon", "tablePSDN_port_cluster.csv"), 
          row.names = FALSE)
rm(table)

### NANC ###
cluster.port.NANC <- cluster.port %>% dplyr::filter(PACFIN_SPECIES_CODE == "NANC") %>%
  group_by(PORT_AREA_CODE) %>% mutate(Percentage = landing / sum(landing))

table <- as.data.frame(xtabs(Percentage ~  PORT_AREA_CODE + group_all, cluster.port.NANC))
library('scales')
table$Freq <- percent(table$Freq, accuracy = 0.1)
table <- table %>%
  spread(key = group_all, value = Freq)

colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", 
                    "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")

write.csv(table, 
          here::here("AtlantisEcon", "tableNANC_port_cluster.csv"), 
          row.names = FALSE)
rm(table)

### MSQD ###
cluster.port.MSQD <- cluster.port %>% dplyr::filter(PACFIN_SPECIES_CODE == "MSQD") %>%
  group_by(PORT_AREA_CODE) %>% mutate(Percentage = landing / sum(landing))

table <- as.data.frame(xtabs(Percentage ~  PORT_AREA_CODE + group_all, cluster.port.MSQD))
library('scales')
table$Freq <- percent(table$Freq, accuracy = 0.1)
table <- table %>%
  spread(key = group_all, value = Freq)

colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", 
                    "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")

write.csv(table, 
          here::here("AtlantisEcon", "tableMSQD_port_cluster.csv"), 
          row.names = FALSE)
rm(table)

### OMCK ###
cluster.port.OMCK <- cluster.port %>% dplyr::filter(PACFIN_SPECIES_CODE == "OMCK") %>%
  group_by(PORT_AREA_CODE) %>% mutate(Percentage = landing / sum(landing))

table <- as.data.frame(xtabs(Percentage ~  PORT_AREA_CODE + group_all, cluster.port.OMCK))
library('scales')
table$Freq <- percent(table$Freq, accuracy = 0.1)
table <- table %>%
  spread(key = group_all, value = Freq)

colnames(table) = c("Port", "Cluster 1", "Cluster 2", "Cluster 3", 
                    "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7")

write.csv(table, 
          here::here("AtlantisEcon", "tableOMCK_port_cluster.csv"), 
          row.names = FALSE)
rm(table)

