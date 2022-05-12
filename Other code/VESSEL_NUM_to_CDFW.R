################################
#### PacFIN data processing #### 
################################

library(data.table)
library(dplyr)
library(distances)
library(forcats)
library(cluster)
library(ggplot2)

#-----------------------
### Read PacFIN database

rm(list=ls())
gc()

## Read PacFIN data by vessels for different decades #
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)

Tickets <- Tickets %>% dplyr::select('VESSEL_NUM', 'CDFW_PLATE_NUM') %>% unique()
Tickets<-as.data.frame(Tickets[which(!Tickets$VESSEL_NUM==""),])
Tickets<-as.data.frame(Tickets[which(!Tickets$VESSEL_NUM=="UNKNOWN"),])
Tickets<-as.data.frame(Tickets[which(!Tickets$VESSEL_NUM=="MISSING"),])
Tickets<-as.data.frame(Tickets[which(!Tickets$CDFW_PLATE_NUM==""),])
Tickets<-as.data.frame(Tickets[which(!Tickets$CDFW_PLATE_NUM=="UNKNOWN"),])
Tickets<-as.data.frame(Tickets[which(!Tickets$CDFW_PLATE_NUM=="MISSING"),])
Tickets<-as.data.frame(Tickets[which(!Tickets$CDFW_PLATE_NUM=="NONE"),])

write.csv(Tickets,"C:\\Data\\VESSEL_NUM_to_CDFW_PLATE.csv", row.names = FALSE)


### There are VESSEL_NUM associated with different CDFW plates
VESSEL_NUM_count <- Tickets %>%
  group_by(VESSEL_NUM) %>% summarize(n_repeat = n())

### Bute there is also CDFW plates associated with different VESSEL_NUM, but is less likely
CDFW_PLATE_NUM_count <- Tickets %>%
  group_by(CDFW_PLATE_NUM) %>% summarize(n_repeat = n())

