#########################
#### Anchovy catches #### 
#########################

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
Tickets3 <- fread("C:/Data/PacFIN data/FutureSeasIII_1990_1999.csv")
Tickets4 <- fread("C:/Data/PacFIN data/FutureSeasIII_1981_1989.csv")
Tickets<-rbind(Tickets1, Tickets2, Tickets3, Tickets4)
rm(Tickets1, Tickets2, Tickets3, Tickets4)


## Subset the data to get remove columns not relevant to this analysis. This will speed things up.
Tickets <- dplyr::select(Tickets, c(PACFIN_SPECIES_CODE, LANDED_WEIGHT_MTONS, LANDING_YEAR, LANDING_MONTH))
options(scipen=999)  
Tickets_anchovy <- Tickets %>% dplyr::filter(PACFIN_SPECIES_CODE == 'NANC') %>% group_by(LANDING_YEAR, LANDING_MONTH) %>%
    summarize(NANC_Landings = sum(LANDED_WEIGHT_MTONS))

write.csv(Tickets_anchovy,"C:\\Data\\Anchovy_landings.csv", row.names = FALSE)

  
