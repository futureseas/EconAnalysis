#########################
#### CPS catches #### 
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
# rm(Tickets1, Tickets2, Tickets3, Tickets4)

# port_area <- read.csv(file = here::here("Data", "Ports", "ports_area_and_name_codes.csv"))
# Tickets <- Tickets %>% merge(port_area, by = c("PACFIN_PORT_CODE"), all.x = TRUE)


## Subset the data to get remove columns not relevant to this analysis. This will speed things up.
Tickets <- dplyr::select(Tickets, c(PACFIN_SPECIES_CODE, LANDED_WEIGHT_MTONS, 
                                    LANDING_YEAR, LANDING_MONTH, LANDING_DAY, 
                                    AGENCY_CODE, PACFIN_PORT_CODE))




options(scipen=999)  
Tickets_CPS <- Tickets %>% dplyr::filter(PACFIN_SPECIES_CODE == 'PSDN'
                                         # | # PACFIN_SPECIES_CODE == 'NANC' | 
                                         # PACFIN_SPECIES_CODE == 'MSQD' |
                                         # PACFIN_SPECIES_CODE == 'JMCK' |
                                         # PACFIN_SPECIES_CODE == 'CMCK' |
                                         # PACFIN_SPECIES_CODE == 'UMCK'
                                         ) %>% 
  dplyr::filter(PACFIN_PORT_CODE == "SP" | PACFIN_PORT_CODE == "MOS" |
                PACFIN_PORT_CODE == "AST") %>%
  group_by(LANDING_YEAR, LANDING_MONTH, PACFIN_PORT_CODE, PACFIN_SPECIES_CODE) %>%
    summarize(Total.Landings.MTONS = sum(LANDED_WEIGHT_MTONS))

library(tidyr)
Tickets_CPS.spread <- spread(Tickets_CPS, key = PACFIN_PORT_CODE, value = Total.Landings.MTONS) %>%
  rename(MossLanding.MTONS = MOS) %>%
  rename(SanPedro.MTONS = SP) %>%
  rename(Astoria.MTONS = AST)

write.csv(Tickets_CPS.spread,"C:\\Data\\monthly_PSDN_landings_by_port_area.csv", row.names = FALSE)

  
