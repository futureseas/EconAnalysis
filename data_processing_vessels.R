########################################
##                                    ##
##### Public PacFIN data processing #### 
##                                    ##
########################################

library(bookdown)
library(doBy)
library(dplyr)
library(fastDummies)
library(ggplot2)
library(here)
library(hrbrthemes)
library(lmtest)
library(lubridate)
library(magrittr)
library(plm)
library(reshape)
library(reshape2)
library(rstan)
library(scales)
library(sjlabelled)
library(summarytools)
library(texreg)
library(tidyr)
library(tidyverse)
library(tinytex)
library(viridis)
library(xtable)
library(xlsx)
library(zoo)


### Functions ###
meanfun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...)) #, v=var(x, na.rm=TRUE, ...), l=length(x))
}

sumfun <- function(x, ...){
  c(sum=sum(x, na.rm=TRUE, ...)) #, v=var(x, na.rm=TRUE, ...), l=length(x))
}

sum_mean_fun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...), sum=sum(x, na.rm=TRUE, ...)) #, l=length(x))
}


###################
### PacFIN data ###
###################

# Read PacFIN data by vessels for different decades #
PacFIN_2010_2020 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
  PacFIN_2000_2009 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
  PacFIN_1990_1999 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_1990_1999.csv")
  PacFIN_1981_1989 <- read.csv(file = "C:/Data/PacFIN data/FutureSeasIII_1981_1989.csv")

# Merge different decades of data #
PacFIN <- rbind.data.frame(PacFIN_1981_1989, PacFIN_1990_1999, PacFIN_2000_2009, PacFIN_2010_2020)
  rm(PacFIN_1981_1989, PacFIN_1990_1999, PacFIN_2000_2009, PacFIN_2010_2020)
  gc()

# Include price per kilogram
  PacFIN$AFI_PRICE_PER_KG <- PacFIN$AFI_PRICE_PER_POUND * 2.20462


# Create monthly data
  PacFIN.month <- summaryBy(LANDED_WEIGHT_MTONS + AFI_PRICE_PER_KG + AFI_EXVESSEL_REVENUE
                            + VESSEL_LENGTH + VESSEL_WEIGHT + VESSEL_HORSEPOWER + NUM_OF_DAYS_FISHED
                          ~ PACFIN_SPECIES_CODE + PACFIN_GEAR_CODE + PORT_NAME + PACFIN_PORT_CODE
                          + LANDING_YEAR + LANDING_MONTH + VESSEL_NUM + AGENCY_CODE,
                          FUN=sum_mean_fun, data=PacFIN)

# PacFIN.month <- read.csv("C:\\Data\\PacFIN data\\PacFIN_month.csv")



#######################
## Merge SDM dataset ##
#######################

# Merge data with SDM Pacific Sardine #
SDM_port_PSDN <- read.csv(file = here::here("Data", "SDM", "PSDN_SDM_port_month.csv"))
  PacFIN.month <- merge(PacFIN.month, SDM_port_PSDN, 
                        by = c("PORT_NAME", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

# Merge data with SDM Market Squid #
SDM_port_MSQD <- read.csv(file = here::here("Data", "SDM", "MSQD_SDM_port_month.csv"))
  PacFIN.month <- merge(PacFIN.month, SDM_port_MSQD, 
                        by = c("PORT_NAME", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

# Merge data with SDM Northern Anchovy #
SDM_port_NANC <- read.csv(file = here::here("Data", "SDM", "NANC_SDM_port_month.csv"))
  PacFIN.month <- merge(PacFIN.month, SDM_port_NANC, 
                        by = c("PORT_NAME", "LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE)

  PacFIN.month <- PacFIN.month %>%
    dplyr::rename(PSDN_SDM_60 = SDM_60) %>%
    dplyr::rename(MSQD_SDM_90 = SDM_90) %>%
    dplyr::rename(NANC_SDM_20 = SDM_20)

  
  

#######################
## Merge ACL dataset ##
#######################
  
    
### Read ACL data and merge with PacFIN ###
   ## Note: Squid is straightforward as it is a constant cap, the sardine one depends on the estimated biomass.
   ## Also when the directed fishery is closed, some incidental catch is still allowed, hence the two columns. 
   ## Also note that the fishing season changed in 2014 from jan-Dec to Jul-Jun so they had to add an interim HC Jan-Jun 2014.
   
# Read the data #
ACL_PSDN <- read.xlsx(file = here::here("Data", "ACL_data", "CPS_quotas_month.xlsx"), 
                         sheetName = "sardine.month")
  
ACL_MSQD <- read.xlsx(file = here::here("Data", "ACL_data", "CPS_quotas_month.xlsx"), 
                        sheetName = "squid.month")


# Rename variables #
 ACL_PSDN <- ACL_PSDN %>%
    dplyr::rename(ACL_PSDN = HG..directed.fishery..mt.)  %>%
    dplyr::rename(ACT_PSDN = ACT..HG.plus.incidental..tribal.and.live.bait..mt.) %>%
    dplyr::rename(CA_LE_PSDN = CA.Limited.entry....of.vessels..federal.CPS.permit.restricted.to.39ON.) %>%
    dplyr::rename(OR_LE_PSDN = OR.limited.entry....of.vessels..state.) %>%
    dplyr::rename(WA_LE_PSDN = WA.limited.entry)

ACL_MSQD <- ACL_MSQD %>%
   dplyr::rename(ACL_MSQD = Catch.limit..mt.)  %>%
   dplyr::rename(CA_LE_MSQD = CA.Limited.entry..state.) 

# Obtain Year and Month variable    

ACL_PSDN$LANDING_YEAR <- as.numeric(format(ACL_PSDN$Date, format="%Y"))
  ACL_PSDN$LANDING_MONTH <- as.numeric(format(ACL_PSDN$Date, format="%m"))
  ACL_MSQD$LANDING_YEAR  <- as.numeric(format(ACL_MSQD$Date, format="%Y"))
  ACL_MSQD$LANDING_MONTH <- as.numeric(format(ACL_MSQD$Date, format="%m"))
  ACL_PSDN <- subset(ACL_PSDN, select = -Date )
  ACL_MSQD <- subset(ACL_MSQD, select = -Date )



# Merge data with ACL #
PacFIN.month.ACL <- merge(PacFIN.month,ACL_MSQD,by=c("LANDING_YEAR", "LANDING_MONTH"),all.x = TRUE)
  PacFIN.month.ACL <- merge(PacFIN.month.ACL,ACL_PSDN,by=c("LANDING_YEAR", "LANDING_MONTH"),all.x = TRUE)

  

####################
### Save DATASET ###
####################
  
sapply(PacFIN.month.ACL, class)
write.csv(PacFIN.month.ACL,"C:\\Data\\PacFIN data\\PacFIN_month.csv", row.names = FALSE)
