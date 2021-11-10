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
                          ~ PACFIN_SPECIES_CODE + PACFIN_GEAR_CODE + PACFIN_PORT_CODE 
                          + LANDING_YEAR + LANDING_MONTH + VESSEL_NUM + AGENCY_CODE, 
                          FUN=sum_mean_fun, data=PacFIN) 

# All species monthly data and CPS species daily data.


####################
### Save DATASET ###
####################

sapply(PacFIN.month, class) 
write.csv(PacFIN.month,"C:\\Data\\PacFIN data\\PacFIN_month.csv", row.names = FALSE)




# ####################
# ## Merge datasets ##
# ####################
# 
# # Merge data with SDM Pacific Sardine #
# SDM_port_PSDN <- read.csv(file = here("Data", "SDM_port_month_PSDN.csv"))
#     names(SDM_port_PSDN)[names(SDM_port_PSDN) == "port"] <- "Port"
#     names(SDM_port_PSDN)[names(SDM_port_PSDN) == "year"] <- "Year"
# 
#   SDM_port_PSDN <- SDM_port_PSDN %>%
#     group_by(Port, Year) %>%
#     summarize(PSDN_SDM_60 = mean(SDM_60, na.rm = T)) %>%
#     ungroup(.) 
# 
#   PacFIN_data_merged <- merge(PacFIN_data_merged,SDM_port_PSDN,by=c("Port","Year"),all.x = TRUE)
# 
# 
# # Merge data with SDM Market Squid #
# SDM_port_MSQD <- read.csv(file = here("Data", "SDM_port_month_MSQD.csv"))
#     names(SDM_port_MSQD)[names(SDM_port_MSQD) == "port"] <- "Port"
#     names(SDM_port_MSQD)[names(SDM_port_MSQD) == "year"] <- "Year"
# 
#   SDM_port_MSQD <- SDM_port_MSQD %>%
#     group_by(Port, Year) %>%
#     summarize(MSQD_SDM_60 = mean(SDM_60, na.rm = T), 
#               MSQD_SDM_90 = mean(SDM_90, na.rm = T), 
#               MSQD_SDM_120 = mean(SDM_120, na.rm = T))  %>%
#     ungroup(.) 
# 
#   PacFIN_data_merged <- merge(PacFIN_data_merged,SDM_port_MSQD,by=c("Port","Year"),all.x = TRUE)
# 
# 
# # Merge data with SDM Northern Anchovy #
# SDM_port_NANC <- read.csv(file = here("Data", "SDM_port_month_NANC_20.csv"))
#     names(SDM_port_NANC)[names(SDM_port_NANC) == "port"] <- "Port"
#     names(SDM_port_NANC)[names(SDM_port_NANC) == "year"] <- "Year"
# 
#   SDM_port_NANC_year <- SDM_port_NANC %>%
#     group_by(Port, Year) %>%
#     summarize(NANC_SDM_20 = mean(SDM_20, na.rm = T)) %>%
#     ungroup(.) 
#   
#   SDM_port_NANC_sep_dec <- SDM_port_NANC %>%
#     filter(month >= 9) %>% 
#     group_by(Port, Year) %>%
#     summarize(NANC_SDM_20_sep_dec = mean(SDM_20, na.rm = T)) %>%
#     ungroup(.) 
# 
#   PacFIN_data_merged <- merge(PacFIN_data_merged,SDM_port_NANC_year,by=c("Port","Year"),all.x = TRUE)
#   PacFIN_data_merged <- merge(PacFIN_data_merged,SDM_port_NANC_sep_dec,by=c("Port","Year"),all.x = TRUE)
#   
# ### Read ACL data and merge with PacFIN ###
#   ## Note: Squid is straightforward as it is a constant cap, the sardine one depends on the estimated biomass.
#   ## Also when the directed fishery is closed, some incidental catch is still allowed, hence the two columns. 
#   ## Also note that the fishing season changed in 2014 from jan-Dec to Jul-Jun so they had to add an interim HC Jan-Jun 2014.
#   
#   # Read the data #
#   ACL_data <- read.xlsx(file = here("Data", "ACL_data", "CPS_quotas.xlsx"), 1, col.names = TRUE, startRow = 2)
#   ACL_data <- ACL_data[-c(42:63),] 
#   ACL_psdn <- ACL_data[c(1:7)]
#   ACL_msqd <- ACL_data[c(1,8:10)]
#   
#   # Rename variables #
#   ACL_psdn <- ACL_psdn %>%
#     dplyr::rename(ACL_PSDN = HG..directed.fishery..mt.)  %>%
#     dplyr::rename(ACT_PSDN = ACT..HG.plus.incidental..tribal.and.live.bait..mt.) %>%
#     dplyr::rename(CA_LE_PSDN = CA.Limited.entry....of.vessels..federal.CPS.permit.restricted.to.39ON.) %>%
#     dplyr::rename(OR_LE_PSDN = OR.limited.entry....of.vessels..state.) %>%
#     dplyr::rename(WA_LE_PSDN = WA.limited.entry)
#   ACL_psdn$ACL_PSDN <- as.numeric(ACL_psdn$ACL_PSDN)
#   ACL_psdn$ACT_PSDN <- as.numeric(ACL_psdn$ACT_PSDN)
#   ACL_psdn$CA_LE_PSDN <- as.numeric(ACL_psdn$CA_LE_PSDN)
#   
#   ACL_msqd <- ACL_msqd %>%
#     dplyr::rename(ACL_MSQD = Catch.limit..mt.)  %>%
#     dplyr::rename(CA_LE_MSQD = CA.Limited.entry..state.) 
#   
#   # Obtain annual data of ACL #
#   ACL_psdn_annual <- summaryBy(ACL_PSDN + ACT_PSDN + CA_LE_PSDN + OR_LE_PSDN + WA_LE_PSDN ~ Year, FUN=sumfun, data=ACL_psdn)
#   ACL_msqd_annual <- summaryBy(ACL_MSQD + CA_LE_MSQD ~ Year, FUN=meanfun, data=ACL_msqd)
#   
# # Merge data with ACL #
# PacFIN_data_merged <- merge(PacFIN_data_merged,ACL_msqd_annual,by=c("Year"),all.x = TRUE)
# PacFIN_data_merged <- merge(PacFIN_data_merged,ACL_psdn_annual,by=c("Year"),all.x = TRUE)
# 
# PacFIN_dat_merged <- PacFIN_dat_merged %>% 
#   dplyr::rename(ACL_PSDN = ACL_PSDN.sum) %>%
#   dplyr::rename(ACT_PSDN = ACT_PSDN.sum) %>%
#   dplyr::rename(ACL_MSQD = ACL_MSQD.mean)
