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


### Read ACL data and merge with PacFIN ###
## Note: Squid is straightforward as it is a constant cap, the sardine one depends on the estimated biomass.
## Also when the directed fishery is closed, some incidental catch is still allowed, hence the two columns. 
## Also note that the fishing season changed in 2014 from jan-Dec to Jul-Jun so they had to add an interim HC Jan-Jun 2014.

# Read the data #
ACL_data <- read.xlsx(file = here("Data", "ACL_data", "CPS_quotas.xlsx"), 1, col.names = TRUE, startRow = 2)
ACL_data <- ACL_data[-c(42:63),] 
ACL_psdn <- ACL_data[c(1:7)]
ACL_msqd <- ACL_data[c(1,8:10)]

# Rename variables #
ACL_psdn <- ACL_psdn %>%
  dplyr::rename(ACL_PSDN = HG..directed.fishery..mt.)  %>%
  dplyr::rename(ACT_PSDN = ACT..HG.plus.incidental..tribal.and.live.bait..mt.) %>%
  dplyr::rename(CA_LE_PSDN = CA.Limited.entry....of.vessels..federal.CPS.permit.restricted.to.39ON.) %>%
  dplyr::rename(OR_LE_PSDN = OR.limited.entry....of.vessels..state.) %>%
  dplyr::rename(WA_LE_PSDN = WA.limited.entry)

ACL_msqd <- ACL_msqd %>%
  dplyr::rename(ACL_MSQD = Catch.limit..mt.)  %>%
  dplyr::rename(CA_LE_MSQD = CA.Limited.entry..state.) 

# Obtain annual data of ACL #
ACL_psdn$ACL_PSDN <- as.numeric(ACL_psdn$ACL_PSDN)
ACL_psdn$ACT_PSDN <- as.numeric(ACL_psdn$ACT_PSDN)
ACL_psdn$CA_LE_PSDN <- as.numeric(ACL_psdn$CA_LE_PSDN)
ACL_psdn_annual <- summaryBy(ACL_PSDN + ACT_PSDN + CA_LE_PSDN + OR_LE_PSDN + WA_LE_PSDN ~ Year, FUN=sumfun, data=ACL_psdn)
ACL_msqd_annual <- summaryBy(ACL_MSQD + CA_LE_MSQD ~ Year, FUN=meanfun, data=ACL_msqd)

  

### Read PacFIN data for California ###
PacFIN_data_C <- read.csv(file = here("Data", "ALL005-C-1980---2021.csv"))
PacFIN_data_C <- PacFIN_data_C %>%
  filter(str_detect(MANAGEMENT_GROUP_CODE, "SUBTOTAL", negate = TRUE))
PacFIN_data_C <- melt(PacFIN_data_C, id.vars = c("LANDING_YEAR", "PACFIN_SPECIES_CODE", "PACFIN_SPECIES_COMMON_NAME", "AGENCY_CODE", "COMPLEX", "MANAGEMENT_GROUP_CODE"))
PacFIN_data_C$Port <- sub("\\_.*", "", PacFIN_data_C$variable)
PacFIN_data_C$variable <- sub("([^\\_]*)\\_", "", PacFIN_data_C$variable)
PacFIN_data_C <- PacFIN_data_C %>% 
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(str_detect(Port, "TOTAL", negate = TRUE)) %>%
  mutate(dummy = ifelse(Port == "SDA" & LANDING_YEAR == 2018 & PACFIN_SPECIES_CODE == "PSDN", 1, 0)) %>%
  filter(dummy == 0) %>%
  subset(select = -c(dummy))

# Read PacFIN data for Oregon #
PacFIN_data_O <- read.csv(file = here("Data", "ALL005-O-1980---2021.csv"))
PacFIN_data_O <- PacFIN_data_O %>%
  filter(str_detect(MANAGEMENT_GROUP_CODE, "SUBTOTAL", negate = TRUE))
PacFIN_data_O <- melt(PacFIN_data_O, id.vars = c("LANDING_YEAR", "PACFIN_SPECIES_CODE", "PACFIN_SPECIES_COMMON_NAME", "AGENCY_CODE", "COMPLEX", "MANAGEMENT_GROUP_CODE"))
PacFIN_data_O$Port <- sub("\\_.*", "", PacFIN_data_O$variable)
PacFIN_data_O$variable <- sub("([^\\_]*)\\_", "", PacFIN_data_O$variable)
PacFIN_data_O <- PacFIN_data_O %>% 
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(str_detect(Port, "TOTAL", negate = TRUE))

# Read PacFIN data for Washington #
PacFIN_data_W <- read.csv(file = here("Data", "ALL005-W-1980---2021.csv"))
PacFIN_data_W <- PacFIN_data_W %>%
  filter(str_detect(MANAGEMENT_GROUP_CODE, "SUBTOTAL", negate = TRUE))
PacFIN_data_W <- melt(PacFIN_data_W, id.vars = c("LANDING_YEAR", "PACFIN_SPECIES_CODE", "PACFIN_SPECIES_COMMON_NAME", "AGENCY_CODE", "COMPLEX", "MANAGEMENT_GROUP_CODE"))
PacFIN_data_W$Port <- sub("\\_.*", "", PacFIN_data_W$variable)
PacFIN_data_W$variable <- sub("([^\\_]*)\\_", "", PacFIN_data_W$variable)
PacFIN_data_W <- PacFIN_data_W %>% 
  pivot_wider(names_from = variable, values_from = value) %>%
  filter(str_detect(Port, "TOTAL", negate = TRUE))

# Merge data.frames #
PacFIN_data_port <- rbind(PacFIN_data_C,PacFIN_data_O)
PacFIN_data_port <- rbind(PacFIN_data_port,PacFIN_data_W)

# Transform other variables to numeric #
PacFIN_data_port$EXVESSEL_REVENUE    <- as.numeric(PacFIN_data_port$EXVESSEL_REVENUE) 
PacFIN_data_port$LANDED_WEIGHT_MTONS <- as.numeric(PacFIN_data_port$LANDED_WEIGHT_MTONS)
PacFIN_data_port$LANDED_WEIGHT_PPP   <- as.numeric(PacFIN_data_port$LANDED_WEIGHT_PPP)        

names(PacFIN_data_port)[names(PacFIN_data_port) == "LANDING_YEAR"              ] <- "Year"
names(PacFIN_data_port)[names(PacFIN_data_port) == "PACFIN_SPECIES_CODE"       ] <- "Species_code"
names(PacFIN_data_port)[names(PacFIN_data_port) == "PACFIN_SPECIES_COMMON_NAME"] <- "Species_name"
names(PacFIN_data_port)[names(PacFIN_data_port) == "LANDED_WEIGHT_MTONS"       ] <- "Landings"
names(PacFIN_data_port)[names(PacFIN_data_port) == "EXVESSEL_REVENUE"          ] <- "Revenue"
names(PacFIN_data_port)[names(PacFIN_data_port) == "LANDED_WEIGHT_PPP"         ] <- "Price"
names(PacFIN_data_port)[names(PacFIN_data_port) == "AGENCY_CODE"               ] <- "State"
names(PacFIN_data_port)[names(PacFIN_data_port) == "MANAGEMENT_GROUP_CODE"     ] <- "Management_group"
names(PacFIN_data_port)[names(PacFIN_data_port) == "COMPLEX"                   ] <- "Complex"


####################################################
### Obtain N_vessel and N_dealers in PacFIN data ###
####################################################

# Read and clean PacFIN data #
PacFIN_data <- read.csv(file = here("Data", "CPS001-1980-2021.csv"))

# Filter PacFIN data #
PacFIN_data <- PacFIN_data %>%
  filter(str_detect(LANDING_YEAR, "TOTAL", negate = TRUE)) %>%
  filter(str_detect(PACFIN_SPECIES_CODE, "Subtotal", negate = TRUE))

# Separate outcome variables #
PacFIN_data <- melt(PacFIN_data, id.vars = c("LANDING_YEAR", "PACFIN_SPECIES_CODE", "PACFIN_SPECIES_COMMON_NAME"))

# Create quarter variable #
PacFIN_data <- PacFIN_data %>%
  separate(variable, c("Quarter","Outcome"), sep = "_QTR_") %>%
  pivot_wider(names_from = Outcome, values_from = value)

# Transform quarter variable to numeric #
PacFIN_data$LANDING_QUARTER <- ifelse(PacFIN_data$Quarter=='FIRST', 1, ifelse(PacFIN_data$Quarter=='SECOND', 2, ifelse(PacFIN_data$Quarter=='THIRD', 3, 4)))
PacFIN_data <- subset(PacFIN_data, select = -c(Quarter))


# Create date variable from quarter and year #
PacFIN_data$date <- as.yearqtr(format(paste(PacFIN_data$LANDING_YEAR, PacFIN_data$LANDING_QUARTER)), "%Y%q")
PacFIN_data$date<- as.Date(PacFIN_data$date,
                           format = "%Y/%q")

# Tranform other variables to numeric #
PacFIN_data$EXVESSEL_REVENUE    <- as.numeric(PacFIN_data$EXVESSEL_REVENUE) 
PacFIN_data$LANDED_WEIGHT_MTONS <- as.numeric(PacFIN_data$LANDED_WEIGHT_MTONS)
PacFIN_data$LANDED_PPP          <- as.numeric(PacFIN_data$LANDED_PPP)          
PacFIN_data$NUMBER_OF_VESSELS   <- as.numeric(PacFIN_data$NUMBER_OF_VESSELS)          
PacFIN_data$NUMBER_OF_DEALERS   <- as.numeric(PacFIN_data$NUMBER_OF_DEALERS) 

names(PacFIN_data)[names(PacFIN_data) == "LANDING_YEAR"              ] <- "Year"
names(PacFIN_data)[names(PacFIN_data) == "PACFIN_SPECIES_CODE"       ] <- "Species_code"
names(PacFIN_data)[names(PacFIN_data) == "PACFIN_SPECIES_COMMON_NAME"] <- "Species_name"
names(PacFIN_data)[names(PacFIN_data) == "LANDED_WEIGHT_MTONS"       ] <- "Landings"
names(PacFIN_data)[names(PacFIN_data) == "EXVESSEL_REVENUE"          ] <- "Revenue"
names(PacFIN_data)[names(PacFIN_data) == "LANDED_PPP"                ] <- "Price"
names(PacFIN_data)[names(PacFIN_data) == "NUMBER_OF_VESSELS"         ] <- "N_vessels"
names(PacFIN_data)[names(PacFIN_data) == "NUMBER_OF_DEALERS"         ] <- "N_dealers"
names(PacFIN_data)[names(PacFIN_data) == "LANDING_QUARTER"           ] <- "Landing_quarter"
names(PacFIN_data)[names(PacFIN_data) == "date"                      ] <- "Date"


####################
## Merge datasets ##
####################

## Collapse PacFIN data to obtain vessel numbers and dealers by years and species ##
PacFIN_data_vessels <- summaryBy(N_dealers + N_vessels ~ Species_code  + Year, FUN=meanfun, data=PacFIN_data)
names(PacFIN_data_vessels)[names(PacFIN_data_vessels) == "N_dealers.mean"] <- "N_dealers"
names(PacFIN_data_vessels)[names(PacFIN_data_vessels) == "N_vessels.mean"] <- "N_vessels"
PacFIN_data_merged <- merge(PacFIN_data_port,PacFIN_data_vessels,by=c("Year","Species_code"),all.x = TRUE)



# Merge data with SDM Pacific Sardine #
SDM_port_PSDN <- read.csv(file = here("Data", "SDM_port_month_PSDN.csv"))
    names(SDM_port_PSDN)[names(SDM_port_PSDN) == "port"] <- "Port"
    names(SDM_port_PSDN)[names(SDM_port_PSDN) == "year"] <- "Year"

  SDM_port_PSDN <- SDM_port_PSDN %>%
    group_by(Port, Year) %>%
    summarize(PSDN_SDM_60 = mean(SDM_60, na.rm = T)) %>%
    ungroup(.) 

  PacFIN_data_merged <- merge(PacFIN_data_merged,SDM_port_PSDN,by=c("Port","Year"),all.x = TRUE)



# Merge data with SDM Market Squid #
SDM_port_MSQD <- read.csv(file = here("Data", "SDM_port_month_MSQD.csv"))
    names(SDM_port_MSQD)[names(SDM_port_MSQD) == "port"] <- "Port"
    names(SDM_port_MSQD)[names(SDM_port_MSQD) == "year"] <- "Year"

  SDM_port_MSQD <- SDM_port_MSQD %>%
    group_by(Port, Year) %>%
    summarize(MSQD_SDM_60 = mean(SDM_60, na.rm = T), 
              MSQD_SDM_90 = mean(SDM_90, na.rm = T), 
              MSQD_SDM_120 = mean(SDM_120, na.rm = T))  %>%
    ungroup(.) 

  PacFIN_data_merged <- merge(PacFIN_data_merged,SDM_port_MSQD,by=c("Port","Year"),all.x = TRUE)



# Merge data with SDM Northern Anchovy #
SDM_port_NANC <- read.csv(file = here("Data", "SDM_port_month_NANC.csv"))
    names(SDM_port_NANC)[names(SDM_port_NANC) == "port"] <- "Port"
    names(SDM_port_NANC)[names(SDM_port_NANC) == "year"] <- "Year"

  SDM_port_NANC <- SDM_port_NANC %>%
    group_by(Port, Year) %>%
    summarize(NANC_SDM_60 = mean(SDM_60, na.rm = T), 
              NANC_SDM_90 = mean(SDM_90, na.rm = T)) %>%
    ungroup(.) 

  PacFIN_data_merged <- merge(PacFIN_data_merged,SDM_port_NANC,by=c("Port","Year"),all.x = TRUE)


# Merge data with ACL Pacific Sardine #
PacFIN_data_merged <- merge(PacFIN_data_merged,ACL_msqd_annual,by=c("Year"),all.x = TRUE)

# Merge data with SDM Market squid #
PacFIN_data_merged <- merge(PacFIN_data_merged,ACL_psdn_annual,by=c("Year"),all.x = TRUE)


####################
### Save DATASET ###
####################
PacFIN_dat_merged <- PacFIN_dat_merged %>% 
  dplyr::rename(ACL_PSDN = ACL_PSDN.sum) %>%
  dplyr::rename(ACT_PSDN = ACT_PSDN.sum) %>%
  dplyr::rename(ACL_MSQD = ACL_MSQD.mean)

names(PacFIN_data_merged)[names(PacFIN_data_merged) == "Year"] <- "Landing_year"
sapply(PacFIN_data_merged, class) 
write.csv(PacFIN_data_merged,"Data\\PacFin.csv", row.names = FALSE)