#-----------------------------#
## Fit discrete choice model ##
#-----------------------------#

gc()
rm(list=ls())

## Load packages ##
library(tidyverse)
library(mlogit)

## Load data ##
rdo <- readRDS(file = "C:\\Data\\PacFIN data\\rdo_Stata_c4.rds") 


#--------------------------------------
rdo2 <- rdo %>% group_by(fished_haul) %>%
  mutate(full = sum(fished)) %>% ungroup() %>% 
  filter(full != 0) %>%
  mutate(fished = as.logical(fished))

table(rdo2$selection)


## Create mlogit data
the_tows <- mlogit.data(rdo2, shape = 'long', choice = 'fished',
                        alt.levels = c("BDA-DCRB", "BDA-MSQD", "BGA-MSQD", "ERA-MSQD", "LAA-BTNA", "LAA-CMCK", "LAA-JMCK", "LAA-MSQD", "LAA-NANC", 
                                       "LAA-PBNT", "LAA-PSDN", "LAA-RHRG", "LAA-STNA", "LAA-YTNA", "MNA-ALBC", "MNA-CMCK", "MNA-JMCK", "MNA-MSQD", 
                                       "MNA-NANC", "MNA-PSDN", "MRA-MSQD", "NPA-MSQD", "NPS-CHUM", "NPS-SOCK", "SBA-CMCK", "SBA-JMCK", "SBA-MSQD",
                                       "SBA-PBNT", "SBA-PSDN", "SBA-UMCK", "SDA-BTNA", "SFA-DCRB", "SFA-MSQD", "SFA-NANC", "SPS-CHUM", "No-Participation"),
                        id.var = "fished_VESSEL_ID", chid.var = "fished_haul")


### Error: the data must be balanced in order to use the levels argument

model <- mlogit(fished ~ mean_price + mean_avail + dist_to_cog + wind_max_220_mh + PSDN.Closure.d | 1 , 
                data = the_tows,
                reflevel = 'No-Participation')

summary(model)



