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
rdo2$mean_avail[is.na(rdo2$mean_avail)] <- 0
rdo2$dist_to_cog[is.na(rdo2$dist_to_cog)] <- 0



## Create mlogit data
the_tows <- mlogit.data(rdo2, shape = 'long', choice = 'fished',
                        alt.levels = c("LAA-BTNA", "LAA-CMCK", "LAA-MSQD", "LAA-NANC", "LAA-PSDN", "LAA-YTNA", 
                          "MNA-MSQD", "MNA-NANC", "MNA-PSDN", "MRA-MSQD", "SBA-CMCK", "SBA-MSQD", "SFA-MSQD", 
                          "SFA-NANC", "No-Participation"),
                        id.var = "fished_VESSEL_NUM", chid.var = "fished_haul")

model <- mlogit(fished ~ mean_price + mean_avail + dist_to_cog + dist_port_to_catch_area + PSDN.Closure.d | 1 + Weekend, 
                data = the_tows,
                reflevel = 'No-Participation')

summary(model)



