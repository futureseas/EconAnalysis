#-----------------------------#
## Fit discrete choice model ##
#-----------------------------#

gc()
rm(list=ls())

## Load packages ##
library(tidyverse)
library(mlogit)

## Load data ##
#rdo <- readRDS(file = "C:\\Data\\PacFIN data\\rdo_Stata_c4.rds") 
samps <- readRDS(file = "C:\\Users\\felip\\OneDrive\\PostDoc\\sample_choice_set_c4_full.rds") 

#--------------------------------------
rdo2 <- samps %>% group_by(fished_haul) %>%
  mutate(full = sum(fished)) %>% ungroup() %>% 
  filter(full != 0) %>%
  mutate(fished = as.logical(fished))

table(rdo2$selection)


## Create mlogit data
the_tows <- mlogit.data(rdo2, shape = 'long', choice = 'fished',
                        alt.levels = c(
                          "LAA-BTNA", "LAA-CMCK", "LAA-MSQD", "LAA-NANC", "LAA-PSDN", "LAA-YTNA", 
                          "MNA-MSQD", "MNA-NANC", "MNA-PSDN", 
                          "MRA-MSQD", 
                          "SBA-CMCK", "SBA-MSQD", "SFA-MSQD", "SFA-NANC",
                          "No-Participation"),
                        id.var = "fished_VESSEL_NUM", chid.var = "fished_haul")


### Error: the data must be balanced in order to use the levels argument

model <- mlogit(fished ~ mean_price + mean_avail + dist_to_cog + wind_max_220_mh + PSDN.Closure.d | 1 , 
                data = the_tows,
                reflevel = 'No-Participation')

summary(model)



