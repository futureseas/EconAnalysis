#-----------------------------#
## Fit discrete choice model ##
#-----------------------------#

gc()
rm(list=ls())

## Load packages ##
library(tidyverse)
library(mlogit)

## Load data ##
rdo <- readRDS(file = "C:\\GitHub\\EconAnalysis\\Participation\\R\\rdo_R_c4.rds") %>%
  group_by(selection) %>%
  mutate(delta = 1 + rnorm(1,0,1)) %>% 
  ungroup() %>% 
  drop_na()

## Create mlogit data
the_tows <- mlogit.data(rdo, shape = 'long', choice = 'fished', alt.var = 'selection',
                        id.var = "fished_VESSEL_NUM", chid.var = "fished_haul")

colnames(rdo)
A <- matrix (c(1,0,0,0,0,0,0,0,0,0))
B <- matrix (c(0))

model <- gmnl::gmnl(fished ~ delta + wind_max_220_mh + mean_rev_adj + travel_cost_adj + factor(dummy_last_day) | -1 , 
                data = the_tows,
                reflevel = 'No-Participation', 
                constraints = list (eqA=A, eqB=B)
                , ranp = c(const_var = "n"),
                model = "mixl",
                correlation = FALSE,
                R = 50,
                halton = Null
                )

summary(model)



