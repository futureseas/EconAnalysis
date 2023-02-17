######################################################################################## 
## Sampling choice set for port-species model based on Peter's code for Hicks's paper ##
########################################################################################

### Note: Thanks Peter for sharing the code used in Hicks's paper. 
### I modify it a little to be used with CPS logbooks. A trip would be similiar to a ticket, 
### a "set" similar to haul, and coordinates "set" and "up" are the same. 

gc()
rm(list=ls())
memory.limit(9999999999)

## Load packages ##
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(reshape2)
library(devtools)
library(maps)
library(doParallel)
library(tidyr)
library(tidyverse)
library(mlogit)
library(parallel)

#-----------------------------------------------------------------------------

## Read participation database ##
participation_data <- read.csv("C:\\GitHub\\EconAnalysis\\Data\\participation_data.csv")


#-----------------------------------------------------------------------------
# Clean dataset for discrete choice model (no haul in this case)

participation_data$trip_id <- udpipe::unique_identifier(participation_data, fields = c("FTID"))
participation_data$set_date<-as.Date(with(
   participation_data,
   paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
   "%Y-%m-%d")

participation_data$set_date <- as.Date(participation_data$set_date, "%d%b%Y %H:%M:%OS")
  participation_data <- participation_data %>% drop_na(set_date)


# --------------------------------------------------
## Save database!
participation_data.save <- participation_data %>%
      dplyr::rename(set_day = LANDING_DAY) %>%
      dplyr::rename(set_month = LANDING_MONTH) %>%
      dplyr::rename(set_year = LANDING_YEAR) %>%
      select("set_date", "set_day", "set_month", "set_year", "Landings", "Revenue",
             "Species_Dominant", "selection", "VESSEL_NUM", "trip_id", "selection", 'group_all')

# write.csv(participation_data.save,"C:\\GitHub\\EconAnalysis\\Data\\discrete_choice_participation.csv", row.names = FALSE)


# #-----------------------------------------------------------------------------
# # ## Sampling choice data including revenue##

source("C:\\GitHub\\EconAnalysis\\Functions\\sampled_rums_participation.R")
samps <- sampled_rums(data_in = participation_data.save, cluster = 1,
                         min_year = 2012, max_year = 2015,
                         min_year_prob = 2013, max_year_prob = 2014,
                         ndays = 60, ndays_participation = 365, nhauls_sampled = 4,
                         seed = 42, ncores = 2, rev_scale = 100)
# 
# saveRDS(samps, file = "C:\\GitHub\\EconAnalysis\\samples_choices.rds")





# Restore the object
samps <- readRDS(file = "C:\\GitHub\\EconAnalysis\\samples_choices.rds")

#-----------------------------------------------------------------------------
## Create additional variables

#---------------------------------------------------------------
## Obtain (year) price by port from PacFIN landing data
# 
# PacFIN_dat <- read.csv(file = here::here("Data", "PacFin.csv"))
# price_PSDN <- PacFIN_dat %>%
#   dplyr::filter(Species_code == "PSDN") %>%
#   group_by(Landing_year) %>%
#   summarize(price.PSDN = mean(Price, na.rm = T)) %>%
#   dplyr::rename(set_year = Landing_year)
# 
# psdn.logbook <- merge(psdn.logbook,price_PSDN,by=c('set_year'),all.x = TRUE) 

#---------------------------------------------------------------
## Create expected and actual revenue -- psdn_rev variable-- using SDMs (maybe moving average?) and catch
## (I need prices by port)

# psdn.logbook <- psdn.logbook %>%
#   mutate(psdn.rev.catch = catch * price.PSDN) %>%
#   mutate(psdn.rev.sdm = pSDM * price.PSDN)

#---------------------------------------------------------------
## Calculate net revenues for each haul
# dat <- dat %>% group_by(haul_id) %>%
#   mutate(haul_net_revenue.sdm = sum(psdn.rev.sdm, na.rm = T))

#---------------------------------------------------------------
## Merge storm warning signals
# warnings.signals <- read.csv(file = "G://My Drive//Data//Storm Warnings//WCports_mww_events09-16.csv")
# warnings.signals <- warnings.signals %>% 
#   select("pcid", "d_issued", "d_expired", "hurricane", "gale", "smcraft", "mww_other") %>%
#   dplyr::rename(PACFIN_PORT_CODE = pcid) 
# port_area <- read.csv(file = here::here("Data", "Ports", "ports_area_and_name_codes.csv"))
# warnings.signals <- warnings.signals %>% merge(port_area, by = c("PACFIN_PORT_CODE"), all.x = TRUE)
# warnings.signals$d_issued  <- as.Date(warnings.signals$d_issued, "%d%b%Y %H:%M:%OS")
# warnings.signals$d_expired <- as.Date(warnings.signals$d_expired, "%d%b%Y %H:%M:%OS")
# warnings.signals <- warnings.signals %>% unique() 
# library(sqldf)
# df1 <- participation_data
# df2 <- warnings.signals
# warnings.signals <-  sqldf("select df1.*, df2.hurricane, df2.gale, df2.smcraft, df2.mww_other
#                                       from df1 left join df2 on
#                                       (df1.Port_Dominant = df2.PORT_AREA_CODE) AND 
#                                       (df1.set_date between df2.d_issued and df2.d_expired)") 
# warnings.signals <- warnings.signals %>% unique() %>% 
#   select("trip_id", "hurricane", "gale", "smcraft", "mww_other")
# warnings.signals <- warning.signals %>% group_by(trip_id) %>%
#   summarise(hurricane = sum(hurricane), gale = sum(gale), 
#             smcraft = sum(smcraft), mww_other = sum(mww_other))
# warnings.signals[is.na(warnings.signals)] <- 0
# participation_data <- merge(participation_data, warnings.signals, 
#                             by=c("trip_id"), all.x = TRUE)
# 


#-----------------------------------------------------------------------------
#Format as mlogit.data
rdo <- samps %>% dplyr::select(fished, fished_haul,dummy_miss, mean_rev, 
                               mean_rev_adj, selection, fished_VESSEL_NUM, set_date)

# rdo <- rdo %>% group_by(fished_haul) %>% mutate(alt_tow = 1:length(fished_haul)) %>% as.data.frame

#-----------------------------------------------------------------------------
##Fit mlogit models returning the coefficients, the models, and the data going into the model

#Fit the model for everything at once
library(data.table)
library(mlogit)


## Drop choice cards that received no choice
# rdo2 <- rdo %>% 
#   group_by(fished_haul) %>% 
#   mutate(full = sum(fished)) %>% 
#   filter(full!=0) %>% 
#   select(-c(full))


# Check hauls in same day (only 30)
fished_haul_select <- rdo %>%
  group_by(fished_VESSEL_NUM, set_date) %>%
  mutate(ncount = n()) %>% dplyr::filter(ncount == 5) %>% ungroup() %>%
  select('fished_haul') %>% unique()

rdo <- data.table::setDT(rdo)[fished_haul %chin% fished_haul_select$fished_haul]


rdo2 <- as.data.frame(rdo[order(rdo$fished_VESSEL_NUM, rdo$fished_haul, -rdo$fished),]) %>%
  group_by(fished_VESSEL_NUM) %>%
  dplyr::mutate(fished_VESSEL_ID = cur_group_id()) %>%
  ungroup() %>% dplyr::select(-c('fished_VESSEL_NUM')) %>%
  group_by(set_date) %>%
  dplyr::mutate(time = cur_group_id()) %>%
  ungroup() %>% dplyr::select(-c('set_date'))


write.csv(rdo2,"C:\\GitHub\\EconAnalysis\\Data\\sampled_mixed_logit_data.csv", row.names = FALSE)


str(rdo2)
## Maybe create new fished_haul number within VESSEL_NUM?

the_tows <- mlogit.data(rdo2, shape = 'long', choice = 'fished', alt.var = 'selection', 
                        id.var = "fished_VESSEL_ID", chid.var = "fished_haul")

# drop.index
# mf <- mFormula(fished ~ dummy_miss | 1 | mean_rev_adj)
# res <- mlogit(mf, the_tows)
# summary(res)

res <- mlogit(fished ~ dummy_miss + mean_rev_adj,
              the_tows, reflevel = 'No-Participation', panel = TRUE, 
              rpar = c(mean_rev_adj = "n"),
              correlation = FALSE, R = 100, halton = NA)
summary(res)


###
# Error in if (abs(x - oldx) < ftol) { : 
#     missing value where TRUE/FALSE needed --- Export to Stata?

#--------------------------------------------------------------------
#Generate and format the predictions
fits <- fitted(res, outcome = FALSE)
mfits <- reshape2::melt(fits) %>% 
  dplyr::rename(fished_haul = Var1) %>%
  dplyr::rename(selection_pred = Var2)

## -- Correct prediction using max probability value -- ##
pred_tows <- mfits %>% group_by(fished_haul) %>% filter(value == max(value)) %>% as.data.frame
pred_tows <- merge(rdo, pred_tows, by = "fished_haul") %>% filter(fished == TRUE) %>% 
  mutate(correct = ifelse(selection_pred == selection, 1, 0))
correct_prediction <- sum(pred_tows$correct) / nrow(pred_tows) #score 1
correct_prediction


pred_tows2 <- pred_tows %>% dplyr::filter(selection != "No-Participation")
correct_prediction <- sum(pred_tows2$correct) / nrow(pred_tows2) #score 1
correct_prediction










