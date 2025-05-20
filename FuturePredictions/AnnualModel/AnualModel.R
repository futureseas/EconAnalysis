##                          ##
# Annual participation model #
##                          ##  

library(tidyverse)
library(data.table)
library(raster)
library(doParallel)
library(brms)
library(tidybayes)
library(ROCR)
library(zoo)


rm(list = ls())
gc()
setwd("C:/GitHub/EconAnalysis/FuturePredictions")


#---- Read data (vessel has more than 20 trips in a year to be considered active) ----

c4 <- read.csv("G:/Mi Unidad/Data/Anonymised data/rdo_Stata_c4_full_noid.csv") %>%
  filter(fished == 1) %>% mutate(group_all = 4) %>% filter(selection != "No-Participation")
c5 <- read.csv("G:/Mi Unidad/Data/Anonymised data/rdo_Stata_c5_full_v2_noid.csv") %>%
  filter(fished == 1) %>% mutate(group_all = 5) %>% dplyr::select(-c(DCRB.Closure.WA)) %>%
  filter(selection != "No-Participation")
c6 <- read.csv("G:/Mi Unidad/Data/Anonymised data/rdo_Stata_c6_full_noid.csv") %>%
  filter(fished == 1) %>% mutate(group_all = 6)  %>% filter(selection != "No-Participation")
c7 <- read.csv("G:/Mi Unidad/Data/Anonymised data/rdo_Stata_c7_full_noid.csv") %>%
  filter(fished == 1) %>% mutate(group_all = 7)  %>% filter(selection != "No-Participation")

annual.part <- rbind(c4, c5, c6, c7)

rm(c4, c5, c6, c7)


#---- Descriptive statistics ---
#
# ## ---- Check how many years each vessel participate ----
#
# n_years_active <- annual.part %>% group_by(fished_VESSEL_anon, set_year, group_all) %>%
#   summarize(total_catch = sum(mean_catch)) %>% dplyr::filter(total_catch > 0) %>%
#   mutate(active_year = 1) %>%
#   group_by(fished_VESSEL_anon, group_all) %>%
#   summarize(n_years = sum(active_year))
#
# ## ---- Plot participation ---
# n_years_active <- annual.part %>%
#   group_by(fished_VESSEL_anon, set_year, group_all) %>%
#   summarize(total_catch = sum(mean_catch), .groups = "drop") %>%
#   filter(total_catch > 0) %>%
#   mutate(
#     active_year = 1,
#     vessel_group_id = paste(fished_VESSEL_anon, group_all, sep = "_")
#   )
#
# ggplot(n_years_active, aes(x=set_year, y=vessel_group_id, color=active_year)) +
#   geom_point(size=1)


#---- Add new SDMs (GAM Boost) ----
MSQD_sdm_data_MA <- read.csv("SDM/Historical/MSQD_SDM_port_day.csv") %>%
  group_by(LANDING_YEAR) %>%
  summarize(mean_avail_MSQD = mean(SDM_90, na.rm = TRUE)) %>% as.data.frame()

PSDN_sdm_data_MA <- read.csv("SDM/Historical/PSDN_SDM_port_day.csv") %>%
  group_by(LANDING_YEAR) %>%
  summarize(mean_avail_PSDN = mean(SDM_60, na.rm = TRUE)) %>% as.data.frame()

NANC_sdm_data_MA <- read.csv("SDM/Historical/NANC_SDM_port_day.csv") %>%
  group_by(LANDING_YEAR) %>%
  summarize(mean_avail_NANC = mean(SDM_60, na.rm = TRUE)) %>% as.data.frame()

CMCK_sdm_data_MA <- read.csv("SDM/Historical/CMCK_SDM_port_day.csv") %>%
  group_by(LANDING_YEAR) %>%
  summarize(mean_avail_CMCK = mean(SDM_60, na.rm = TRUE)) %>% as.data.frame()

JMCK_sdm_data_MA <- read.csv("SDM/Historical/JMCK_SDM_port_day.csv") %>%
  group_by(LANDING_YEAR) %>%
  summarize(mean_avail_JMCK = mean(SDM_60, na.rm = TRUE)) %>% as.data.frame()


## ---- Prepare the long_data by extracting port codes from selection names ----

annual.part <- annual.part %>%
  mutate(
    SPECIES = toupper(substr(selection, 5, 8)),
    date = as.Date(set_date)
  )


## ---- Prepare database adding general variables ----


vessel_vars <- annual.part %>%
  mutate(VESSEL_NUM = group_all * 100 + fished_VESSEL_anon) %>%
  group_by(VESSEL_NUM) %>%
  summarize(
    lat_cgo = mean(lat_cg, na.rm = TRUE),
    group_all = mean(group_all, na.rm = TRUE),
    length = mean(Vessel.length, na.rm = TRUE),
    .groups = "drop"
  )

general_vars <- annual.part %>%
  group_by(set_year) %>%
  summarize(
    diesel_price = mean(diesel_price, na.rm = TRUE),
    unem_rate = mean(unem_rate, na.rm = TRUE),
    fishmeal_price = mean(Price.Fishmeal.AFI, na.rm = TRUE),
    .groups = "drop"
  )

prices_vars <- annual.part %>%
  group_by(set_year, SPECIES) %>%
  summarize(
    mean_price = mean(mean_price, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(SPECIES %in% c("PSDN", "MSQD", "NANC", "CMCK"))
prices_vars <- bind_rows(prices_vars, tibble(
  set_year = 2017,
  SPECIES = "PSDN",
  mean_price = 167
))
prices_wide <- prices_vars %>%
  pivot_wider(
    names_from = SPECIES,
    values_from = mean_price,
    names_prefix = "mean_price_"
  )

all_vessel_years <- annual.part %>%
  mutate(VESSEL_NUM = group_all * 100 + fished_VESSEL_anon) %>%
  distinct(VESSEL_NUM) %>%
  crossing(set_year = seq(min(annual.part$set_year), max(annual.part$set_year)))

vessel_part <- annual.part %>%
  mutate(VESSEL_NUM = group_all * 100 + fished_VESSEL_anon) %>%
  group_by(VESSEL_NUM, set_year, SPECIES) %>%
  summarize(total_catch = sum(mean_catch, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = SPECIES, values_from = total_catch) %>%
  dplyr::select(VESSEL_NUM, set_year) %>%
  mutate(year_participation = 1)

vessel_part_expanded <- all_vessel_years %>%
  left_join(vessel_part, by = c("VESSEL_NUM", "set_year")) %>%
  mutate(year_participation = ifelse(is.na(year_participation), 0, year_participation))


database <- left_join(vessel_part_expanded, general_vars, by = c("set_year"))
database <- left_join(database, prices_wide, by = c("set_year"))
database <- left_join(database, vessel_vars, by = c("VESSEL_NUM"))


## ---- Merge SDM data ----
database <- database %>%
  left_join(MSQD_sdm_data_MA,
            by = c("set_year" = "LANDING_YEAR")) %>%
  left_join(PSDN_sdm_data_MA,
            by = c("set_year" = "LANDING_YEAR")) %>%
  left_join(NANC_sdm_data_MA,
            by = c("set_year" = "LANDING_YEAR")) %>%
  left_join(CMCK_sdm_data_MA,
            by = c("set_year" = "LANDING_YEAR"))

## ---- Add closure ----
database <- database %>%
  mutate(PSDN_closure = ifelse(set_year>2015, 1, 0))




#---- Estimate model ----

rm(list = setdiff(ls(), "database"))
saveRDS(database, "annual_data_part.RDS")
database <- readRDS("annual_data_part.RDS")

# get lagged values!!!

annual_logit <- brm(year_participation ~ diesel_price + unem_rate +  + PSDN_closure + 
                     mean_price_CMCK + mean_price_MSQD + mean_price_NANC + mean_price_PSDN + 
                     mean_avail_MSQD + mean_avail_PSDN + mean_avail_NANC + mean_avail_CMCK + 
                     length + lat_cgo + (1 | group_all),
              data = database, seed = 123, family = bernoulli(link = "logit"), warmup = 1000,
              iter = 3000, chain = 4, cores = 4,
              control = list(adapt_delta = 0.999))

            summary(annual_logit)
            saveRDS(annual_logit, "annual_logit.RDS")
           
         



##---- Check model ----
pp_check(anual_logit)

## Check convergence
launch_shinystan(anual_logit)
# 
# ### Population parameters ###
# mcmc_plot(logit_all_2, regex = TRUE, variable = 
#             c("b_mean_SDM",
#               "b_mean_unem",
#               "b_PSDN.Closure",
#               "b_mean_HHI",
#               "b_Intercept")) +
#   theme(axis.text.y = element_text(hjust = 0)) + scale_y_discrete(
#     labels = c(
#       "b_mean_SDM" = "mean(AVailability)",
#       "b_mean_unem" = "mean(Unemployment)",
#       "b_PSDN.Closure1" = "PSDN Closure",
#       "b_mean_HHI" = "mean(Diversity)",
#       "b_Intercept" = "Intercept"))
# 
# ### Obtain AUC # 89%
# Prob <- predict(logit_all_2, type="response")
# Prob <- Prob[,1]
# Pred <- ROCR::prediction(Prob, as.vector(pull(annual.part, active_year)))
# AUC <- ROCR::performance(Pred, measure = "auc")
# AUC <- AUC@y.values[[1]]
# AUC
# 
# 
# ##---- Plot marginal effects ----    
# conditional_effects(logit_all_2)
# 
