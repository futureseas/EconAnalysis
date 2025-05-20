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


#---- Descriptive statistics ----

## ---- Check how many years each vessel participate ----

n_years_active <- annual.part %>% group_by(fished_VESSEL_anon, set_year, group_all) %>%
  summarize(total_catch = sum(mean_catch)) %>% dplyr::filter(total_catch > 0) %>%
  mutate(active_year = 1) %>%
  group_by(fished_VESSEL_anon, group_all) %>%
  summarize(n_years = sum(active_year))

## ---- Plot participation ----
n_years_active <- annual.part %>% 
  group_by(fished_VESSEL_anon, set_year, group_all) %>%
  summarize(total_catch = sum(mean_catch), .groups = "drop") %>% 
  filter(total_catch > 0) %>%
  mutate(
    active_year = 1,
    vessel_group_id = paste(fished_VESSEL_anon, group_all, sep = "_")
  )
  
ggplot(n_years_active, aes(x=set_year, y=vessel_group_id, color=active_year)) +
  geom_point(size=1)


 
# ---- Computing moving averages ----

# annual.part <- annual.part %>%
#   arrange(VESSEL_NUM, set_year_actual) %>%
#   group_by(VESSEL_NUM) %>%
#   mutate(years_active        = RcppRoll::roll_sum(active_year,           5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_HHI            = RcppRoll::roll_mean(diversity_all,        5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_COG            = RcppRoll::roll_mean(LAT,                  5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_LI             = RcppRoll::roll_mean(DISTANCE_A,           5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_NANC_PRICE     = RcppRoll::roll_mean(NANC_Price,           5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_NANC_SDM       = RcppRoll::roll_mean(NANC_SDM_20,          5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_PSDN_PRICE     = RcppRoll::roll_mean(PSDN_Price,           5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_PSDN_SDM       = RcppRoll::roll_mean(PSDN_SDM_60,          5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_MSQD_PRICE     = RcppRoll::roll_mean(MSQD_Price,           5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_MSQD_SDM       = RcppRoll::roll_mean(MSQD_SDM_90,          5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_MSQD_SPAWN_SDM = RcppRoll::roll_mean(MSQD_SPAWN_SDM_90,    5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_unem.CA        = RcppRoll::roll_mean(mean.unemployment.CA, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_diesel.CA      = RcppRoll::roll_mean(diesel.price.CA,      5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_unem.OR        = RcppRoll::roll_mean(mean.unemployment.OR, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_diesel.OR      = RcppRoll::roll_mean(diesel.price.OR,      5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_unem.WA        = RcppRoll::roll_mean(mean.unemployment.WA, 5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   mutate(mean_diesel.WA      = RcppRoll::roll_mean(diesel.price.WA,      5, fill = NA, align = "right", na.rm = TRUE)) %>%
#   ungroup() %>%
#   filter(set_year_actual>=2005) %>%
#   dplyr::select(c('years_active', 'mean_HHI', 'mean_COG' , 'mean_LI' , 'mean_MSQD_PRICE', 'mean_MSQD_SDM',
#                   'mean_NANC_PRICE', 'mean_NANC_SDM', 'mean_MSQD_SPAWN_SDM', 'mean_PSDN_PRICE', 'mean_PSDN_SDM',
#                   'VESSEL_NUM', 'set_year', 'set_year_actual', 'active_year', 'group_all', 
#                   'mean_unem.CA', 'mean_diesel.CA', 'mean_unem.OR', 'mean_diesel.OR', 'mean_unem.WA', 'mean_diesel.WA'))
# 


######################
### Estimate model ###
######################


# ## Load data
# annual.part <- readRDS("C:/Data/PacFIN data/annual_part.RDS") %>% 
#     mutate(mean_COG = ifelse(is.na(mean_COG), mean(mean_COG, na.rm=TRUE), mean_COG)) %>%
#     mutate(mean_HHI = ifelse(is.na(mean_HHI), 0, mean_HHI)) %>%
#     mutate(mean_LI  = ifelse(is.na(mean_LI), 0, mean_LI)) %>%
#     mutate(PSDN.Closure = as.factor(ifelse(set_year_actual >= 2015, 1, 0))) %>%
#     mutate(years_active = years_active - active_year) 
# 
# ## Calculate weight averages for prices and SDM
# annual.part <- annual.part %>%
#   mutate(mean_SDM = ifelse(group_all == 4, mean_PSDN_SDM * 0.0608 + mean_MSQD_SDM * 0.9005 + mean_NANC_SDM * 0.0101, 
#                     ifelse(group_all == 5, mean_PSDN_SDM * 0.3132 + mean_MSQD_SDM * 0.5222 + mean_NANC_SDM * 0.0014, 
#                     ifelse(group_all == 6, mean_PSDN_SDM * 0.7960 + mean_MSQD_SDM * 0.0028 + mean_NANC_SDM * 0.1288,
#                     ifelse(group_all == 7, mean_PSDN_SDM * 0.2005 + mean_MSQD_SDM * 0.3092 + mean_NANC_SDM * 0.1927, 
#                            NA))))) %>%
#   mutate(mean_PRICE = ifelse(group_all == 4, mean_PSDN_PRICE * 0.0608 + mean_MSQD_PRICE * 0.9005 + mean_NANC_PRICE * 0.0101,		
#                       ifelse(group_all == 5, mean_PSDN_PRICE * 0.3132 + mean_MSQD_PRICE * 0.5222 + mean_NANC_PRICE * 0.0014,
#                       ifelse(group_all == 6, mean_PSDN_PRICE * 0.7960 + mean_MSQD_PRICE * 0.0028 + mean_NANC_PRICE * 0.1288,
#                       ifelse(group_all == 7, mean_PSDN_PRICE * 0.2005 + mean_MSQD_PRICE * 0.3092 + mean_NANC_PRICE * 0.1927,
#                            NA))))) %>%
#   mutate(mean_unem = ifelse(group_all == 4, 0.93 * mean_unem.CA + 0.01 * mean_unem.OR, 
#                      ifelse(group_all == 5, 0.54 * mean_unem.CA + 0.21 * mean_unem.OR + 0.21 * mean_unem.WA,
#                      ifelse(group_all == 6, 0.44 * mean_unem.OR + 0.55 * mean_unem.WA,
#                      ifelse(group_all == 7, 0.95 * mean_unem.CA,
#                             NA))))) %>%
#   dplyr::filter(group_all == 4 | group_all == 5 | group_all == 6 | group_all == 7) %>% drop_na()


# ##  Estimate Bayesian model
# 
# logit_all <- brm(active_year ~ mean_SDM + mean_PRICE + mean_unem + PSDN.Closure + mean_HHI + 
#                           (1 | group_all) + (1 | VESSEL_NUM) + (1 | set_year_actual), 
#               data = annual.part, seed = 123, family = bernoulli(link = "logit"), warmup = 1000, 
#               iter = 3000, chain = 4, cores = 4,
#               prior = c(set_prior("lognormal(0,1)", class = "b", coef = "mean_PRICE")),
#               #           set_prior("lognormal(0,1)", class = "b", coef = "mean_PRICE")),
#               control = list(adapt_delta = 0.999))
#             summary(logit_all)
#             saveRDS(logit_all, "logit_all.RDS")
#             
# logit_all_2 <- brm(active_year ~ mean_SDM + mean_unem + PSDN.Closure + mean_HHI + 
#                                (1 | group_all) + (1 | VESSEL_NUM) + (1 | set_year_actual), 
#                              data = annual.part, seed = 123, family = bernoulli(link = "logit"), warmup = 1000, 
#                              iter = 3000, chain = 4, cores = 4,
#                              control = list(adapt_delta = 0.999))
#             summary(logit_all_2)
#             saveRDS(logit_all_2, "logit_all_2.RDS")
# 
# LOO(logit_all, logit_all_2)

logit_all_2 <- readRDS("logit_all_2.RDS")


## Check model
pp_check(logit_all_2)

## Check convergence
launch_shinystan(logit_all_2)

### Population parameters ###
mcmc_plot(logit_all_2, regex = TRUE, variable = 
            c("b_mean_SDM",
              "b_mean_unem",
              "b_PSDN.Closure",
              "b_mean_HHI",
              "b_Intercept")) +
  theme(axis.text.y = element_text(hjust = 0)) + scale_y_discrete(
    labels = c(
      "b_mean_SDM" = "mean(AVailability)",
      "b_mean_unem" = "mean(Unemployment)",
      "b_PSDN.Closure1" = "PSDN Closure",
      "b_mean_HHI" = "mean(Diversity)",
      "b_Intercept" = "Intercept"))

### Obtain AUC # 89%
Prob <- predict(logit_all_2, type="response")
Prob <- Prob[,1]
Pred <- ROCR::prediction(Prob, as.vector(pull(annual.part, active_year)))
AUC <- ROCR::performance(Pred, measure = "auc")
AUC <- AUC@y.values[[1]]
AUC


## Plot marginal effects     
conditional_effects(logit_all_2)

