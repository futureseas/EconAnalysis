###########################
### SQUID Landing model ###
###########################

#----------------------------
# Setup #
rm(list = ls(all.names = TRUE)) 
gc()

library("googlesheets4")
gs4_auth(
  email = "fequezad@ucsc.edu",
  path = NULL,
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)


## Read packages 
library("brms")
library("sjPlot")
library("sjlabelled")
library("sjmisc")
library("insight")
library("httr")
library("tidyr")
library("dplyr") 
library("data.table") 
library("reshape2")
library('doBy')

## Read dataset
dataset <- read.csv(file ="C:\\Data\\PacFIN data\\dataset_estimation.csv")

#-----------------------------------------------
## Create dataset for estimation and run landing models 

### Market squid ###

#### Select data for estimation, replace N/A landings to zero ####
dataset_msqd <- dataset %>%
  dplyr::select(PORT_AREA_ID, PORT_AREA_CODE, VESSEL_NUM, group_all, 
                LANDING_YEAR, LANDING_MONTH,
                MSQD_SPAWN_SDM_90, MSQD_SDM_90, MSQD_Landings, MSQD_Price, 
                PSDN_Landings, NANC_Landings, PSDN_Price, NANC_Price, 
                PSDN_SDM_60, NANC_SDM_20,
                MSQD_Price_z, PSDN_Price_z, MSQD_SPAWN_SDM_90_z, MSQD_SDM_90_z, 
                PSDN_SDM_60_z, NANC_SDM_20_z,
                PSDN.Open, MSQD.Open, Price.Fishmeal, Price.Fishmeal_z, 
                Price.Fishmeal.AFI, Price.Fishmeal.AFI_z,
                diesel.price, diesel.price.AFI, diesel.price_z, diesel.price.AFI_z,
                Length, Length_z, avg_set_MSQD, avg_set_MSQD_z) %>% 
  dplyr::mutate(MSQD_Landings = coalesce(MSQD_Landings, 0)) %>%
  dplyr::mutate(PSDN_Landings = coalesce(PSDN_Landings, 0)) %>%
  dplyr::mutate(NANC_Landings = coalesce(NANC_Landings, 0)) %>%
  mutate(MSQD_Landings = ifelse(MSQD_Landings<= 0, 0, MSQD_Landings)) %>%
  mutate(MSQD_Landings = ifelse(MSQD_Landings< 0.0001, 0, MSQD_Landings)) %>%
  mutate(PSDN_Landings = ifelse(PSDN_Landings<= 0, 0, MSQD_Landings)) %>%
  mutate(PSDN_Landings = ifelse(PSDN_Landings< 0.0001, 0, MSQD_Landings)) %>%
  mutate(NANC_Landings = ifelse(NANC_Landings<= 0, 0, MSQD_Landings)) %>%
  mutate(NANC_Landings = ifelse(NANC_Landings< 0.0001, 0, MSQD_Landings)) %>%
  dplyr::mutate(PSDN.Participation = ifelse(PSDN_Landings > 0, 1, 0)) %>% 
  dplyr::mutate(PSDN.Total.Closure = ifelse(LANDING_YEAR > 2015, 1, 0)) %>%
  dplyr::mutate(PSDN.Total.Closure = ifelse((LANDING_YEAR == 2015 & LANDING_MONTH >= 7), 1, PSDN.Total.Closure)) %>% 
  dplyr::mutate(ln_MSQD_Landings = log(MSQD_Landings)) %>%
    filter(group_all == 1 | group_all == 2 | group_all == 4 | group_all == 5 | group_all == 7) %>%
    filter(PORT_AREA_CODE == "SBA" | PORT_AREA_CODE == "LAA" | PORT_AREA_CODE == "MNA") %>% drop_na()


## install.packages(c("fastDummies", "recipes"))
# library('fastDummies')
# dataset_msqd <- dummy_cols(dataset_msqd, select_columns = 'cluster')

#### Convert variables to factor #### HERE I CHANGE THE ID
dataset_msqd$port_ID            <- factor(dataset_msqd$PORT_AREA_CODE)
dataset_msqd$cluster            <- factor(dataset_msqd$group_all)
class(dataset_msqd$port_ID)
class(dataset_msqd$cluster)
class(dataset_msqd$PSDN.Total.Closure)


dataset_msqd_landing <- dataset_msqd %>%
  dplyr::filter(MSQD_Landings > 0) %>%
  dplyr::filter(MSQD.Open == 1) 

### Descriptive statistics 
desc_data <- dataset_msqd_landing %>%
  subset(select = c(Length, MSQD_Landings, MSQD_Price, 
                    MSQD_SDM_90, MSQD_SPAWN_SDM_90, PSDN_SDM_60, NANC_SDM_20,  
                    PSDN.Open, Price.Fishmeal.AFI))

table <- psych::describe(desc_data, fast=TRUE) %>%
  mutate(vars = ifelse(vars == 1, "Vessel Length", vars))%>%
  mutate(vars = ifelse(vars == 2, "Landings: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 3, "Price: MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 4, "Prob(presence): MSQD", vars)) %>%
  mutate(vars = ifelse(vars == 5, "Prob(presence): PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 6, "Prob(presence): NANC", vars)) %>%
  mutate(vars = ifelse(vars == 7, "Fraction of month open: PSDN", vars)) %>%
  mutate(vars = ifelse(vars == 8, "Fishmeal price", vars)) 

# gs4_create("SummaryMonthly_Q_MSQD", sheets = table)
rm(desc_data, table)


#-------------------------------------------------------------------
## Check stationarity in the panel dataset
library("plm")

dataset_msqd_landing$Date <- 
  zoo::as.yearmon(paste(dataset_msqd_landing$LANDING_YEAR, dataset_msqd_landing$LANDING_MONTH), "%Y %m")

pDataset <- dataset_msqd_landing %>% mutate(Unique_ID = paste(VESSEL_NUM, PORT_AREA_CODE, sep = " ")) %>%
  group_by(Unique_ID) %>% mutate(n_obs_group = n()) %>% ungroup() %>% filter(n_obs_group >= 12) %>% drop_na()
pDataset <- pdata.frame(pDataset, index = c('Unique_ID', 'Date'))

# duplicate_indexes <- dataset_msqd %>%
#   group_by(PORT_AREA_CODE, Date, VESSEL_NUM) %>% mutate(dupe = n()>1)

purtest(pDataset$MSQD_Landings, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$MSQD_Price, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$MSQD_SPAWN_SDM_90, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$PSDN_SDM_60, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$NANC_SDM_20, pmax = 4, exo = "intercept", test = "Pm")
purtest(pDataset$Price.Fishmeal.AFI, pmax = 4, exo = "intercept", test = "Pm")

rm(pDataset)

## -------------------------------------------------------------------
### Market squid landing model

#### Base model 
price_model   <- bf(MSQD_Price_z ~ 1 + Price.Fishmeal.AFI_z + (1 | port_ID))

# landing_model <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z + 
#                                         (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z | cluster) + 
#                                         (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z | port_ID))
#
# fit_qMSQD_FINAL_endog <- add_criterion(fit_qMSQD_FINAL_endog, "loo", overwrite = TRUE)
# LOO(fit_qMSQD_FINAL_endog)
# 
# #### Add PSDN interaction effects
# landing_model_PSDNInteraction <- bf(log(MSQD_Landings) ~ 
#             1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z +
#            (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z | cluster) + 
#            (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z | port_ID))
# 
# fit_qMSQD_FINAL_endog_PSDNInteraction <- add_criterion(fit_qMSQD_FINAL_endog_PSDNInteraction, "loo", overwrite = TRUE)
# LOO(fit_qMSQD_FINAL_endog_PSDNInteraction)
# 
# loo_compare(
#   fit_qMSQD_FINAL_endog,
#   fit_qMSQD_FINAL_endog_PSDNInteraction,
#   criterion = "loo") 
# 
# # -30.6 / 9.7 > 3
# 
# #### Add NANC interaction effects
# 
# landing_model_PSDNInteraction_NANCInteraction <- bf(log(MSQD_Landings) ~ 
#     1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + NANC_SDM_20_z:MSQD_SPAWN_SDM_90_z +
#    (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + NANC_SDM_20_z:MSQD_SPAWN_SDM_90_z | cluster) + 
#    (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + NANC_SDM_20_z:MSQD_SPAWN_SDM_90_z | port_ID))
# 
# fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction <- add_criterion(fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction, "loo", overwrite = TRUE)
# LOO(fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction)
# 
# loo_compare(
#   fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction,
#   fit_qMSQD_FINAL_endog_PSDNInteraction,
#   criterion = "loo") 
# 
# # -37.5 / 11.6  > 3
# 
# 
# #### Add PSDN closure

landing_model_PSDNInteraction_NANCInteraction_Closure <- bf(log(MSQD_Landings) ~
  1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + NANC_SDM_20_z:MSQD_SPAWN_SDM_90_z + PSDN.Total.Closure +
 (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + NANC_SDM_20_z:MSQD_SPAWN_SDM_90_z + PSDN.Total.Closure | cluster) +
 (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + NANC_SDM_20_z:MSQD_SPAWN_SDM_90_z + PSDN.Total.Closure | port_ID))

# fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction_Closure <- add_criterion(fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction_Closure, "loo", overwrite = TRUE)


# fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction_Closure <-
#   brm(data = dataset_msqd_landing,
#       family = gaussian,
#       price_model + landing_model_PSDNInteraction_NANCInteraction_Closure + set_rescor(TRUE),
#       prior = c(# E model
#         prior(normal(0, 1), class = b, resp = MSQDPricez),
#         prior(exponential(1), class = sigma, resp = MSQDPricez),
#         # W model
#         prior(normal(0, 1), class = b, resp = logMSQDLandings),
#         prior(exponential(1), class = sigma, resp = logMSQDLandings),
#         # rho
#         prior(lkj(2), class = rescor)),
#       iter = 2000, warmup = 1000, chains = 4, cores = 4,
#       control = list(max_treedepth = 15, adapt_delta = 0.99),
#       file = "Estimations/fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction_Closure")


# fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction_Closure <- add_criterion(fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction_Closure, "loo", overwrite = TRUE)
# LOO(fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction_Closure)
# 
# loo_compare(
#   fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction_Closure,
#   fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction,
#   criterion = "loo") 



# #### Exclude NANC

# landing_model_PSDNInteraction_Closure <- bf(log(MSQD_Landings) ~
#                                               1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN.Total.Closure +
#                                              (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN.Total.Closure | cluster) +
#                                              (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN.Total.Closure | port_ID))

# fit_qMSQD_FINAL_endog_PSDNInteraction_Closure <-
#   brm(data = dataset_msqd_landing,
#       family = gaussian,
#       price_model + landing_model_PSDNInteraction_Closure + set_rescor(TRUE),
#       prior = c(# E model
#         prior(normal(0, 1), class = b, resp = MSQDPricez),
#         prior(exponential(1), class = sigma, resp = MSQDPricez),
#         # W model
#         prior(normal(0, 1), class = b, resp = logMSQDLandings),
#         prior(exponential(1), class = sigma, resp = logMSQDLandings),
#         # rho
#         prior(lkj(2), class = rescor)),
#       iter = 2000, warmup = 1000, chains = 4, cores = 4,
#       control = list(max_treedepth = 15, adapt_delta = 0.99),
#       file = "Estimations/fit_qMSQD_FINAL_endog_PSDNInteraction_Closure")
# 
# # fit_qMSQD_FINAL_endog_PSDNInteraction_Closure <- add_criterion(fit_qMSQD_FINAL_endog_PSDNInteraction_Closure, "loo", overwrite = TRUE)
# LOO(fit_qMSQD_FINAL_endog_PSDNInteraction_Closure)


#### Exclude variables that are not important

# landing_model_PSDNInteraction_noSDM <- bf(log(MSQD_Landings) ~ 
#                                    1 + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z +
#                                   (1 + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z | cluster) + 
#                                   (1 + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z | port_ID))
# 
# landing_model_PSDNInteraction_Closure_noSDM <- bf(log(MSQD_Landings) ~ 
#                                    1 + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN.Total.Closure +
#                                   (1 + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN.Total.Closure | cluster) + 
#                                   (1 + MSQD_Price_z + Length_z + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN.Total.Closure | port_ID))

# fit_qMSQD_FINAL_endog_PSDNInteraction_noSDM         <- add_criterion(fit_qMSQD_FINAL_endog_PSDNInteraction_noSDM, "loo", overwrite = TRUE)
# fit_qMSQD_FINAL_endog_PSDNInteraction_Closure_noSDM <- add_criterion(fit_qMSQD_FINAL_endog_PSDNInteraction_Closure_noSDM, "loo", overwrite = TRUE)
# LOO(fit_qMSQD_FINAL_endog_PSDNInteraction_noSDM)
# LOO(fit_qMSQD_FINAL_endog_PSDNInteraction_Closure_noSDM)



######
# Read previous models 

# fit_qMSQD_FINAL_endog                                         <- readRDS(here::here("Estimations", "fit_qMSQD_FINAL_endog.RDS"))
# fit_qMSQD_FINAL_endog_PSDNInteraction                         <- readRDS(here::here("Estimations", "fit_qMSQD_FINAL_endog_PSDNInteraction.RDS"))
# fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction         <- readRDS(here::here("Estimations", "fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction.RDS"))
fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction_Closure <- readRDS(here::here("Estimations", "fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction_Closure.RDS"))
# fit_qMSQD_FINAL_endog_PSDNInteraction_Closure                 <- readRDS(here::here("Estimations", "fit_qMSQD_FINAL_endog_PSDNInteraction_Closure.RDS"))
# fit_qMSQD_FINAL_endog_PSDNInteraction_noSDM                   <- readRDS(here::here("Estimations", "fit_qMSQD_FINAL_endog_PSDNInteraction_noSDM.RDS"))
# fit_qMSQD_FINAL_endog_PSDNInteraction_Closure_noSDM           <- readRDS(here::here("Estimations", "fit_qMSQD_FINAL_endog_PSDNInteraction_Closure_noSDM.RDS"))


############################
# Calculate estimation error

# ## Calculate average error
# set.seed(123)
# predict1 <- as.data.frame(predict(fit_qMSQD_FINAL_endog))
# predict2 <- as.data.frame(predict(fit_qMSQD_FINAL_endog_PSDNInteraction))
# predict3 <- as.data.frame(predict(fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction))
# predict5 <- as.data.frame(predict(fit_qMSQD_FINAL_endog_PSDNInteraction_Closure))
# predict6 <- as.data.frame(predict(fit_qMSQD_FINAL_endog_PSDNInteraction_noSDM))
# predict7 <- as.data.frame(predict(fit_qMSQD_FINAL_endog_PSDNInteraction_Closure_noSDM))

# prediction1 <- cbind(predict1, dataset_msqd_landing)
# prediction2 <- cbind(predict2, dataset_msqd_landing)
# prediction3 <- cbind(predict3, dataset_msqd_landing)
# prediction4 <- cbind(predict4, dataset_msqd_landing)
# prediction5 <- cbind(predict5, dataset_msqd_landing)
# prediction6 <- cbind(predict6, dataset_msqd_landing)
# prediction7 <- cbind(predict7, dataset_msqd_landing)

 
# sqrt(sum((prediction1$Estimate.logMSQDLandings - prediction1$ln_MSQD_Landings)^2)/(nrow(prediction1)-2))
# sqrt(sum((prediction2$Estimate.logMSQDLandings - prediction2$ln_MSQD_Landings)^2)/(nrow(prediction2)-2))
# sqrt(sum((prediction3$Estimate.logMSQDLandings - prediction3$ln_MSQD_Landings)^2)/(nrow(prediction3)-2))
# sqrt(sum((prediction4$Estimate.logMSQDLandings - prediction4$ln_MSQD_Landings)^2)/(nrow(prediction4)-2))
# sqrt(sum((prediction5$Estimate.logMSQDLandings - prediction5$ln_MSQD_Landings)^2)/(nrow(prediction5)-2))
# sqrt(sum((prediction6$Estimate.logMSQDLandings - prediction6$ln_MSQD_Landings)^2)/(nrow(prediction6)-2))
# sqrt(sum((prediction7$Estimate.logMSQDLandings - prediction7$ln_MSQD_Landings)^2)/(nrow(prediction7)-2))


##############################################################################################
## LOO comparision between models 


# loo_compare(
#   fit_qMSQD_FINAL_endog,
#   fit_qMSQD_FINAL_endog_PSDNInteraction,
#   criterion = "loo") 
# 
# # 3.15
# 
# loo_compare(
#   fit_qMSQD_FINAL_endog,
#   fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction,
#   criterion = "loo") 
# 
# # 5.23
# 
# loo_compare(
#   fit_qMSQD_FINAL_endog,
#   fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction_Closure,
#   criterion = "loo") 
# 
# # 5.84
# 
# loo_compare(
#   fit_qMSQD_FINAL_endog,
#   fit_qMSQD_FINAL_endog_PSDNInteraction_Closure,
#   criterion = "loo") 
# 
# # 4.77
# 
# loo_compare(
#   fit_qMSQD_FINAL_endog,
#   fit_qMSQD_FINAL_endog_PSDNInteraction_noSDM,
#   criterion = "loo") 
# 
# # 1.84
# 
# loo_compare(
#   fit_qMSQD_FINAL_endog,
#   fit_qMSQD_FINAL_endog_PSDNInteraction_Closure_noSDM,
#   criterion = "loo") 

# 4.26

# # LOO_compare <- 
# #   as.data.frame(
#     
#   loo_compare(
#       fit_qMSQD_FINAL_endog,
#       fit_qMSQD_FINAL_endog_PSDNInteraction,
#       criterion = "loo")
# # )

# LOO_compare <- tibble::rownames_to_column(LOO_compare, "model")
# gs4_create("LOO", sheets = LOO_compare)


###############################################
### Analyze convergence ###

#launch_shinystan(fit_qMSQD_FINAL_endog)
#launch_shinystan(fit_qMSQD_FINAL_endog_PSDNInteraction)
#launch_shinystan(fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction)
#launch_shinystan(fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction_Closure)
#launch_shinystan(fit_qMSQD_FINAL_endog_PSDNInteraction_noSDM)
#launch_shinystan(fit_qMSQD_FINAL_endog_PSDNInteraction_Closure_noSDM)
# launch_shinystan(fit_qMSQD_FINAL_endog_PSDNInteraction_Closure)



###############################################
### Create result tables ###

library(XML)

# tab_model <-
#   sjPlot::tab_model(fit_qMSQD_FINAL_endog_PSDNInteraction_Closure)
# 
# df <- data.frame(readHTMLTable(htmlParse(tab_model))[1])
# colnames(df) <- df[1,]
# df <- df[-1,]
# 
# gs4_create("Squid_landings_FINAL_Model_5", sheets = df)


# ### Population parameters ###
# mcmc_plot(fit_qMSQD_endog_PSDN_NANC, regex = TRUE, variable = 
#             c("b_logMSQDLandings_MSQD_SPAWN_SDM_90_z", 
#               "b_logMSQDLandings_MSQD_Price_z",
#               "b_logMSQDLandings_Length_z",
#               "b_logMSQDLandings_Intercept")) +
#   theme(axis.text.y = element_text(hjust = 0)) + scale_y_discrete(
#     labels = c(
#       "b_logMSQDLandings_MSQD_Price_z" = "MSQD price",
#       "b_logMSQDLandings_MSQD_SPAWN_SDM_90_z" = "MSQD availability (SDM)",
#       "b_logMSQDLandings_Length_z" = "Vessel length",
#       "b_logMSQDLandings_Intercept" = "Intercept"))

# ### Hypothesis test ###
# hypothesis(fit_qMSQD, "MSQD_SPAWN_SDM_90 = 0") 


# ######## Check correlation ###########
# 
# dataset_select <- dataset_msqd_landing %>%
#   dplyr::select(MSQD_SPAWN_SDM_90_z,
#                 PSDN_SDM_60_z,
#                 NANC_SDM_20_z,
#                 Length_z,
#                 MSQD_Price_z, 
#                 PSDN_Price_z, 
#                 diesel.price.AFI_z, 
#                 Price.Fishmeal.AFI_z,
#                 avg_set_MSQD_z)
# res <- as.data.frame(cor(dataset_select))
# round(res, 2)

# gs4_create("correlation_exp_variables_MSQD_landings", sheets = res)
fit_qMSQD <- fit_qMSQD_FINAL_endog_PSDNInteraction_NANCInteraction_Closure ## Preferred model


#####################################################
## Model summary ##
library(patchwork)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tibble)
theme_set(theme_sjplot())

# ### Posterior predictive check ###
# pp_check(fit_qMSQD, resp = "logMSQDLandings") +
#   scale_color_manual(name = "", values = c("y" = "royalblue4", "yrep" = "azure3"),
#                      labels = c("y" = "Observed", "yrep" = "Replicated")) + 
#   theme(legend.position = "right", plot.title = element_text(size=12, face="bold.italic"))  + 
#   xlim(-5, 11) + xlab("Natural logarithm of market squid landing")

#------------------------------------------------------
### Group parameters ###

#### Intercepts ####

coef(fit_qMSQD)$port_ID 
coeff_port_sdm <- as.data.frame(coef(fit_qMSQD)$port_ID[, c(1, 3:4), 2]) %>% 
  round(digits = 2) 
names <- rownames(coeff_port_sdm)
rownames(coeff_port_sdm) <- NULL
coeff_port_sdm <- cbind(names,coeff_port_sdm)

gg1 <-  ggplot(coeff_port_sdm, aes(x=names, y=Estimate)) +
  geom_point() +  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5),
    width=.2, position=position_dodge(0.05)) + coord_flip() + ggtitle("(a) Port areas") +
  ylab("") + xlab("") + theme(plot.title = element_text(size=9, face="bold.italic")) +
    scale_x_discrete(labels=c("LAA" = "Los Angeles",
                              "MNA" = "Monterey",
                              "SBA" = "Santa Barbara"))

coef(fit_qMSQD)$cluster
coeff_cluster_sdm <- as.data.frame(coef(fit_qMSQD)$cluster[, c(1, 3:4), 1]) %>% 
  round(digits = 2)
cluster <- rownames(coeff_cluster_sdm)
rownames(coeff_cluster_sdm) <- NULL
coeff_cluster_sdm <- cbind(cluster,coeff_cluster_sdm)  


gg2 <-  ggplot(coeff_cluster_sdm, aes(y=cluster, x=Estimate)) +
    geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5), 
                                  width=.2, position=position_dodge(0.05))  + ggtitle("(b) Clusters") +  
    xlab("") + ylab("") + theme(plot.title = element_text(size=9, face="bold.italic")) + 
    scale_y_discrete(labels=c("1" = "Southern CCS\nsmall-scale\nsquid-specialists",
                              "2" = "Southern CCS\nsmall-scale\nCPS-opportunists",
                              "4" = "Southern CCS\nindustrial\nsquid-specialists",
                              "5" = "Roving industrial\nsardine-squid\nswitchers",
                              "7" = "Southern CCS\nforage fish\ndiverse"))
  
gg1 + gg2
  
# ### Compare multilevel effects ###
# as_draws_df(fit_qMSQD, add_chain = T) %>%
#   ggplot(aes(x = sd_cluster__logMSQDLandings_Intercept)) +
#   geom_density(size = 0, fill = "orange1", alpha = 3/4) +
#   geom_density(aes(x = sd_port_ID__logMSQDLandings_Intercept),
#                size = 0, fill = "orange4", alpha = 3/4) +
#   scale_y_continuous(NULL, breaks = NULL) +
#   labs(title = expression(sigma), subtitle = "Market squid SDM") +
#   annotate("text", x = 2, y = 1/10, label = "Port area", color = "orange4") +
#   annotate("text", x = 2, y = 1/5, label = "Cluster", color = "orange1") +
#   theme_fivethirtyeight()



###################################################################
#### Explanatory variables by clusters ####

coef(fit_qMSQD)$cluster

coeff_cluster <- coef(fit_qMSQD)$cluster[, c(1, 3:4), 2] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
gg_1 <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("SDM: Market squid") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=10)) +
  scale_y_discrete(labels=c("1" = "Southern CCS\nsmall-scale\nsquid-specialists",
                            "2" = "Southern CCS\nsmall-scale\nCPS-opportunists",
                            "3" = "Southern CCS\nindustrial\nsquid-specialists",
                            "4" = "Roving industrial\nsardine-squid\nswitchers",
                            "5" = "Southern CCS\nforage fish\ndiverse")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 

coeff_cluster <- coef(fit_qMSQD)$cluster[, c(1, 3:4), 3] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
gg_2 <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) + geom_point() +
  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                width=.2, position=position_dodge(0.05)) + ggtitle("Price: Market squid") +
  xlab("") + ylab("")  +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(plot.title = element_text(size=10)) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 

coeff_cluster <- coef(fit_qMSQD)$cluster[, c(1, 3:4), 4] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
gg_3 <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) + geom_point() +
  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                width=.2, position=position_dodge(0.05)) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + xlab("") + ylab("") +
  ggtitle("Vessel lenght") +
  theme(plot.title = element_text(size=10)) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 

coeff_cluster <- coef(fit_qMSQD)$cluster[, c(1, 3:4), 7] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
gg_4 <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) + geom_point() +
  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                width=.2, position=position_dodge(0.05)) +
  xlab("") + ylab("") +
  ggtitle("SDM: Market squid x SDM: Pacific Sardine") +
  theme(plot.title = element_text(size=10)) +
  scale_y_discrete(labels=c("1" = "Southern CCS\nsmall-scale\nsquid-specialists",
                            "2" = "Southern CCS\nsmall-scale\nCPS-opportunists",
                            "3" = "Southern CCS\nindustrial\nsquid-specialists",
                            "4" = "Roving industrial\nsardine-squid\nswitchers",
                            "5" = "Southern CCS\nforage fish\ndiverse")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 

coeff_cluster <- coef(fit_qMSQD)$cluster[, c(1, 3:4), 6] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
gg_5 <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) + geom_point() +
  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                width=.2, position=position_dodge(0.05)) +
  xlab("") + ylab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("SDM: Market squid x SDM: Northern Anchovy") +
  theme(plot.title = element_text(size=10)) +
  geom_vline(xintercept = 0, color = "blue", linetype="dashed", size=0.5)

coeff_cluster <- coef(fit_qMSQD)$cluster[, c(1, 3:4), 5] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
gg_6 <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) + geom_point() +
  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                width=.2, position=position_dodge(0.05)) + ggtitle("PSDN Closure") +
  xlab("") + ylab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(plot.title = element_text(size=10)) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 

gg_1 + gg_2 + gg_3 + gg_4 + gg_5 + gg_6



# Port ID
coef(fit_qMSQD)$port_ID
coeff_cluster <- coef(fit_qMSQD)$port_ID[, c(1, 3:4), 3] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
gg_1 <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  
  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                width=.2, position=position_dodge(0.05)) + 
  ggtitle("SDM: Market squid")+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  xlab("") + ylab("") + theme(plot.title = element_text(size=10)) +  
  scale_y_discrete(labels=c("1" = "Los Angeles",
                            "2" = "Monterey",
                            "3" = "Santa Barbara")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 

coeff_cluster <- coef(fit_qMSQD)$port_ID[, c(1, 3:4), 4] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
gg_2 <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) + geom_point() +
  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                width=.2, position=position_dodge(0.05)) + 
  ggtitle("Price: Market squid") +
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=10)) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 

coeff_cluster <- coef(fit_qMSQD)$port_ID[, c(1, 3:4), 5] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
gg_3 <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) + geom_point() +
  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                width=.2, position=position_dodge(0.05)) + 
  theme(axis.text.y=element_blank(), axis.ticks.y=element_blank()) + 
  xlab("") + ylab("") +
  ggtitle("Vessel lenght") + 
  theme(plot.title = element_text(size=10)) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 

coeff_cluster <- coef(fit_qMSQD)$port_ID[, c(1, 3:4), 6] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
gg_4 <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) + geom_point() +
  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                width=.2, position=position_dodge(0.05)) + ggtitle("PSDN Closure") +
  xlab("") + ylab("") +
  scale_y_discrete(labels=c("1" = "Los Angeles",
                            "2" = "Monterey",
                            "3" = "Santa Barbara")) + 
  theme(plot.title = element_text(size=10)) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 

coeff_cluster <- coef(fit_qMSQD)$port_ID[, c(1, 3:4), 8] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
gg_5 <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) + geom_point() +
  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                width=.2, position=position_dodge(0.05)) + ggtitle("SDM: Market squid x SDM: Pacific sardine") +
  xlab("") + ylab("") + theme(axis.text.y=element_blank(),
                              axis.ticks.y=element_blank()) +
  theme(plot.title = element_text(size=10)) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 

coeff_cluster <- coef(fit_qMSQD)$port_ID[, c(1, 3:4), 7] %>%
  as_tibble() %>% round(digits = 2) %>% mutate(cluster = as.factor(1:n()))
gg_6 <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) + geom_point() +
  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                width=.2, position=position_dodge(0.05)) + ggtitle("SDM: Market squid x SDM: Northern anchovy") +
  xlab("") + ylab("") + theme(axis.text.y=element_blank(),
                              axis.ticks.y=element_blank()) +
  theme(plot.title = element_text(size=10)) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 

gg_1 + gg_2 + gg_3 + gg_4 + gg_5 + gg_6



# ###########################################################
# ### Conditional effect of MSQD presence
# conditional_effects_msqd_sdm <-
#   conditional_effects(
#     fit_qMSQD,
#     "MSQD_SPAWN_SDM_90_z",
#     surface=TRUE,
#     conditions = conditions_cluster,
#     re_formula = NULL)#, transform = log, method = "posterior_predict"))
# 
# gg1 <- plot(conditional_effects_msqd_sdm, plot = FALSE, nrow = 3, ncol = 2)[[2]] +
#   theme(plot.title = element_text(size=9, face="bold.italic"),
#         axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
#   scale_x_continuous(name = "MSQD: Prob(Presence)") +
#   scale_y_continuous(name = "ln(MSQD: Landings)")
# gg1$facet$params$labeller <- cond_label
# gg1


####################################################################
### Interaction effects 

#### Conditions
conditions_cluster <- data.frame(cluster = unique(dataset_msqd_landing$cluster)) 
  rownames(conditions_cluster) <- unique(dataset_msqd_landing$cluster)
  conditions_cluster <- conditions_cluster %>% 
    arrange(-desc(cluster)) 
  
cluster_label <- as_labeller(c("1" = "Southern CCS small-scale squid-specialists",
                               "2" = "Southern CCS small-scale CPS-opportunists",
                               "4" = "Southern CCS industrial squid-specialists",
                               "5" = "Roving industrial sardine-squid switchers",
                               "7" = "Southern CCS forage fish diverse"))
  

conditions_port <- data.frame(port_ID = unique(dataset_msqd_landing$port_ID)) 
  rownames(conditions_port) <- unique(dataset_msqd_landing$port_ID)
  conditions_port <- conditions_port %>% 
    arrange(-desc(port_ID)) 
  
port_label <- as_labeller(c("LAA" = "Los Angeles",
                            "MNA" = "Monterey",
                            "SBA" = "Santa Barbara"))


### Squid v/s Sardine ###

conditional_effects_psdn_msqd_sdm_cluster <- (conditional_effects(
    fit_qMSQD, "PSDN_SDM_60_z:MSQD_SPAWN_SDM_90_z", 
    surface=TRUE, 
    conditions = conditions_cluster, re_formula = NULL))

conditional_effects_psdn_msqd_sdm_port <- (conditional_effects(
  fit_qMSQD, "PSDN_SDM_60_z:MSQD_SPAWN_SDM_90_z", 
  surface=TRUE, 
  conditions = conditions_port, re_formula = NULL))

# Plot
gg_int <- plot(conditional_effects_psdn_msqd_sdm_cluster, plot = FALSE)[[2]] + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)) + 
  ggtitle("(a) Clusters") +  
  theme(plot.title = element_text(size=9, face="bold.italic")) + 
  guides(colour=guide_legend(title="ln(MSQD: Landings)")) +
  scale_x_continuous(name = "PSDN: Prob(Presence)") + scale_y_continuous(name = "MSQD: Prob(Presence)")
  gg_int$facet$params$labeller <- cluster_label
  gg_int
  
gg_int_2 <- plot(conditional_effects_psdn_msqd_sdm_port, plot = FALSE)[[2]] + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)) + 
  ggtitle("(b) Ports") +  
  theme(plot.title = element_text(size=9, face="bold.italic")) +
  guides(colour=guide_legend(title="ln(MSQD: Landings)")) +
  scale_x_continuous(name = "PSDN: Prob(Presence)") + scale_y_continuous(name = "MSQD: Prob(Presence)")
  gg_int_2$facet$params$labeller <- port_label

gg_int / gg_int_2


### Interaction effects Squid v/s Anchovy ###

conditional_effects_nanc_msqd_sdm_cluster <- (conditional_effects(
  fit_qMSQD, "NANC_SDM_20_z:MSQD_SPAWN_SDM_90_z", 
  surface=TRUE, 
  conditions = conditions_cluster, re_formula = NULL))

conditional_effects_nanc_msqd_sdm_port <- (conditional_effects(
  fit_qMSQD, "NANC_SDM_20_z:MSQD_SPAWN_SDM_90_z",
  surface=TRUE,
  conditions = conditions_port, re_formula = NULL))

# Plot
gg_int <- plot(conditional_effects_nanc_msqd_sdm_cluster, plot = FALSE)[[2]] + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)) + 
  ggtitle("(a) Clusters") +  
  theme(plot.title = element_text(size=9, face="bold.italic")) + 
  guides(colour=guide_legend(title="ln(MSQD: Landings)")) +
  scale_x_continuous(name = "NANC: Prob(Presence)") + scale_y_continuous(name = "MSQD: Prob(Presence)")
gg_int$facet$params$labeller <- cluster_label

gg_int_2 <- plot(conditional_effects_nanc_msqd_sdm_port, plot = FALSE)[[2]] +
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)) +
  ggtitle("(b) Ports") +
  theme(plot.title = element_text(size=9, face="bold.italic")) +
  guides(colour=guide_legend(title="ln(MSQD: Landings)")) +
  scale_x_continuous(name = "NANC: Prob(Presence)") + scale_y_continuous(name = "MSQD: Prob(Presence)")
 gg_int_2$facet$params$labeller <- port_label

gg_int / gg_int_2



##########################################################################
### Predictions ###

#### Using the data estimation
set.seed(123)
prediction <- cbind(predict(fit_qMSQD), dataset_msqd_landing)

prediction_sel <- prediction[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]

# prediction_sel %>% group_by(group_all) %>%
#   mutate(mean.y = mean(ln_MSQD_Landings)) %>%
#   summarize(SSR = sum((ln_MSQD_Landings - Estimate.logMSQDLandings)^2), 
#             SST = sum((ln_MSQD_Landings - mean.y)^2)) %>%
#   mutate(R2 = 1 - (SSR / SST))

prediction_sel %>% group_by(group_all) %>%
  summarize(SSR = mean(Est.Error.logMSQDLandings))

# by cluster
df_cluster <- prediction_sel %>% 
  dplyr::select(Estimate.logMSQDLandings, ln_MSQD_Landings, Date, group_all, VESSEL_NUM) %>%
  group_by(Date, group_all, VESSEL_NUM) %>% 
  summarise(Est_landings = sum(Estimate.logMSQDLandings), Landings = sum(ln_MSQD_Landings)) %>%
  group_by(Date, group_all) %>% 
  summarise(Est_landings_mean = mean(Est_landings), Landings_mean = mean(Landings)) %>%
  group_by(group_all) %>%
  arrange(group_all, Date) %>%
  mutate(Est_landings_mean_MA = rollapply(data = Est_landings_mean, 
    width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(Landings_mean_MA     = rollapply(data = Landings_mean    , 
    width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))

cond_label <- as_labeller(c("1" = "Southern CCS small-scale squid-specialists",
                            "2" = "Southern CCS small-scale CPS-opportunists",
                            "4" = "Southern CCS industrial squid-specialists",
                            "5" = "Roving industrial sardine-squid switchers",
                            "7" = "Southern CCS forage fish diverse"))

ggplot(df_cluster) + 
  geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings"), size = 0.75) + 
  geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Estimated landings (MA)"), size = 1) +
  facet_wrap(~group_all, labeller = cond_label) +
  scale_x_continuous(name = "Date")  +
  scale_y_continuous(name = "ln(Landings)") +
  scale_color_manual(name = "Variable: ",
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85"))


# by port
library(zoo)
df_port <- prediction_sel %>% 
  dplyr::select(Estimate.logMSQDLandings, ln_MSQD_Landings, Date, PORT_AREA_CODE, VESSEL_NUM) %>%
  group_by(Date, PORT_AREA_CODE, VESSEL_NUM) %>% 
  summarise(Est_landings = sum(Estimate.logMSQDLandings), Landings = sum(ln_MSQD_Landings)) %>%
  group_by(Date, PORT_AREA_CODE) %>% 
  summarise(Est_landings_mean = mean(Est_landings), Landings_mean = mean(Landings)) %>%
  group_by(PORT_AREA_CODE) %>%
  arrange(PORT_AREA_CODE, Date) %>%
  mutate(Est_landings_mean_MA = rollapply(data = Est_landings_mean, 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(Landings_mean_MA     = rollapply(data = Landings_mean    , 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))


port_label <- as_labeller(c("LAA" = "Los Angeles",
                            "MNA" = "Monterey",
                            "SBA" = "Santa Barbara"))

ggplot(df_port) + 
  geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings")) + 
  geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Estimated landings (MA)"), size = 1, linetype = "dashed") +
  facet_wrap(~PORT_AREA_CODE, labeller = port_label) +
  scale_x_continuous(name = "Date")  +
  scale_y_continuous(name = "ln(Landings)") +
  scale_color_manual(name = "Variable: ",
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85"))



### Predict if sardine would have been open
# prediction_mod <- cbind(predict(fit_qMSQD_Spawning, newdata = data.frame(MSQD_Landings = dataset_msqd$MSQD_Landings,
#                                            MSQD_SPAWN_SDM_90 = dataset_msqd$MSQD_SPAWN_SDM_90,
#                                            PSDN_SDM.Open = dataset_msqd$PSDN_SDM.Open,
#                                            cluster = dataset_msqd$cluster,
#                                            port_ID = dataset_msqd$port_ID, 
#                                            PSDN.Closure = 0), dataset_msqd) 
# 

# # NOT RUN {
# ## fit a model
# fit <- brm(rating ~ treat + period + carry + (1|subject), 
#            data = inhaler)
# 
# ## compute expected predictions
# fitted_values <- fitted(fit)
# head(fitted_values)
# 
# ## plot expected predictions against actual response
# dat <- as.data.frame(cbind(Y = standata(fit)$Y, fitted_values))
# ggplot(dat) + geom_point(aes(x = Estimate, y = Y))
# # }
# # NOT RUN {
# # }



