#####################
## OLD ESTIMATIONS ##
#####################
# fit_qMSQD <-
#   brm(data = dataset_msqd_landing,
#       formula = log(MSQD_Landings) ~
#         1 + MSQD_SPAWN_SDM_90_z  + MSQD_Price_z + Length_z +
#         (1 | port_ID) + (1 | cluster),
#       prior = c(
#         prior(normal(0, 1), class = b),
#         prior(exponential(1), class = sigma)),
#       control = list(adapt_delta = 0.90, max_treedepth = 12),
#       chains = 2,
#       family = gaussian,
#       cores = 4,
#       file = "Estimations/fit_qMSQD")
# 
# fit_qMSQD_b <-
#   brm(data = dataset_msqd_landing,
#       formula = log(MSQD_Landings) ~
#         1 + MSQD_SPAWN_SDM_90  + MSQD_Price + Length +
#         (1 | port_ID) + (1 | cluster),
#       prior = c(
#         prior(normal(0, 1), class = b),
#         prior(exponential(1), class = sigma)),
#       control = list(adapt_delta = 0.90, max_treedepth = 12),
#       chains = 2,
#       family = gaussian,
#       cores = 4,
#       file = "Estimations/fit_qMSQD_b")

### Model Comparision ###
# tab_model(fit_qMSQD, fit_qMSQD_b)

# fit_qMSQD   <- add_criterion(fit_qMSQD,   "loo")
# fit_qMSQD_b <- add_criterion(fit_qMSQD_b, "loo")


# # w <- as.data.frame(
# loo_compare(fit_qMSQD,
#             fit_qMSQD_b,
#             criterion = "loo")
# )
# gs4_create("LOO", sheets = w)


# ### Add sardine SDM  ###
# landing_model_b <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z +
#                         PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open +
#                           (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z +
#                         PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open || cluster))
# 
# fit_qMSQD_endog_b <- readRDS(here::here("Estimations", "fit_qMSQD_endog_b.RDS"))
# 
# 
# ### Only interaction ###
# landing_model_c <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z +
#                         PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z +
#                           (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z +
#                         PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z || cluster))
# 
# fit_qMSQD_endog_c <- readRDS(here::here("Estimations", "fit_qMSQD_endog_c.RDS"))
# 
# ### Without interaction ###
# landing_model_d <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z
#                       + PSDN_SDM_60_z:PSDN.Open
#                       + (1 | port_ID)
#                       + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z
#                       + PSDN_SDM_60_z:PSDN.Open || cluster))
# 
# fit_qMSQD_endog_d <- readRDS(here::here("Estimations", "fit_qMSQD_endog_d.RDS"))


fit_qMSQD_endog_b_2 <- readRDS(here::here("Estimations", "fit_qMSQD_endog_b_2.RDS"))


# ### Only interaction ###
# landing_model_c_2 <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z
#                         + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN.Open 
#                         + (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
#                                            + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN.Open || cluster))
# 
# fit_qMSQD_endog_c_2 <- readRDS(here::here("Estimations", "fit_qMSQD_endog_c_2.RDS"))
# 
# 
# ### Without interaction ###
# landing_model_d_2 <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
#                       + PSDN_SDM_60_z:PSDN.Open + PSDN.Open
#                       + (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
#                       + PSDN_SDM_60_z:PSDN.Open + PSDN.Open || cluster))
# 
# fit_qMSQD_endog_d_2 <- readRDS(here::here("Estimations", "fit_qMSQD_endog_d_2.RDS"))




# #####################################################################################################
# 
# ### Preferred model with diesel price by port
# landing_model_b_3 <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
#                        + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open + PSDN.Open
#                        + diesel.price.AFI_z
#                        + (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
#                        + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open + PSDN.Open
#                        + diesel.price.AFI_z || cluster))
# 
# fit_qMSQD_endog_b_3 <- readRDS(here::here("Estimations", "fit_qMSQD_endog_b_3.RDS"))
# 
# 
# ### Using average number of set
# class(dataset_msqd_landing$LANDING_YEAR)
# landing_model_b_4 <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
#                         + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open + PSDN.Open
#                         + avg_set_MSQD_z
#                         + (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
#                                            + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open + PSDN.Open
#                                            + avg_set_MSQD_z || cluster))
# 
# fit_qMSQD_endog_b_4 <- readRDS(here::here("Estimations", "fit_qMSQD_endog_b_4.RDS"))
# 
# 
# ### Using year trend 
# landing_model_b_5 <- bf(log(MSQD_Landings) ~ 1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
#                         + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open + PSDN.Open
#                         + LANDING_YEAR
#                         + (1 | port_ID) + (1 + MSQD_SPAWN_SDM_90_z + MSQD_Price_z + Length_z 
#                                            + PSDN_SDM_60_z:PSDN.Open:MSQD_SPAWN_SDM_90_z + PSDN_SDM_60_z:PSDN.Open + PSDN.Open
#                                            + LANDING_YEAR || cluster))
# 
# fit_qMSQD_endog_b_5 <- readRDS(here::here("Estimations", "fit_qMSQD_endog_b_5.RDS"))

fit_qMSQD_endog_b_6 <- readRDS(here::here("Estimations", "fit_qMSQD_endog_b_6.RDS"))