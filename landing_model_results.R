###############################
### Landing result analysis ###
###############################

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
library("patchwork")
library('ggplot2')
library('ggthemes')
library('tibble')
library('XML')
theme_set(theme_sjplot())

##### Read landing models
fit_qMSQD <- readRDS(here::here("Estimations", "fit_qMSQD.RDS"))
fit_qPSDN <- readRDS(here::here("Estimations", "fit_qPSDN.RDS"))
fit_qNANC <- readRDS(here::here("Estimations", "fit_qNANC2.RDS"))

#### Read database 
dataset_msqd_landing <- read.csv(file ="C:\\Data\\PacFIN data\\dataset_estimation_MSQD.csv")
dataset_nanc_landing <- read.csv(file ="C:\\Data\\PacFIN data\\dataset_estimation_NANC.csv")
dataset_psdn_landing <- read.csv(file ="C:\\Data\\PacFIN data\\dataset_estimation_PSDN.csv")


# fit_qMSQD[["prior"]][["coef"]]
# fit_qMSQD[["prior"]][["prior"]]
# fit_qPSDN[["prior"]][["coef"]]
# fit_qPSDN[["prior"]][["prior"]]
# fit_qNANC[["prior"]][["coef"]]
# fit_qNANC[["prior"]][["prior"]]


# summary(fit_qMSQD)
# summary(fit_qPSDN)
# summary(fit_qNANC)

############################
# Calculate R2

# ## Compute Bayesian R2
# y_pred <- brms::posterior_linpred(fit_qMSQD, resp = 'logMSQDLandings') 
# var_fit <- apply(y_pred, 1, var)
# var_res <- as.matrix(fit_qMSQD, pars = c("sigma"))^2
# rsq_bayes <- as.data.frame(var_fit / (var_fit + var_res))
# hist(rsq_bayes$sigma_logMSQDLandings)
# print(c(median(rsq_bayes$sigma_logMSQDLandings), mean(rsq_bayes$sigma_logMSQDLandings),
#         sd(rsq_bayes$sigma_logMSQDLandings)))
# 
# cluster_groups <- fit_qMSQD$data %>% dplyr::select("port_cluster_ID") %>% unique()
# list = as.list(cluster_groups$port_cluster_ID)
# 
# for (p in list) {
# fitdata <- subset(fit_qMSQD$data, port_cluster_ID == p)
# newdf <- data.frame(
#   port_cluster_ID = p,
#   MSQD_Price_z = fitdata$MSQD_Price_z,
#   Price.Fishmeal.AFI_z = fitdata$Price.Fishmeal.AFI_z,
#   MSQD_Landings = fitdata$MSQD_Landings,
#   MSQD_SPAWN_SDM_90 = fitdata$MSQD_SPAWN_SDM_90,
#   MSQD_Price_z = fitdata$MSQD_Price_z,
#   PSDN_SDM_60 = fitdata$PSDN_SDM_60,
#   PSDN.Open = fitdata$PSDN.Open,
#   NANC_SDM_20 = fitdata$NANC_SDM_20,
#   PSDN.Total.Closure = fitdata$PSDN.Total.Closure,
#   Length_z = fitdata$Length_z)
# fit_qMSQD_subset <- extract_draws(fit_qMSQD, newdata = newdf, allow_new_levels = T)
# y_pred <- brms::posterior_linpred(fit_qMSQD, newdata = newdf, allow_new_levels = T, resp = 'logMSQDLandings')
# var_fit <- apply(y_pred, 1, var)
# var_res <- as.matrix(fit_qMSQD_subset$resps$logMSQDLandings$dpars$sigma)^2
# rsq_bayes <- as.data.frame(var_fit / (var_fit + var_res))
# print(mean(rsq_bayes$V1))
# print(p)
# }
# 
# y_pred <- brms::posterior_linpred(fit_qPSDN, resp = 'logPSDNLandings') 
# var_fit <- apply(y_pred, 1, var)
# var_res <- as.matrix(fit_qPSDN, pars = c("sigma"))^2
# rsq_bayes <- as.data.frame(var_fit / (var_fit + var_res))
# hist(rsq_bayes$sigma_logPSDNLandings)
# print(c(median(rsq_bayes$sigma_logPSDNLandings), mean(rsq_bayes$sigma_logPSDNLandings),
#         sd(rsq_bayes$sigma_logPSDNLandings)))
# 
# cluster_groups <- fit_qPSDN$data %>% dplyr::select("port_cluster_ID") %>% unique()
# list = as.list(cluster_groups$port_cluster_ID)
# 
# for (p in list) {
#   fitdata <- subset(fit_qPSDN$data, port_cluster_ID == p)
#   newdf <- data.frame(
#     port_cluster_ID = p,
#     PSDN_Price_z = fitdata$PSDN_Price_z,
#     Price.Fishmeal.AFI_z = fitdata$Price.Fishmeal.AFI_z,
#     PSDN_Landings = fitdata$PSDN_Landings,
#     MSQD_SPAWN_SDM_90 = fitdata$MSQD_SPAWN_SDM_90,
#     PSDN_SDM_60 = fitdata$PSDN_SDM_60,
#     MSQD.Open = fitdata$MSQD.Open,
#     NANC_SDM_20 = fitdata$NANC_SDM_20,
#     WA.Restriction = fitdata$WA.Restriction,
#     Length_z = fitdata$Length_z)
#   fit_qPSDN_subset <- extract_draws(fit_qPSDN, newdata = newdf, allow_new_levels = T)
#   y_pred <- brms::posterior_linpred(fit_qPSDN, newdata = newdf, allow_new_levels = T, resp = 'logPSDNLandings')
#   var_fit <- apply(y_pred, 1, var)
#   var_res <- as.matrix(fit_qPSDN_subset$resps$logPSDNLandings$dpars$sigma)^2
#   rsq_bayes <- as.data.frame(var_fit / (var_fit + var_res))
#   print(median(rsq_bayes$V1))
#   print(p)
# }
# 
# y_pred <- brms::posterior_linpred(fit_qNANC, resp = 'logNANCLandings') 
# var_fit <- apply(y_pred, 1, var)
# var_res <- as.matrix(fit_qNANC, pars = c("sigma"))^2
# rsq_bayes <- as.data.frame(var_fit / (var_fit + var_res))
# hist(rsq_bayes$sigma_logNANCLandings)
# print(c(median(rsq_bayes$sigma_logNANCLandings), mean(rsq_bayes$sigma_logNANCLandings),
#         sd(rsq_bayes$sigma_logNANCLandings)))
# 
# 
# cluster_groups <- fit_qNANC$data %>% dplyr::select("port_cluster_ID") %>% unique()
# list = as.list(cluster_groups$port_cluster_ID)
# 
# for (p in list) {
#   fitdata <- subset(fit_qNANC$data, port_cluster_ID == p)
#   newdf <- data.frame(data.frame(
#     port_cluster_ID = p,
#     NANC_Price_z = fitdata$NANC_Price_z,
#     Price.Fishmeal.AFI_z = fitdata$Price.Fishmeal.AFI_z,
#     NANC_Landings = fitdata$NANC_Landings,
#     MSQD_SPAWN_SDM_90 = fitdata$MSQD_SPAWN_SDM_90,
#     PSDN_SDM_60 = fitdata$PSDN_SDM_60,
#     PSDN.Open = fitdata$PSDN.Open,
#     MSQD.Open = fitdata$MSQD.Open,
#     NANC_SDM_20 = fitdata$NANC_SDM_20,
#     PSDN.Total.Closure = fitdata$PSDN.Total.Closure,
#     Length_z = fitdata$Length_z))
#   fit_qNANC_subset <- extract_draws(fit_qNANC, newdata = newdf, allow_new_levels = T)
#   y_pred <- brms::posterior_linpred(fit_qNANC, newdata = newdf, allow_new_levels = T, resp = 'logNANCLandings')
#   var_fit <- apply(y_pred, 1, var)
#   var_res <- as.matrix(fit_qNANC_subset$resps$logNANCLandings$dpars$sigma)^2
#   rsq_bayes <- as.data.frame(var_fit / (var_fit + var_res))
#   print(mean(rsq_bayes$V1))
#   print(p)
# }


############################
# Calculate estimation error

# Calculate average error
# set.seed(123)
# predict1 <- as.data.frame(predict(fit_qMSQD))
# prediction1 <- cbind(predict1, dataset_msqd_landing)
# sqrt(sum((prediction1$Estimate.logMSQDLandings - prediction1$ln_MSQD_Landings)^2)/(nrow(prediction1)-2))

# predict2 <- as.data.frame(predict(fit_qPSDN))
# prediction2 <- cbind(predict2, dataset_psdn_landing)
# sqrt(sum((prediction2$Estimate.logPSDNLandings - prediction2$ln_PSDN_Landings)^2)/(nrow(prediction2)-2))

# predict3 <- as.data.frame(predict(fit_qNANC))
# prediction3 <- cbind(predict3, dataset_nanc_landing)
# sqrt(sum((prediction3$Estimate.logNANCLandings - prediction3$ln_NANC_Landings)^2)/(nrow(prediction3)-2))


##############################################################################################
## LOO comparision between models 

# LOO(
#   fit_qMSQD_FINAL_flat,
#   fit_qMSQD_FINAL_lognormal)


###############################################
### Analyze convergence ###

# launch_shinystan(fit_qMSQD)
# launch_shinystan(fit_qPSDN)
# launch_shinystan(fit_qNANC)



###############################################
### Create result tables ###


# tab_model <-
#   sjPlot::tab_model(fit_qMSQD)
# df <- data.frame(readHTMLTable(htmlParse(tab_model))[1])
# colnames(df) <- df[1,]
# df <- df[-1,]
# # gs4_create("MSQD_landings_results", sheets = df)
# 
# 
# tab_model <-
#   sjPlot::tab_model(fit_qPSDN)
# df <- data.frame(readHTMLTable(htmlParse(tab_model))[1])
# colnames(df) <- df[1,]
# df <- df[-1,]
# #gs4_create("PSDN_landings_results", sheets = df)
# 
# 
# tab_model <-
#   sjPlot::tab_model(fit_qNANC)
# df <- data.frame(readHTMLTable(htmlParse(tab_model))[1])
# colnames(df) <- df[1,]
# df <- df[-1,]
# #gs4_create("NANC_landings_results", sheets = df)


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
#   dplyr::select(MSQD_SPAWN_SDM_90,
#                 PSDN_SDM_60,
#                 NANC_SDM_20,
#                 Length_z,
#                 MSQD_Price_z, 
#                 Price.Fishmeal.AFI_z)
# res <- as.data.frame(cor(dataset_select))
# round(res, 2)

# gs4_create("correlation_exp_variables_MSQD_landings", sheets = res)



#####################################################
## Model summary ##

### Posterior predictive check ###
gg1 <- pp_check(fit_qMSQD, resp = "logMSQDLandings") +
  scale_color_manual(name = "", values = c("y" = "royalblue4", "yrep" = "azure3"),
                     labels = c("y" = "Observed", "yrep" = "Replicated")) +
  theme(legend.position = "none", plot.title = element_text(size=12, face="bold.italic"))  +
  xlim(-5, 12) + xlab("") + ggtitle("(a) Market squid")

gg3 <- pp_check(fit_qNANC, resp = "logNANCLandings") +
  scale_color_manual(name = "", values = c("y" = "royalblue4", "yrep" = "azure3"),
                     labels = c("y" = "Observed", "yrep" = "Replicated")) +
  theme(legend.position="right", plot.title = element_text(size=12, face="bold.italic"))  +
  xlim(-5, 12) + xlab("") + ggtitle("(c) Northern anchovy")

gg2 <- pp_check(fit_qPSDN, resp = "logPSDNLandings") +
  scale_color_manual(name = "", values = c("y" = "royalblue4", "yrep" = "azure3"),
                     labels = c("y" = "Observed", "yrep" = "Replicated")) +
  theme(legend.position="none", plot.title = element_text(size=12, face="bold.italic"))  +
  xlim(-5, 12) + xlab("ln(Landing)") + ggtitle("(b) Pacific sardine")

gg1 + gg2 + gg3

#------------------------------------------------------
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



######################################################################
############# Explanatory variables by clusters-port ################
#####################################################################


###################################################################################
### Figure 10. Price effect

coeff_cluster <- as.data.frame(coef(fit_qMSQD)$port_cluster_ID[, c(1, 3:4), 3]) %>%
  round(digits = 2) 
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)  


gg_2_MSDQ <- ggplot(coeff_cluster, aes(x=Estimate, y=cluster)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) +
  ggtitle("(a) Price effect on squid landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("1-LAA" = "Southern CCS small-scale\nsquid-specialists (Los Angeles)",
                            "1-SBA" = "Southern CCS small-scale\nsquid-specialists (Santa Barbara)",
                            "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                            "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                            "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                            "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                            "5-SBA" = "Roving industrial sardine-squid\ngeneralists (Santa Barbara)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)",
                            "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 


coeff_cluster <- as.data.frame(coef(fit_qNANC)$port_cluster_ID[, c(1, 3:4), 3]) %>%
  round(digits = 2) 
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)  


gg_2_NANC <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) +
  ggtitle("(c) Price effect on anchovy landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("6-CWA" = "PNW sardine specialists\n(Coastal Washington Ports)",
                            "6-CLW" = "PNW sardine specialists\n(Columbia River OR)",
                            "6-CLO" = "PNW sardine specialists\n(Columbia River WA)",
                            "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)",
                            "7-SDA" = "Southern CCS forage fish\ndiverse (San Diego)",
                            "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 



coeff_cluster <- as.data.frame(coef(fit_qPSDN)$port_cluster_ID[, c(1, 3:4), 3]) %>%
  round(digits = 2) 
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)  


gg_2_PSDN <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) +
  ggtitle("(b) Price effect on sardine landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("3-CLO" = "PNW sardine opportunists\n(Columbia River OR)",
                            "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                            "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                            "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                            "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                            "5-CLO" = "Roving industrial sardine-squid\ngeneralists (Columbia River OR)",
                            "5-CLW" = "Roving industrial sardine-squid\ngeneralists (Columbia River WA)",
                            "6-CLO" = "PNW sardine specialists\n(Columbia River OR)",
                            "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 


gg_2_MSDQ  + gg_2_PSDN + gg_2_NANC


# ##############################################################
# ### Price effect V2
# coeff_cluster_MSQD <- as.data.frame(coef(fit_qMSQD)$port_cluster_ID[, c(1, 3:4), 3]) %>%
#   round(digits = 2) %>% mutate(species = "MSQD")
# 
# coeff_cluster_NANC <- as.data.frame(coef(fit_qNANC)$port_cluster_ID[, c(1, 3:4), 3]) %>%
#   round(digits = 2) %>% mutate(species = "NANC")
# 
# coeff_cluster_PSDN <- as.data.frame(coef(fit_qPSDN)$port_cluster_ID[, c(1, 3:4), 3]) %>%
#   round(digits = 2) %>% mutate(species = "PSDN")
# 
# coeff_cluster <- rbind.data.frame(coeff_cluster_MSQD, coeff_cluster_NANC, coeff_cluster_PSDN)
# 
# cluster <- rownames(coeff_cluster)
# rownames(coeff_cluster) <- NULL
# coeff_cluster <- cbind(cluster,coeff_cluster)  %>%
#   mutate(cluster = substr(cluster, 1, 5))
# 
# effect_names <- as_labeller(c(`MSQD` = "(a) Price effect on squid landings", 
#                               `PSDN` = "(b) Price effect on sardine landings",
#                               `NANC` = "(c) Price effect on anchovy landings"))
# 
# 
# ggplot(coeff_cluster, aes(x=Estimate, y=cluster)) +
#   geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
#                                 width=.2, position=position_dodge(0.05)) + 
#   facet_grid(~ species, labeller = effect_names) + 
#   xlab("") + ylab("") +
#   scale_y_discrete(labels=c("1-LAA" = "Southern CCS small-scale squid-specialists (Los Angeles)",
#                             "1-SBA" = "Southern CCS small-scale squid-specialists (Santa Barbara)",
#                             "4-LAA" = "Southern CCS industrial squid-specialists (Los Angeles)",
#                             "4-MNA" = "Southern CCS industrial squid-specialists (Monterey)",
#                             "4-SBA" = "Southern CCS industrial squid-specialists (Santa Barbara)",
#                             "5-LAA" = "Roving industrial sardine-squid generalists (Los Angeles)",
#                             "5-SBA" = "Roving industrial sardine-squid generalists (Santa Barbara)",
#                             "7-LAA" = "Southern CCS forage fish diverse (Los Angeles)",
#                             "7-SBA" = "Southern CCS forage fish diverse (Santa Barbara)",
#                             "7-SDA" = "Southern CCS forage fish diverse (San Diego)",
#                             "7-MNA" = "Southern CCS forage fish diverse (Monterey)",
#                             "6-CWA" = "PNW sardine specialists (Coastal Washington Ports)",
#                             "6-CLW" = "PNW sardine specialists (Columbia River OR)",
#                             "6-CLO" = "PNW sardine specialists (Columbia River WA)",
#                             "3-CLO" = "PNW sardine opportunists (Columbia River OR)",
#                             "5-CLO" = "Roving industrial sardine-squid generalists (Columbia River OR)",
#                             "5-CLW" = "Roving industrial sardine-squid generalists (Columbia River WA)")) +
#   geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 


###################################################################################
### Figure 11. Closure effect

coeff_cluster <- as.data.frame(coef(fit_qMSQD)$port_cluster_ID[, c(1, 3:4), 5]) %>%
  round(digits = 2) 
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)  


gg_3_MSDQ <- 
  
  ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) +
  ggtitle("(a) PSDN Closure effect on squid landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("1-LAA" = "Southern CCS small-scale\nsquid-specialists (Los Angeles)",
                            "1-SBA" = "Southern CCS small-scale\nsquid-specialists (Santa Barbara)",
                            "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                            "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                            "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                            "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                            "5-SBA" = "Roving industrial sardine-squid\ngeneralists (Santa Barbara)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)",
                            "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 


coeff_cluster <- as.data.frame(coef(fit_qNANC)$port_cluster_ID[, c(1, 3:4), 4]) %>%
  round(digits = 2) 
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)  

gg_3_NANC <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + 
  ggtitle("(b)  PSDN Closure effect on anchovy landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("6-CWA" = "PNW sardine specialists\n(Coastal Washington Ports)",
                            "6-CLW" = "PNW sardine specialists\n(Columbia River OR)",
                            "6-CLO" = "PNW sardine specialists\n(Columbia River WA)",
                            "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)",
                            "7-SDA" = "Southern CCS forage fish\ndiverse (San Diego)",
                            "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 


gg_3_MSDQ + gg_3_NANC 


########################################################
### Figure 12. SDM effect

coeff_cluster <- as.data.frame(coef(fit_qMSQD)$port_cluster_ID[, c(1, 3:4), 2]) %>%
  round(digits = 2)

cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)


gg_1_MSDQ <- 
  ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(a) Pr(MSQD) effect on squid landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("1-LAA" = "Southern CCS small-scale\nsquid-specialists (Los Angeles)",
                            "1-SBA" = "Southern CCS small-scale\nsquid-specialists (Santa Barbara)",
                            "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                            "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                            "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                            "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                            "5-SBA" = "Roving industrial sardine-squid\ngeneralists (Santa Barbara)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)",
                            "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)


coeff_cluster <- as.data.frame(coef(fit_qNANC)$port_cluster_ID[, c(1, 3:4), 2]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)

gg_1_NANC <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(c) Pr(NANC) effect on anchovy landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("6-CWA" = "PNW sardine specialists\n(Coastal Washington Ports)",
                            "6-CLW" = "PNW sardine specialists\n(Columbia River OR)",
                            "6-CLO" = "PNW sardine specialists\n(Columbia River WA)",
                            "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)",
                            "7-SDA" = "Southern CCS forage fish\ndiverse (San Diego)",
                            "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)


coeff_cluster <- as.data.frame(coef(fit_qPSDN)$port_cluster_ID[, c(1, 3:4), 2]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)

gg_1_PSDN <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(b) Pr(PSDN) effect on sardine landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("3-CLO" = "PNW sardine opportunists\n(Columbia River OR)",
                            "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                            "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                            "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                            "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                            "5-CLO" = "Roving industrial sardine-squid\ngeneralists (Columbia River OR)",
                            "5-CLW" = "Roving industrial sardine-squid\ngeneralists (Columbia River WA)",
                            "6-CLO" = "PNW sardine specialists\n(Columbia River OR)",
                            "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)


gg_1_MSDQ +  gg_1_PSDN + gg_1_NANC



#####################################
#### Effect of other species SDM ####
#####################################


coef(fit_qMSQD)$port_cluster_ID
coef(fit_qPSDN)$port_cluster_ID
coef(fit_qNANC)$port_cluster_ID

#############################
### Figure 13. Market squid 

# Effect NANC on MSQD ###
coeff_cluster <- as.data.frame(coef(fit_qMSQD)$port_cluster_ID[, c(1, 3:4), 4]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)
gg_NANC <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(b) Pr(NANC) effect on squid landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("1-LAA" = "Southern CCS small-scale\nsquid-specialists (Los Angeles)",
                            "1-SBA" = "Southern CCS small-scale\nsquid-specialists (Santa Barbara)",
                            "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                            "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                            "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                            "5-SBA" = "Roving industrial sardine-squid\ngeneralists (Santa Barbara)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)

# Effect PSDN on MSQD ###
coeff_cluster <- as.data.frame(coef(fit_qMSQD)$port_cluster_ID[, c(1, 3:4), 7]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)


gg_PSDN <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(a) Pr(PSDN) effect on squid landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("1-LAA" = "Southern CCS small-scale\nsquid-specialists (Los Angeles)",
                            "1-SBA" = "Southern CCS small-scale\nsquid-specialists (Santa Barbara)",
                            "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                            "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                            "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                            "5-SBA" = "Roving industrial sardine-squid\ngeneralists (Santa Barbara)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)

# Interaction effects NANC v/s SQUID on MSQD ###
coeff_cluster <- as.data.frame(coef(fit_qMSQD)$port_cluster_ID[, c(1, 3:4), 6]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)

gg_NANC_MSQD <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(d) Pr(NANC) x Pr(MSQD) effect on squid landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("1-LAA" = "Southern CCS small-scale\nsquid-specialists (Los Angeles)",
                            "1-SBA" = "Southern CCS small-scale\nsquid-specialists (Santa Barbara)",
                            "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                            "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                            "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                            "5-SBA" = "Roving industrial sardine-squid\ngeneralists (Santa Barbara)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)

# Interaction effects PSDN v/s SQUID on MSQD
coeff_cluster <- as.data.frame(coef(fit_qMSQD)$port_cluster_ID[, c(1, 3:4), 8]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)

gg_PSDN_MSQD <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(c) Pr(PSDN) x Pr(MSQD) effect on squid landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("1-LAA" = "Southern CCS small-scale\nsquid-specialists (Los Angeles)",
                            "1-SBA" = "Southern CCS small-scale\nsquid-specialists (Santa Barbara)",
                            "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                            "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                            "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                            "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                            "5-SBA" = "Roving industrial sardine-squid\ngeneralists (Santa Barbara)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)",
                            "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)


(gg_PSDN + gg_NANC)  / (gg_PSDN_MSQD + gg_NANC_MSQD) 



#############################
#### Figure 15. Pacific Sardine

# Effect MSQD on PSDN
coeff_cluster <- as.data.frame(coef(fit_qPSDN)$port_cluster_ID[, c(1, 3:4), 7]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)


gg_MSQD <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(a) Pr(MSQD) effect on sardine landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("3-CLO" = "PNW sardine opportunists\n(Columbia River OR)",
                            "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                            "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                            "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                            "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                            "5-CLO" = "Roving industrial sardine-squid\ngeneralists (Columbia River OR)",
                            "5-CLW" = "Roving industrial sardine-squid\ngeneralists (Columbia River WA)",
                            "6-CLO" = "PNW sardine specialists\n(Columbia River OR)",
                            "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)



# Effect NANC on PSDN
coeff_cluster <- as.data.frame(coef(fit_qPSDN)$port_cluster_ID[, c(1, 3:4), 4]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)


gg_NANC <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(b) Pr(NANC) effect on sardine landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("3-CLO" = "PNW sardine opportunists\n(Columbia River OR)",
                            "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                            "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                            "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                            "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                            "5-CLO" = "Roving industrial sardine-squid\ngeneralists (Columbia River OR)",
                            "5-CLW" = "Roving industrial sardine-squid\ngeneralists (Columbia River WA)",
                            "6-CLO" = "PNW sardine specialists\n(Columbia River OR)",
                            "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)


# Interaction effects PSDN v/s NANC on PSDN
coeff_cluster <- as.data.frame(coef(fit_qPSDN)$port_cluster_ID[, c(1, 3:4), 6]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)

gg_PSDN_NANC <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(d) Pr(NANC) x Pr(PSDN) effect on sardine landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("3-CLO" = "PNW sardine opportunists\n(Columbia River OR)",
                            "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                            "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                            "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                            "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                            "5-CLO" = "Roving industrial sardine-squid\ngeneralists (Columbia River OR)",
                            "5-CLW" = "Roving industrial sardine-squid\ngeneralists (Columbia River WA)",
                            "6-CLO" = "PNW sardine specialists\n(Columbia River OR)",
                            "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)


# Interaction effects PSDN v/s SQUID on PSDN
coeff_cluster <- as.data.frame(coef(fit_qPSDN)$port_cluster_ID[, c(1, 3:4), 8]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)

gg_PSDN_MSQD <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(c) Pr(MSQD) x Pr(PSDN) effect on sardine landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("3-CLO" = "PNW sardine opportunists\n(Columbia River OR)",
                            "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                            "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                            "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                            "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                            "5-CLO" = "Roving industrial sardine-squid\ngeneralists (Columbia River OR)",
                            "5-CLW" = "Roving industrial sardine-squid\ngeneralists (Columbia River WA)",
                            "6-CLO" = "PNW sardine specialists\n(Columbia River OR)",
                            "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)


(gg_MSQD + gg_NANC)  / (gg_PSDN_MSQD + gg_PSDN_NANC) 



#################################
#### Figure 17. Northern anchovy

### Effect MSQD on NANC ###
coeff_cluster <- as.data.frame(coef(fit_qNANC)$port_cluster_ID[, c(1, 3:4), 5]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)


gg_MSQD <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(a) Pr(MSQD) effect on anchovy landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("6-CWA" = "PNW sardine specialists\n(Coastal Washington Ports)",
                            "6-CLW" = "PNW sardine specialists\n(Columbia River OR)",
                            "6-CLO" = "PNW sardine specialists\n(Columbia River WA)",
                            "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)",
                            "7-SDA" = "Southern CCS forage fish\ndiverse (San Diego)",
                            "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
    geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)


### Effect PSDN on NANC ###
coeff_cluster <- as.data.frame(coef(fit_qNANC)$port_cluster_ID[, c(1, 3:4), 6]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)


gg_PSDN <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(b) Pr(PSDN) effect on anchovy landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("6-CWA" = "PNW sardine specialists\n(Coastal Washington Ports)",
                            "6-CLW" = "PNW sardine specialists\n(Columbia River OR)",
                            "6-CLO" = "PNW sardine specialists\n(Columbia River WA)",
                            "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)",
                            "7-SDA" = "Southern CCS forage fish\ndiverse (San Diego)",
                            "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)


### Interaction effects NANC v/s SQUID on NANC ###
coeff_cluster <- as.data.frame(coef(fit_qNANC)$port_cluster_ID[, c(1, 3:4), 7]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)

gg_NANC_MSQD <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(c) Pr(NANC) x Pr(MSQD) effect on anchovy landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("6-CWA" = "PNW sardine specialists\n(Coastal Washington Ports)",
                            "6-CLW" = "PNW sardine specialists\n(Columbia River OR)",
                            "6-CLO" = "PNW sardine specialists\n(Columbia River WA)",
                            "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)",
                            "7-SDA" = "Southern CCS forage fish\ndiverse (San Diego)",
                            "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)


### Interaction effects NANC v/s PSDN on NANC ###
coeff_cluster <- as.data.frame(coef(fit_qNANC)$port_cluster_ID[, c(1, 3:4), 8]) %>%
  round(digits = 2)
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)

gg_NANC_PSDN <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + ggtitle("(d) Pr(NANC) x Pr(PSDN) effect on anchovy landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=9), axis.text.y = element_text(size = 9)) +
  scale_y_discrete(labels=c("6-CWA" = "PNW sardine specialists\n(Coastal Washington Ports)",
                            "6-CLW" = "PNW sardine specialists\n(Columbia River OR)",
                            "6-CLO" = "PNW sardine specialists\n(Columbia River WA)",
                            "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)",
                            "7-SDA" = "Southern CCS forage fish\ndiverse (San Diego)",
                            "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                            "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5)


(gg_MSQD + gg_PSDN)  / (gg_NANC_MSQD +  gg_NANC_PSDN) 


########################
### Marginal effects ###
########################

################################################
# Figure 14. Squid v/s Sardine on squid landings

conditions_port <- data.frame(port_cluster_ID = unique(dataset_msqd_landing$port_cluster_ID))
rownames(conditions_port) <-  conditions_port$port_cluster_ID

port_label <- as_labeller(c("LAA" = "Los Angeles",
                            "MNA" = "Monterey",
                            "SBA" = "Santa Barbara"))

port_label <- as_labeller(c("1-LAA" = "Southern CCS small-scale\nsquid-specialists (Los Angeles)",
                          "1-SBA" = "Southern CCS small-scale\nsquid-specialists (Santa Barbara)",
                          "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                          "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                          "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                          "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                          "5-SBA" = "Roving industrial sardine-squid\ngeneralists (Santa Barbara)",
                          "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)",
                          "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)"))

int_cond <- list(
  MSQD_SPAWN_SDM_90 = setNames(c(0.1, 0.5, 1), c("0.1", "0.5", "1"))
)

conditions_port1 <- conditions_port %>% 
  filter(port_cluster_ID == "5-SBA" | port_cluster_ID == "4-SBA")

conditional_effects_psdn_msqd_sdm <- (conditional_effects(
  fit_qMSQD, "PSDN_SDM_60:MSQD_SPAWN_SDM_90", 
  surface=FALSE, 
  conditions = conditions_port1, 
  int_conditions = int_cond,
  re_formula = NULL))

gg_int <- plot(conditional_effects_psdn_msqd_sdm, plot = FALSE)[[2]] + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)) + 
  ggtitle("SDM: Market squid x SDM: Pacific sardine") +  
  guides(fill=guide_legend(title="MSQD: Prob(Presence)")) +
  guides(colour=guide_legend(title="MSQD: Prob(Presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic")) + 
  scale_x_continuous(name = "PSDN: Prob(Presence)") + scale_y_continuous(name = "ln(MSQD: Landings)")
gg_int$facet$params$labeller <- port_label
gg_int



####################################################
# Figure 16. Anchovy v/s Sardine on sardine landings

conditions_port <- data.frame(port_cluster_ID = unique(dataset_psdn_landing$port_cluster_ID))
rownames(conditions_port) <-  conditions_port$port_cluster_ID

port_label <- as_labeller(c("3-CLO" = "PNW sardine opportunists (Columbia River OR)",
                          "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                          "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                          "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                          "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                          "5-CLO" = "Roving industrial sardine-squid\ngeneralists (Columbia River OR)",
                          "5-CLW" = "Roving industrial sardine-squid\nswitchers (Columbia River WA)",
                          "6-CLO" = "PNW sardine specialists (Columbia River OR)",
                          "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                          "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)"))

int_cond <- list(
  PSDN_SDM_60 = setNames(c(0.1, 0.5, 1), c("0.1", "0.5", "1"))
)

conditions_port2 <- conditions_port %>% 
  filter(port_cluster_ID == "4-LAA" | port_cluster_ID == "7-LAA")

conditional_effects_nanc_psdn_sdm_port <- (conditional_effects(
  fit_qPSDN, "NANC_SDM_20:PSDN_SDM_60", 
  surface=FALSE, 
  conditions = conditions_port2, 
  int_conditions = int_cond,
  re_formula = NULL))

gg_int_2 <- plot(conditional_effects_nanc_psdn_sdm_port, plot = FALSE)[[2]] + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)) + 
  ggtitle("SDM: Pacific sardine x SDM: Northern anchovy (Los Angeles)") +  
  theme(plot.title = element_text(size=9, face="bold.italic")) +
  guides(fill=guide_legend(title="PSDN: Prob(Presence)")) +
  guides(colour=guide_legend(title="PSDN: Prob(Presence)")) +
  scale_y_continuous(name = "ln(PSDN: Landings)") + 
  scale_x_continuous(name = "NANC: Prob(Presence)")
gg_int_2$facet$params$labeller <- port_label
gg_int_2


  ###########################################
  ### Conditional effect of MSQD presence ###
  ###########################################
  # 
  # conditions_cluster <- data.frame(cluster = unique(dataset_msqd_landing$cluster)) 
  # rownames(conditions_cluster) <- unique(dataset_msqd_landing$cluster)
  # conditions_cluster <- conditions_cluster %>% 
  #   arrange(-desc(cluster)) 
  # 
  # cluster_label <- as_labeller(c("1" = "Southern CCS small-scale squid-specialists",
  #                                "2" = "Southern CCS small-scale CPS-opportunists",
  #                                "3" = "Southern CCS industrial squid-specialists",
  #                                "4" = "Roving industrial sardine-squid switchers",
  #                                "5" = "Southern CCS forage fish diverse"))
  # 
  # 
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
  # 
  # gg1$facet$params$labeller <- cluster_label
  # gg1
  

##########################################################################
### Predictions ###

# Predict

# set.seed(123)
# prediction_MSQD <- cbind(predict(fit_qMSQD), dataset_msqd_landing)
# saveRDS(prediction_MSQD, file = "prediction_MSQD_v3.rds")
# 
# set.seed(123)
# prediction_NANC <- cbind(predict(fit_qNANC), dataset_nanc_landing)
# saveRDS(prediction_NANC, file = "prediction_NANC_v4.rds")
# 
# set.seed(123)
# prediction_PSDN <- cbind(predict(fit_qPSDN), dataset_psdn_landing)
# saveRDS(prediction_PSDN, file = "prediction_PSDN_v4.rds")

prediction_MSQD <- readRDS(file = "prediction_MSQD_v3.rds")
prediction_NANC <- readRDS(file = "prediction_NANC_v4.rds")
prediction_PSDN <- readRDS(file = "prediction_PSDN_v4.rds")



library(zoo)

####  Marker squid
prediction_sel <- prediction_MSQD[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]

# prediction_sel %>% group_by(port_cluster_ID) %>%
#   summarize(SSR = sum(abs(ln_MSQD_Landings - Estimate.logMSQDLandings))/n())
# 
# prediction_sel %>% group_by(port_cluster_ID) %>%
#   summarize(SSR = cor(ln_MSQD_Landings, Estimate.logMSQDLandings))
# 
# prediction_sel %>% group_by(port_cluster_ID) %>%
#   summarize(SSR = mean(Est.Error.logMSQDLandings))

prediction_sel %>% group_by(port_cluster_ID) %>%
  summarize(R_2 = (var(Estimate.logMSQDLandings) / 
                     (var(Estimate.logMSQDLandings) +  
                        var(Estimate.logMSQDLandings - ln_MSQD_Landings)))) %>% ungroup()



# by cluster
df_cluster_MSQD <- prediction_sel %>% 
  dplyr::select(Estimate.logMSQDLandings, ln_MSQD_Landings, 
                LANDING_YEAR, LANDING_MONTH, port_cluster_ID, VESSEL_NUM) %>%
  mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
  mutate(Date = zoo::as.yearmon(Date)) %>%
  group_by(Date, port_cluster_ID, VESSEL_NUM) %>% 
  summarise(Est_landings = sum(Estimate.logMSQDLandings), Landings = sum(ln_MSQD_Landings)) %>%
  group_by(Date, port_cluster_ID) %>% 
  summarise(Est_landings_mean = mean(Est_landings), Landings_mean = mean(Landings)) %>%
  group_by(port_cluster_ID) %>%
  arrange(port_cluster_ID, Date) %>%
  mutate(Est_landings_mean_MA = rollapply(data = Est_landings_mean, 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(Landings_mean_MA     = rollapply(data = Landings_mean    , 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))





#### Using the data estimation -- Northern anchovy

prediction_sel <- prediction_NANC[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]

# prediction_sel %>% group_by(port_cluster_ID) %>%
#   summarize(SSR = mean(Est.Error.logNANCLandings))
# 
# prediction_sel %>% group_by(port_cluster_ID) %>%
#   summarize(SSR = sum(abs(ln_NANC_Landings - Estimate.logNANCLandings))/n())
# 
# prediction_sel %>% group_by(port_cluster_ID) %>%
#   summarize(SSR = cor(ln_NANC_Landings, Estimate.logNANCLandings))

prediction_sel %>% group_by(port_cluster_ID) %>%
  summarize(R_2 = (var(Estimate.logNANCLandings) / 
                     (var(Estimate.logNANCLandings) +  
                        var(Estimate.logNANCLandings - ln_NANC_Landings)))) %>% ungroup()

df_cluster_NANC <- prediction_sel %>% 
  dplyr::select(Estimate.logNANCLandings, ln_NANC_Landings, 
                LANDING_YEAR, LANDING_MONTH, port_cluster_ID, VESSEL_NUM) %>%
  mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
  mutate(Date = zoo::as.yearmon(Date)) %>%
  group_by(Date, port_cluster_ID, VESSEL_NUM) %>% 
  summarise(Est_landings = sum(Estimate.logNANCLandings), Landings = sum(ln_NANC_Landings)) %>%
  group_by(Date, port_cluster_ID) %>% 
  summarise(Est_landings_mean = mean(Est_landings), Landings_mean = mean(Landings)) %>%
  group_by(port_cluster_ID) %>%
  arrange(port_cluster_ID, Date) %>%
  mutate(Est_landings_mean_MA = rollapply(data = Est_landings_mean, 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(Landings_mean_MA     = rollapply(data = Landings_mean    , 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))


#### Using the data estimation -- Pacific sardine


prediction_sel <- prediction_PSDN[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]

# prediction_sel %>% group_by(port_cluster_ID) %>%
#   summarize(SSR = mean(Est.Error.logPSDNLandings))
# 
# prediction_sel %>% group_by(port_cluster_ID) %>%
#   summarize(SSR = sum(abs(ln_PSDN_Landings - Estimate.logPSDNLandings))/n())
# 
# prediction_sel %>% group_by(port_cluster_ID) %>%
#   summarize(SSR = cor(ln_PSDN_Landings, Estimate.logPSDNLandings))
# 
prediction_sel %>% group_by(port_cluster_ID) %>%
  summarize(R_2 = (var(Estimate.logPSDNLandings) / 
                     (var(Estimate.logPSDNLandings) +  
                        var(Estimate.logPSDNLandings - ln_PSDN_Landings)))) %>% ungroup()



df_cluster_PSDN <- prediction_sel %>% 
  dplyr::select(Estimate.logPSDNLandings, ln_PSDN_Landings, 
                LANDING_YEAR, LANDING_MONTH, port_cluster_ID, VESSEL_NUM) %>%
  mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
  mutate(Date = zoo::as.yearmon(Date)) %>%
  group_by(Date, port_cluster_ID, VESSEL_NUM) %>% 
  summarise(Est_landings = sum(Estimate.logPSDNLandings), Landings = sum(ln_PSDN_Landings)) %>%
  group_by(Date, port_cluster_ID) %>% 
  summarise(Est_landings_mean = mean(Est_landings), Landings_mean = mean(Landings)) %>%
  group_by(port_cluster_ID) %>%
  arrange(port_cluster_ID, Date) %>%
  mutate(Est_landings_mean_MA = rollapply(data = Est_landings_mean, 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(Landings_mean_MA     = rollapply(data = Landings_mean    , 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))


# Plot


cond_label_msqd <- as_labeller(c("1-LAA" = "Southern CCS small-scale\nsquid-specialists (Los Angeles)",
                                 "1-SBA" = "Southern CCS small-scale\nsquid-specialists (Santa Barbara)",
                                 "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                                 "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                                 "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                                 "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                                 "5-SBA" = "Roving industrial sardine-squid\ngeneralists (Santa Barbara)",
                                 "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)",
                                 "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)"))

cond_label_psdn <- as_labeller(c("3-CLO" = "PNW sardine opportunists\n(Columbia River OR)",
                                 "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                                 "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                                 "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                                 "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                                 "5-CLO" = "Roving industrial sardine-squid\ngeneralists (Columbia River OR)",
                                 "5-CLW" = "Roving industrial sardine-squid\ngeneralists (Columbia River WA)",
                                 "6-CLO" = "PNW sardine specialists\n(Columbia River OR)",
                                 "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                                 "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)"))

cond_label_nanc <- as_labeller(c("6-CWA" = "PNW sardine specialists\n(Coastal Washington Ports)",
                                 "6-CLW" = "PNW sardine specialists\n(Columbia River OR)",
                                 "6-CLO" = "PNW sardine specialists\n(Columbia River WA)",
                                 "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)",
                                 "7-SDA" = "Southern CCS forage fish\ndiverse (San Diego)",
                                 "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                                 "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)"))


gg_msqd <- ggplot(df_cluster_MSQD) + 
  geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings"), size = 0.75) + 
  geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Estimated landings (MA)"), size = 1) +
  facet_wrap(~port_cluster_ID, labeller = cond_label_msqd, ncol = 3) +
  theme(legend.position="none") +
  # ggtitle("Market squid predictions") +
  scale_x_continuous(name = "")  +
  scale_y_continuous(name = "ln(Landings)") +
  scale_color_manual(name = "Variable: ",
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85"))


gg_nanc <- ggplot(df_cluster_NANC) + 
  geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings"), size = 0.75) + 
  geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Estimated landings (MA)"), size = 1) +
  facet_wrap(~port_cluster_ID, labeller = cond_label_nanc, ncol = 3) +
  theme(legend.position="right") +
  # ggtitle("Northern anchovy predictions") +
  scale_x_continuous(name = "")  +
  scale_y_continuous(name = "") +
  scale_color_manual(name = "Variable: ",
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85"))


gg_psdn <- ggplot(df_cluster_PSDN) + 
  geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings"), size = 0.75) + 
  geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Estimated landings (MA)"), size = 1) +
  facet_wrap(~port_cluster_ID, labeller = cond_label_psdn, ncol = 3) + 
  theme(legend.position="none") +
  scale_color_manual(name = "Variable: ",
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85")) +
  # ggtitle("Pacific sardine predictions") +
  scale_x_continuous(name = "Date")  +
  scale_y_continuous(name = "") 


gg_msqd 

gg_psdn
  
gg_nanc


#----------------------------------------
### Aggregate plots
#### By Cluster 

####  Marker squid
prediction_sel <- prediction_MSQD[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]

#### Market squid
df_cluster_MSQD <- prediction_sel %>% 
  dplyr::select(Estimate.logMSQDLandings, ln_MSQD_Landings, 
                LANDING_YEAR, LANDING_MONTH, port_cluster_ID) %>%
  mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
  mutate(Date = zoo::as.yearmon(Date)) %>%
  group_by(Date, port_cluster_ID) %>% 
  summarise(Est_landings = sum(Estimate.logMSQDLandings), Landings = sum(ln_MSQD_Landings)) %>% 
  group_by(port_cluster_ID) %>%
  arrange(port_cluster_ID, Date) %>%
  mutate(Est_landings_MA = rollapply(data = Est_landings, 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(Landings_MA     = rollapply(data = Landings    , 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))

### Compute bayes R-squared
df_cluster_MSQD %>% ungroup() %>% summarize(R_2 = (var(Est_landings) / (var(Est_landings) +  var(Est_landings - Landings)))) 

df_cluster_MSQD %>% group_by(port_cluster_ID) %>%
  summarize(R_2 = (var(Est_landings) / (var(Est_landings) +  var(Est_landings - Landings)))) %>% ungroup()


#### Pacific sardine
prediction_sel <- prediction_PSDN[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]

df_cluster_PSDN <- prediction_sel %>% 
  dplyr::select(Estimate.logPSDNLandings, ln_PSDN_Landings, 
                LANDING_YEAR, LANDING_MONTH, port_cluster_ID) %>%
  mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
  mutate(Date = zoo::as.yearmon(Date)) %>%
  group_by(Date, port_cluster_ID) %>% 
  summarise(Est_landings = sum(Estimate.logPSDNLandings), Landings = sum(ln_PSDN_Landings)) %>% 
  group_by(port_cluster_ID) %>%
  arrange(port_cluster_ID, Date) %>%
  mutate(Est_landings_MA = rollapply(data = Est_landings, 
                                     width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(Landings_MA     = rollapply(data = Landings    , 
                                     width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))

df_cluster_PSDN %>% ungroup() %>% summarize(R_2 = (var(Est_landings) / (var(Est_landings) +  var(Est_landings - Landings)))) 
df_cluster_PSDN %>% group_by(port_cluster_ID) %>%
  summarize(R_2 = (var(Est_landings) / (var(Est_landings) +  var(Est_landings - Landings)))) %>% ungroup()


#### Northern anchovy
prediction_sel <- prediction_NANC[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]

df_cluster_NANC <- prediction_sel %>% 
  dplyr::select(Estimate.logNANCLandings, ln_NANC_Landings, 
                LANDING_YEAR, LANDING_MONTH, port_cluster_ID) %>%
  mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
  mutate(Date = zoo::as.yearmon(Date)) %>%
  group_by(Date, port_cluster_ID) %>% 
  summarise(Est_landings = sum(Estimate.logNANCLandings), Landings = sum(ln_NANC_Landings)) %>% 
  group_by(port_cluster_ID) %>%
  arrange(port_cluster_ID, Date) %>%
  mutate(Est_landings_MA = rollapply(data = Est_landings, 
                                     width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(Landings_MA     = rollapply(data = Landings    , 
                                     width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))


df_cluster_NANC %>% ungroup() %>% summarize(R_2 = (var(Est_landings) / (var(Est_landings) +  var(Est_landings - Landings)))) 
df_cluster_NANC %>% group_by(port_cluster_ID) %>%
  summarize(R_2 = (var(Est_landings) / (var(Est_landings) +  var(Est_landings - Landings)))) %>% ungroup()



cond_label_msqd <- as_labeller(c("1-LAA" = "Southern CCS small-scale\nsquid-specialists (Los Angeles)",
                                 "1-SBA" = "Southern CCS small-scale\nsquid-specialists (Santa Barbara)",
                                 "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                                 "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                                 "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                                 "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                                 "5-SBA" = "Roving industrial sardine-squid\ngeneralists (Santa Barbara)",
                                 "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)",
                                 "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)"))

cond_label_psdn <- as_labeller(c("3-CLO" = "PNW sardine opportunists\n(Columbia River OR)",
                                 "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
                                 "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
                                 "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
                                 "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
                                 "5-CLO" = "Roving industrial sardine-squid\ngeneralists (Columbia River OR)",
                                 "5-CLW" = "Roving industrial sardine-squid\ngeneralists (Columbia River WA)",
                                 "6-CLO" = "PNW sardine specialists\n(Columbia River OR)",
                                 "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                                 "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)"))

cond_label_nanc <- as_labeller(c("6-CWA" = "PNW sardine specialists\n(Coastal Washington Ports)",
                                 "6-CLW" = "PNW sardine specialists\n(Columbia River OR)",
                                 "6-CLO" = "PNW sardine specialists\n(Columbia River WA)",
                                 "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)",
                                 "7-SDA" = "Southern CCS forage fish\ndiverse (San Diego)",
                                 "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
                                 "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)"))

gg_msqd <- ggplot(df_cluster_MSQD) + 
  geom_line(mapping = aes(x = Date, y = Landings_MA, color = "Actual landings"), size = 0.75) + 
  geom_line(mapping = aes(x = Date, y = Est_landings_MA, color = "Estimated landings (MA)"), size = 1) +
  facet_wrap(~port_cluster_ID, labeller = cond_label_msqd, ncol = 2) +
  theme(legend.position="none") +
  ggtitle("(a) Market squid predictions") +
  scale_x_continuous(name = "")  +
  scale_y_continuous(name = "ln(Landings)") +
  scale_color_manual(name = "Variable: ",
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85"))


gg_nanc <- ggplot(df_cluster_NANC) + 
  geom_line(mapping = aes(x = Date, y = Landings_MA, color = "Actual landings"), size = 0.75) + 
  geom_line(mapping = aes(x = Date, y = Est_landings_MA, color = "Estimated landings (MA)"), size = 1) +
  facet_wrap(~port_cluster_ID, labeller = cond_label_nanc, ncol = 1) +
  theme(legend.position="right") +
  ggtitle("(c) Northern anchovy predictions") +
  scale_x_continuous(name = "")  +
  scale_y_continuous(name = "") +
  scale_color_manual(name = "Variable: ",
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85"))


gg_psdn <- ggplot(df_cluster_PSDN) + 
  geom_line(mapping = aes(x = Date, y = Landings_MA, color = "Actual landings"), size = 0.75) + 
  geom_line(mapping = aes(x = Date, y = Est_landings_MA, color = "Estimated landings (MA)"), size = 1) +
  facet_wrap(~port_cluster_ID, labeller = cond_label_psdn, ncol = 3) + 
  theme(legend.position="none") +
  scale_color_manual(name = "Variable: ",
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85")) +
  ggtitle("Pacific sardine predictions") +
  scale_x_continuous(name = "Date")  +
  scale_y_continuous(name = "") 


  gg_msqd  
  gg_psdn 
  gg_nanc




##################################################################

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


# Separate hurdle model 
# 
#  hu <- plot(conditional_effects(fit_qPSDN, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="hu"), 
#             plot = FALSE, ask = FALSE)
#  mu <- plot(conditional_effects(fit_qPSDN, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="mu"), 
#             plot = FALSE, ask = FALSE)
# 
#  mu[[1]] + hu[[1]] + plot_layout(nrow = 2)
# 
 
#  hu <- plot(conditional_effects(fit_qMSQD_gamma, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="hu"), 
#               plot = FALSE, ask = FALSE) 
#  mu <- plot(conditional_effects(fit_qMSQD_gamma, "PSDN_SDM:MSQD_SDM", surface=TRUE, dpar="mu"), 
#               plot = FALSE, ask = FALSE) 
# 
#  mu[[1]] + hu[[1]] + plot_layout(nrow = 2) 
#   



