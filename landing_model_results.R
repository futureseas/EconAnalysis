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
fit_qMSQD <- readRDS(here::here("Estimations", "fit_qMSQD_v2.RDS"))
fit_qPSDN <- readRDS(here::here("Estimations", "fit_qPSDN_v4.RDS"))
fit_qNANC <- readRDS(here::here("Estimations", "fit_qNANC_v2.RDS"))

#### Read database 
dataset_msqd_landing <- read.csv(file ="C:\\Data\\PacFIN data\\dataset_estimation_MSQD.csv")
dataset_nanc_landing <- read.csv(file ="C:\\Data\\PacFIN data\\dataset_estimation_NANC.csv")
dataset_psdn_landing <- read.csv(file ="C:\\Data\\PacFIN data\\dataset_estimation_PSDN.csv")


# summary(fit_qMSQD)
# summary(fit_qPSDN)
# summary(fit_qNANC)


############################
# Calculate estimation error

## Calculate average error
# 
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
# 
# df <- data.frame(readHTMLTable(htmlParse(tab_model))[1])
# colnames(df) <- df[1,]
# df <- df[-1,]
# gs4_create("MSQD_landings_results", sheets = df)


# tab_model <-
#   sjPlot::tab_model(fit_qPSDN)
# 
# df <- data.frame(readHTMLTable(htmlParse(tab_model))[1])
# colnames(df) <- df[1,]
# df <- df[-1,]
# gs4_create("PSDN_landings_results_v4", sheets = df)
# # 
# 
# tab_model <-
#   sjPlot::tab_model(fit_qNANC)
# 
# df <- data.frame(readHTMLTable(htmlParse(tab_model))[1])
# colnames(df) <- df[1,]
# df <- df[-1,]
# gs4_create("NANC_landings_results", sheets = df)


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
### Group parameters ###

# #### Intercepts ####
# 
# coef(fit_qMSQD)$port_ID 
# coeff_port_sdm <- as.data.frame(coef(fit_qMSQD)$port_ID[, c(1, 3:4), 2]) %>% 
#   round(digits = 2) 
# names <- rownames(coeff_port_sdm)
# rownames(coeff_port_sdm) <- NULL
# coeff_port_sdm <- cbind(names,coeff_port_sdm)
# 
# gg1 <-  ggplot(coeff_port_sdm, aes(x=names, y=Estimate)) +
#   geom_point() +  geom_errorbar(aes(ymin=Q2.5, ymax=Q97.5),
#                                 width=.2, position=position_dodge(0.05)) + coord_flip() + ggtitle("(a) Port areas") +
#   ylab("") + xlab("") + theme(plot.title = element_text(size=9, face="bold.italic")) +
#   scale_x_discrete(labels=c("LAA" = "Los Angeles",
#                             "MNA" = "Monterey",
#                             "SBA" = "Santa Barbara"))
# 
# coef(fit_qMSQD)$cluster
# coeff_cluster_sdm <- as.data.frame(coef(fit_qMSQD)$cluster[, c(1, 3:4), 1]) %>% 
#   round(digits = 2)
# cluster <- rownames(coeff_cluster_sdm)
# rownames(coeff_cluster_sdm) <- NULL
# coeff_cluster_sdm <- cbind(cluster,coeff_cluster_sdm)  
# 
# 
# gg2 <-  ggplot(coeff_cluster_sdm, aes(y=cluster, x=Estimate)) +
#   geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5), 
#                                 width=.2, position=position_dodge(0.05))  + ggtitle("(b) Clusters") +  
#   xlab("") + ylab("") + theme(plot.title = element_text(size=9, face="bold.italic")) + 
#   scale_y_discrete(labels=c("1" = "Southern CCS\nsmall-scale\nsquid-specialists",
#                             "2" = "Southern CCS\nsmall-scale\nCPS-opportunists",
#                             "4" = "Southern CCS\nindustrial\nsquid-specialists",
#                             "5" = "Roving industrial\nsardine-squid\nswitchers",
#                             "7" = "Southern CCS\nforage fish\ndiverse"))
# 
# gg1 + gg2

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


# ### SDM effect
# 
# coef(fit_qMSQD)$cluster
# 
# coeff_cluster <- as.data.frame(coef(fit_qMSQD)$cluster[, c(1, 3:4), 2]) %>%
#   round(digits = 2) 
#   cluster <- rownames(coeff_cluster)
#   rownames(coeff_cluster) <- NULL
#   coeff_cluster <- cbind(cluster,coeff_cluster)  
# 
# 
# gg_1_MSDQ <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
#   geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
#                                 width=.2, position=position_dodge(0.05)) + ggtitle("(a) Pr(MSQD) effect on MSQD landings") +
#   xlab("") + ylab("") +
#   theme(plot.title = element_text(size=10)) +
#   scale_y_discrete(labels=c("1" = "Southern CCS\nsmall-scale\nsquid-specialists",
#                             "2" = "Southern CCS\nsmall-scale\nCPS-opportunists",
#                             "4" = "Southern CCS\nindustrial\nsquid-specialists",
#                             "5" = "Roving industrial\nsardine-squid\nswitchers",
#                             "7" = "Southern CCS\nforage fish\ndiverse")) +
#   geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 
# 
# 
# coeff_cluster <- as.data.frame(coef(fit_qNANC)$cluster[, c(1, 3:4), 2]) %>%
#   round(digits = 2) 
#   cluster <- rownames(coeff_cluster)
#   rownames(coeff_cluster) <- NULL
#   coeff_cluster <- cbind(cluster,coeff_cluster)  
# 
# gg_1_NANC <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
#   geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
#                                 width=.2, position=position_dodge(0.05)) + ggtitle("(b) Pr(NANC) effect on NANC landings") +
#   xlab("") + ylab("") +
#   theme(plot.title = element_text(size=10)) +
#   scale_y_discrete(labels=c("6" = "PNW sardine\nspecialists",
#                             "7" = "Southern CCS\nforage fish\ndiverse")) +
#   geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 
# 
# 
# coeff_cluster <- as.data.frame(coef(fit_qPSDN)$cluster[, c(1, 3:4), 2]) %>%
#   round(digits = 2) 
# cluster <- rownames(coeff_cluster)
# rownames(coeff_cluster) <- NULL
# coeff_cluster <- cbind(cluster,coeff_cluster)  
# 
# gg_1_PSDN <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
#   geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
#                                 width=.2, position=position_dodge(0.05)) + ggtitle("(c) Pr(PSDN) effect on PSDN landings") +
#   xlab("") + ylab("") +
#   theme(plot.title = element_text(size=10)) +
#   scale_y_discrete(labels=c("3" = "PNW sardine\nopportunists",
#                             "4" = "Southern CCS\nindustrial\nsquid-specialists",
#                             "5" = "Roving industrial\nsardine-squid\nswitchers",
#                             "6" = "PNW sardine\nspecialists",
#                             "7" = "Southern CCS\nforage fish\ndiverse")) +
#   geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 
# 
# 
# gg_1_MSDQ + gg_1_NANC + gg_1_PSDN


### Price effect

coeff_cluster <- as.data.frame(coef(fit_qMSQD)$cluster[, c(1, 3:4), 3]) %>%
  round(digits = 2) 
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)  


gg_2_MSDQ <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) +
  ggtitle("(a) Price effect on MSQD landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=10)) +
  scale_y_discrete(labels=c("1" = "Southern CCS\nsmall-scale\nsquid-specialists",
                            "2" = "Southern CCS\nsmall-scale\nCPS-opportunists",
                            "4" = "Southern CCS\nindustrial\nsquid-specialists",
                            "5" = "Roving industrial\nsardine-squid\nswitchers",
                            "7" = "Southern CCS\nforage fish\ndiverse")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 


coeff_cluster <- as.data.frame(coef(fit_qNANC)$cluster[, c(1, 3:4), 3]) %>%
  round(digits = 2) 
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)  

gg_2_NANC <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) +
  ggtitle("(c) Price effect on NANC landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=10)) +
  scale_y_discrete(labels=c("6" = "PNW sardine\nspecialists",
                            "7" = "Southern CCS\nforage fish\ndiverse")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 


coeff_cluster <- as.data.frame(coef(fit_qPSDN)$cluster[, c(1, 3:4), 3]) %>%
  round(digits = 2) 
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)  

gg_2_PSDN <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) +
  ggtitle("(b) Price effect on PSDN landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=10)) +
  scale_y_discrete(labels=c("1" = "Southern CCS\nsmall-scale\nsquid-specialists",
                            "3" = "PNW sardine\nopportunists",
                            "4" = "Southern CCS\nindustrial\nsquid-specialists",
                            "5" = "Roving industrial\nsardine-squid\nswitchers",
                            "6" = "PNW sardine\nspecialists",
                            "7" = "Southern CCS\nforage fish\ndiverse")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 


gg_2_MSDQ  + gg_2_PSDN + gg_2_NANC



### Closure effect


coeff_cluster <- as.data.frame(coef(fit_qMSQD)$cluster[, c(1, 3:4), 4]) %>%
  round(digits = 2) 
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)  


gg_3_MSDQ <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) +
  ggtitle("(a) PSDN Closure effect on MSQD landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=10)) +
  scale_y_discrete(labels=c("1" = "Southern CCS\nsmall-scale\nsquid-specialists",
                            "2" = "Southern CCS\nsmall-scale\nCPS-opportunists",
                            "4" = "Southern CCS\nindustrial\nsquid-specialists",
                            "5" = "Roving industrial\nsardine-squid\nswitchers",
                            "7" = "Southern CCS\nforage fish\ndiverse")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 


coeff_cluster <- as.data.frame(coef(fit_qNANC)$cluster[, c(1, 3:4), 4]) %>%
  round(digits = 2) 
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)  

gg_3_NANC <- ggplot(coeff_cluster, aes(y=cluster, x=Estimate)) +
  geom_point() +  geom_errorbar(aes(xmin=Q2.5, xmax=Q97.5),
                                width=.2, position=position_dodge(0.05)) + 
  ggtitle("(b)  PSDN Closure effect on NANC landings") +
  xlab("") + ylab("") +
  theme(plot.title = element_text(size=10)) +
  scale_y_discrete(labels=c("6" = "PNW sardine\nspecialists",
                            "7" = "Southern CCS\nforage fish\ndiverse")) +
  geom_vline(xintercept = 0, linetype="dashed", color = "blue", size=0.5) 


coeff_cluster <- as.data.frame(coef(fit_qPSDN)$cluster[, c(1, 3:4), 2]) %>%
  round(digits = 2) 
cluster <- rownames(coeff_cluster)
rownames(coeff_cluster) <- NULL
coeff_cluster <- cbind(cluster,coeff_cluster)  

gg_3_MSDQ + gg_3_NANC 




###########################################################
### Conditional effect of MSQD presence

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


####################################################################
### Interaction effects  

#### Market squid
conditions_cluster <- data.frame(cluster = unique(dataset_msqd_landing$cluster)) 
rownames(conditions_cluster) <- conditions_cluster$cluster

cluster_label <- as_labeller(c("1" = "Southern CCS\nsmall-scale squid-specialists",
                               "2" = "Southern CCS\nsmall-scale CPS-opportunists",
                               "4" = "Southern CCS\nindustrial squid-specialists",
                               "5" = "Roving industrial\nsardine-squid switchers",
                               "7" = "Southern CCS\nforage fish diverse"))


### Squid v/s Sardine ###

conditional_effects_psdn_msqd_sdm_cluster <- (conditional_effects(
  fit_qMSQD, "PSDN_SDM_60_z:MSQD_SPAWN_SDM_90_z", 
  surface=TRUE, 
  conditions = conditions_cluster, re_formula = NULL))

### Squid v/s Anchovy ###

conditional_effects_nanc_msqd_sdm_cluster <- (conditional_effects(
  fit_qMSQD, "NANC_SDM_20_z:MSQD_SPAWN_SDM_90_z", 
  surface=TRUE, 
  conditions = conditions_cluster, re_formula = NULL))


# Plot

gg_int <- plot(conditional_effects_psdn_msqd_sdm_cluster, plot = FALSE)[[2]] + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)) + 
  ggtitle("(a) SDM: Market squid x SDM: Pacific sardine") +  
  theme(plot.title = element_text(size=9, face="bold.italic")) + 
  guides(colour=guide_legend(title="ln(MSQD: Landings)")) +
  scale_x_continuous(name = "PSDN: Prob(Presence)") + scale_y_continuous(name = "MSQD: Prob(Presence)")
gg_int$facet$params$labeller <- cluster_label

gg_int_2 <- plot(conditional_effects_nanc_msqd_sdm_cluster, plot = FALSE)[[2]] + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)) + 
  ggtitle("(a) SDM: Market squid x SDM: Northern anchovy") +  
  theme(plot.title = element_text(size=9, face="bold.italic")) + 
  guides(colour=guide_legend(title="ln(MSQD: Landings)")) +
  scale_x_continuous(name = "NANC: Prob(Presence)") + scale_y_continuous(name = "MSQD: Prob(Presence)")
gg_int_2$facet$params$labeller <- cluster_label

gg_int / gg_int_2



#### Northern anchovy

conditions_cluster <- data.frame(cluster = unique(dataset_nanc_landing$cluster)) 
rownames(conditions_cluster) <- conditions_cluster$cluster

cluster_label <- as_labeller(c("6" = "PNW sardine specialists",
                               "7" = "Southern CCS forage fish diverse"))

### Anchovy v/s Sardine ###
conditional_effects_nanc_psdn_sdm_cluster <- (conditional_effects(
  fit_qNANC, "PSDN_SDM_60_z:NANC_SDM_20_z", 
  surface=TRUE, 
  conditions = conditions_cluster, re_formula = NULL))

### Anchovy v/s squid ###
conditional_effects_nanc_msqd_sdm_cluster <- (conditional_effects(
  fit_qNANC, "MSQD_SPAWN_SDM_90_z:NANC_SDM_20_z", 
  surface=TRUE, 
  conditions = conditions_cluster, re_formula = NULL))


# Plot
gg_int <- plot(conditional_effects_nanc_psdn_sdm_cluster, plot = FALSE)[[2]] + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)) + 
  ggtitle("(a) SDM: Northern anchovy x SDM: Pacific sardine") +  
  theme(plot.title = element_text(size=9, face="bold.italic")) + 
  guides(colour=guide_legend(title="ln(NANC: Landings)")) +
  scale_y_continuous(name = "NANC: Prob(Presence)") + scale_x_continuous(name = "PSDN: Prob(Presence)")
gg_int$facet$params$labeller <- cluster_label

gg_int_2 <- plot(conditional_effects_nanc_msqd_sdm_cluster, plot = FALSE)[[2]] + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)) + 
  ggtitle("(b) SDM: Northern anchovy x SDM: Market squid") +  
  theme(plot.title = element_text(size=9, face="bold.italic")) + 
  guides(colour=guide_legend(title="ln(NANC: Landings)")) +
  scale_y_continuous(name = "NANC: Prob(Presence)") + scale_x_continuous(name = "MSQD: Prob(Presence)")
gg_int_2$facet$params$labeller <- cluster_label

gg_int / gg_int_2
  
  


#### Pacific sardine
conditions_cluster <- data.frame(cluster = unique(dataset_psdn_landing$cluster)) 
rownames(conditions_cluster) <- conditions_cluster$cluster

cluster_label <- as_labeller(c("1" = "Southern CCS\nsmall-scale squid-specialists",
                               "3" = "PNW sardine\nopportunists",
                               "4" = "Southern CCS\nindustrial squid-specialists",
                               "5" = "Roving industrial\nsardine-squid switchers",
                               "6" = "PNW sardine\nspecialists",
                               "7" = "Southern CCS\nforage fish diverse"))

### Sardine v/s Squid ###

conditional_effects_psdn_msqd_sdm_cluster <- (conditional_effects(
  fit_qPSDN, "MSQD_SPAWN_SDM_90_z:PSDN_SDM_60_z", 
  surface=TRUE, 
  conditions = conditions_cluster, re_formula = NULL))

### Sardine v/s Anchovy ###

conditional_effects_nanc_psdn_sdm_cluster <- (conditional_effects(
  fit_qPSDN, "NANC_SDM_20_z:PSDN_SDM_60_z", 
  surface=TRUE, 
  conditions = conditions_cluster, re_formula = NULL))



# Plot

gg_int <- plot(conditional_effects_psdn_msqd_sdm_cluster, plot = FALSE)[[2]] + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)) + 
  ggtitle("(a) SDM: Pacific sardine x SDM: Market squid") +  
  theme(plot.title = element_text(size=9, face="bold.italic")) + 
  guides(colour=guide_legend(title="ln(PSDN: Landings)")) +
  scale_y_continuous(name = "PSDN: Prob(Presence)") + scale_x_continuous(name = "MSQD: Prob(Presence)")
gg_int$facet$params$labeller <- cluster_label

gg_int_2 <- plot(conditional_effects_nanc_psdn_sdm_cluster, plot = FALSE)[[2]] + 
  theme(
    plot.title = element_text(size=9, face="bold.italic"),
    axis.text = element_text(size = 7),
    axis.title = element_text(size = 8),
    legend.title = element_text(size = 9),
    legend.text = element_text(size=8)) + 
  ggtitle("(b) SDM: Pacific sardine x SDM: Northern anchovy") +  
  theme(plot.title = element_text(size=9, face="bold.italic")) + 
  guides(colour=guide_legend(title="ln(PSDN: Landings)")) +
  scale_x_continuous(name = "NANC: Prob(Presence)") + scale_y_continuous(name = "PSDN: Prob(Presence)")
gg_int_2$facet$params$labeller <- cluster_label

gg_int / gg_int_2


####### By port

# conditions_port <- data.frame(port_ID = unique(dataset_msqd_landing$port_ID)) 
# rownames(conditions_port) <- unique(dataset_msqd_landing$port_ID)
# conditions_port <- conditions_port %>% 
#   arrange(-desc(port_ID)) 
# 
# port_label <- as_labeller(c("LAA" = "Los Angeles",
#                             "MNA" = "Monterey",
#                             "SBA" = "Santa Barbara"))


# conditional_effects_psdn_msqd_sdm_port <- (conditional_effects(
#   fit_qMSQD, "PSDN_SDM_60_z:MSQD_SPAWN_SDM_90_z", 
#   surface=TRUE, 
#   conditions = conditions_port, re_formula = NULL))

# conditional_effects_nanc_msqd_sdm_port <- (conditional_effects(
#   fit_qMSQD, "NANC_SDM_20_z:MSQD_SPAWN_SDM_90_z",
#   surface=TRUE,
#   conditions = conditions_port, re_formula = NULL))

# gg_int_2 <- plot(conditional_effects_psdn_msqd_sdm_port, plot = FALSE)[[2]] + 
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"),
#     axis.text = element_text(size = 7),
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9),
#     legend.text = element_text(size=8)) + 
#   ggtitle("(b) Ports") +  
#   theme(plot.title = element_text(size=9, face="bold.italic")) +
#   guides(colour=guide_legend(title="ln(MSQD: Landings)")) +
#   scale_x_continuous(name = "PSDN: Prob(Presence)") + scale_y_continuous(name = "MSQD: Prob(Presence)")
# gg_int_2$facet$params$labeller <- port_label

# gg_int_2 <- plot(conditional_effects_nanc_msqd_sdm_port, plot = FALSE)[[2]] +
#   theme(
#     plot.title = element_text(size=9, face="bold.italic"),
#     axis.text = element_text(size = 7),
#     axis.title = element_text(size = 8),
#     legend.title = element_text(size = 9),
#     legend.text = element_text(size=8)) +
#   ggtitle("(b) Ports") +
#   theme(plot.title = element_text(size=9, face="bold.italic")) +
#   guides(colour=guide_legend(title="ln(MSQD: Landings)")) +
#   scale_x_continuous(name = "NANC: Prob(Presence)") + scale_y_continuous(name = "MSQD: Prob(Presence)")
# gg_int_2$facet$params$labeller <- port_label


##########################################################################
### Predictions ###

# Predict

set.seed(123)
prediction_MSQD <- cbind(predict(fit_qMSQD), dataset_msqd_landing)
saveRDS(prediction_MSQD, file = "prediction_MSQD_v2.rds")

set.seed(123)
prediction_NANC <- cbind(predict(fit_qNANC), dataset_nanc_landing)
saveRDS(prediction_NANC, file = "prediction_NANC_v2.rds")

set.seed(123)
prediction_PSDN <- cbind(predict(fit_qPSDN), dataset_psdn_landing)
saveRDS(prediction_PSDN, file = "prediction_PSDN_v2.rds")

# prediction_MSQD <- readRDS(file = "prediction_MSQD_v2.rds")
# prediction_NANC <- readRDS(file = "prediction_NANC_v2.rds")
# prediction_PSDN <- readRDS(file = "prediction_PSDN_v2.rds")



library(zoo)

####  Marker squid
prediction_sel <- prediction_MSQD[,-1]
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
df_cluster_MSQD <- prediction_sel %>% 
  dplyr::select(Estimate.logMSQDLandings, ln_MSQD_Landings, 
                LANDING_YEAR, LANDING_MONTH, group_all, VESSEL_NUM) %>%
  mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
  mutate(Date = zoo::as.yearmon(Date)) %>%
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


#### Using the data estimation -- Northern anchovy

prediction_sel <- prediction_NANC[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]

prediction_sel %>% group_by(group_all) %>%
  summarize(SSR = mean(Est.Error.logNANCLandings))

df_cluster_NANC <- prediction_sel %>% 
  dplyr::select(Estimate.logNANCLandings, ln_NANC_Landings, 
                LANDING_YEAR, LANDING_MONTH, group_all, VESSEL_NUM) %>%
  mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
  mutate(Date = zoo::as.yearmon(Date)) %>%
  group_by(Date, group_all, VESSEL_NUM) %>% 
  summarise(Est_landings = sum(Estimate.logNANCLandings), Landings = sum(ln_NANC_Landings)) %>%
  group_by(Date, group_all) %>% 
  summarise(Est_landings_mean = mean(Est_landings), Landings_mean = mean(Landings)) %>%
  group_by(group_all) %>%
  arrange(group_all, Date) %>%
  mutate(Est_landings_mean_MA = rollapply(data = Est_landings_mean, 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(Landings_mean_MA     = rollapply(data = Landings_mean    , 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))


#### Using the data estimation -- Pacific sardine


prediction_sel <- prediction_PSDN[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]
prediction_sel <- prediction_sel[,-1]

prediction_sel %>% group_by(group_all) %>%
  summarize(SSR = mean(Est.Error.logPSDNLandings))


df_cluster_PSDN <- prediction_sel %>% 
  dplyr::select(Estimate.logPSDNLandings, ln_PSDN_Landings, 
                LANDING_YEAR, LANDING_MONTH, group_all, VESSEL_NUM) %>%
  mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
  mutate(Date = zoo::as.yearmon(Date)) %>%
  group_by(Date, group_all, VESSEL_NUM) %>% 
  summarise(Est_landings = sum(Estimate.logPSDNLandings), Landings = sum(ln_PSDN_Landings)) %>%
  group_by(Date, group_all) %>% 
  summarise(Est_landings_mean = mean(Est_landings), Landings_mean = mean(Landings)) %>%
  group_by(group_all) %>%
  arrange(group_all, Date) %>%
  mutate(Est_landings_mean_MA = rollapply(data = Est_landings_mean, 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
  mutate(Landings_mean_MA     = rollapply(data = Landings_mean    , 
                                          width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))


# Plot


cond_label_msqd <- as_labeller(c("1" = "Southern CCS\nsmall-scale squid-specialists",
                                 "2" = "Southern CCS\nsmall-scale CPS-opportunists",
                                 "4" = "Southern CCS\nindustrial squid-specialists",
                                 "5" = "Roving industrial\nsardine-squid switchers",
                                 "7" = "Southern CCS\nforage fish diverse"))

cond_label_psdn <- as_labeller(c("1" = "Southern CCS\nsmall-scale squid-specialists",
                                 "3" = "PNW sardine\nopportunists",
                                 "4" = "Southern CCS\nindustrial squid-specialists",
                                 "5" = "Roving industrial\nsardine-squid switchers",
                                 "6" = "PNW sardine\nspecialists",
                                 "7" = "Southern CCS\nforage fish diverse"))

cond_label_nanc <- as_labeller(c("6" = "PNW sardine\nspecialists",
                                 "7" = "Southern CCS\nforage fish\ndiverse"))


gg_msqd <- ggplot(df_cluster_MSQD) + 
  geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings"), size = 0.75) + 
  geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Estimated landings (MA)"), size = 1) +
  facet_wrap(~group_all, labeller = cond_label_msqd, ncol = 2) +
  theme(legend.position="none") +
  ggtitle("(a) Market squid predictions") +
  scale_x_continuous(name = "")  +
  scale_y_continuous(name = "ln(Landings)") +
  scale_color_manual(name = "Variable: ",
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85"))


gg_nanc <- ggplot(df_cluster_NANC) + 
  geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings"), size = 0.75) + 
  geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Estimated landings (MA)"), size = 1) +
  facet_wrap(~group_all, labeller = cond_label_nanc, ncol = 1) +
  theme(legend.position="right") +
  ggtitle("(c) Northern anchovy predictions") +
  scale_x_continuous(name = "")  +
  scale_y_continuous(name = "") +
  scale_color_manual(name = "Variable: ",
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85"))


gg_psdn <- ggplot(df_cluster_PSDN) + 
  geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings"), size = 0.75) + 
  geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Estimated landings (MA)"), size = 1) +
  facet_wrap(~group_all, labeller = cond_label_psdn, ncol = 2) + 
  theme(legend.position="none") +
  scale_color_manual(name = "Variable: ",
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85")) +
  ggtitle("(b) Pacific sardine predictions") +
  scale_x_continuous(name = "Date")  +
  scale_y_continuous(name = "") 


gg_msqd  + gg_psdn + gg_nanc


# # by port
# library(zoo)
# df_port <- prediction_sel %>% 
#   dplyr::select(Estimate.logMSQDLandings, ln_MSQD_Landings, Date, PORT_AREA_CODE, VESSEL_NUM) %>%
#   group_by(Date, PORT_AREA_CODE, VESSEL_NUM) %>% 
#   summarise(Est_landings = sum(Estimate.logMSQDLandings), Landings = sum(ln_MSQD_Landings)) %>%
#   group_by(Date, PORT_AREA_CODE) %>% 
#   summarise(Est_landings_mean = mean(Est_landings), Landings_mean = mean(Landings)) %>%
#   group_by(PORT_AREA_CODE) %>%
#   arrange(PORT_AREA_CODE, Date) %>%
#   mutate(Est_landings_mean_MA = rollapply(data = Est_landings_mean, 
#                                           width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
#   mutate(Landings_mean_MA     = rollapply(data = Landings_mean    , 
#                                           width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))
# 
# 
# port_label <- as_labeller(c("LAA" = "Los Angeles",
#                             "MNA" = "Monterey",
#                             "SBA" = "Santa Barbara"))
# 
# ggplot(df_port) + 
#   geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings")) + 
#   geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Estimated landings (MA)"), size = 1, linetype = "dashed") +
#   facet_wrap(~PORT_AREA_CODE, labeller = port_label) +
#   scale_x_continuous(name = "Date")  +
#   scale_y_continuous(name = "ln(Landings)") +
#   scale_color_manual(name = "Variable: ",
#                      values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85"))


################################################################
################################################################

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



