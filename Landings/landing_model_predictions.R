###################
### Predictions ###
###################

#----------------------------
# Setup #
rm(list = ls(all.names = TRUE)) 
gc()

library("googlesheets4")
gs4_auth(
  email = "fequezad@ucsc.edu",
  scopes = "https://www.googleapis.com/auth/spreadsheets",
  cache = gargle::gargle_oauth_cache(),
  use_oob = gargle::gargle_oob_default(),
  token = NULL)


# ## Read packages 
# library("brms")
# library("sjPlot")
# library("sjlabelled")
# library("sjmisc")
# library("insight")
# library("httr")
# library("tidyr")
library("dplyr") 
library("zoo")
# library("data.table") 
# library("reshape2")
# library('doBy')
# library("patchwork")
library('ggplot2')
# library('ggthemes')
# library('tibble')
# library('XML')
# theme_set(theme_sjplot())


# Predict

# set.seed(123)
# prediction_MSQD <- cbind(predict(fit_qMSQD), dataset_msqd_landing)
# saveRDS(prediction_MSQD, file = "prediction_MSQD.rds")
# 
# set.seed(123)
# prediction_NANC <- cbind(predict(fit_qNANC), dataset_nanc_landing)
# saveRDS(prediction_NANC, file = "prediction_NANC.rds")
# 
# set.seed(123)
# prediction_PSDN <- cbind(predict(fit_qPSDN), dataset_psdn_landing)
# saveRDS(prediction_PSDN, file = "prediction_PSDN.rds")

prediction_MSQD <- readRDS(file = here::here("Landings", "Predictions", "prediction_MSQD.rds"))
prediction_NANC <- readRDS(file = here::here("Landings", "Predictions", "prediction_NANC.rds"))
prediction_PSDN <- readRDS(file = here::here("Landings", "Predictions", "prediction_PSDN.rds"))


# ###  Marker squid
# prediction_sel <- prediction_MSQD[,-1]
# prediction_sel <- prediction_sel[,-1]
# prediction_sel <- prediction_sel[,-1]
# prediction_sel <- prediction_sel[,-1]
# 
# # prediction_sel %>% group_by(port_cluster_ID) %>%
# #   summarize(SSR = sum(abs(ln_MSQD_Landings - Estimate.logMSQDLandings))/n())
# # 
# # prediction_sel %>% group_by(port_cluster_ID) %>%
# #   summarize(cor = cor(ln_MSQD_Landings, Estimate.logMSQDLandings))
# # 
# # prediction_sel %>% group_by(port_cluster_ID) %>%
# #   summarize(mean_error = mean(Est.Error.logMSQDLandings))
# 
# # by cluster
# df_cluster_MSQD <- prediction_sel %>% 
#   dplyr::select(Estimate.logMSQDLandings, ln_MSQD_Landings, 
#                 LANDING_YEAR, LANDING_MONTH, port_cluster_ID, VESSEL_NUM) %>%
#   mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
#   mutate(Date = zoo::as.yearmon(Date)) %>%
#   group_by(Date, port_cluster_ID, VESSEL_NUM) %>% 
#   summarise(Est_landings = sum(Estimate.logMSQDLandings), Landings = sum(ln_MSQD_Landings)) %>%
#   group_by(Date, port_cluster_ID) %>% 
#   summarise(Est_landings_mean = mean(Est_landings), Landings_mean = mean(Landings)) %>%
#   group_by(port_cluster_ID) %>%
#   arrange(port_cluster_ID, Date) %>%
#   mutate(Est_landings_mean_MA = rollapply(data = Est_landings_mean, 
#                                           width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
#   mutate(Landings_mean_MA     = rollapply(data = Landings_mean    , 
#                                           width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))
# 
# ### Northern anchovy
# 
# prediction_sel <- prediction_NANC[,-1]
# prediction_sel <- prediction_sel[,-1]
# prediction_sel <- prediction_sel[,-1]
# prediction_sel <- prediction_sel[,-1]
# 
# df_cluster_NANC <- prediction_sel %>% 
#   dplyr::select(Estimate.logNANCLandings, ln_NANC_Landings, 
#                 LANDING_YEAR, LANDING_MONTH, port_cluster_ID, VESSEL_NUM) %>%
#   mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
#   mutate(Date = zoo::as.yearmon(Date)) %>%
#   group_by(Date, port_cluster_ID, VESSEL_NUM) %>% 
#   summarise(Est_landings = sum(Estimate.logNANCLandings), Landings = sum(ln_NANC_Landings)) %>%
#   group_by(Date, port_cluster_ID) %>% 
#   summarise(Est_landings_mean = mean(Est_landings), Landings_mean = mean(Landings)) %>%
#   group_by(port_cluster_ID) %>%
#   arrange(port_cluster_ID, Date) %>%
#   mutate(Est_landings_mean_MA = rollapply(data = Est_landings_mean, 
#                                           width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
#   mutate(Landings_mean_MA     = rollapply(data = Landings_mean    , 
#                                           width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))
# 
# 
# ### Pacific sardine
# prediction_sel <- prediction_PSDN[,-1]
# prediction_sel <- prediction_sel[,-1]
# prediction_sel <- prediction_sel[,-1]
# prediction_sel <- prediction_sel[,-1]
# 
# # prediction_sel %>% group_by(port_cluster_ID) %>%
# #   summarize(R_2 = (var(Estimate.logPSDNLandings) / 
# #                      (var(Estimate.logPSDNLandings) +  
# #                         var(Estimate.logPSDNLandings - ln_PSDN_Landings)))) %>% ungroup()
# 
# 
# df_cluster_PSDN <- prediction_sel %>% 
#   dplyr::select(Estimate.logPSDNLandings, ln_PSDN_Landings, 
#                 LANDING_YEAR, LANDING_MONTH, port_cluster_ID, VESSEL_NUM) %>%
#   mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
#   mutate(Date = zoo::as.yearmon(Date)) %>%
#   group_by(Date, port_cluster_ID, VESSEL_NUM) %>% 
#   summarise(Est_landings = sum(Estimate.logPSDNLandings), Landings = sum(ln_PSDN_Landings)) %>%
#   group_by(Date, port_cluster_ID) %>% 
#   summarise(Est_landings_mean = mean(Est_landings), Landings_mean = mean(Landings)) %>%
#   group_by(port_cluster_ID) %>%
#   arrange(port_cluster_ID, Date) %>%
#   mutate(Est_landings_mean_MA = rollapply(data = Est_landings_mean, 
#                                           width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
#   mutate(Landings_mean_MA     = rollapply(data = Landings_mean    , 
#                                           width = 3, FUN = mean, align = "right", fill = NA, na.rm = T))
# 
# 
# 
# 
# 
# # Plot
# cond_label_msqd <- as_labeller(c("1-LAA" = "Southern CCS small-scale\nsquid-specialists (Los Angeles)",
#                                  "1-SBA" = "Southern CCS small-scale\nsquid-specialists (Santa Barbara)",
#                                  "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
#                                  "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
#                                  "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
#                                  "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
#                                  "5-SBA" = "Roving industrial sardine-squid\ngeneralists (Santa Barbara)",
#                                  "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)",
#                                  "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)"))
# 
# cond_label_psdn <- as_labeller(c("3-CLO" = "PNW sardine opportunists\n(Columbia River OR)",
#                                  "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
#                                  "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
#                                  "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
#                                  "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
#                                  "5-CLO" = "Roving industrial sardine-squid\ngeneralists (Columbia River OR)",
#                                  "5-CLW" = "Roving industrial sardine-squid\ngeneralists (Columbia River WA)",
#                                  "6-CLO" = "PNW sardine specialists\n(Columbia River OR)",
#                                  "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
#                                  "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)"))
# 
# cond_label_nanc <- as_labeller(c("6-CWA" = "PNW sardine specialists\n(Coastal Washington Ports)",
#                                  "6-CLW" = "PNW sardine specialists\n(Columbia River OR)",
#                                  "6-CLO" = "PNW sardine specialists\n(Columbia River WA)",
#                                  "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)",
#                                  "7-SDA" = "Southern CCS forage fish\ndiverse (San Diego)",
#                                  "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
#                                  "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)"))
# 
# 
# ### Figure S2
# gg_msqd <- ggplot(df_cluster_MSQD) + 
#   geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings"), linewidth = 0.75) + 
#   geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Estimated landings (MA)"), linewidth = 1) +
#   facet_wrap(~port_cluster_ID, labeller = cond_label_msqd, ncol = 3) +
#   theme(legend.position="right") +
#   # ggtitle("Market squid predictions") +
#   scale_x_continuous(name = "")  +
#   scale_y_continuous(name = "ln(Landings)") +
#   scale_color_manual(name = "Variable: ",
#                      values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85")) + 
#   theme(axis.text.x = element_text(size = 7))
# gg_msqd
# 
# ### Figure S3
# gg_nanc <- ggplot(df_cluster_NANC) + 
#   geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings"), size = 0.75) + 
#   geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Estimated landings (MA)"), size = 1) +
#   facet_wrap(~port_cluster_ID, labeller = cond_label_nanc, ncol = 3) +
#   theme(legend.position="right") +
#   # ggtitle("Northern anchovy predictions") +
#   scale_x_continuous(name = "")  +
#   scale_y_continuous(name = "ln(Landings)") +
#   scale_color_manual(name = "Variable: ",
#                      values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85")) +
#   theme(axis.text.x = element_text(size = 7))
# gg_nanc
# 
# ### Figure S4
# gg_psdn <- ggplot(df_cluster_PSDN) + 
#   geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings"), size = 0.75) + 
#   geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Estimated landings (MA)"), size = 1) +
#   facet_wrap(~port_cluster_ID, labeller = cond_label_psdn, ncol = 3) + 
#   theme(legend.position="right") +
#   scale_color_manual(name = "Variable: ",
#                      values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "gray85")) +
#   # ggtitle("Pacific sardine predictions") +
#   scale_x_continuous(name = "")  +
#   scale_y_continuous(name = "ln(Landings)") +  
#   theme(axis.text.x = element_text(size = 7))
# gg_psdn


#----------------------------------------
### Aggregate plots
#### By cluster/port

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
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "darkgreen"))

gg_msqd

gg_nanc <- ggplot(df_cluster_NANC) +
  geom_line(mapping = aes(x = Date, y = Landings_MA, color = "Actual landings"), size = 0.75) +
  geom_line(mapping = aes(x = Date, y = Est_landings_MA, color = "Estimated landings (MA)"), size = 1) +
  facet_wrap(~port_cluster_ID, labeller = cond_label_nanc, ncol = 7) +
  theme(legend.position="right") +
  ggtitle("(c) Northern anchovy predictions") +
  scale_x_continuous(name = "")  +
  scale_y_continuous(name = "") +
  scale_color_manual(name = "Variable: ",
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "darkgreen"))

gg_nanc


gg_psdn <- ggplot(df_cluster_PSDN) +
  geom_line(mapping = aes(x = Date, y = Landings_MA, color = "Actual landings"), size = 0.75) +
  geom_line(mapping = aes(x = Date, y = Est_landings_MA, color = "Estimated landings (MA)"), size = 1) +
  facet_wrap(~port_cluster_ID, labeller = cond_label_psdn, ncol = 5) +
  theme(legend.position="none") +
  scale_color_manual(name = "Variable: ",
                     values = c("Estimated landings (MA)" = "royalblue", "Actual landings" = "darkgreen")) +
  ggtitle("(b) Pacific sardine predictions") +
  scale_x_continuous(name = "Date")  +
  scale_y_continuous(name = "")

  gg_psdn




##################################################################
### Other code 

### Predict if sardine would have been open

# # Market squid
# 
# # set.seed(123)
# # prediction_MSQD_noclosure <- cbind(predict(fit_qMSQD,
# #                                 newdata = data.frame(
# #                                   MSQD_Price_z = dataset_msqd_landing$MSQD_Price_z,
# #                                   Price.Fishmeal.AFI_z = dataset_msqd_landing$Price.Fishmeal.AFI_z,
# #                                   port_ID = dataset_msqd_landing$port_ID,
# #                                   MSQD_Landings = dataset_msqd_landing$MSQD_Landings,
# #                                   MSQD_SPAWN_SDM_90 = dataset_msqd_landing$MSQD_SPAWN_SDM_90,
# #                                   NANC_SDM_20 = dataset_msqd_landing$NANC_SDM_20,
# #                                   PSDN.Total.Closure = 0,
# #                                   Length_z = dataset_msqd_landing$Length_z,
# #                                   PSDN_SDM_60 = dataset_msqd_landing$PSDN_SDM_60,
# #                                   PSDN.Open = 1,
# #                                   port_cluster_ID = dataset_msqd_landing$port_cluster_ID)),
# #                                   dataset_msqd_landing)
# # saveRDS(prediction_MSQD_noclosure, file = "prediction_MSQD_noclosure.rds")
# 
# prediction_MSQD_noclosure <- readRDS(file = "prediction_MSQD_noclosure.rds")
# prediction_MSQD_noclosure <- prediction_MSQD_noclosure %>%
#   rename(Estimate.logMSQDLandings.closure = Estimate.logMSQDLandings) 
# Estimate.logMSQDLandings <- prediction_MSQD[,5]
# prediction_MSQD_noclosure <- cbind.data.frame(prediction_MSQD_noclosure, Estimate.logMSQDLandings) %>%
#   filter(LANDING_YEAR>2013)
# prediction_MSQD_noclosure$LANDING_YEAR   <- as.integer(prediction_MSQD_noclosure$LANDING_YEAR)
# 
# prediction_sel <- prediction_MSQD_noclosure [,-1]
# prediction_sel <- prediction_sel[,-1]
# prediction_sel <- prediction_sel[,-1]
# prediction_sel <- prediction_sel[,-1]
# 
# # by cluster
# df_cluster_MSQD <- prediction_sel %>% 
#   dplyr::select(Estimate.logMSQDLandings.closure, Estimate.logMSQDLandings, ln_MSQD_Landings, 
#                 LANDING_YEAR, LANDING_MONTH, port_cluster_ID, VESSEL_NUM, PSDN.Total.Closure) %>%
#   mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
#   mutate(Date = zoo::as.yearmon(Date)) %>%
#   group_by(Date, port_cluster_ID, VESSEL_NUM) %>% 
#   summarise(Est_landings.closure = sum(Estimate.logMSQDLandings.closure), 
#             Est_landings = sum(Estimate.logMSQDLandings), 
#             Landings = sum(ln_MSQD_Landings)) %>%
#   group_by(Date, port_cluster_ID) %>% 
#   summarise(Est_landings_mean = mean(Est_landings),
#             Est_landings_mean.closure = mean(Est_landings.closure),
#             Landings_mean = mean(Landings)) %>%
#   group_by(port_cluster_ID) %>%
#   arrange(port_cluster_ID, Date) %>%
#   mutate(Est_landings_mean_MA = rollapply(data = Est_landings_mean, 
#                                           width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
#   mutate(Est_landings_mean_MA.closure = rollapply(data = Est_landings_mean.closure, 
#                                                   width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
#   mutate(Landings_mean_MA     = rollapply(data = Landings_mean    , 
#                                           width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
#   mutate(Est_landings_mean_MA.closure = ifelse(Date < "July 2015", Landings_mean_MA, Est_landings_mean_MA.closure)) %>%
#   mutate(Est_landings_mean_MA = ifelse(Date < "July 2015", Landings_mean_MA, Est_landings_mean_MA))
# 
# 
# 
# # Plot
# cond_label_msqd <- as_labeller(c("1-LAA" = "Southern CCS small-scale\nsquid-specialists (Los Angeles)",
#                                  "1-SBA" = "Southern CCS small-scale\nsquid-specialists (Santa Barbara)",
#                                  "4-LAA" = "Southern CCS industrial\nsquid-specialists (Los Angeles)",
#                                  "4-MNA" = "Southern CCS industrial\nsquid-specialists (Monterey)",
#                                  "4-SBA" = "Southern CCS industrial\nsquid-specialists (Santa Barbara)",
#                                  "5-LAA" = "Roving industrial sardine-squid\ngeneralists (Los Angeles)",
#                                  "5-SBA" = "Roving industrial sardine-squid\ngeneralists (Santa Barbara)",
#                                  "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)",
#                                  "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)"))
# 
# gg_msqd <- ggplot(df_cluster_MSQD) + 
#   geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Predicted landing (closure)"), size = 0.75,linetype="dashed") +
#   geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA.closure, color = "Predicted landing (no closure)"), size = 0.75,linetype="dashed") +
#   geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings"), size = 1) + 
#   facet_wrap(~port_cluster_ID, labeller = cond_label_msqd, ncol = 3) +
#   theme(legend.position="right") +
#   # ggtitle("Market squid predictions") +
#   scale_x_continuous(name = "")  +
#   scale_y_continuous(name = "ln(Landings)") +
#   scale_color_manual(name = "Variable: ",
#                      values = c("Actual landings" = "gray85", "Predicted landing (no closure)" = "royalblue",
#                                 "Predicted landing (closure)" = "green")) + 
#   theme(axis.text.x = element_text(size = 7))
# gg_msqd
# 
# 
# 
# # Northern anchovy
# 
# # set.seed(123)
# # prediction_NANC_noclosure <- cbind(predict(fit_qNANC,
# #                                 newdata = data.frame(
# #                                   NANC_Price_z = dataset_nanc_landing$NANC_Price_z,
# #                                   Price.Fishmeal.AFI_z = dataset_nanc_landing$Price.Fishmeal.AFI_z,
# #                                   port_ID = dataset_nanc_landing$port_ID,
# #                                   NANC_Landings = dataset_nanc_landing$NANC_Landings,
# #                                   MSQD_SPAWN_SDM_90 = dataset_nanc_landing$MSQD_SPAWN_SDM_90,
# #                                   NANC_SDM_20 = dataset_nanc_landing$NANC_SDM_20,
# #                                   PSDN.Total.Closure = 0,
# #                                   Length_z = dataset_nanc_landing$Length_z,
# #                                   PSDN_SDM_60 = dataset_nanc_landing$PSDN_SDM_60,
# #                                   MSQD.Open = dataset_nanc_landing$MSQD.Open,
# #                                   PSDN.Open = 1,
# #                                   port_cluster_ID = dataset_nanc_landing$port_cluster_ID)),
# #                                   dataset_nanc_landing)
# # saveRDS(prediction_NANC_noclosure, file = "prediction_NANC_noclosure.rds")
# prediction_NANC_noclosure <- readRDS(file = "prediction_NANC_noclosure.rds")
# prediction_NANC_noclosure <- prediction_NANC_noclosure %>%
#   rename(Estimate.logNANCLandings.closure = Estimate.logNANCLandings) 
# Estimate.logNANCLandings <- prediction_NANC[,5]
# prediction_NANC_noclosure <- cbind.data.frame(prediction_NANC_noclosure, Estimate.logNANCLandings) %>%
#   filter(LANDING_YEAR>2013)
# prediction_NANC_noclosure$LANDING_YEAR   <- as.integer(prediction_NANC_noclosure$LANDING_YEAR)
# 
# prediction_sel <- prediction_NANC_noclosure [,-1]
# prediction_sel <- prediction_sel[,-1]
# prediction_sel <- prediction_sel[,-1]
# prediction_sel <- prediction_sel[,-1]
# 
# # by cluster
# df_cluster_NANC <- prediction_sel %>% 
#   dplyr::select(Estimate.logNANCLandings.closure, Estimate.logNANCLandings, ln_NANC_Landings, 
#                 LANDING_YEAR, LANDING_MONTH, port_cluster_ID, VESSEL_NUM) %>%
#   mutate(Date = paste(LANDING_YEAR, LANDING_MONTH,sep="-")) %>% 
#   mutate(Date = zoo::as.yearmon(Date)) %>%
#   group_by(Date, port_cluster_ID, VESSEL_NUM) %>% 
#   summarise(Est_landings.closure = sum(Estimate.logNANCLandings.closure), 
#             Est_landings = sum(Estimate.logNANCLandings), 
#             Landings = sum(ln_NANC_Landings)) %>%
#   group_by(Date, port_cluster_ID) %>% 
#   summarise(Est_landings_mean = mean(Est_landings),
#             Est_landings_mean.closure = mean(Est_landings.closure),
#             Landings_mean = mean(Landings)) %>%
#   group_by(port_cluster_ID) %>%
#   arrange(port_cluster_ID, Date) %>%
#   mutate(Est_landings_mean_MA = rollapply(data = Est_landings_mean, 
#                                           width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
#   mutate(Est_landings_mean_MA.closure = rollapply(data = Est_landings_mean.closure, 
#                                                   width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
#   mutate(Landings_mean_MA     = rollapply(data = Landings_mean    , 
#                                           width = 3, FUN = mean, align = "right", fill = NA, na.rm = T)) %>%
#   mutate(Est_landings_mean_MA.closure = ifelse(Date < "July 2015", Landings_mean_MA, Est_landings_mean_MA.closure)) %>%
#   mutate(Est_landings_mean_MA = ifelse(Date < "July 2015", Landings_mean_MA, Est_landings_mean_MA))
# 
# 
# # Plot
# cond_label_nanc <- as_labeller(c("6-CWA" = "PNW sardine specialists\n(Coastal Washington Ports)",
#                                  "6-CLW" = "PNW sardine specialists\n(Columbia River OR)",
#                                  "6-CLO" = "PNW sardine specialists\n(Columbia River WA)",
#                                  "7-SBA" = "Southern CCS forage fish\ndiverse (Santa Barbara)",
#                                  "7-SDA" = "Southern CCS forage fish\ndiverse (San Diego)",
#                                  "7-MNA" = "Southern CCS forage fish\ndiverse (Monterey)",
#                                  "7-LAA" = "Southern CCS forage fish\ndiverse (Los Angeles)"))
# 
# gg_nanc <- ggplot(df_cluster_NANC) + 
#   geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA, color = "Predicted landing (closure)"), size = 0.75,linetype="dashed") +
#   geom_line(mapping = aes(x = Date, y = Est_landings_mean_MA.closure, color = "Predicted landing (no closure)"), size = 0.75,linetype="dashed") +
#   geom_line(mapping = aes(x = Date, y = Landings_mean_MA, color = "Actual landings"), size = 1) + 
#   facet_wrap(~port_cluster_ID, labeller = cond_label_nanc, ncol = 3) +
#   theme(legend.position="right") +
#   scale_x_continuous(name = "")  +
#   scale_y_continuous(name = "ln(Landings)") +
#   scale_color_manual(name = "Variable: ",
#                      values = c("Actual landings" = "gray85", "Predicted landing (no closure)" = "royalblue",
#                                 "Predicted landing (closure)" = "green")) + 
#   theme(axis.text.x = element_text(size = 7))
# gg_nanc