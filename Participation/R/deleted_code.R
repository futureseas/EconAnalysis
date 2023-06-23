#-----------------------------------------------
### Merge data to SDM 

psdn.sdm <- read.csv(file = 'Participation/SDMs/sdm_psdn.csv')
msqd.sdm <- readRDS(file = 'Participation/SDMs/sdm_msqd.rds')
nanc.sdm <- readRDS(file = 'Participation/SDMs/sdm_nanc.rds')
phrg.sdm <- readRDS(file = 'Participation/SDMs/sdm_phrg.rds')
cmck.sdm <- readRDS(file = 'Participation/SDMs/sdm_cmck.rds')
jmck.sdm <- readRDS(file = 'Participation/SDMs/sdm_jmck.rds')
msqd_spawn.sdm <- readRDS(file = 'Participation/SDMs/sdm_msqd_spawn.rds')

psdn.sdm[is.na(psdn.sdm)] <- 0
msqd.sdm[is.na(msqd.sdm)] <- 0
nanc.sdm[is.na(nanc.sdm)] <- 0
phrg.sdm[is.na(phrg.sdm)] <- 0
cmck.sdm[is.na(cmck.sdm)] <- 0
jmck.sdm[is.na(jmck.sdm)] <- 0                     
msqd_spawn.sdm[is.na(msqd_spawn.sdm)] <- 0


Tickets_SDM <- merge(Tickets_CPUE, psdn.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)
Tickets_SDM <- merge(Tickets_SDM, msqd.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)
Tickets_SDM <- merge(Tickets_SDM, nanc.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)
Tickets_SDM <- merge(Tickets_SDM, phrg.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)
Tickets_SDM <- merge(Tickets_SDM, cmck.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)
Tickets_SDM <- merge(Tickets_SDM, jmck.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)
Tickets_SDM <- merge(Tickets_SDM, msqd_spawn.sdm,
                     by = (c('LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)


##-------------------------------
## Lagged SDMs and prices

### Lagged date in SDM databases
psdn.sdm$set_date<-as.Date(with(
  psdn.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")
msqd.sdm$set_date<-as.Date(with(
  msqd.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")
nanc.sdm$set_date<-as.Date(with(
  nanc.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")
phrg.sdm$set_date<-as.Date(with(
  phrg.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")
cmck.sdm$set_date<-as.Date(with(
  cmck.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")
jmck.sdm$set_date<-as.Date(with(
  jmck.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")
msqd_spawn.sdm$set_date<-as.Date(with(
  msqd_spawn.sdm,
  paste(LANDING_YEAR, LANDING_MONTH, LANDING_DAY,sep="-")),
  "%Y-%m-%d")


### Lagged date in price database
prices <- Tickets_SDM %>% 
  dplyr::select(c(set_date, PORT_AREA_CODE, PACFIN_SPECIES_CODE, Price_mtons)) %>% 
  drop_na() %>%
  unique() %>%  
  rename(lag_Price_mtons = Price_mtons) %>%
  rename(prev_days_date = set_date)

Tickets_SDM <- merge(Tickets_SDM, prices, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE', 'PACFIN_SPECIES_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(PSDN)
psdn.sdm <- psdn.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_PSDN_SDM_30 = PSDN_SDM_30) %>%
  rename(lag_PSDN_SDM_60 = PSDN_SDM_60) %>%
  rename(lag_PSDN_SDM_90 = PSDN_SDM_90) %>%
  rename(lag_PSDN_SDM_220 = PSDN_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, psdn.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(MSQD)
msqd.sdm <- msqd.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_MSQD_SDM_30 =  MSQD_SDM_30) %>%
  rename(lag_MSQD_SDM_90 =  MSQD_SDM_90) %>%
  rename(lag_MSQD_SDM_220 = MSQD_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, msqd.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(NANC)
nanc.sdm <- nanc.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_NANC_SDM_20  = NANC_SDM_20) %>%
  rename(lag_NANC_SDM_30  = NANC_SDM_30) %>%
  rename(lag_NANC_SDM_90  = NANC_SDM_90) %>%
  rename(lag_NANC_SDM_220 = NANC_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, nanc.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(PHRG)
phrg.sdm <- phrg.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_PHRG_SDM_30  = PHRG_SDM_30) %>%
  rename(lag_PHRG_SDM_90  = PHRG_SDM_90) %>%
  rename(lag_PHRG_SDM_220 = PHRG_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, phrg.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(CMCK)
cmck.sdm <- cmck.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_CMCK_SDM_30  = CMCK_SDM_30) %>%
  rename(lag_CMCK_SDM_90  = CMCK_SDM_90) %>%
  rename(lag_CMCK_SDM_220 = CMCK_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, cmck.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(JMCK)
jmck.sdm <- jmck.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_JMCK_SDM_30  = JMCK_SDM_30) %>%
  rename(lag_JMCK_SDM_90  = JMCK_SDM_90) %>%
  rename(lag_JMCK_SDM_220 = JMCK_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, jmck.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)

### lagged SDM(MSQD_SPAWN)
msqd_spawn.sdm <- msqd_spawn.sdm %>% select(-c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  rename(lag_MSQD_SPAWN_SDM_30  = MSQD_SPAWN_SDM_30) %>%
  rename(lag_MSQD_SPAWN_SDM_90  = MSQD_SPAWN_SDM_90) %>%
  rename(lag_MSQD_SPAWN_SDM_220 = MSQD_SPAWN_SDM_220) %>%
  rename(prev_days_date = set_date)
Tickets_SDM <- merge(Tickets_SDM, msqd_spawn.sdm, 
                     by = (c('prev_days_date', 'PORT_AREA_CODE')),
                     all.x = TRUE, all.y = FALSE)
saveRDS(Tickets_SDM, "Tickets_temp.rds")


#-----------------------------------------------
# ### Compare CPUE_index to SDMs
# compare.SDM.CPUE <- Tickets_SDM %>% filter(Species_Dominant == "PSDN") %>%
#   dplyr::select(c('CPUE_index', 'PSDN_SDM_60', 'PORT_AREA_CODE', 
#                   'LANDING_YEAR', 'LANDING_MONTH', 'LANDING_DAY')) %>%
#   group_by(LANDING_YEAR, PORT_AREA_CODE) %>% 
#   summarize(CPUE_index = mean(CPUE_index, na.rm = TRUE), 
#             PSDN_SDM_60 = mean(PSDN_SDM_60, na.rm = TRUE))
# library(hrbrthemes)
# 
# # Basic scatter plot
# ggplot(compare.SDM.CPUE, aes(x=CPUE_index, y=PSDN_SDM_60)) +
#   facet_wrap(~ PORT_AREA_CODE) + 
#   geom_point( color="#69b3a2") +
#   geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)


### Include SDMs ###

# "PSDN_SDM_30", "PSDN_SDM_60", "PSDN_SDM_90","PSDN_SDM_220",
# "MSQD_SDM_30", "MSQD_SDM_90", "MSQD_SDM_220",
# "NANC_SDM_20", "NANC_SDM_30", "NANC_SDM_90", "NANC_SDM_220",
# "PHRG_SDM_30", "PHRG_SDM_90" , "PHRG_SDM_220",
# "CMCK_SDM_30", "CMCK_SDM_90", "CMCK_SDM_220",
# "JMCK_SDM_30", "JMCK_SDM_90", "JMCK_SDM_220",
# "MSQD_SPAWN_SDM_30", "MSQD_SPAWN_SDM_90", "MSQD_SPAWN_SDM_220",
# "lag_PSDN_SDM_30", "lag_PSDN_SDM_60", "lag_PSDN_SDM_90", "lag_PSDN_SDM_220",
# "lag_MSQD_SDM_30", "lag_MSQD_SDM_90", "lag_MSQD_SDM_220",       
# "lag_NANC_SDM_20", "lag_NANC_SDM_30", "lag_NANC_SDM_90", "lag_NANC_SDM_220",       
# "lag_PHRG_SDM_30", "lag_PHRG_SDM_90", "lag_PHRG_SDM_220",       
# "lag_CMCK_SDM_30", "lag_CMCK_SDM_90", "lag_CMCK_SDM_220",       
# "lag_JMCK_SDM_30", "lag_JMCK_SDM_90", "lag_JMCK_SDM_220",
# "lag_MSQD_SPAWN_SDM_30", "lag_MSQD_SPAWN_SDM_90", "lag_MSQD_SPAWN_SDM_220"




## Estimate models for landings
#
#
# model_landings <- lm(ln_Landings_mtons ~ factor(Species_Dominant):factor(PORT_AREA_CODE) +
#                          factor(set_month) + trend + Vessel.length,
#                        data = datPanel)
# summary(model_landings)
# 
#   models <- list(
#   "ln(Landings)"  = model_landings,
#   "ln(Prices)"     = model_price)
# 
# gm <- modelsummary::gof_map
# options(OutDec=".")
# modelsummary::modelsummary(models, fmt = 2,
#                            gof_map = c("nobs", "adj.r.squared"),
#                            statistic = "({std.error}){stars}",
#                            output = "general_landings_price_models.docx")
# 
#
## Estimate models for landings using SDM (conditional vessel have decide to participate)
#
# ### Pacific sardine landing model
# datPanel_PSDN <- datPanel %>% filter(Species_Dominant == "PSDN") %>%
#   dplyr::mutate(Closure = ifelse(set_date >= "2008-05-29" & set_date < "2008-07-01", 1, 
#                           ifelse(set_date >= "2008-08-08" & set_date < "2008-09-01", 1, 
#                           ifelse(set_date >= "2008-09-23" & set_date < "2009-01-01", 1, 
#                           ifelse(set_date >= "2009-02-20" & set_date < "2009-07-01", 1, 
#                           ifelse(set_date >= "2009-07-18" & set_date < "2009-09-01", 1, 
#                           ifelse(set_date >= "2009-09-23" & set_date < "2010-01-01", 1,  
#                           ifelse(set_date >= "2010-06-12" & set_date < "2010-07-01", 1, 
#                           ifelse(set_date >= "2010-07-22" & set_date < "2010-09-01", 1, 
#                           ifelse(set_date >= "2010-09-24" & set_date < "2011-01-01", 1, 
#                           ifelse(set_date >= "2011-03-05" & set_date < "2011-07-01", 1, 
#                           ifelse(set_date >= "2011-07-12" & set_date < "2011-09-01", 1, 
#                           ifelse(set_date >= "2011-09-21" & set_date < "2012-01-01", 1, 
#                           ifelse(set_date >= "2012-08-23" & set_date < "2012-09-01", 1, 
#                           ifelse(set_date >= "2013-08-22" & set_date < "2013-09-01", 1, 
#                           ifelse(set_date >= "2015-04-28", 1, 0)))))))))))))))) %>% 
#   filter(Closure == 0) %>%
#   dplyr::mutate(ln_Landings_mtons = log(Landings_mtons)) %>% 
#   mutate(trend = set_year-min_year_est + 1)
# 
# qPSDN <- lm(ln_Landings_mtons ~ PSDN_SDM_60 + 
#             factor(set_month) + trend + 
#             Vessel.length , data = datPanel_PSDN)
# 
# ### Market squid landing model
# datPanel_MSQD <- datPanel %>% filter(Species_Dominant == "MSQD") %>%
#   dplyr::mutate(Closure = ifelse(set_date >= "2010-12-17" & set_date < "2011-03-31", 1, 
#                           ifelse(set_date >= "2011-11-18" & set_date < "2012-03-31", 1, 
#                           ifelse(set_date >= "2012-11-21" & set_date < "2013-03-31", 1, 0)))) %>% 
#   dplyr::mutate(weekend = ifelse(chron::is.weekend(set_date), 1, 0)) %>%
#   filter(weekend == 0) %>%  filter(Closure == 0) %>% mutate(trend = set_year-min_year_est + 1) %>%
#   dplyr::mutate(ln_Landings_mtons = log(Landings_mtons))
# 
# qMSQD <- lm(ln_Landings_mtons ~ MSQD_SDM_90 + 
#               factor(set_month) + trend +
#               Vessel.length, data = datPanel_MSQD)
# summary(qMSQD)
# 
# ### Northern anchovy
# datPanel_NANC <- datPanel %>% filter(Species_Dominant == "NANC") %>%
#   dplyr::mutate(PSDN.Open = ifelse(set_date >= "2008-05-29" & set_date < "2008-07-01", 0, 
#                           ifelse(set_date >= "2008-08-08" & set_date < "2008-09-01", 0, 
#                           ifelse(set_date >= "2008-09-23" & set_date < "2009-01-01", 0, 
#                           ifelse(set_date >= "2009-02-20" & set_date < "2009-07-01", 0, 
#                           ifelse(set_date >= "2009-07-18" & set_date < "2009-09-01", 0, 
#                           ifelse(set_date >= "2009-09-23" & set_date < "2010-01-01", 0,  
#                           ifelse(set_date >= "2010-06-12" & set_date < "2010-07-01", 0, 
#                           ifelse(set_date >= "2010-07-22" & set_date < "2010-09-01", 0, 
#                           ifelse(set_date >= "2010-09-24" & set_date < "2011-01-01", 0, 
#                           ifelse(set_date >= "2011-03-05" & set_date < "2011-07-01", 0, 
#                           ifelse(set_date >= "2011-07-12" & set_date < "2011-09-01", 0, 
#                           ifelse(set_date >= "2011-09-21" & set_date < "2012-01-01", 0, 
#                           ifelse(set_date >= "2012-08-23" & set_date < "2012-09-01", 0, 
#                           ifelse(set_date >= "2013-08-22" & set_date < "2013-09-01", 0, 
#                           ifelse(set_date >= "2015-04-28", 0, 1)))))))))))))))) %>%
#   dplyr::mutate(ln_Landings_mtons = log(Landings_mtons)) %>% mutate(trend = set_year-min_year_est + 1)
# 
# qNANC <- lm(ln_Landings_mtons ~ NANC_SDM_20 +
#               factor(set_month) + trend +
#               Vessel.length, data = datPanel_NANC)
# summary(qNANC)
#
# datPanelcor <- datPanel %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower, Vessel.length)) %>% drop_na() %>% unique()
# cor(datPanelcor$Vessel.horsepower, datPanelcor$Vessel.length)
#
#
## Create table for paper (all species)
# 
#   models <- list(
#   "Pacific sardine"  = qPSDN,
#   "Market squid"     = qMSQD,
#   "Northern anchovy" = qNANC)
# 
# gm <- modelsummary::gof_map
# options(OutDec=".")
# modelsummary::modelsummary(models, fmt = 2,
#                            gof_map = c("nobs", "adj.r.squared"),
#                            statistic = "({std.error}){stars}",
#                            output = "landings_models.docx")


