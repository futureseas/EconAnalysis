###########################################
##                                       ##
##### Run STAN model using PacFIN data #### 
##                                       ##
###########################################

rm(list = ls(all.names = TRUE)) 
gc()

library(bookdown)
library(doBy)
library(dplyr)
library(fastDummies)
library(ggplot2)
library(here)
library(hrbrthemes)
library(lmtest)
library(lubridate)
library(magrittr)
library(plm)
library(reshape)
library(reshape2)
library(rstan)
library(scales)
library(sjlabelled)
library(summarytools)
library(texreg)
library(tidyr)
library(tidyverse)
library(tinytex)
library(viridis)
library(xtable)
library(zoo)

# Read data processed
PacFIN_dat <- read.csv(file = here::here("Data", "PacFin.csv"))



# Clean dataset

est_data <- PacFIN_dat %>%
  filter(Species_name == "PACIFIC SARDINE"  |
           Species_name == "MARKET SQUID") %>% 
  melt(id.vars=c("Species_name", "State", "Species_code", "Landing_year", "Port", "Complex", "Management_group")) %>% 
  dcast(Landing_year + Port ~ Species_code + variable) %>%
  mutate(port_num = as.numeric(as.factor(Port))) %>%
  dplyr::rename(PSDN_SDM = PSDN_PSDN_SDM) %>%
  dplyr::rename(MSQD_SDM = MSQD_MSQD_SDM_60)

  # Change chr to numeric #
  est_data$PSDN_SDM      <- as.numeric(est_data$PSDN_SDM)
  est_data$MSQD_SDM      <- as.numeric(est_data$MSQD_SDM)


###############################
## Pacific Sardine Equation ###
###############################

  # hist_psdn <- est_data %>%
  #   filter(MSQD_Landings>0) %>%
  #   mutate(log_land_psdn = MSQD_Landings)
  # 
  # hist(hist_psdn$log_land_psdn)
  
  
# Change chr to numeric #
est_data$PSDN_Landings <- as.numeric(est_data$PSDN_Landings)

# Select data for estimation, replace N/A landings to zero #
est_data <- est_data %>%
  dplyr::select(port_num, Port, PSDN_SDM, MSQD_SDM, PSDN_Landings, Landing_year) %>%
  dplyr::mutate(PSDN_Landings = coalesce(PSDN_Landings, 0)) %>% 
  filter(Landing_year >= 2000 & Landing_year <= 2015)


# Select ports that at least they have landing PSDN once, and drop N/A #
Tot.landings.ports <- as.data.frame(cbind(rowsum(est_data$PSDN_Landings, est_data$Port, na.rm = TRUE)))

Tot.landings.ports <- Tot.landings.ports %>%
  mutate(dTotalPSDN = ifelse(V1>0, 1, 0)) %>%
  mutate(Port = rownames(Tot.landings.ports)) %>%
  dplyr::select(Port, dTotalPSDN)


### NOTE: If I use drop_na, some ports without MSQD landings are excluded. 
est_data <- est_data %>%
  merge(Tot.landings.ports, by.x = "Port", by.y = "Port", all.x = TRUE, all.y = FALSE) %>%
  filter(dTotalPSDN==1) %>%
  # mutate(dSDA = ifelse(Port == "SDA", 1, 0)) %>%
  # filter(dSDA == 0) %>%
  drop_na()
  

# Create matrices and vectors to run STAN model #
year_id <- as.vector(cbind(as.numeric(factor(est_data$Landing_year))));
est_data$port_ID <- udpipe::unique_identifier(est_data, fields = "port_num", start_from = 1) 

portID_names <- est_data %>%
  dplyr::select(Port, port_ID) %>%
  unique()

### Estimate using BRMS package ###
library(brms)
library(cmdstanr)

# Notes: BRMS cannot handle splines in group-level terms.
# fit_qPSDN_ns <- brm(bf(PSDN_Landings ~ MSQD_SDM + PSDN_SDM + (1 + PSDN_SDM + MSQD_SDM | port_ID), 
#                     hu ~ PSDN_SDM + MSQD_SDM + (1 + PSDN_SDM + MSQD_SDM | port_ID)),
#                  data = est_data,
#                  family = hurdle_gamma(), 
#                  control = list(adapt_delta = 0.999, max_treedepth = 20),
#                  chains = 4, cores = 4)

fit_qPSDN_lognormal <- brm(bf(PSDN_Landings ~ t2(PSDN_SDM, MSQD_SDM) + (1 + PSDN_SDM + MSQD_SDM | port_ID), 
                    hu ~  t2(PSDN_SDM, MSQD_SDM) + (1 + PSDN_SDM + MSQD_SDM | port_ID)),
                 data = est_data,
                 family = hurdle_lognormal(), 
                 control = list(adapt_delta = 0.999, max_treedepth = 20),
                 chains = 4, cores = 4)

fit_qPSDN_gamma <- brm(bf(PSDN_Landings ~ t2(PSDN_SDM, MSQD_SDM) + (1 + PSDN_SDM + MSQD_SDM | port_ID), 
                          hu ~  t2(PSDN_SDM, MSQD_SDM) + (1 + PSDN_SDM + MSQD_SDM | port_ID)),
                       data = est_data,
                       family = hurdle_gamma(), 
                       control = list(adapt_delta = 0.999, max_treedepth = 20),
                       chains = 4, cores = 4)

# Compare models #
# pp_check(fit_qPSDN_t2)
# pp_check(fit_qPSDN_t2nc)
# LOO(fit_qPSDN_t2nc_linh, fit_qPSDN_t2nc)

# Effect of SDM's on Sardine landings #
summary(fit_qPSDN_t2nc) 
plot(conditional_effects(fit_qPSDN_t2nc, surface=TRUE), ask = FALSE)
plot(conditional_smooths(fit_qPSDN_t2nc))

# Investigate chain and posterior distributions. 
# https://cran.r-project.org/web/packages/bayesplot/vignettes/graphical-ppcs.html
# plot(fit_qPSDN_t2nc, pars = c("PSDN_SDM"))
# launch_shinystan(fit_qPSDN) 

# ### Predictions ###
# pred_data <- data.frame(PSDN_SDM = c(0.5, 0.25), MSQD_SDM = c(0.5), Port_ID = 1)
# predict(fit_qPSDN, newdata = pred_data, re_formula = NA)

# ## Compare models ##
# loo(fit1, fit2)

## Check https://www.pcouncil.org/stock-assessments-and-fishery-evaluation-safe-documents/ for ACL and closures.