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

# Change chr to numeric #
est_data$PSDN_Landings <- as.numeric(est_data$PSDN_Landings)

# Select data for estimation, replace N/A landings to zero #
est_data <- est_data %>%
  dplyr::select(port_num, Port, PSDN_SDM, MSQD_SDM, PSDN_Landings, Landing_year) %>%
  dplyr::mutate(PSDN_Landings = coalesce(PSDN_Landings, 0))


# Select ports that at least they have landing MSQD and PSDN once, and drop N/A #
Tot.landings.ports <- as.data.frame(cbind(rowsum(est_data$PSDN_Landings, est_data$Port, na.rm = TRUE),
                                      rowsum(est_data$MSQD_Landings, est_data$Port, na.rm = TRUE)))

Tot.landings.ports <- Tot.landings.ports %>%
  mutate(all_species = ifelse(V1>0, ifelse(V2>0, 1, 0),0)) %>%
  mutate(Port = rownames(Tot.landings.ports)) %>%
  dplyr::select(Port, all_species)

est_data <- merge(est_data, Tot.landings.ports, by.x = "Port", 
                   by.y = "Port", all.x = TRUE, all.y = FALSE)

est_data <- est_data %>%
  filter(all_species==1) %>%
  mutate(dSDA = ifelse(Port == "SDA", 1, 0)) %>%
  filter(dSDA == 0) %>% 
  drop_na() # Should I drop N/A in database?
  # mutate(dCBA = ifelse(Port == "CBA", 1, 0)) %>%
  # filter(dCBA == 0) %>% 
  


# Create matrices and vectors to run STAN model #
year_id <- as.vector(cbind(as.numeric(factor(est_data$Landing_year))));

  # %>%
  # dummy_cols(select_columns = 'year_num') %>%
  # dplyr::select(starts_with("year_num_")) %>%
  # as.matrix()


est_data$port_ID <- udpipe::unique_identifier(est_data, fields = "port_num", start_from = 1) 
Q=as.matrix(cbind(est_data$PSDN_Landings, est_data$MSQD_Landings))
Q_cen=as.matrix(cbind(ifelse(est_data$PSDN_Landings == 0, 1 , 0), 
                      ifelse(est_data$MSQD_Landings == 0, 1 , 0)));
Q_probit=as.matrix(cbind(ifelse(est_data$PSDN_Landings == 0, 0 , 1), 
                      ifelse(est_data$MSQD_Landings == 0, 0 , 1)));


# hist_psdn <- est_data %>%
#   filter(MSQD_Landings>0) %>%
#   mutate(log_land_psdn = MSQD_Landings)
# 
# hist(hist_psdn$log_land_psdn)



portID_names <- est_data %>%
  dplyr::select(Port, port_ID) %>%
  unique()

# Dataset to be used by Stan #
data_stan <- list(qPSDN = as.vector(cbind(est_data$PSDN_Landings)),
                  qMSQD = as.vector(cbind(est_data$MSQD_Landings)),
                  q_probit=Q_probit,
                  N=nrow(Q),
                  J=ncol(Q),
                  Y=max(year_id), 
                  L=nrow(portID_names),
                  portID = est_data$port_ID,
                  sdm_psdn   = as.vector(cbind(est_data$PSDN_SDM)), 
                  sdm_msqd   = as.vector(cbind(est_data$MSQD_SDM)), 
                  yearID = year_id
                  )


# Run Stan model
fit <- stan(
  file = here::here("stan_model_hurdle.stan"),  # Stan program
  data = data_stan,    # named list of data
  chains = 4,             # number of Markov chains. I use 4 to match the number of cores on my PC
  warmup = 1000,          # number of warmup iterations per chain
  iter = 5000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
  control = list(max_treedepth = 20) ## When I use 10 (default), STAN tells me that I need to increase treedepth
)

# summary(fit)$summary %>% head()

# pdf(file = "Figures\\prob_psdn.pdf")
# plot(fit, pars = c("prob_psdn"))
# dev.off()
# 
# 
# pdf(file = "Figures\\prob_msqd.pdf")
# plot(fit, pars = c("prob_msqd"))
# dev.off()
# 
# pdf(file = "Figures\\Alpha_ports.pdf")
# plot(fit, pars = c("alpha"))
# dev.off()

plot(fit, pars = c("prob_psdn"))


# 0.37 correlation between sardine and squid.
print(fit, pars = c("beta_year"))
print(fit, pars = c("prob_psdn"))

