## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)
rm(list = ls(all.names = TRUE)) 
load("stan_fit.RData")
# gc()
library(bookdown)
library(brms)
library(cmdstanr)
library(doBy)
library(dplyr)
library(fastDummies)
library(ggplot2)
library(here)
library(hrbrthemes)
library(kableExtra)
library(lmtest)
library(lubridate)
library(magrittr)
library(patchwork)
library(plm)
library(papaja)
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


## ----read_data, include=FALSE-------------------------------------------------
PacFIN_dat <- read.csv(file = here("Data", "PacFin.csv"))
sapply(PacFIN_dat, class) 


## ----functions, include=FALSE-------------------------------------------------

meanfun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...)) #, v=var(x, na.rm=TRUE, ...), l=length(x))
}

sumfun <- function(x, ...){
  c(sum=sum(x, na.rm=TRUE, ...)) #, v=var(x, na.rm=TRUE, ...), l=length(x))
}

sum_mean_fun <- function(x, ...){
  c(mean=mean(x, na.rm=TRUE, ...), sum=sum(x, na.rm=TRUE, ...)) #, l=length(x))
}

port_by_state <- function(state_name) {
  if (state_name == "SOME") {
    collapse <- PacFIN_dat %>%
    filter(Port == "CLO" | Port == "LAA" | Port == "MNA" | Port == "SBA" | Port == "SFA") %>%
    filter(Species_name == "PACIFIC SARDINE" | Species_name == "NORTHERN ANCHOVY" | Species_name == "MARKET SQUID" | Species_name == "PACIFIC HERRING") %>%
    mutate(Port = fct_relevel(Port, "LAA", "SBA", "MNA", "SFA", "CLO"))
  }
  if (state_name == "ALL") {
    collapse <- PacFIN_dat %>%
    filter(Port == "BDA" | Port == "CBA" | Port == "CCA" | Port == "CLO" | Port == "ERA" | Port == "LAA" | Port == "MNA" | Port == "MRA" | Port == "NPA" | Port == "NPS" | Port == "SBA" | Port == "SDA" | Port == "SFA") %>%
    filter(Species_name == "PACIFIC SARDINE" | Species_name == "NORTHERN ANCHOVY" | Species_name == "MARKET SQUID" | Species_name == "PACIFIC HERRING") %>%
    mutate(Port = fct_relevel(Port, "SDA", "LAA", "SBA", "MNA", "MRA", "SFA", "BDA", "ERA", "CCA", "CBA", "NPA", "CLO", "NPS"))
  }
  else {
    collapse <- PacFIN_dat %>%
    filter(Port == "BDA" | Port == "CBA" | Port == "CCA" | Port == "CLO" | Port == "ERA" | Port == "LAA" | Port == "MNA" | Port == "MRA" | Port == "NPA" | Port == "NPS" | Port == "SBA" | Port == "SDA" | Port == "SFA") %>%
    filter(State == state_name & (Species_name == "PACIFIC SARDINE" | Species_name == "NORTHERN ANCHOVY" | Species_name == "MARKET SQUID" | Species_name == "PACIFIC HERRING")) %>%
    mutate(Port = fct_relevel(Port, "SDA", "LAA", "SBA", "MNA", "MRA", "SFA", "BDA", "ERA", "CCA", "CBA", "NPA", "CLO", "NPS"))
  }
  
  ggplot(collapse, aes(y=Landings, x=Landing_year, color=Species_code, group=Species_code)) +
  geom_line(size=1) + 
  facet_wrap(~ Port) +
  theme(legend.position="bottom") + 
  theme(axis.text.x = element_text(angle=90))
}



## ----avg_landings, echo=FALSE, fig.cap="Average annual landing and revenues for the CPS fishery by species.\\label{fig:avg_lan_rev}", message=FALSE, warning=FALSE----
landings <- summaryBy(Landings ~ Species_code + Landing_year + Management_group, FUN=sumfun, data=PacFIN_dat) %>%
  filter(Management_group == "CPEL") %>%
  filter(Species_code != "PBNT" & Species_code != "CMCK" & 
           Species_code != "JMCK" & Species_code != "RHRG" & Species_code != "UMCK")

landings <- summaryBy(Landings.sum ~ Species_code, FUN=meanfun, data=landings)

g1 <- ggplot(landings, aes(Species_code, Landings.sum.mean)) +
  geom_col(width=0.4) + ggtitle("(a) Landings") + 
  theme(plot.title = element_text(size=9, face="bold.italic")) +
  xlab("Port areas") + ylab("Landings (tons)")


revenue <- summaryBy(Revenue ~ Species_code + Landing_year + Management_group, FUN=sumfun, data=PacFIN_dat) %>%
  filter(Management_group == "CPEL") %>%
  filter(Species_code != "PBNT" & Species_code != "CMCK" & 
           Species_code != "JMCK" & Species_code != "RHRG" & Species_code != "UMCK")

revenue <- summaryBy(Revenue.sum ~ Species_code, FUN=meanfun, data=revenue)


g2 <- ggplot(revenue, aes(Species_code, Revenue.sum.mean)) +
  geom_col(width=0.4) + ggtitle("(b) Revenue") + 
  theme(plot.title = element_text(size=9, face="bold.italic")) +
  xlab("Port areas") + ylab("Revenues (USD)")


g1 + g2

## ----ts_landings, eval=FALSE, fig.cap="Total annual landing by CPS species.\\label{fig:ts_landings}", message=FALSE, warning=FALSE, include=FALSE----
#> collapse <- PacFIN_dat %>%
#>   filter(Management_group == "CPEL") %>%
#>   filter(Species_code != "PBNT" & Species_code != "CMCK" & Species_code != "JMCK" & Species_code != "RHRG" & Species_code != "UMCK")
#> 
#> collapse <- summaryBy(Landings + Price ~ Species_name  + Landing_year, FUN=sumfun, data=collapse) %>%
#>   dplyr::select(Landing_year, Price.sum, Landings.sum, Species_name)
#> 
#> ggplot(collapse, aes(x = Landing_year, y = Landings.sum, group= Species_name, colour =  Species_name)) +
#>   geom_line(size=1.5) +
#>   scale_y_continuous(name = "Landings (M Tons)")


## ----ts_landings_vessels, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----
#> collapse <- PacFIN_dat %>%
#>   filter(Management_group == "CPEL") %>%
#>   filter(Species_code != "PBNT" & Species_code != "RHRG" & Species_code != "UMCK")
#> 
#> collapse <- summaryBy(Landings + Price + N_vessels ~ Species_name  + Landing_year, FUN=sum_mean_fun, data=collapse) %>%
#>   dplyr::select(Landing_year, Landings.sum, N_vessels.mean, Species_name)
#> coeff <- 1000
#> collapse$N_vessels.mean <- collapse$N_vessels.mean * coeff
#> df <- gather(collapse, key = Variable, value = value,
#>              c("Landings.sum", "N_vessels.mean"))
#> 
#> # Plot
#> ggplot(df, aes(x=Landing_year, y = value, group = Variable, colour = Variable)) +
#> geom_line(size=1) +
#> scale_y_continuous(name = "Landings (M Tons)",
#>                    sec.axis = sec_axis(~./coeff, name = "Number of Vessels")) +
#>   facet_wrap(~Species_name)


## ----avg_landings_by_port, echo=FALSE, fig.cap="Annual average landings by port area.\\label{fig:avg_landings_by_ports}", message=FALSE, warning=FALSE----
collapse <- summaryBy(Landings ~ Species_name + Species_code + Port + State, FUN=meanfun, data=PacFIN_dat) %>%
  filter(Port == "BDA" | Port == "CBA" | Port == "CCA" | Port == "CLO" | Port == "ERA" | Port == "LAA" | Port == "MNA" | Port == "MRA" | Port == "NPA" | Port == "NPS" | Port == "SBA" | Port == "SDA" | Port == "SFA") %>%
  filter(Species_name == "PACIFIC SARDINE" | Species_name == "NORTHERN ANCHOVY" | Species_name == "MARKET SQUID" | Species_name == "PACIFIC HERRING") %>%
  mutate(Port = fct_relevel(Port, "SDA", "LAA", "SBA", "MNA", "MRA", "SFA", "BDA", "ERA", "CCA", "CBA", "NPA", "CLO", "NPS"))

ggplot(collapse, aes(fill=Species_code, y=Landings.mean, x=Port)) +
  geom_bar(position="dodge", stat="identity") + 
  facet_grid(~ State, scale="free", space="free_x") +
  theme(legend.position="bottom") + 
  scale_fill_viridis(discrete = T) + 
  theme(axis.text.x = element_text(angle=90)) + 
  xlab("Port areas") + ylab("Landings (tons)") +
  guides(fill=guide_legend(title="Species: ")) +
  scale_color_brewer(palette="Set2")


## ----ts_by_port,  fig.cap = "Total annual landing by port area. 2000 - 2020. \\textit{Notes:} CLO = Columbia River (OR); LAA = Los Angeles; MNA = Monterey; SBA = Santa Barbara; SFA = San Francisco.\\label{fig:ts_landings_by_ports}" , echo=FALSE, message=FALSE, warning=FALSE----

collapse <- PacFIN_dat %>%
    filter(Port == "CLO" | Port == "LAA" | Port == "MNA" | Port == "SBA" | Port == "SFA") %>%
    filter(Species_name == "PACIFIC SARDINE" | Species_name == "NORTHERN ANCHOVY" | 
             Species_name == "MARKET SQUID") %>%
    mutate(Port = fct_relevel(Port, "LAA", "SBA", "MNA", "SFA", "CLO")) %>%
  filter(Landing_year >= 2000 & Landing_year <= 2020)

ggplot(collapse, aes(y=Landings, x=Landing_year, color=Species_code, group=Species_code)) +
  geom_line(size=1) + xlab("Port areas") + ylab("Landings (tons)") + 
  facet_wrap(~ Port) +
  theme(legend.position="bottom") + 
  theme(axis.text.x = element_text(angle=90)) +
  guides(color=guide_legend(title="Species: ")) +
  scale_color_brewer(palette="Set2")


## ----ts_n_vessels, fig.cap = "Total number of vessel by species. 2000 - 2020.\\label{fig:ts_n_vessels}", echo=FALSE, message=FALSE, warning=FALSE----
collapse <- PacFIN_dat %>%
  filter(Management_group == "CPEL") %>%
  filter(Species_name == "PACIFIC SARDINE" | Species_name == "NORTHERN ANCHOVY" | 
           Species_name == "MARKET SQUID") %>%
  filter(Landing_year >= 2000 & Landing_year <= 2020)

collapse <- summaryBy(N_vessels ~ Species_name  + Landing_year, FUN=meanfun, data=collapse) 

# Plot
ggplot(collapse, aes(x=Landing_year, y = N_vessels.mean,  colour = Species_name)) + 
  geom_line(size=1) +
  scale_y_continuous(name = "Number of vessels") +
  facet_wrap(~Species_name) + theme(legend.position="bottom") +
  guides(colour=guide_legend(title="Species: ")) +
  scale_color_brewer(palette="Set2") + 
  theme(legend.position="none") + xlab("Year") 


## ----SDM_land_by_port,  fig.cap = "Landings v/s probability of presence by port area. \\textit{Notes:} LAA = Los Angeles; MNA = Monterey; SBA = Santa Barbara.\\label{fig:sdm_land_by_port}", echo=FALSE, message=FALSE, warning=FALSE----

collapse <- PacFIN_dat %>%
  filter(Species_code == "PSDN" | Species_code == "MSQD"  | Species_code == "NANC") %>%
  select(Landing_year, Port, PSDN_SDM_60, MSQD_SDM_90, NANC_SDM_90, Landings, Species_code) %>% 
  filter(Port == "LAA" | Port == "MNA" | Port == "SBA") %>%
      mutate(Port = fct_relevel(Port, "LAA", "SBA", "MNA")) %>% 
  spread(Species_code, Landings) %>% 
  dplyr::rename(Landings_PSDN = PSDN) %>% dplyr::rename(Landings_MSQD = MSQD) %>% dplyr::rename(Landings_NANC = NANC) %>%
  filter(Landing_year >= 2000 & Landing_year <= 2018)
  
# Plot
coeff <- 50000
g1 <- ggplot(collapse) + 
  geom_line(mapping = aes(x = Landing_year, y = Landings_PSDN),
            size = 0.5, color = "grey") +
  geom_line(mapping = aes(x = Landing_year, y = PSDN_SDM_60*coeff), 
            size = 0.5, color = "blue", linetype = "dashed") + 
  facet_wrap(~ Port) + 
  scale_x_continuous(name = element_blank(), labels = NULL) +
  scale_y_continuous(name = element_blank(), sec.axis = sec_axis(~./coeff, name = "")) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(a) Pacific sardine")

g2 <- ggplot(collapse) + 
  geom_line(mapping = aes(x = Landing_year, y = Landings_MSQD),
            size = 0.5, color = "grey") +
  geom_line(mapping = aes(x = Landing_year, y = MSQD_SDM_90*coeff), 
            size = 0.5, color = "blue", linetype = "dashed") + 
  facet_wrap(~ Port) + 
  scale_x_continuous(name = element_blank(), labels = NULL) +
  scale_y_continuous(name = "Landings", sec.axis = sec_axis(~./coeff, name = "P(presence)")) +
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(b) Market squid")

g3 <- ggplot(collapse) + 
  geom_line(mapping = aes(x = Landing_year, y = Landings_NANC, color = "Landings"),
            size = 0.5) +
  geom_line(mapping = aes(x = Landing_year, y = NANC_SDM_90*coeff, color = "Probability of presence"), 
            size = 0.5, linetype = "dashed") + 
  facet_wrap(~ Port) + 
  scale_x_continuous(name = "Year", labels = NULL) +
  scale_y_continuous(name = element_blank(), sec.axis = sec_axis(~./coeff, name = element_blank())) +
  theme(legend.position="bottom", plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  ggtitle("(c) Northern anchovy") +  scale_color_manual(name = "Variable: ", 
                     values = c("Landings" = "grey", "Probability of presence" = "blue"))

g1 / g2 / g3



## ----desc_table, echo=FALSE, results='asis'-----------------------------------
# Clean dataset
PacFIN_dat = PacFIN_dat[,!grepl("*_LE_",names(PacFIN_dat))]
PacFIN_dat <- PacFIN_dat %>%
  filter(Species_code == "PSDN" | Species_code == "MSQD"  | Species_code == "NANC") %>%
  select(Landing_year, Port, PSDN_SDM_60, MSQD_SDM_90, NANC_SDM_90, ACL_PSDN.sum,
         Landings, Price, Revenue, Species_code) %>% 
  melt(id.vars=c("Species_code", "Landing_year", "Port")) %>% 
  dcast(Landing_year + Port ~ Species_code + variable) %>%
  mutate(port_num = as.numeric(as.factor(Port))) %>%
  dplyr::rename(PSDN_SDM_60 = PSDN_PSDN_SDM_60) %>%
  dplyr::rename(MSQD_SDM_90 = MSQD_MSQD_SDM_90) %>%
  dplyr::rename(NANC_SDM_90 = NANC_NANC_SDM_90) %>%
  dplyr::rename(ACL_PSDN = PSDN_ACL_PSDN.sum) %>%
  filter(Landing_year >= 2000 & Landing_year <= 2018)
  
est_data = subset(PacFIN_dat, select = -c(MSQD_PSDN_SDM_60, MSQD_NANC_SDM_90, MSQD_ACL_PSDN.sum, 
                                          NANC_PSDN_SDM_60, NANC_MSQD_SDM_90, NANC_ACL_PSDN.sum,
                                          PSDN_MSQD_SDM_90, PSDN_NANC_SDM_90))

# Calculate year price for N/A's
price.year.PSDN <- aggregate(x= est_data$PSDN_Price, by = list(est_data$Landing_year), FUN = mean, na.rm=T)
  price.year.PSDN <- price.year.PSDN %>%
    dplyr::rename(Landing_year = Group.1) %>%
    dplyr::rename(PSDN_Price.year = x)
  
price.year.MSQD <- aggregate(x= est_data$MSQD_Price, by = list(est_data$Landing_year), FUN = mean, na.rm=T)
  price.year.MSQD <- price.year.MSQD %>%
    dplyr::rename(Landing_year = Group.1) %>%
    dplyr::rename(MSQD_Price.year = x)

price.year.NANC <- aggregate(x= est_data$NANC_Price, by = list(est_data$Landing_year), FUN = mean, na.rm=T)
  price.year.NANC <- price.year.NANC %>%
    dplyr::rename(Landing_year = Group.1) %>%
    dplyr::rename(NANC_Price.year = x)
    
est_data <- est_data %>%
  left_join(price.year.PSDN, by = "Landing_year") %>%
    left_join(price.year.MSQD, by = "Landing_year") %>%
    left_join(price.year.NANC, by = "Landing_year") %>%
  mutate(PSDN_Price_full = ifelse(is.na(PSDN_Price), PSDN_Price.year, PSDN_Price)) %>%
    mutate(MSQD_Price_full = ifelse(is.na(MSQD_Price), MSQD_Price.year, MSQD_Price)) %>%
    mutate(NANC_Price_full = ifelse(is.na(NANC_Price), NANC_Price.year, NANC_Price)) %>%
  subset(select = -c(PSDN_Price, MSQD_Price, NANC_Price, PSDN_Price.year, MSQD_Price.year, NANC_Price.year)) %>%
    dplyr::rename(PSDN_Price = PSDN_Price_full) %>% 
    dplyr::rename(MSQD_Price = MSQD_Price_full) %>%
    dplyr::rename(NANC_Price = NANC_Price_full)


# Change chr to numeric #
est_data$PSDN_SDM_60   <- as.numeric(est_data$PSDN_SDM_60)
est_data$MSQD_SDM_90   <- as.numeric(est_data$MSQD_SDM_90)
est_data$NANC_SDM_90   <- as.numeric(est_data$NANC_SDM_90)
est_data$PSDN_Landings <- as.numeric(est_data$PSDN_Landings)
est_data$MSQD_Landings <- as.numeric(est_data$MSQD_Landings)
est_data$NANC_Landings <- as.numeric(est_data$NANC_Landings)
est_data$ACL_PSDN <- as.numeric(est_data$ACL_PSDN)
  
sjlabelled::set_label(est_data$MSQD_SDM_90)   <- "Prob(presence): MSQD"
sjlabelled::set_label(est_data$PSDN_SDM_60)   <- "Prob(presence): PSDN"
sjlabelled::set_label(est_data$NANC_SDM_90)   <- "Prob(presence): NANC"
sjlabelled::set_label(est_data$Landing_year)  <- "Year"
sjlabelled::set_label(est_data$Port)          <- "Port area"
sjlabelled::set_label(est_data$MSQD_Landings) <- "Landings: MSQD"
sjlabelled::set_label(est_data$MSQD_Price)    <- "Price: MSQD"
sjlabelled::set_label(est_data$MSQD_Revenue)  <- "Revenue: MSQD"
sjlabelled::set_label(est_data$NANC_Landings) <- "Landings: NANC"
sjlabelled::set_label(est_data$NANC_Price)    <- "Price: NANC"
sjlabelled::set_label(est_data$NANC_Revenue)  <- "Revenue: NANC"
sjlabelled::set_label(est_data$ACL_PSDN)      <- "Annual Catch Limit: PSDN"
sjlabelled::set_label(est_data$PSDN_Landings) <- "Landings: PSDN"
sjlabelled::set_label(est_data$PSDN_Price)    <- "Price: PSDN"
sjlabelled::set_label(est_data$PSDN_Revenue)  <- "Revenue: PSDN"
sjlabelled::set_label(est_data$port_num)      <- "Port ID"

est_data_set <- est_data %>%
  subset(select = -c(port_num, Landing_year))

sjlabelled::set_label(est_data_set$MSQD_SDM_90)   <- "Prob(presence): MSQD"
sjlabelled::set_label(est_data_set$PSDN_SDM_60)   <- "Prob(presence): PSDN"
sjlabelled::set_label(est_data_set$NANC_SDM_90)   <- "Prob(presence): NANC"
sjlabelled::set_label(est_data_set$Landing_year)  <- "Year"
sjlabelled::set_label(est_data_set$Port)          <- "Port area"
sjlabelled::set_label(est_data_set$MSQD_Landings) <- "Landings: MSQD"
sjlabelled::set_label(est_data_set$MSQD_Price)    <- "Price: MSQD"
sjlabelled::set_label(est_data_set$MSQD_Revenue)  <- "Revenue: MSQD"
sjlabelled::set_label(est_data_set$NANC_Landings) <- "Landings: NANC"
sjlabelled::set_label(est_data_set$NANC_Price)    <- "Price: NANC"
sjlabelled::set_label(est_data_set$NANC_Revenue)  <- "Revenue: NANC"
sjlabelled::set_label(est_data_set$ACL_PSDN)      <- "Annual Catch Limit: PSDN"
sjlabelled::set_label(est_data_set$PSDN_Landings) <- "Landings: PSDN"
sjlabelled::set_label(est_data_set$PSDN_Price)    <- "Price: PSDN"
sjlabelled::set_label(est_data_set$PSDN_Revenue)  <- "Revenue: PSDN"
sjlabelled::set_label(est_data_set$port_num)      <- "Port ID"

est_data_set <- est_data_set[c("Port", "PSDN_Landings", "MSQD_Landings", "NANC_Landings", 
                                "PSDN_SDM_60", "MSQD_SDM_90", "NANC_SDM_90", "PSDN_Price", "MSQD_Price" , "NANC_Price",
                                "PSDN_Revenue", "MSQD_Revenue", "NANC_Revenue", "ACL_PSDN")]
                                 
vtable::st(est_data_set, labels = TRUE, title='Summary Statistics \\label{tab:sum_stats}', out='latex')


## ----psdn_data, message=FALSE, warning=FALSE, include=FALSE, results='asis'----
# Select data for estimation, replace N/A landings to zero #
est_data_psdn <- est_data %>%
  dplyr::select(port_num, Port, PSDN_SDM_60, PSDN_Landings, PSDN_Price, ACL_PSDN,
                MSQD_SDM_90, NANC_SDM_90, Landing_year) %>%
  dplyr::mutate(PSDN_Landings = coalesce(PSDN_Landings, 0)) %>% 
  filter(Landing_year >= 2000 & Landing_year < 2015)

# Select ports that at least they have landing PSDN once, and drop N/A #
Tot.landings.ports <- as.data.frame(cbind(rowsum(est_data_psdn$PSDN_Landings, est_data_psdn$Port, na.rm = TRUE)))

Tot.landings.ports <- Tot.landings.ports %>%
  mutate(dTotalPSDN = ifelse(V1>0, 1, 0)) %>%
  mutate(Port = rownames(Tot.landings.ports)) %>%
  dplyr::select(Port, dTotalPSDN)

### NOTE: If I use drop_na, some ports without MSQD landings are excluded. ###
est_data_psdn <- est_data_psdn %>%
  merge(Tot.landings.ports, by.x = "Port", by.y = "Port", all.x = TRUE, all.y = FALSE) %>%
  filter(dTotalPSDN==1) %>%
  drop_na()
  # mutate(dSDA = ifelse(Port == "SDA", 1, 0)) %>%
  # filter(dSDA == 0) %>%  

# Create matrices and vectors to run STAN model #
year_id <- as.vector(cbind(as.numeric(factor(est_data_psdn$Landing_year))));
est_data_psdn$port_ID <- udpipe::unique_identifier(est_data_psdn, fields = "port_num", start_from = 1) 

portID_names_PSDN <- est_data_psdn %>%
  dplyr::select(Port, port_ID) %>%
  unique()

est_data_psdn$port_ID <- as.factor(est_data_psdn$port_ID)
class(est_data_psdn$port_ID)


## ----psdn_est, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE--------
#> fit_qPSDN <- brm(
#>   bf(PSDN_Landings ~ PSDN_SDM_60 + MSQD_SDM_90 + NANC_SDM_90 + s(ACL_PSDN)
#>                       + (1 + PSDN_SDM_60 + MSQD_SDM_90 + NANC_SDM_90 | port_ID),
#>                 hu ~ PSDN_SDM_60 + MSQD_SDM_90 + NANC_SDM_90 + s(ACL_PSDN)
#>                       + (1 + PSDN_SDM_60 + MSQD_SDM_90 + NANC_SDM_90 | port_ID)),
#>                           data = est_data_psdn,
#>                           family = hurdle_gamma(),
#>                           control = list(adapt_delta = 0.999, max_treedepth = 20),
#>                        chains = 4, cores = 4)
#> save.image (file = "stan_fit.RData")
#> 


## ----psdn_est_price, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----
#> fit_qPSDN_price <- brm(
#>   bf(PSDN_Landings ~ PSDN_SDM_60 + MSQD_SDM_90 + NANC_SDM_90 + PSDN_Price + s(ACL_PSDN)
#>                       + (1 + PSDN_SDM_60 + MSQD_SDM_90 + NANC_SDM_90 + PSDN_Price | port_ID),
#>                 hu ~ PSDN_SDM_60 + MSQD_SDM_90 + NANC_SDM_90 + PSDN_Price + s(ACL_PSDN)
#>                       + (1 + PSDN_SDM_60 + MSQD_SDM_90 + NANC_SDM_90 + PSDN_Price | port_ID)),
#>                           data = est_data_psdn,
#>                           family = hurdle_gamma(),
#>                           control = list(adapt_delta = 0.999, max_treedepth = 20),
#>                        chains = 4, cores = 4)
#> 
#> save.image (file = "stan_fit.RData")
#> 


## ----msqd_data, message=FALSE, warning=FALSE, include=FALSE, results='asis'----
# Select data for estimation, replace N/A landings to zero #
est_data_msqd <- est_data %>%
  dplyr::select(port_num, Port, MSQD_SDM_90, MSQD_Landings, MSQD_Price, 
                  PSDN_SDM_60, NANC_SDM_90, Landing_year) %>%
  dplyr::mutate(MSQD_Landings = coalesce(MSQD_Landings, 0)) %>% 
  dplyr::mutate(dClose = ifelse(Landing_year >= 2015,1,0)) %>%
  dplyr::mutate(dOpen = ifelse(Landing_year < 2015,1,0)) %>%
  dplyr::mutate(PSDN_SDM_60_dOpen = PSDN_SDM_60 * dOpen) %>%
  filter(Landing_year >= 2000)

# Select ports that at least they have landing PSDN once, and drop N/A #
Tot.landings.ports <- as.data.frame(cbind(rowsum(est_data_msqd$MSQD_Landings, est_data_msqd$Port, na.rm = TRUE)))

Tot.landings.ports <- Tot.landings.ports %>%
  mutate(dTotalMSQD = ifelse(V1>0, 1, 0)) %>%
  mutate(Port = rownames(Tot.landings.ports)) %>%
  dplyr::select(Port, dTotalMSQD)

### NOTE: If I use drop_na, some ports without MSQD landings are excluded. ###
est_data_msqd <- est_data_msqd %>%
  merge(Tot.landings.ports, by.x = "Port", by.y = "Port", all.x = TRUE, all.y = FALSE) %>%
  filter(dTotalMSQD==1) %>%
  drop_na() 
  
# Create matrices and vectors to run STAN model #
year_id <- as.vector(cbind(as.numeric(factor(est_data_msqd$Landing_year))));
est_data_msqd$port_ID <- udpipe::unique_identifier(est_data_msqd, fields = "port_num", start_from = 1) 
portID_names_MSQD <- est_data_msqd %>%
  dplyr::select(Port, port_ID) %>%
  unique()

### Estimate using BRMS package ###
est_data_msqd$dClose    <- factor(est_data_msqd$dClose)
est_data_msqd$port_ID   <- factor(est_data_msqd$port_ID)
class(est_data_msqd$dClose)
class(est_data_msqd$port_ID)
class(est_data_msqd$dOpen)


## ----msqd_est, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE--------
#> fit_qMSQD <- brm(bf(MSQD_Landings ~ MSQD_SDM_90 + PSDN_SDM_60_dOpen + NANC_SDM_90 + dClose
#>                       + (1 + MSQD_SDM_90 + PSDN_SDM_60_dOpen + NANC_SDM_90 + dClose | port_ID),
#>                                hu ~ MSQD_SDM_90 + PSDN_SDM_60_dOpen + NANC_SDM_90 + dClose
#>                       + (1 + MSQD_SDM_90 + PSDN_SDM_60_dOpen + NANC_SDM_90 + dClose | port_ID)),
#>                        data = est_data_msqd,
#>                        family = hurdle_gamma(),
#>                        control = list(adapt_delta = 0.999, max_treedepth = 20),
#>                        chains = 4, cores = 4)
#> save.image (file = "stan_fit.RData")


## ----msqd_est_price, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----
#> fit_qMSQD_price <- brm(bf(MSQD_Landings ~ MSQD_SDM_90 + MSQD_Price + PSDN_SDM_60_dOpen + NANC_SDM_90 + dClose
#>                       + (1 + MSQD_SDM_90 + MSQD_Price + PSDN_SDM_60_dOpen + NANC_SDM_90 + dClose | port_ID),
#>                                hu ~ MSQD_SDM_90 + MSQD_Price + PSDN_SDM_60_dOpen + NANC_SDM_90 + dClose
#>                       + (1 + MSQD_SDM_90 + MSQD_Price + PSDN_SDM_60_dOpen + NANC_SDM_90 + dClose | port_ID)),
#>                        data = est_data_msqd,
#>                        family = hurdle_gamma(),
#>                        control = list(adapt_delta = 0.999, max_treedepth = 20),
#>                        chains = 4, cores = 4)
#> save.image (file = "stan_fit.RData")


## ----nanc_data, message=FALSE, warning=FALSE, include=FALSE, results='asis'----
# Select data for estimation, replace N/A landings to zero #
est_data_nanc <- est_data %>%
  dplyr::select(port_num, Port, NANC_SDM_90, NANC_Landings, NANC_Price, 
                  PSDN_SDM_60, MSQD_SDM_90, Landing_year) %>%
  dplyr::mutate(NANC_Landings = coalesce(NANC_Landings, 0)) %>% 
  dplyr::mutate(dClose = ifelse(Landing_year >= 2015,1,0)) %>%
  dplyr::mutate(dOpen = ifelse(Landing_year < 2015,1,0)) %>%
  dplyr::mutate(PSDN_SDM_60_dOpen = PSDN_SDM_60 * dOpen) %>%
  filter(Landing_year >= 2000)

# Select ports that at least they have landing PSDN once, and drop N/A #
Tot.landings.ports <- as.data.frame(cbind(rowsum(est_data_nanc$NANC_Landings, est_data_nanc$Port, na.rm = TRUE)))

Tot.landings.ports <- Tot.landings.ports %>%
  mutate(dTotalNANC = ifelse(V1>0, 1, 0)) %>%
  mutate(Port = rownames(Tot.landings.ports)) %>%
  dplyr::select(Port, dTotalNANC)

### NOTE: If I use drop_na, some ports without MSQD landings are excluded. ###
est_data_nanc <- est_data_nanc %>%
  merge(Tot.landings.ports, by.x = "Port", by.y = "Port", all.x = TRUE, all.y = FALSE) %>%
  filter(dTotalNANC==1) %>%
  drop_na() 
  
# Create matrices and vectors to run STAN model #
year_id <- as.vector(cbind(as.numeric(factor(est_data_nanc$Landing_year))));
est_data_nanc$port_ID <- udpipe::unique_identifier(est_data_nanc, fields = "port_num", start_from = 1) 
portID_names_NANC <- est_data_nanc %>%
  dplyr::select(Port, port_ID) %>%
  unique()

### Estimate using BRMS package ###
est_data_nanc$dClose    <- factor(est_data_nanc$dClose)
est_data_nanc$port_ID   <- factor(est_data_nanc$port_ID)
class(est_data_nanc$dClose)
class(est_data_nanc$port_ID)
class(est_data_nanc$dOpen)


## ----nanc_est_price, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----
#> fit_qNANC_price <- brm(bf(NANC_Landings ~ NANC_SDM_90 + NANC_Price + PSDN_SDM_60_dOpen + MSQD_SDM_90 + dClose
#>                       + (1 +  NANC_SDM_90 + NANC_Price + PSDN_SDM_60_dOpen + MSQD_SDM_90 + dClose | port_ID),
#>                                      hu ~ NANC_SDM_90 + NANC_Price + PSDN_SDM_60_dOpen + MSQD_SDM_90 + dClose + dClose
#>                       + (1 + NANC_SDM_90 + NANC_Price + PSDN_SDM_60_dOpen + MSQD_SDM_90 + dClose | port_ID)),
#>                        data = est_data_nanc,
#>                        family = hurdle_gamma(),
#>                        control = list(adapt_delta = 0.999, max_treedepth = 20),
#>                        chains = 4, cores = 4)
#> save.image (file = "stan_fit.RData")


## ----y_rep, message=FALSE, warning=FALSE, fig.cap = "Graphical posterior predictive checks\\label{fig:posteriors}"----
g1 <- pp_check(fit_qPSDN_price) + ggtitle('(a) Pacific sardine') + 
  theme(legend.position = "none", plot.title = element_text(size=9, face="bold.italic")) +  xlim(0.1, 15000) 
g2 <- pp_check(fit_qMSQD_price) + ggtitle('(b) Market Squid') + 
  theme(legend.position = "none", plot.title = element_text(size=9, face="bold.italic"))  +  xlim(0.1, 15000) 
g3 <- pp_check(fit_qNANC_price) + ggtitle('(c) Northern Anchovy') +
  theme(legend.position = "right", plot.title = element_text(size=9, face="bold.italic"))  +  xlim(0.1, 15000) 


g1 + g2 + g3


## ----shinystan, eval=FALSE, include=FALSE-------------------------------------
#> shinystan::launch_shinystan(fit_qPSDN_price)
#> shinystan::launch_shinystan(fit_qMSQD_price)
#> shinystan::launch_shinystan(fit_qNANC_price)


## ----y_rep_zero, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE------
#> g1 <- pp_check(fit_qPSDN_price) + ggtitle('(a) Pacific sardine') + theme(legend.position = "none") +  xlim(0, 0.1)
#> g2 <- pp_check(fit_qMSQD_price) + ggtitle('(b) Market Squid') + theme(legend.position = "none")  +  xlim(0, 0.1)
#> g3 <- pp_check(fit_qNANC_price) +
#>   ggtitle('(c) Northern anchovy') +
#>   theme(legend.position = "right", plot.title = element_text(size=9, face="bold.italic"))  +  xlim(0, 0.1)
#> 
#> 
#> g1 + g2 + g3


## ----model-comparision, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE, results='asis'----
#> fit_qMSQD <- add_criterion(fit_qMSQD, "loo")
#> fit_qMSQD_90 <- add_criterion(fit_qMSQD_90, "loo")
#> 
#> comp <- loo_compare(loo(fit_qMSQD), loo(fit_qMSQD_90)) %>%
#>   as.data.frame() %>%
#>   select(elpd_loo, elpd_diff, se_diff, p_loo, looic) %>%
#>   dplyr::rename(
#>     "ELPD-Diff" = elpd_diff, "SE-Diff" = se_diff,
#>     "ELPD-LOO" = elpd_loo, "P-LOO" = p_loo, "LOOIC" = looic
#>   )
#> 
#> mw1 <- model_weights(fit_qMSQD, fit_qMSQD_90, weights = "loo")[rownames(comp)]
#> 
#> comp %>%
#>   cbind("Akaike-Weight" = mw1) %>%
#>   apa_table(
#>     format = "latex", booktabs = TRUE, digits = 2,
#>     caption = "Comparison of models fit1 to fit3 based on approximate leave-one-out cross-validation. Market squid landigns model.",
#>     note =  "ELPD-LOO = expected log posterior predictive density (higher is better); ELPD-DIFF = difference in ELPD values compared to the best model. SE-DIFF = standard error of the ELPD difference. P-LOO = effective number of model parameters (lower is better); LOOIC: leave-one-out information criterion (lower is better); Akaike-Weight = Model weight based on the LOOIC values (higher is better).",
#>     align = c("l", rep("r", 6))
#>   )


## ----by_port_sdm, fig.cap = "Effect of probability of presence on landings by species and port\\label{fig:sdmeffects}"----
# PSDN plots
conditions <- data.frame(port_ID = unique(est_data_psdn$port_ID))
  rownames(conditions) <- unique(est_data_psdn$Port)
conditions_psdn <- conditions %>% 
  rownames_to_column('port_name') %>%
  filter(port_ID == 1 | port_ID == 2 | port_ID == 3) %>%
  column_to_rownames('port_name')

c_eff_psdn <- (conditional_effects
               (fit_qPSDN_price, "PSDN_SDM_60", surface=TRUE, conditions = conditions_psdn, re_formula = NULL))
g1 <- plot(c_eff_psdn, plot = FALSE, nrow = 3, ncol = 1)[[1]] + ggtitle('(a) Pacific sardine')+ 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_continuous(name = element_blank(), labels = NULL) +
  scale_y_continuous(name = "Landings (tons)")

# MSQD plots
conditions2 <- data.frame(port_ID = unique(est_data_msqd$port_ID))
  rownames(conditions2) <- unique(est_data_msqd$Port)
conditions_msqd <- conditions2 %>% 
  rownames_to_column('port_name') %>%
  filter(port_ID == 4  | port_ID == 5  | port_ID == 8) %>%
  column_to_rownames('port_name')
c_eff_msqd <- (conditional_effects
               (fit_qMSQD_price, "MSQD_SDM_90", surface=TRUE, conditions = conditions_msqd, re_formula = NULL))
g2 <- plot(c_eff_msqd, plot = FALSE, nrow = 3, ncol = 1)[[1]] + ggtitle('(b) Market squid') + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_continuous(name = "Year", labels = NULL) +
  scale_y_continuous(name = element_blank()) 

# NANC plots
conditions3 <- data.frame(port_ID = unique(est_data_nanc$port_ID))
  rownames(conditions3) <- unique(est_data_nanc$Port)
conditions_nanc <- conditions3 %>% 
  rownames_to_column('port_name') %>%
  filter(port_ID == 3  | port_ID == 4  | port_ID == 5) %>%
  column_to_rownames('port_name')
c_eff_nanc <- (conditional_effects
               (fit_qNANC_price, "NANC_SDM_90", surface=TRUE, conditions = conditions_nanc, re_formula = NULL))
g3 <- plot(c_eff_nanc, plot = FALSE, nrow = 3, ncol = 1)[[1]] + ggtitle('(c) Northern anchovy') + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_continuous(name = element_blank(), labels = NULL) +
  scale_y_continuous(name = element_blank()) 

# Merge plots
g1 + g2 + g3



## ----int_effect_PSDN_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----
#> c_eff_int_psdn_msqd <- (conditional_effects(fit_qPSDN_price, "PSDN_SDM_60:MSQD_SDM_90", surface=TRUE,
#>                                             conditions = conditions_psdn, re_formula = NULL))
#> c_eff_int_psdn_nanc <- (conditional_effects(fit_qPSDN_price, "PSDN_SDM_60:NANC_SDM_90", surface=TRUE,
#>                                             conditions = conditions_psdn, re_formula = NULL))
#> c_eff_int_msqd_nanc <- (conditional_effects(fit_qPSDN_price, "MSQD_SDM_90:NANC_SDM_90", surface=TRUE,
#>                                             conditions = conditions_psdn, re_formula = NULL))
#> 
#> g1_PSDN <-  plot(c_eff_int_psdn_msqd, plot = FALSE)[[1]] + ggtitle('(a) Pacific sardine x Market squid') +
#>          theme(
#>           plot.title = element_text(size=9, face="bold.italic"),
#>           axis.text = element_text(size = 7),
#>           axis.title = element_text(size = 8),
#>           legend.title = element_text(size = 9),
#>           legend.text = element_text(size=8)
#>         ) + guides(colour=guide_legend(title="Landings: PSDN")) +
#>   scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): MSQD")
#> 
#> g2_PSDN <-  plot(c_eff_int_psdn_nanc, plot = FALSE)[[1]] + ggtitle('(b) Pacific sardine x Northern anchovy') +
#>          theme(
#>           plot.title = element_text(size=9, face="bold.italic"),
#>           axis.text = element_text(size = 7),
#>           axis.title = element_text(size = 8),
#>           legend.title = element_text(size = 9),
#>           legend.text = element_text(size=8)
#>         ) + guides(colour=guide_legend(title="Landings: PSDN")) +
#>         scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): NANC")
#> 
#> g3_PSDN <-  plot(c_eff_int_msqd_nanc, plot = FALSE)[[1]] + ggtitle('(c) Northern anchovy x Market squid') +
#>          theme(
#>           plot.title = element_text(size=9, face="bold.italic"),
#>           axis.text = element_text(size = 7),
#>           axis.title = element_text(size = 8),
#>           legend.title = element_text(size = 9),
#>           legend.text = element_text(size=8)
#>         ) + guides(colour=guide_legend(title="Landings: PSDN")) +
#>         scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): NANC")
#> save.image (file = "stan_fit.RData")


## ----int_effect_PSDN_by_port_PLOT, echo=FALSE, message=FALSE, warning=FALSE, fig.cap = "Interaction effects between species distribution on Pacific sardine landings by port\\label{fig:sdm_int_psdn}"----
g1_PSDN / g2_PSDN / g3_PSDN


## ----int_effect_MSQD_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----
#> c_eff_int_msqd_psdn <- (conditional_effects(fit_qMSQD_price, "MSQD_SDM_90:PSDN_SDM_60_dOpen", surface=TRUE,
#>                                             conditions = conditions_msqd, re_formula = NULL))
#> c_eff_int_msqd_nanc <- (conditional_effects(fit_qMSQD_price, "MSQD_SDM_90:NANC_SDM_90", surface=TRUE,
#>                                             conditions = conditions_msqd, re_formula = NULL))
#> c_eff_int_psdn_nanc <- (conditional_effects(fit_qMSQD_price, "PSDN_SDM_60_dOpen:NANC_SDM_90", surface=TRUE,
#>                                             conditions = conditions_msqd, re_formula = NULL))
#> 
#> g1_MSQD <-  plot(c_eff_int_msqd_psdn, plot = FALSE)[[1]] + ggtitle('(a) Market squid x Pacific sardine') +
#>          theme(
#>           plot.title = element_text(size=9, face="bold.italic"),
#>           axis.text = element_text(size = 7),
#>           axis.title = element_text(size = 8),
#>           legend.title = element_text(size = 9),
#>           legend.text = element_text(size=8)
#>         ) + guides(colour=guide_legend(title="Landings: MSQD")) +
#>   scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): PSDN")
#> 
#> g2_MSQD <-  plot(c_eff_int_msqd_nanc, plot = FALSE)[[1]] + ggtitle('(b) Market squid x Northern anchovy') +
#>          theme(
#>           plot.title = element_text(size=9, face="bold.italic"),
#>           axis.text = element_text(size = 7),
#>           axis.title = element_text(size = 8),
#>           legend.title = element_text(size = 9),
#>           legend.text = element_text(size=8)
#>         ) + guides(colour=guide_legend(title="Landings: MSQD")) +
#>         scale_x_continuous(name = "P(Pres): MSQD") + scale_y_continuous(name = "P(Pres): NANC")
#> 
#> g3_MSQD <-  plot(c_eff_int_psdn_nanc, plot = FALSE)[[1]] + ggtitle('(c) Pacific sardine x Northern anchovy') +
#>          theme(
#>           plot.title = element_text(size=9, face="bold.italic"),
#>           axis.text = element_text(size = 7),
#>           axis.title = element_text(size = 8),
#>           legend.title = element_text(size = 9),
#>           legend.text = element_text(size=8)
#>         ) + guides(colour=guide_legend(title="Landings: MSQD")) +
#>         scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): NANC")
#> save.image (file = "stan_fit.RData")


## ----int_effect_MSQD_by_port_PLOT, echo=FALSE, message=FALSE, warning=FALSE, fig.cap = "Interaction effects between species distribution on market squid landings by port\\label{fig:sdm_int_msqd}"----
g1_MSQD / g2_MSQD / g3_MSQD


## ----int_effect_NANC_by_port, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE----
#> conditions_nanc <- conditions_nanc %>% filter(port_ID == 3  | port_ID == 5)
#> 
#> c_eff_int_nanc_psdn <- (conditional_effects(fit_qNANC_price, "NANC_SDM_90:PSDN_SDM_60_dOpen", surface=TRUE,
#>                                             conditions = conditions_nanc, re_formula = NULL))
#> c_eff_int_nanc_msqd <- (conditional_effects(fit_qNANC_price, "NANC_SDM_90:MSQD_SDM_90", surface=TRUE,
#>                                             conditions = conditions_nanc, re_formula = NULL))
#> c_eff_int_psdn_msqd <- (conditional_effects(fit_qNANC_price, "PSDN_SDM_60_dOpen:MSQD_SDM_90", surface=TRUE,
#>                                             conditions = conditions_nanc, re_formula = NULL))
#> 
#> g1_NANC <-  plot(c_eff_int_nanc_psdn, plot = FALSE)[[1]] + ggtitle('(a) Northern anchovy x Pacific sardine') +
#>          theme(
#>           plot.title = element_text(size=9, face="bold.italic"),
#>           axis.text = element_text(size = 7),
#>           axis.title = element_text(size = 8),
#>           legend.title = element_text(size = 9),
#>           legend.text = element_text(size=8)
#>         ) + guides(colour=guide_legend(title="Landings: PSDN")) +
#>   scale_x_continuous(name = "P(Pres): NANC") + scale_y_continuous(name = "P(Pres): PSDN")
#> 
#> g2_NANC <-  plot(c_eff_int_nanc_msqd, plot = FALSE)[[1]] + ggtitle('(b) Northern anchovy x Market squid') +
#>          theme(
#>           plot.title = element_text(size=9, face="bold.italic"),
#>           axis.text = element_text(size = 7),
#>           axis.title = element_text(size = 8),
#>           legend.title = element_text(size = 9),
#>           legend.text = element_text(size=8)
#>         ) + guides(colour=guide_legend(title="Landings: PSDN")) +
#>         scale_x_continuous(name = "P(Pres): NANC") + scale_y_continuous(name = "P(Pres): MSQD")
#> 
#> g3_NANC <-  plot(c_eff_int_psdn_msqd, plot = FALSE)[[1]] + ggtitle('(c) Pacific sardine x Market squid') +
#>          theme(
#>           plot.title = element_text(size=9, face="bold.italic"),
#>           axis.text = element_text(size = 7),
#>           axis.title = element_text(size = 8),
#>           legend.title = element_text(size = 9),
#>           legend.text = element_text(size=8)
#>         ) + guides(colour=guide_legend(title="Landings: PSDN")) +
#>         scale_x_continuous(name = "P(Pres): PSDN") + scale_y_continuous(name = "P(Pres): MSQD")
#> save.image (file = "stan_fit.RData")


## ----int_effect_NANC_by_port_PLOT, echo=FALSE, message=FALSE, warning=FALSE, fig.cap = "Interaction effects between species distribution on Northern anchovy landings by port\\label{fig:sdm_int_nanc}"----
g1_NANC / g2_NANC / g3_NANC


## ----by_port_msqd_dclose, echo=FALSE, fig.cap = "Effect of Pacific sardine fishery closure on markt squid landings by species and  port\\label{fig:sdmeffects}"----
# conditions_dClose <- data.frame(port_ID = unique(est_data_msqd$dClose))
# rownames(conditions_dClose) <- unique(est_data_msqd$dClose)
c_eff_close_msqd <- (conditional_effects(fit_qMSQD_price, "dClose", conditions = conditions_msqd, re_formula = NULL))
g1 <-  plot(c_eff_close_msqd, plot = FALSE)[[1]] + ggtitle('(a) Market squid') + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) +
  scale_x_discrete(name = "") +
  scale_y_continuous(name = "Landings (tons)") 

c_eff_close_nanc <- (conditional_effects(fit_qNANC_price, "dClose", conditions = conditions_nanc, re_formula = NULL))
g2 <- plot(c_eff_close_nanc, plot = FALSE)[[1]] + ggtitle('(b) Northern anchovy') + 
  theme(plot.title = element_text(size=9, face="bold.italic"), 
        axis.text = element_text(size = 7), axis.title = element_text(size = 8)) + 
  scale_x_discrete(name = "Closure? (1 = True; 0 = False)") +
  scale_y_continuous(name = "Landings (tons)") 

g1 / g2

