#------------------------#
## Check comments paper ##
#------------------------#

gc()
rm(list=ls())

## Load packages ##
library(tidyverse)

## Load data ##
c4 <- read.csv(file = "G:\\Mi unidad\\Data\\Anonymised data\\rdo_Stata_c4_full_noid.csv") 
c5 <- read.csv(file = "G:\\Mi unidad\\Data\\Anonymised data\\rdo_Stata_c5_full_v2_noid.csv") 
c6 <- read.csv(file = "G:\\Mi unidad\\Data\\Anonymised data\\rdo_Stata_c6_full_noid.csv") 
c7 <- read.csv(file = "G:\\Mi unidad\\Data\\Anonymised data\\rdo_Stata_c7_full_noid.csv") 

### Revisar autocorrelacion de SDM por dia

data_c4 <- c4 %>% group_by(selection, set_date, set_year, set_month) %>% 
  summarize(mean_sdm = mean(mean_avail, na.rm = TRUE)) %>%
  mutate(date = as.Date(set_date)) %>% filter(set_year == 2014) %>% 
  filter(selection != "LAA-BTNA") %>% 
  filter(selection != "LAA-YTNA") %>% 
  filter(selection != "No-Participation")
  
tabla <- table(c4$selection)
print(tabla)

# LAA-CMCK
# LAA-MSQD 
# LAA-NANC
# LAA-PSDN
# MNA-MSQD
# MNA-NANC
# MNA-PSDN
# MRA-MSQD
# SBA-CMCK 
# SBA-MSQD
# SFA-MSQD
# SFA-NANC 



library(ggplot2)
library(dplyr)

# Assuming your summarized data is stored in data_c4
ggplot(data_c4, aes(x = set_date, y = mean_sdm, group = selection)) +
  geom_line() + 
  labs(title = "SDM Over Time by Alternative (2014)",
       x = "Day",
       y = "SDM") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) + 
  facet_wrap(~ selection, scales = "free_y")

data_c4 <- data_c4 %>% filter(set_month == 6) 
ggplot(data_c4, aes(x = set_date, y = mean_sdm, group = selection)) +
  geom_line() + 
  labs(title = "SDM Over Time by Alternative (June 2014)",
       x = "Day",
       y = "SDM") +
  theme_minimal() +
  theme(axis.text.x = element_blank()) + 
  facet_wrap(~ selection, scales = "free_y")
