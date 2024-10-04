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

