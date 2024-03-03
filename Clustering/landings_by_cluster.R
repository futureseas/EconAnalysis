# Initial setup ####

## Clear working space ####
rm(list=ls())
gc()

## Load packages ####

library(data.table)
library(dplyr)
library(lubridate)
library(tidyr)
library(geosphere)
library(tidyverse)  

setwd("C:/GitHub/EconAnalysis")

# Landings by clusters ####

## Obtain PacFIN data ####
Tickets1 <- fread("C:/Data/PacFIN data/FutureSeasIII_2000_2009.csv")
Tickets2 <- fread("C:/Data/PacFIN data/FutureSeasIII_2010_2020.csv")
Tickets_raw<-rbind(Tickets1, Tickets2)
rm(Tickets1, Tickets2)

## Obtain cluster by specific vessel ####\

