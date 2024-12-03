### Clear memory
rm(list = ls())
PC = "work" ### Where I am working?

if (PC == "home") {
  google_dir <- "H:/My Drive/"
} else if (PC == "work") {
  google_dir <- "G:/Mi unidad/"
}

setwd("C:/GitHub/EconAnalysis/Participation/R")

## Read database
library(tidyr)
library(dplyr)

c4 <- readRDS(paste0(google_dir, "Data/Anonymised data/part_model_c4.rds")) %>% dplyr::select(c(dprice30)) 
summary(c4)

c5 <- readRDS(paste0(google_dir, "Data/Anonymised data/part_model_c5.rds")) %>% dplyr::select(c(dprice30)) 
summary(c5)

c6 <- readRDS(paste0(google_dir, "Data/Anonymised data/part_model_c6.rds")) %>% dplyr::select(c(dprice30)) 
summary(c6)

c7 <- readRDS(paste0(google_dir, "Data/Anonymised data/part_model_c7.rds")) %>% dplyr::select(c(dprice30)) 
summary(c7)

