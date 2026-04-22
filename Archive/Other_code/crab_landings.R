#################################################
### Dungeness crab landings by port and month ###
#################################################

#----------------------------
# Setup #
rm(list = ls(all.names = TRUE)) 
gc()

## Read packages 
library("tidyr")
library("dplyr") 
library("data.table") 
library("reshape2")

Crab.landings <- read.csv(file = "C:\\GitHub\\EconAnalysis\\Data\\Port landings\\CRAB002-W-O-C-2000---2022.csv")%>%
  dplyr::select(-c(TOTAL_ROUND_WEIGHT_MTONS, TOTAL_ROUND_WEIGHT_PPP, TOTAL_EXVESSEL_REVENUE, TOTAL_CONFIDENTIAL_FLAG))

# Separate outcome variables #
Crab.landings <- melt(Crab.landings, id.vars = c("CRAB_YEAR", "AGENCY_CODE",  "PACFIN_GROUP_PORT_CODE", "PORT_DESCRIPTION"))

# Create quarter variable #
Crab.landings <- Crab.landings  %>%
  separate(variable, c("MONTH","A1", "A2", "A3"), sep = "_") %>%
  mutate(Outcome = paste(A1, A2, A3, sep="_")) %>%
  dplyr::select(-c(A1, A2, A3)) %>%
  pivot_wider(names_from = Outcome, values_from = value) %>%
  dplyr::select(-c(ROUND_WEIGHT_PPP, EXVESSEL_REVENUE_NA, CONFIDENTIAL_FLAG_NA)) %>%
  dplyr::rename(PORT_AREA_CODE = PACFIN_GROUP_PORT_CODE) %>%
  dplyr::rename(LANDING_YEAR = CRAB_YEAR) 

Crab.landings$LANDING_MONTH      <- sapply(Crab.landings$MONTH,function(x) grep(paste("(?i)",x,sep=""),month.abb))
Crab.landings$ROUND_WEIGHT_MTONS <- as.numeric(Crab.landings$ROUND_WEIGHT_MTONS)
Crab.landings <- Crab.landings  %>% dplyr::select(-c(PORT_DESCRIPTION, MONTH, AGENCY_CODE))
Crab.landings[is.na(Crab.landings)] <- 0
str(Crab.landings)

write.csv(Crab.landings,"C:\\GitHub\\EconAnalysis\\Data\\Port landings\\DCRB_landings.csv", row.names = FALSE)