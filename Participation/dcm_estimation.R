#-----------------------------#
## Fit discrete choice model ##
#-----------------------------#

gc()
rm(list=ls())

## Load packages ##
library(tidyverse)
library(mlogit)

## Load data ##
rdo <- readRDS(file = "C:\\GitHub\\EconAnalysis\\Participation\\rdo_Stata_c4.rds") %>%
  mutate(travel_cost_adj = travel_cost / 1000)


## Create mlogit.data

# the_tows <- mlogit.data(rdo, shape = 'long', choice = 'fished', alt.var = 'selection',
#                         id.var = "fished_VESSEL_NUM", chid.var = "fished_haul")

the_tows <- mlogit.data(rdo, shape = 'long', choice = 'fished', alt.var = 'selection',
                        id.var = "fished_VESSEL_ID", chid.var = "fished_haul")

## Fit model -- (fished ~ Generic coefficient (alt variable) | Individual variable | Alternative specific coefficient)

colnames(rdo)


# head(model.matrix(f, the_tows))
f1 <- mFormula(fished ~ wind_max_220_mh + mean_rev_adj + travel_cost_adj)
m1 <- mlogit(f1, the_tows, reflevel = 'No-Participation')
summary(m1)
f2 <- mFormula(fished ~ wind_max_220_mh + mean_rev_adj + travel_cost_adj + factor(dummy_last_day))
m2 <- mlogit(f2, the_tows, reflevel = 'No-Participation')
summary(m2)
f3 <- mFormula(fished ~ wind_max_220_mh + mean_rev_adj + travel_cost_adj + factor(dummy_last_day) + factor(MSQD.Closure) + factor(PSDN.Closure)) 
m3 <- mlogit(f3, the_tows, reflevel = 'No-Participation')
summary(m3)
f4 <- mFormula(fished ~ wind_max_220_mh + mean_rev_adj + travel_cost_adj + factor(dummy_last_day) + factor(MSQD.Closure) + factor(PSDN.Closure) + factor(MSQD.Weekend))
m4 <- mlogit(f4, the_tows, reflevel = 'No-Participation')
summary(m4)
f5 <- mFormula(fished ~ wind_max_220_mh + mean_rev_adj + travel_cost_adj + factor(MSQD.Closure) + factor(PSDN.Closure) + factor(MSQD.Weekend))
m5 <- mlogit(f5, the_tows, reflevel = 'No-Participation')
summary(m5)



models <- list(
  "Model 1" = m1,
  "Model 2" = m2,
  "Model 3" = m3,
  "Model 4" = m4,
  "Model 5" = m5)

gm <- modelsummary::gof_map
options(OutDec=".")
modelsummary::modelsummary(models, fmt = 2,
                           gof_map = c("nobs", "adj.r.squared"),
                           statistic = "({std.error}){stars}",
                           output = "dcm_models.docx")


#---------------------------------------#
## Generate and format the predictions ##
#---------------------------------------#
 
fits <- fitted(m5, outcome = FALSE)
mfits <- reshape2::melt(fits) %>%
   dplyr::rename(fished_haul = Var1) %>%
   dplyr::rename(selection_pred = Var2)

## Compare to correct prediction using max probability value
pred_tows <- mfits %>% group_by(fished_haul) %>% filter(value == max(value)) %>% as.data.frame
pred_tows <- merge(rdo, pred_tows, by = "fished_haul") %>% 
 filter(fished == TRUE) %>%
 mutate(correct = ifelse(selection_pred == selection, 1, 0))
pred_tows2 <- mfits %>% group_by(fished_haul) %>% filter(value == max(value)) %>% as.data.frame
pred_tows2 <- merge(rdo, pred_tows2, by = "fished_haul") %>%
  filter(fished == TRUE) %>%
  mutate(correct = ifelse(selection_pred == selection, 1, 0)) %>%
  filter(selection != "No-Participation")

sum(pred_tows$correct) / nrow(pred_tows) # 60% accuracy!
sum(pred_tows2$correct) / nrow(pred_tows2) ## 52% accuracy!

