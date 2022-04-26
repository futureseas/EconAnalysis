########################################################

### Create full database.
### If a vessel land more than 5,000 USD in value on a port, then he have to decide during that port and month 
### to participate or not in a fishery. Also, those are the landing that count in the model, not in random ports.

vessel.participation <- PacFIN.month.aggregate  %>%
  group_by(VESSEL_NUM, PORT_AREA_CODE, AGENCY_CODE, group_all) %>%
  summarize(total_rev = sum(AFI_EXVESSEL_REVENUE.sum.sum)) %>% filter(total_rev >= 5000)

year = expand.grid(PORT_AREA_CODE = unique(vessel.participation$PORT_AREA_CODE), LANDING_YEAR = 2000:2020)
vessel.participation <- left_join(vessel.participation, year, by = "PORT_AREA_CODE")

months = expand.grid(LANDING_YEAR = unique(vessel.participation$LANDING_YEAR), LANDING_MONTH = 1:12)
vessel.participation <- left_join(vessel.participation, months, by = "LANDING_YEAR")

PacFIN.month.left.join <- left_join(vessel.participation, PacFIN.month.aggregate,
                                    by = c("LANDING_YEAR" ,"VESSEL_NUM", "PORT_AREA_CODE", 
                                           "AGENCY_CODE", "group_all", "LANDING_MONTH"))

# PacFIN.month.merge <- merge(vessel.participation, PacFIN.month.aggregate,
#   by = c("LANDING_YEAR" ,"VESSEL_NUM", "PORT_AREA_CODE", 
#   "AGENCY_CODE", "group_all", "LANDING_MONTH"), all.x = TRUE, all.y = TRUE) %>%
#   dplyr::filter(is.na(total_rev))
#   rm(PacFIN.month.merge)


########################################################
PacFIN.month.dataset <- PacFIN.month.left.join %>% 
  dplyr::select(LANDING_YEAR, LANDING_MONTH, VESSEL_NUM,
                LANDED_WEIGHT_MTONS.sum.sum, AFI_PRICE_PER_MTON.mean.mean, 
                PACFIN_SPECIES_CODE, AGENCY_CODE, group_all, PORT_AREA_CODE) %>%
  dplyr::rename(AFI_PRICE_PER_MTON.mean = AFI_PRICE_PER_MTON.mean.mean) %>%
  dplyr::rename(LANDED_WEIGHT_MTONS.sum = LANDED_WEIGHT_MTONS.sum.sum) %>%
  mutate(AFI_PRICE_PER_MTON.mean = na_if(AFI_PRICE_PER_MTON.mean, 0)) %>% 
  filter(group_all != is.na(group_all)) %>%
  reshape2::melt(id.vars=c("LANDING_YEAR", "LANDING_MONTH", "VESSEL_NUM", 
                           "PACFIN_SPECIES_CODE", "AGENCY_CODE", 'PORT_AREA_CODE', "group_all")) %>% 
  reshape2::dcast(LANDING_YEAR + LANDING_MONTH + VESSEL_NUM + AGENCY_CODE + PORT_AREA_CODE + group_all ~
                    PACFIN_SPECIES_CODE + variable, fun.aggregate=mean, rm.na = T) %>%
  dplyr::select('LANDING_YEAR', 'LANDING_MONTH', 'VESSEL_NUM', 
                'PSDN_LANDED_WEIGHT_MTONS.sum', 'MSQD_LANDED_WEIGHT_MTONS.sum', 'NANC_LANDED_WEIGHT_MTONS.sum',  
                'PSDN_AFI_PRICE_PER_MTON.mean', 'MSQD_AFI_PRICE_PER_MTON.mean', 'NANC_AFI_PRICE_PER_MTON.mean', 
                'AGENCY_CODE', 'PORT_AREA_CODE', 'group_all')
rm(PacFIN.month.aggregate)
