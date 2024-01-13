library(tidyverse)
Tickets_final <- readRDS("C:\\Data\\PacFIN data\\participation_data.rds")

#################################
### Daily participation graph ###
#################################

df.subs <- Tickets_final %>%
  dplyr::select(c(VESSEL_NUM, Species_Dominant, selection, set_month,
                  set_date, set_year, group_all)) %>%
  dplyr::filter(set_year == 2013,
                set_month ==10,
                group_all == 4) %>%
 dplyr::filter(VESSEL_NUM == "648720" | VESSEL_NUM == "643518" |
               VESSEL_NUM == "598813" | VESSEL_NUM == "625449" |
               VESSEL_NUM == "WN5102SK") %>% drop_na() %>%   
  group_by(VESSEL_NUM) %>%
  dplyr::mutate(Vessel = cur_group_id()) %>%
  ungroup() %>% rename(Month = set_date) %>%
  rename(Species = Species_Dominant) %>%
  mutate(Species = ifelse(Species == "CMCK", "Chub Mackerel", 
                   ifelse(Species == "MSQD", "Market Squid",
                   ifelse(Species == "NANC", "Northern Anchovy",
                          "Pacific Sardine"))))
  

ggplot(df.subs, aes(x=Month, y=Vessel, color=Species)) +
  geom_point(size=4)