library(tidyverse)
c4_data <- read_csv("G:/Mi unidad/Data/Anonymised data/rdo_Stata_c4_full_noid.csv")


#################################
### Daily participation graph ###
#################################

df.subs <- c4_data %>%
  dplyr::select(c(fished_VESSEL_anon, selection, set_month,
                  set_date, set_year)) %>%
  dplyr::filter(set_year == 2013,
                set_month ==10) %>% drop_na() 

  

ggplot(df.subs, aes(x=set_date, y=fished_VESSEL_anon, color=selection)) +
  geom_point(size=3)