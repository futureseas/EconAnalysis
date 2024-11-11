#------------------------#
## Check comments paper ##
#------------------------#

gc()
rm(list=ls())

#dir = "H:\\My Drive\\"
dir = "G:\\Mi unidad\\"


## Load packages ##
library(tidyverse)

## Load data ##
c4 <- read.csv(file = paste0(dir, "Data\\Anonymised data\\rdo_Stata_c4_full_noid.csv")) 
c5 <- read.csv(file = paste0(dir, "Data\\Anonymised data\\rdo_Stata_c5_full_v2_noid.csv")) 
c6 <- read.csv(file = paste0(dir, "Data\\Anonymised data\\rdo_Stata_c6_full_noid.csv")) 
c7 <- read.csv(file = paste0(dir, "Data\\Anonymised data\\rdo_Stata_c7_full_noid.csv")) 


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


#################################
### Daily participation graph ###
#################################

# Select and filter the data
df.subs <- c4 %>%
  dplyr::select(c(fished_VESSEL_anon, selection, set_month, set_date, set_year, fished)) %>%
  dplyr::filter(set_year == 2014, set_month == 7, fished == 1) %>%
  drop_na() %>% 
  mutate(Species = substr(selection, nchar(selection) - 3, nchar(selection)),
         Port = substr(selection, 1, 3)
  ) 


# Define custom color and shape mappings
selection_colors <- c(
  "No-Participation" = "black",
  "SFA-MSQD" = "orange",
  "MRA-MSQD" = "red",
  "MNA-PSDN" = "lightgreen",
  "SFA-NANC" = "orange",
  "LAA-MSQD" = "blue",
  "LAA-PSDN" = "blue",
  "LAA-YTNA" = "blue",
  "SBA-CMCK" = "purple",
  "LAA-CMCK" = "blue",
  "MNA-MSQD" = "lightgreen",
  "LAA-NANC" = "blue",
  "MNA-NANC" = "lightgreen",
  "LAA-BTNA" = "blue",
  "SBA-MSQD" = "purple")


selection_shapes <- c(
  "No-Participation" = 4,
  "SFA-MSQD" = 18,
  "MRA-MSQD" = 18,
  "MNA-PSDN" = 15,
  "SFA-NANC" = 19,
  "LAA-MSQD" = 18,
  "LAA-PSDN" = 15,
  "LAA-YTNA" = 20,
  "SBA-CMCK" = 17,
  "LAA-CMCK" = 17,
  "MNA-MSQD" = 18,
  "LAA-NANC" = 19,
  "MNA-NANC" = 19,
  "LAA-BTNA" = 16,
  "SBA-MSQD" = 18)  


# Plot with color representing Port and shape representing Species
ggplot(df.subs, aes(x = set_date, y = fished_VESSEL_anon, color = selection, shape = selection)) +
  geom_point(size = 4) +
  scale_color_manual(values = selection_colors) +  # Apply color mapping by Port
  scale_shape_manual(values = selection_shapes) + 
  theme_minimal()

