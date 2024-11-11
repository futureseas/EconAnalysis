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
         Port = substr(selection, 1, 3)) 


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


# Ensure set_date is in Date format
df.subs <- df.subs %>%
  mutate(set_date = as.Date(set_date)) %>%  # Convert to Date format
  mutate(days_since_start = as.numeric(set_date - min(set_date)))  # Calculate days since the first date

# Update the plot with the new x-axis label and variable
ggplot(df.subs, aes(x = days_since_start, y = fished_VESSEL_anon, color = selection, shape = selection)) +
  geom_point(size = 3) +
  scale_color_manual(values = selection_colors) +  # Apply color mapping by Port
  scale_shape_manual(values = selection_shapes) + 
  labs(x = "Days", y = "Vessel anonymized ID", color = "Alternative", shape = "Alternative") 

## Create metier table
library(dplyr)


calculate_switching <- function(data, dataset_name) {
  data %>%
    dplyr::select(fished_VESSEL_anon, selection, set_date, fished) %>%
    dplyr::filter(fished == 1 & selection != "No-Participation") %>%
    drop_na() %>%
    arrange(fished_VESSEL_anon, set_date) %>%
    mutate(Species = substr(selection, nchar(selection) - 3, nchar(selection)),  # Extract species
           Port = substr(selection, 1, 3),  # Extract port
           Previous_Species = lag(Species),
           Previous_Port = lag(Port),
           Species_Switch = Species != Previous_Species & !is.na(Previous_Species),
           Port_Switch = Port != Previous_Port & !is.na(Previous_Port)) %>%
    group_by(fished_VESSEL_anon) %>%
    summarise(Total_Species_Switches = sum(Species_Switch, na.rm = TRUE),
              Unique_Species = n_distinct(Species),
              Total_Port_Switches = sum(Port_Switch, na.rm = TRUE),
              Unique_Ports = n_distinct(Port)) %>%
    ungroup() %>%
    summarise(Dataset = dataset_name,
              Average_Total_Species_Switches = mean(Total_Species_Switches),
              SD_Total_Species_Switches = sd(Total_Species_Switches),
              Average_Unique_Species = mean(Unique_Species),
              SD_Unique_Species = sd(Unique_Species),
              Average_Total_Port_Switches = mean(Total_Port_Switches),
              SD_Total_Port_Switches = sd(Total_Port_Switches),
              Average_Unique_Ports = mean(Unique_Ports),
              SD_Unique_Ports = sd(Unique_Ports))
}

# Apply the function to each dataset
summary_c4 <- calculate_switching(c4, "c4")
summary_c5 <- calculate_switching(c5, "c5")
summary_c6 <- calculate_switching(c6, "c6")
summary_c7 <- calculate_switching(c7, "c7")

# Combine all summaries into a single table
combined_summary <- bind_rows(summary_c4, summary_c5, summary_c6, summary_c7)
