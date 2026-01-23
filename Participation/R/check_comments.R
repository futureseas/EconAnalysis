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


# ### Revisar autocorrelacion de SDM por dia
# 
# data_c4 <- c4 %>% group_by(selection, set_date, set_year, set_month) %>% 
#   summarize(mean_sdm = mean(mean_avail, na.rm = TRUE)) %>%
#   mutate(date = as.Date(set_date)) %>% filter(set_year == 2014) %>% 
#   filter(selection != "LAA-BTNA") %>% 
#   filter(selection != "LAA-YTNA") %>% 
#   filter(selection != "No-Participation")
#   
# tabla <- table(c4$selection)
# print(tabla)
# 
# # LAA-CMCK
# # LAA-MSQD 
# # LAA-NANC
# # LAA-PSDN
# # MNA-MSQD
# # MNA-NANC
# # MNA-PSDN
# # MRA-MSQD
# # SBA-CMCK 
# # SBA-MSQD
# # SFA-MSQD
# # SFA-NANC 
# 
# 
# 
# library(ggplot2)
# library(dplyr)
# 
# # Assuming your summarized data is stored in data_c4
# ggplot(data_c4, aes(x = set_date, y = mean_sdm, group = selection)) +
#   geom_line() + 
#   labs(title = "SDM Over Time by Alternative (2014)",
#        x = "Day",
#        y = "SDM") +
#   theme_minimal() +
#   theme(axis.text.x = element_blank()) + 
#   facet_wrap(~ selection, scales = "free_y")
# 
# data_c4 <- data_c4 %>% filter(set_month == 6) 
# ggplot(data_c4, aes(x = set_date, y = mean_sdm, group = selection)) +
#   geom_line() + 
#   labs(title = "SDM Over Time by Alternative (June 2014)",
#        x = "Day",
#        y = "SDM") +
#   theme_minimal() +
#   theme(axis.text.x = element_blank()) + 
#   facet_wrap(~ selection, scales = "free_y")




#################################################
### 1. Data Preparation & Participation Plot  ###
#################################################

library(tidyverse)
library(viridis)

set.seed(123) 
df.subs <- c4 %>%
  dplyr::filter(fished == 1) %>%
  mutate(set_date = as.Date(set_date)) %>%
  group_by(fished_VESSEL_anon) %>%
  mutate(random_offset = sample(1:60, 1),
         set_date = set_date + random_offset) %>% 
  ungroup() %>%
  mutate(set_year = year(set_date), set_month = month(set_date)) %>%
  dplyr::filter(set_year == 2014 & set_month == 9) %>%
  drop_na(selection) %>% 
  mutate(
    Species = substr(selection, nchar(selection) - 3, nchar(selection)),
    Port = substr(selection, 1, 3),
    # Forzar etiquetas de No-Participation
    Port = ifelse(selection == "No-Participation", "No-Participation", Port),
    Species = ifelse(selection == "No-Participation", "No-Participation", Species),
    days_since_start = as.numeric(set_date - min(set_date))
  )

# Ordenar barcos por frecuencia de cambio
vessel_order <- df.subs %>%
  arrange(fished_VESSEL_anon, set_date) %>%
  group_by(fished_VESSEL_anon) %>%
  mutate(switched = selection != lag(selection)) %>%
  summarize(switch_count = sum(switched, na.rm = TRUE)) %>%
  arrange(switch_count)

df.subs$fished_VESSEL_anon <- factor(df.subs$fished_VESSEL_anon, levels = vessel_order$fished_VESSEL_anon)

# Paletas
colores_pro <- c("No-Participation"="#999999", "LAA"="#56B4E9", "MNA"="#009E73", "MRA"="#CC79A7", "SBA"="#F0E442", "SFA"="#E69F00")
shapes_pro <- c("No-Participation"=16, "MSQD"=18, "PSDN"=15, "CMCK"=17, "NANC"=19, "YTNA"=20, "BTNA"=8)

# Gráfico de Dinámica Temporal
ggplot(df.subs, aes(x = days_since_start, y = fished_VESSEL_anon)) +
  geom_hline(aes(yintercept = as.numeric(fished_VESSEL_anon)), color = "grey90", size = 0.2) +
  geom_point(aes(color = Port, shape = Species,
                 size = ifelse(selection == "No-Participation", 1.2, 3.5), 
                 alpha = ifelse(selection == "No-Participation", 0.5, 1))) +
  scale_color_manual(values = colores_pro) +
  scale_shape_manual(values = shapes_pro) +
  scale_size_identity() + scale_alpha_identity() +
  theme_minimal() +
  labs(title = "Vessel Dynamics: Port (Color) and Species (Shape)",
       x = "Days", y = "Vessel ID (Sorted by switches)") +
  theme(axis.text.y = element_text(size = 7), panel.grid = element_blank())




library(tidyverse)
library(viridis)

# Usamos c4 completo para las matrices (sin filtros de fecha ni offsets aleatorios)
df_total <- c4 %>%
  dplyr::filter(fished == 1) %>%
  mutate(set_date = as.Date(set_date)) %>%
  # Aseguramos que existan las columnas de Port y Species
  mutate(
    Species = substr(selection, nchar(selection) - 3, nchar(selection)),
    Port = substr(selection, 1, 3),
    # Forzar etiquetas consistentes para No-Participation
    Port = ifelse(selection == "No-Participation", "No-Participation", Port),
    Species = ifelse(selection == "No-Participation", "No-Participation", Species)
  ) %>%
  arrange(fished_VESSEL_anon, set_date) %>%
  group_by(fished_VESSEL_anon) %>%
  # Creamos los estados t+1
  mutate(
    port_next = lead(Port),
    spec_next = lead(Species),
    selection_next = lead(selection)
  ) %>%
  filter(!is.na(selection_next)) %>%
  ungroup()

# Función para generar Heatmaps con alta legibilidad de números
plot_full_transition <- function(df, current_col, next_col, title_lab) {
  counts <- df %>%
    count(!!sym(current_col), !!sym(next_col)) %>%
    group_by(!!sym(current_col)) %>%
    mutate(prob = n / sum(n), total_n = sum(n)) %>%
    mutate(y_label = paste0(!!sym(current_col), "\n(n=", total_n, ")"))
  
  ggplot(counts, aes(x = !!sym(next_col), y = y_label, fill = prob)) +
    geom_tile(color = "white", size = 0.3) + # Bordes blancos para separar celdas
    geom_text(aes(label = sprintf("%.2f", prob), 
                  # Color dinámico: Blanco en fondos oscuros (prob < 0.5), 
                  # Negro en fondos claros.
                  color = prob < 0.5), 
              size = 3.5, fontface = "bold") +
    
    # Definición manual de colores de texto
    scale_color_manual(values = c("TRUE" = "black", "FALSE" = "white"), guide = "none") +
    
    # Escala de color de fondo (Mako es excelente para legibilidad científica)
    scale_fill_viridis_c(option = "mako", direction = -1, end = 0.95) + 
    
    labs(
      # title = title_lab, 
      #    subtitle = "High-contrast labels (White on dark / Black on light)",
         x = "State Tomorrow (t+1)", 
         y = "State Today (t)", 
         fill = "Prob.") +
    theme_minimal() + 
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
      axis.text.y = element_text(size = 9),
      panel.grid = element_blank()
    )
}

# Generar Matrices Finales actualizadas
matrix_port <- plot_full_transition(df_total, "Port", "port_next", "Full Port Transition Matrix")
matrix_species <- plot_full_transition(df_total, "Species", "spec_next", "Full Species Transition Matrix")

# Mostrar resultados
print(matrix_port)
print(matrix_species)


#################################################
### 3. Summary Statistics (CORREGIDO)        ###
#################################################

# 1. Switch Rate usando el dataset TOTAL
# Esto calcula el % de días donde el barco cambió de puerto O de especie
overall_switch_rate <- mean(df_total$selection != df_total$selection_next)

# 2. Median Run Length usando el dataset TOTAL
run_lengths_total <- c4 %>%
  dplyr::filter(fished == 1) %>%
  arrange(fished_VESSEL_anon, set_date) %>%
  group_by(fished_VESSEL_anon) %>%
  do(data.frame(len = rle(as.character(.$selection))$lengths))

final_median_run <- median(run_lengths_total$len)

cat("\n--- Descriptive Switching Evidence (Full Data) ---\n",
    "Overall Switch Rate:", round(overall_switch_rate, 3), "\n",
    "Median Run Length:", final_median_run, "days\n")




#################################
### Total participation ###
#################################

library(dplyr)

# Define a function to find vessels that participate every year 
# in the sample for a given dataset
calculate_full_participation <- function(data) {
  data %>%
    # Filter only rows where vessels participated
    dplyr::filter(fished == 1) %>%
    # Select only relevant columns
    dplyr::select(fished_VESSEL_anon, set_year) %>%
    # Drop missing values
    drop_na() %>%
    # Count unique years each vessel participated in
    group_by(fished_VESSEL_anon) %>%
    summarise(years_participated = n_distinct(set_year)) %>%
    ungroup() %>%
    # Filter vessels that participated in all years
    filter(years_participated == n_distinct(data$set_year)) %>%
    # Count these vessels
    summarise(full_participation_vessels = (n()/n_distinct(data$fished_VESSEL_anon))*100)
}

# Calculate full participation for each dataset
full_participation_c4 <- calculate_full_participation(c4) %>% mutate(dataset = "c4")
full_participation_c5 <- calculate_full_participation(c5) %>% mutate(dataset = "c5")
full_participation_c6 <- calculate_full_participation(c6) %>% mutate(dataset = "c6")
full_participation_c7 <- calculate_full_participation(c7) %>% mutate(dataset = "c7")

# Combine the results
full_participation_summary <- bind_rows(full_participation_c4, 
                                        full_participation_c5, 
                                        full_participation_c6, 
                                        full_participation_c7)

# View the summary
full_participation_summary




#################################
### Yearly participation  ###
#################################

library(dplyr)

# Define a function to calculate participation summary for a given dataset
calculate_participation_summary <- function(data) {
  data %>%
    dplyr::select(fished_VESSEL_anon, set_year, fished) %>%
    dplyr::filter(fished == 1) %>%
    drop_na() %>%
    group_by(set_year) %>%
    summarise(
      participating_vessels = n_distinct(fished_VESSEL_anon)
    ) %>%
    mutate(
      total_vessels = n_distinct(data$fished_VESSEL_anon),
      participation_percentage = (participating_vessels / total_vessels) * 100
    )
}

# Calculate participation summaries for each dataset
participation_summary_c4 <- calculate_participation_summary(c4)
participation_summary_c5 <- calculate_participation_summary(c5)
participation_summary_c6 <- calculate_participation_summary(c6)
participation_summary_c7 <- calculate_participation_summary(c7)

# Add a column to identify the dataset source
participation_summary_c4 <- participation_summary_c4 %>% mutate(dataset = "c4")
participation_summary_c5 <- participation_summary_c5 %>% mutate(dataset = "c5")
participation_summary_c6 <- participation_summary_c6 %>% mutate(dataset = "c6")
participation_summary_c7 <- participation_summary_c7 %>% mutate(dataset = "c7")

# Combine the summaries into a single dataframe
participation_summary <- bind_rows(participation_summary_c4, 
                                   participation_summary_c5, 
                                   participation_summary_c6, 
                                   participation_summary_c7)

# View the combined participation summary
participation_summary



############################
## Create Switching table ##
############################
library(dplyr)


calculate_switching <- function(data, dataset_name) {
  data %>%
    dplyr::select(fished_VESSEL_anon, selection, set_date, fished) %>%
    dplyr::filter(fished == 1 & selection != "No-Participation") %>%
    drop_na() %>%
    arrange(fished_VESSEL_anon, set_date) %>%
    mutate(Species = substr(selection, nchar(selection) - 3, nchar(selection)),  
           Port = substr(selection, 1, 3), 
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

library(flextable)

# Convert to flextable for DOCX formatting
summary_table <- flextable(combined_summary)

# Save the table as a DOCX file
summary_table %>% save_as_docx(
  path = "C:/GitHub/EconAnalysis/Participation/Results/Switching_Behavior_Summary.docx")

