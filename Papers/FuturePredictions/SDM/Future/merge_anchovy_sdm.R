rm(list = ls())
gc()

library(data.table)

# Cargar ambos archivos
sdm_60 <- fread(here::here("FuturePredictions", "SDM", "Future", "NANC_FutureSDM_port_day.csv"))
sdm_20 <- fread(here::here("FuturePredictions", "SDM", "Future", "NANC_FutureSDM_port_day_20km.csv"))


# Hacer merge por las claves comunes: fecha y puerto
merged <- merge(
  sdm_60,
  sdm_20[, .(date, PORT_AREA_CODE, SDM_20_GFDL, SDM_20_IPSL, SDM_20_HADL)],
  by = c("date", "PORT_AREA_CODE"),
  all = TRUE
)

# Guardar el archivo combinado
fwrite(merged, here::here("FuturePredictions", "SDM", "Future", "NANC_FutureSDM_port_day_merged.csv"))
