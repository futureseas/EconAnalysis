library(dplyr)

gc()

library(apollo)
apollo_initialise()

library(here)  # portable paths anchored at EconAnalysis.Rproj

# ---- Portable project paths (anchored at EconAnalysis.Rproj) ----
part_dir      <- here::here("Papers", "Participation")
data_dir      <- file.path(part_dir, "data")
sdm_dir       <- file.path(part_dir, "code", "SDMs")
apollo_dir    <- file.path(part_dir, "Results", "apollo")
pred_dir      <- file.path(part_dir, "Results", "predictions")
r_output_dir  <- file.path(part_dir, "Results", "r_output")

setwd(part_dir)
res_c4 <- readRDS(file.path(pred_dir, "res_c4.rds")) 
res_c5 <- readRDS(file.path(pred_dir, "res_c5.rds")) 
res_c6 <- readRDS(file.path(pred_dir, "res_c6.rds")) 
res_c7 <- readRDS(file.path(pred_dir, "res_c7.rds")) 




# ==================================== #
#### SIMPLE EXTRACT + RUN + COMPARE ####
# ==================================== #

library(dplyr)

# ---------------------------- #
##### (A) Pick best SDM spec per cluster #####
# ----------------------------  #
oos_table <- readRDS(file.path(pred_dir, "oos_table.RDS"))

# best_spec <- oos_table %>%
#   group_by(cluster) %>%
#   slice_max(order_by = pseudoR2_oos, n = 1, with_ties = FALSE) %>%
#   ungroup() 

best_spec <- oos_table %>%
  filter(spec == "daily") %>%
  select(cluster, spec, pseudoR2_oos, LL_test_full) 

print(best_spec)

# ---------------------------- #
##### (B) Collect res objects (must exist in memory) ##### 
# ---------------------------- #
res_list <- list(
  c4 = res_c4,
  c5 = res_c5,
  c6 = res_c6,
  c7 = res_c7
)

# ----------------------------
# (C) Extract databases and original models
# ----------------------------
extract_cluster_assets <- function(res_obj, spec){
  slot <- res_obj$models[[spec]]
  if(is.null(slot)) stop(paste0("Spec '", spec, "' not found in res_obj$models"))
  
  db <- slot$database_wide
  if(is.null(db)) stop(paste0("database_wide NULL for spec '", spec, "'"))
  
  # original estimated model (if stored)
  m0 <- slot$model
  if(is.null(m0)) warning(paste0("No slot$model for spec '", spec, "' (will skip compare)."))
  
  list(database_wide = db, model_original = m0)
}

assets <- list()

for(cl in names(res_list)){
  spec_cl <- best_spec %>% filter(cluster == cl) %>% pull(spec)
  if(length(spec_cl)==0) stop(paste("No best spec for", cl))
  
  cat("\n--- Extracting", cl, "spec:", spec_cl, "---\n")
  assets[[cl]] <- extract_cluster_assets(res_list[[cl]], spec_cl)
  assets[[cl]]$spec <- spec_cl
}

segment_names <- c(
  c4 = "Southern CCS industrial squid specialist",
  c5 = "Roving industrial squid-sardine generalist",
  c6 = "PNW sardine specialist",
  c7 = "Southern CCS forage fish diverse"
)

results <- list()

db <- assets$c4$database_wide

# Cuantas filas totales
cat("Nrow:", nrow(db), "\n")

# Cuantos trips unicos
cat("Unique trips:", n_distinct(db$fished_haul_anon), "\n")

# Cuantas filas por trip (deberia ser 1 si es wide)
rows_per_trip <- db %>% 
  group_by(fished_haul_anon) %>% 
  summarise(n = n(), .groups="drop")

cat("Rows per trip - min:", min(rows_per_trip$n), 
    " max:", max(rows_per_trip$n), 
    " mean:", round(mean(rows_per_trip$n), 2), "\n")


for(cl in names(assets)){
  db <- assets[[cl]]$database_wide
  
  # todas las columnas de dist_to_cog (excluyendo no_participation)
  cog_cols <- grep("^dist_to_cog_", names(db), value=TRUE)
  cog_cols <- cog_cols[!grepl("no_participation", cog_cols)]
  
  # todas las columnas de dist_port_to_catch_area_zero (excluyendo no_participation)
  port_cols <- grep("^dist_port_to_catch_area_zero_", names(db), value=TRUE)
  port_cols <- port_cols[!grepl("no_participation", port_cols)]
  
  # promedio por trip (rowMeans), luego promedio general
  mean_cog  <- round(mean(rowMeans(db[, cog_cols],  na.rm=TRUE), na.rm=TRUE), 1)
  mean_port <- round(mean(rowMeans(db[, port_cols], na.rm=TRUE), na.rm=TRUE), 1)
  
  cat("\n===", cl, "===\n")
  cat("Mean dist to CoG:            ", mean_cog, "km\n")
  cat("Mean dist port to catch area:", mean_port, "km\n")
}

for(cl in names(assets)){
  db <- assets[[cl]]$database_wide
  db <- db %>% mutate(year = as.integer(substr(set_date, 1, 4)))
  
  vessels_yr <- db %>%
    group_by(year) %>%
    summarise(vessels = n_distinct(fished_vessel_anon), .groups="drop")
  
  trips_yr <- db %>%
    group_by(year) %>%
    summarise(trips = n(), .groups="drop")  # n() porque 1 fila = 1 trip
  
  cat("\n===", cl, "===\n")
  cat("Total trips (N):", nrow(db), "\n")
  print(merge(vessels_yr, trips_yr, by="year"))
}

summary_table <- do.call(rbind, results)
print(summary_table)