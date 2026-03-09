library(dplyr)

gc()

library(apollo)
apollo_initialise()

setwd("D:/GitHub/EconAnalysis/Participation")
res_c4 <- readRDS("res_c4.rds") 
res_c5 <- readRDS("res_c5.rds") 
res_c6 <- readRDS("res_c6.rds") 
res_c7 <- readRDS("res_c7.rds") 


# ==================================== #
#### SIMPLE EXTRACT + RUN + COMPARE ####
# ==================================== #

library(dplyr)

# ---------------------------- #
##### (A) Pick best SDM spec per cluster #####
# ----------------------------  #
oos_table <- readRDS("oos_table.RDS")

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

for(cl in names(assets)){
  db <- assets[[cl]]$database_wide
  db <- db %>% mutate(year = as.integer(substr(set_date, 1, 4)))
  
  vessels_yr <- db %>%
    group_by(year) %>%
    summarise(vessels = n_distinct(fished_vessel_anon), .groups="drop")
  
  hauls_yr <- db %>%
    group_by(year) %>%
    summarise(hauls = n_distinct(fished_haul_anon), .groups="drop")
  
  results[[cl]] <- data.frame(
    segment   = segment_names[cl],
    cluster   = cl,
    years     = paste(range(db$year), collapse="-"),
    mean_vessels_yr = round(mean(vessels_yr$vessels), 0),
    mean_hauls_yr   = round(mean(hauls_yr$hauls), 0)
  )
  
  # also print year-by-year so you can check
  cat("\n===", cl, "===\n")
  print(merge(vessels_yr, hauls_yr, by="year"))
}

summary_table <- do.call(rbind, results)
print(summary_table)