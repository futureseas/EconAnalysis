# ==================================== #
#### REQUIRED OBJECTS (must exist in your session) ####
# ==================================== #


gc()

library(dplyr)


setwd("D:/GitHub/EconAnalysis/Participation")
res_c4 <- readRDS("res_c4.rds") 
res_c5 <- readRDS("res_c5.rds") 
res_c6 <- readRDS("res_c6.rds") 
res_c7 <- readRDS("res_c7.rds") 

oos_table <- readRDS("oos_table.RDS")

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

# ----------------------------
# (D) Save databases to disk (one file per cluster) + a combined list
# ----------------------------
dir.create(file.path("R","output","databases_bestSDM"), showWarnings = FALSE, recursive = TRUE)

for(cl in names(assets)){
  saveRDS(
    assets[[cl]]$database_wide,
    file = file.path("R","output","databases_bestSDM", paste0("database_", cl, "_", assets[[cl]]$spec, ".rds"))
  )
}

saveRDS(assets, file = file.path("R","output","assets_bestSDM_byCluster.rds"))

cat("\nSaved:\n")
for(cl in names(assets)){
  cat(" - ", file.path("R","output","databases_bestSDM", paste0("database_", cl, "_", assets[[cl]]$spec, ".rds")), "\n")
}
cat(" - ", file.path("R","output","assets_bestSDM_byCluster.rds"), "\n")



# ---- Descriptive Statistics ---- #

library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(officer)
library(flextable)


db_c4 <- assets$c4$database_wide
db_c5 <- assets$c5$database_wide
db_c6 <- assets$c6$database_wide
db_c7 <- assets$c7$database_wide

# ==========================
# Helpers
# ==========================

desc_stats <- function(x, digits = 3){
  x_num <- suppressWarnings(as.numeric(x))
  out <- tibble::tibble(
    N      = sum(!is.na(x_num)),
    mean   = mean(x_num, na.rm=TRUE),
    sd     = sd(x_num, na.rm=TRUE),
    min    = min(x_num, na.rm=TRUE),
    # p05    = quantile(x_num, 0.05, na.rm=TRUE, names=FALSE),
    # p25    = quantile(x_num, 0.25, na.rm=TRUE, names=FALSE),
    # median = quantile(x_num, 0.50, na.rm=TRUE, names=FALSE),
    # p75    = quantile(x_num, 0.75, na.rm=TRUE, names=FALSE),
    # p95    = quantile(x_num, 0.95, na.rm=TRUE, names=FALSE),
    max    = max(x_num, na.rm=TRUE),
    # share_zero = mean(x_num == 0, na.rm=TRUE),
    # share_one  = mean(x_num == 1, na.rm=TRUE)
  )
  
  # redondea todo excepto N
  out <- out %>%
    dplyr::mutate(
      dplyr::across(-N, ~ round(.x, digits))
    )
  
  out
}


# Descriptiva de variables CASE-LEVEL (columnas sin sufijo de alternativa)
make_case_level_desc <- function(db, drop_cols = c("choice","fished_vessel_anon")){
  # Heurística: nos quedamos con columnas que NO contienen "_<alt>" al final,
  # y que NO son claramente alt-level (mean_avail_, mean_price_, etc.)
  # Ajusta si tienes nombres raros.
  alt_prefixes <- c(
    "mean_avail_", "mean_price_", "mean_price_3_",
    "wind_max_220_mh_", "dist_to_cog_", "dist_port_to_catch_area_zero_",
    "dummy_prev_days_", "dummy_prev_year_days_",
    "unem_rate_", "d_c_", "d_d_", "d_cd_"
  )
  
  case_cols <- setdiff(names(db), drop_cols)
  case_cols <- case_cols[!Reduce(`|`, lapply(alt_prefixes, \(p) str_starts(case_cols, p)))]
  
  # Además, algunas variables alt-level podrían no estar en esa lista: filtra por patrón "_[a-z]{2,3}_.+"
  # (si esto te elimina cosas que sí quieres, comenta esta línea)
  case_cols <- case_cols[!str_detect(case_cols, "_.+_.+")]  # muy conservador para tu set actual
  
  out <- map_dfr(case_cols, \(v){
    tibble(variable = v) %>% bind_cols(desc_stats(db[[v]]))
  }) %>%
    arrange(variable)
  
  out
}

# Descriptiva ALT-LEVEL: agrupa columnas por prefijo (var_prefix),
# pivotea a long, calcula stats "pooled over alternatives"

alt_desc_compact <- function(db, var_prefix,
                             drop_alts = c("no_participation","nopart","no_part")){
  
  cols <- names(db)[stringr::str_starts(names(db), paste0(var_prefix, "_"))]
  if(length(cols)==0) return(NULL)
  
  long <- db %>%
    select(all_of(cols)) %>%
    tidyr::pivot_longer(everything(), names_to="name", values_to="value") %>%
    mutate(
      alt = stringr::str_remove(name, paste0("^", var_prefix, "_"))
    ) %>%
    filter(!alt %in% drop_alts)   # <<< CLAVE
  
  if(nrow(long)==0) return(NULL)
  
  # (A) pooled over alternatives × rows
  pooled <- tibble(variable = var_prefix, level = "pooled") %>%
    bind_cols(desc_stats(long$value))
  
  # (B) distribution across alternatives (means by alt)
  alt_means <- long %>%
    group_by(alt) %>%
    summarise(alt_mean = mean(value, na.rm=TRUE), .groups="drop")
  
  across_alt <- tibble(
    variable = var_prefix,
    level = "across_alternatives (alt_means)"
  ) %>%
    bind_cols(desc_stats(alt_means$alt_mean))
  
  bind_rows(pooled)
}


make_desc_by_cluster_compact <- function(db){
  
  prefixes <- c(
    "mean_avail",
    "mean_price", "mean_price_3",
    "wind_max_220_mh",
    "dist_to_cog", "dist_port_to_catch_area_zero",
    "dummy_prev_days", "dummy_prev_year_days",
    "unem_rate"
  )
  
  tab_case <- make_case_level_desc(db)
  
  alt_tabs <- purrr::keep(purrr::map(prefixes, \(p) alt_desc_compact(db, p)), ~ !is.null(.x))
  alt_compact <- dplyr::bind_rows(alt_tabs)
  
  list(case_level = tab_case, alt_compact = alt_compact)
}








# ==========================
# RUN for your four clusters
# ==========================
db_list <- list(c4=db_c4, c5=db_c5, c6=db_c6, c7=db_c7)

desc_all <- purrr::imap(db_list, \(db, cl){
  make_desc_by_cluster_compact(db)
})

doc <- officer::read_docx()

for(cl in names(desc_all)){
  doc <- officer::body_add_par(doc, paste0("Descriptive statistics — Cluster ", cl), style="heading 1")
  
  doc <- officer::body_add_par(doc, "Case-level variables", style="heading 2")
  ft1 <- flextable::flextable(desc_all[[cl]]$case_level)
  ft1 <- flextable::colformat_num(ft1, digits = 3)   
  ft1 <- flextable::colformat_num(ft1, j = setdiff(names(ft1$body$dataset), "N"), digits = 3) |> flextable::autofit()
  ft1 <- flextable::fontsize(ft1, size = 8, part="all")
  doc <- flextable::body_add_flextable(doc, value = ft1)   # <<<<< CAMBIO
  
  doc <- officer::body_add_par(doc, "Alternative-level variables (compact)", style="heading 2")
  ft2 <- flextable::flextable(desc_all[[cl]]$alt_compact) 
  ft2 <- flextable::colformat_num(ft2, digits = 3)   
  ft2 <- flextable::colformat_num(ft2, j = setdiff(names(ft2$body$dataset), "N"), digits = 3) |> flextable::autofit()
  ft2 <- flextable::fontsize(ft2, size = 8, part="all")
  doc <- flextable::body_add_flextable(doc, value = ft2)   # <<<<< CAMBIO
}

out_doc <- file.path("R","output","descriptive_stats_by_cluster_COMPACT.docx")
print(doc, target = out_doc)
out_doc


# ==========================
# OPTIONAL: also export CSVs
# ==========================
dir.create(file.path("R","output","desc_stats"), showWarnings = FALSE, recursive = TRUE)

for(cl in names(desc_all)){
  write.csv(desc_all[[cl]]$case_level, file.path("R","output","desc_stats", paste0("desc_case_", cl, ".csv")), row.names=FALSE)
  write.csv(desc_all[[cl]]$alt_pooled, file.path("R","output","desc_stats", paste0("desc_alt_pooled_", cl, ".csv")), row.names=FALSE)
  write.csv(desc_all[[cl]]$alt_by_alt, file.path("R","output","desc_stats", paste0("desc_alt_byalt_", cl, ".csv")), row.names=FALSE)
}



# ==========================
# SUPPLEMENTARY S3: Correlation heatmap of main regressors
# ==========================

library(ggplot2)
library(reshape2)

# --- Helper: extract and label the main regressors for one cluster ---
# --- Helper: extract and label the main regressors for one cluster ---
get_regressor_matrix <- function(db) {
  
  # Pick the correct price prefix depending on what exists in the database
  price_prefix <- if (any(stringr::str_starts(names(db), "mean_price_3_"))) {
    "mean_price_3"
  } else {
    "mean_price"
  }
  
  alt_prefixes <- c("mean_avail", price_prefix,
                    "wind_max_220_mh", "dist_to_cog",
                    "dist_port_to_catch_area_zero")
  
  alt_means <- purrr::map_dfc(alt_prefixes, function(pfx) {
    cols <- names(db)[stringr::str_starts(names(db), paste0(pfx, "_"))]
    cols <- cols[!stringr::str_detect(cols, "no_part")]
    if (length(cols) == 0) return(NULL)
    avg <- rowMeans(db[, cols], na.rm = TRUE)
    tibble::tibble(!!pfx := avg)
  })
  
  alt_means %>%
    dplyr::select(where(~ var(.x, na.rm = TRUE) > 0))
}


nice_labels <- c(
  mean_avail                    = "Mean availability",
  mean_price                    = "Mean price",       # c4, c7
  mean_price_3                  = "Mean price",       # c5, c6
  wind_max_220_mh               = "Max wind speed",
  dist_to_cog                   = "Dist. to center of gravity",
  dist_port_to_catch_area_zero  = "Dist. port to catch area"
)

plot_corr_heatmap <- function(db, cluster_id) {
  
  mat <- get_regressor_matrix(db)
  
  cor_mat <- cor(mat, use = "pairwise.complete.obs", method = "pearson")
  
  colnames(cor_mat) <- rownames(cor_mat) <-
    dplyr::recode(colnames(cor_mat), !!!nice_labels)
  
  cor_mat_idx <- cor_mat
  cor_mat_idx[upper.tri(cor_mat_idx, diag = TRUE)] <- NA
  cor_long2 <- reshape2::melt(cor_mat_idx, varnames = c("Var1", "Var2"), value.name = "r") %>%
    dplyr::filter(!is.na(r))
  
  ggplot(cor_long2, aes(x = Var1, y = Var2, fill = r)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = sprintf("%.2f", r)),
              size = 2.8, color = "black") +
    scale_fill_gradient2(
      low      = "#2166ac",
      mid      = "white",
      high     = "#d6604d",
      midpoint = 0,
      limits   = c(-1, 1),
      name     = "Pearson r"
    ) +
    coord_fixed() +
    labs(
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x      = element_text(angle = 45, hjust = 1, size = 8),
      axis.text.y      = element_text(size = 8),
      panel.grid       = element_blank(),
      legend.position  = "right",
      plot.title       = element_text(face = "bold", size = 11),
      plot.subtitle    = element_text(size = 8, color = "grey40")
    )
}


dir.create(file.path("R", "output", "corr_heatmaps"), showWarnings = FALSE, recursive = TRUE)

db_list_raw <- list(c4 = db_c4, c5 = db_c5, c6 = db_c6, c7 = db_c7)

for (cl in names(db_list_raw)) {
  p <- plot_corr_heatmap(db_list_raw[[cl]], cl)
  
  out_png <- file.path("R", "output", "corr_heatmaps",
                       paste0("corr_heatmap_cluster_", cl, ".png"))
  
  ggsave(out_png, plot = p, width = 8, height = 7, dpi = 300, bg = "white")
  cat("Saved:", out_png, "\n")
}

