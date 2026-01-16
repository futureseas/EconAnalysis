### ============================================================
### APPENDIX ELASTICITIES: ALL CLUSTERS (PRICE + AVAIL) -> DOCX
### ============================================================

library(dplyr)
library(tibble)
library(officer)
library(flextable)

# -----------------------------
# 0) SOURCE YOUR FUNCTIONS / NL STRUCTURES
# -----------------------------
source("R/R&R/nl_structure_c4.R"); nlStructure_c4 <- nlStructure
source("R/R&R/nl_structure_c5.R"); nlStructure_c5 <- nlStructure
source("R/R&R/nl_structure_c6.R"); nlStructure_c6 <- nlStructure
source("R/R&R/nl_structure_c7.R"); nlStructure_c7 <- nlStructure

# -----------------------------
# 1) GET PROBABILITY MATRIX (N x J) FROM APOLLO PREDICTION
#    (Your P$model is a list -> cbind)
# -----------------------------
get_Pmat_prediction <- function(database_wide, apollo_beta, apollo_fixed, apollo_prob_fun,
                                indivID="fished_vessel_anon", nCores=16,
                                functionality_pred="prediction"){
  
  apollo_control <- list(
    modelName       = "PRED_ELAS_TMP",
    modelDescr      = "Prediction for elasticities",
    indivID         = indivID,
    outputDirectory = tempdir(),
    panelData       = TRUE,
    nCores          = nCores,
    workInLogs      = FALSE
  )
  
  assign("database", database_wide, envir=.GlobalEnv)
  assign("apollo_control", apollo_control, envir=.GlobalEnv)
  assign("apollo_beta", apollo_beta, envir=.GlobalEnv)
  assign("apollo_fixed", apollo_fixed, envir=.GlobalEnv)
  
  apollo_inputs <- apollo_validateInputs()
  
  P <- apollo_prob_fun(apollo_beta, apollo_inputs, functionality=functionality_pred)
  m <- P$model
  if(is.null(m)) stop("P$model is NULL.")
  
  if(is.list(m)){
    if("chosen" %in% names(m)) m <- m[names(m) != "chosen"]
    Pmat <- do.call(cbind, m)
    colnames(Pmat) <- names(m)
    return(Pmat)
  }
  
  if(is.matrix(m) || is.data.frame(m)){
    Pmat <- as.matrix(m)
    if(is.null(colnames(Pmat))) stop("P$model matrix has no colnames.")
    return(Pmat)
  }
  
  stop("Unsupported P$model type.")
}

# -----------------------------
# 2) ROBUST P(participate) + NEST PROBS
#    Works whether Pmat includes "no_participation" or not
# -----------------------------
compute_part_and_nests <- function(Pmat, nlStructure){
  
  cn <- colnames(Pmat)
  
  # P(part)
  if("no_participation" %in% cn){
    Ppart <- 1 - Pmat[, "no_participation"]
  } else {
    # fallback: sum of fishing alts under part
    fishing_alts <- character(0)
    if("part" %in% names(nlStructure)){
      fishing_alts <- unique(unlist(nlStructure[["part"]], use.names=FALSE))
      expand <- unlist(nlStructure[intersect(fishing_alts, names(nlStructure))], use.names=FALSE)
      fishing_alts <- unique(c(fishing_alts, expand))
    } else {
      fishing_alts <- unique(unlist(nlStructure[setdiff(names(nlStructure), "root")], use.names=FALSE))
    }
    fishing_alts <- setdiff(fishing_alts, c("root","part","no_participation"))
    fishing_alts <- intersect(fishing_alts, cn)
    if(length(fishing_alts) == 0) stop("Could not infer fishing alternatives to build P(part).")
    Ppart <- rowSums(Pmat[, fishing_alts, drop=FALSE])
  }
  
  # nest set (exclude root/part)
  nests <- setdiff(names(nlStructure), c("root","part"))
  
  Pnest <- lapply(nests, function(k){
    alts <- intersect(nlStructure[[k]], cn)
    if(length(alts)==0) return(rep(0, nrow(Pmat)))
    rowSums(Pmat[, alts, drop=FALSE])
  })
  names(Pnest) <- nests
  
  Pnest_cond <- lapply(Pnest, function(x) x / pmax(Ppart, 1e-12))
  
  list(Ppart=Ppart, Pnest=Pnest, Pnest_cond=Pnest_cond)
}

# helper
get_Ppart_from_Pmat <- function(Pmat, nlStructure){
  cn <- colnames(Pmat)
  if("no_participation" %in% cn) return(1 - Pmat[, "no_participation"])
  # fallback sum fishing alts
  fishing_alts <- character(0)
  if("part" %in% names(nlStructure)){
    fishing_alts <- unique(unlist(nlStructure[["part"]], use.names=FALSE))
    expand <- unlist(nlStructure[intersect(fishing_alts, names(nlStructure))], use.names=FALSE)
    fishing_alts <- unique(c(fishing_alts, expand))
  } else {
    fishing_alts <- unique(unlist(nlStructure[setdiff(names(nlStructure), "root")], use.names=FALSE))
  }
  fishing_alts <- setdiff(fishing_alts, c("root","part","no_participation"))
  fishing_alts <- intersect(fishing_alts, cn)
  rowSums(Pmat[, fishing_alts, drop=FALSE])
}

# -----------------------------
# 3) OWN/CROSS ELASTICITY MATRIX (conditional on participation)
#    IMPORTANT: shock only when x>0, and average over shocked obs
#    Output E[j,k] = response j to shock in k
# -----------------------------
elasticity_matrix_own_cross <- function(database_wide, apollo_beta, apollo_fixed,
                                        apollo_prob_fun, nlStructure,
                                        prefix,
                                        delta = 0.01,
                                        indivID="fished_vessel_anon", nCores=16){
  
  P0 <- get_Pmat_prediction(database_wide, apollo_beta, apollo_fixed, apollo_prob_fun,
                            indivID=indivID, nCores=nCores)
  M0 <- compute_part_and_nests(P0, nlStructure)
  
  P0c <- P0 / pmax(M0$Ppart, 1e-12)
  
  pmin <- 1e-3  # umbral (ajústalo)
  
  alts <- setdiff(colnames(P0), "no_participation")
  J <- length(alts)
  E <- matrix(NA_real_, nrow=J, ncol=J, dimnames=list(alts, alts))
  
  for(k in alts){
    
    var <- paste0(prefix, "_", k)
    if(!var %in% names(database_wide)) next
    
    db1 <- database_wide
    x0 <- db1[[var]]
    mask <- is.finite(x0) & x0 > 0
    if(!any(mask)) next
    
    db1[[var]] <- x0
    db1[[var]][mask] <- x0[mask] * (1 + delta)
    
    P1 <- get_Pmat_prediction(db1, apollo_beta, apollo_fixed, apollo_prob_fun,
                              indivID=indivID, nCores=nCores)
    M1 <- compute_part_and_nests(P1, nlStructure)
    P1c <- P1 / pmax(M1$Ppart, 1e-12)
    
    for(j in alts){
      denom_ok <- is.finite(P0c[, j]) & (P0c[, j] >= pmin)
      keep <- mask & denom_ok
      
      if(!any(keep)){
        E[j, k] <- NA_real_
      } else {
        e_jk <- ((P1c[, j] - P0c[, j]) / pmax(P0c[, j], 1e-12)) / delta
        E[j, k] <- mean(e_jk[keep], na.rm=TRUE)
      }
    }
  }
  
  E
}

# -----------------------------
# 4) EXTENSIVE ELASTICITY BY ALT (participation)
#    IMPORTANT: shock only when x>0 and summarize over shocked obs
# -----------------------------
extensive_elasticity_by_alt_1pct <- function(database_wide,
                                             apollo_beta, apollo_fixed, apollo_prob_fun,
                                             var_prefix,
                                             nlStructure,
                                             delta = 0.01,
                                             eps = 1e-12,
                                             verbose = TRUE){
  
  P0 <- get_Pmat_prediction(database_wide, apollo_beta, apollo_fixed, apollo_prob_fun)
  alts <- colnames(P0)
  if(is.null(alts)) stop("P0 has no colnames.")
  
  Ppart0 <- get_Ppart_from_Pmat(P0, nlStructure)
  
  out <- list()
  
  for(k in alts){
    xk <- paste0(var_prefix, "_", k)
    if(!xk %in% names(database_wide)) next
    
    db1 <- database_wide
    x0 <- db1[[xk]]
    mask <- is.finite(x0) & x0 > 0
    if(!any(mask)) next
    
    db1[[xk]] <- x0
    db1[[xk]][mask] <- x0[mask] * (1 + delta)
    
    P1 <- get_Pmat_prediction(db1, apollo_beta, apollo_fixed, apollo_prob_fun)
    Ppart1 <- get_Ppart_from_Pmat(P1, nlStructure)
    
    e <- ((Ppart1 - Ppart0) / pmax(Ppart0, eps)) / delta
    
    out[[k]] <- data.frame(
      alt = k,
      var = xk,
      mean = mean(e[mask], na.rm=TRUE),
      p05  = as.numeric(quantile(e[mask], 0.05, na.rm=TRUE)),
      p50  = as.numeric(quantile(e[mask], 0.50, na.rm=TRUE)),
      p95  = as.numeric(quantile(e[mask], 0.95, na.rm=TRUE))
    )
  }
  
  bind_rows(out) |>
    arrange(desc(abs(mean)))
}

# -----------------------------
# 5) MAKE TABLE (first col = extensive mean, rest = own/cross)
#    E is transposed so rows = shocked alt (k)
# -----------------------------
make_elast_table <- function(db, beta, fixed, prob_fun, nlStructure,
                             prefix, var_prefix,
                             delta = 0.01, digits = 3){
  
  E <- elasticity_matrix_own_cross(db, beta, fixed, prob_fun, nlStructure,
                                   prefix = prefix, delta = delta)
  E <- round(t(E), digits)   # rows = shock k
  
  ext <- extensive_elasticity_by_alt_1pct(db, beta, fixed, prob_fun,
                                          var_prefix = var_prefix,
                                          nlStructure = nlStructure,
                                          delta = delta)
  ext$mean <- round(ext$mean, digits)
  
  ext_vec <- setNames(ext$mean, ext$alt)
  
  alts <- intersect(rownames(E), names(ext_vec))
  E2 <- E[alts, alts, drop=FALSE]
  ext_vec_use <- ext_vec[alts]
  
  tab <- as.data.frame(E2) |>
    mutate(ext_participation = ext_vec_use, .before = 1) |>
    rownames_to_column(var = "Shock alternative (k)")
  
  tab
}

# -----------------------------
# 6) ADD TABLE TO WORD
# -----------------------------
add_table_to_doc <- function(doc, tab, title_txt, note_txt){
  
  ft <- flextable(tab)
  num_cols <- setdiff(names(tab), "Shock alternative (k)")
  ft <- colformat_num(ft, j = num_cols, digits = 3)
  ft <- theme_booktabs(ft)
  ft <- autofit(ft)
  ft <- fontsize(ft, part = "all", size = 9)
  ft <- align(ft, align = "left", j = 1, part = "all")
  ft <- align(ft, align = "center", j = 2:ncol(tab), part = "all")
  ft <- set_table_properties(ft, width = 1, layout = "autofit")
  
  doc <- body_add_par(doc, title_txt, style = "heading 2")
  doc <- body_add_flextable(doc, ft)
  doc <- body_add_par(doc, note_txt, style = "Normal")
  doc <- body_add_break(doc)
  
  doc
}



# -----------------------------
# 7) NOTES
# -----------------------------
note_price <- paste(
  "Rows indicate the shocked alternative k (1% increase in its expected price, applied only when the price covariate is > 0).",
  "First column reports the participation (extensive-margin) elasticity: d ln P(participate) / d ln price_k.",
  "Remaining columns report elasticities of conditional choice probabilities across fishing alternatives: d ln P(j|participate) / d ln price_k.",
  "All quantities are summarized by the sample mean.",
  sep=" "
)

note_avail <- paste(
  "Rows indicate the shocked alternative k (1% increase in its expected availability, applied only when the availability covariate is > 0).",
  "First column reports the participation (extensive-margin) elasticity: d ln P(participate) / d ln avail_k.",
  "Remaining columns report elasticities of conditional choice probabilities across fishing alternatives: d ln P(j|participate) / d ln avail_k.",
  "All quantities are summarized by the sample mean.",
  sep=" "
)

# 
# ------------------------------

# -----------------------------
# 1) CLUSTERS OBJECTS (you already have these in memory)
# -----------------------------
clusters <- list(
  c4 = list(db = db_c4, beta = apollo_beta_c4, fixed = apollo_fixed_c4,
            prob = apollo_probabilities_c4, nl = nlStructure_c4),
  c5 = list(db = db_c5, beta = apollo_beta_c5, fixed = apollo_fixed_c5,
            prob = apollo_probabilities_c5, nl = nlStructure_c5),
  c6 = list(db = db_c6, beta = apollo_beta_c6, fixed = apollo_fixed_c6,
            prob = apollo_probabilities_c6, nl = nlStructure_c6),
  c7 = list(db = db_c7, beta = apollo_beta_c7, fixed = apollo_fixed_c7,
            prob = apollo_probabilities_c7, nl = nlStructure_c7)
)

# -----------------------------
# 2) PREFIX MAPS (PRICE differs for c5/c6)
# -----------------------------
price_prefix_by_cluster <- c(c4="mean_price", c5="mean_price_3", c6="mean_price_3", c7="mean_price")
avail_prefix_by_cluster <- c(c4="mean_avail", c5="mean_avail",   c6="mean_avail",   c7="mean_avail")


# -----------------------------
# 3) RUN + EXPORT WORD
# -----------------------------

doc <- read_docx()

for(cl in names(clusters)){
  obj <- clusters[[cl]]
  
  # PRICE
  pref_p <- price_prefix_by_cluster[[cl]]
  tab_price <- make_elast_table(
    db=obj$db, beta=obj$beta, fixed=obj$fixed, prob_fun=obj$prob, nlStructure=obj$nl,
    prefix=pref_p, var_prefix=pref_p, delta=0.01, digits=3
  )
  doc <- add_table_to_doc(
    doc, tab_price,
    paste0("Appendix Table. Extensive- and cross-price elasticities (Cluster ", cl, ")"),
    note_price
  )
  
  # AVAIL
  pref_a <- avail_prefix_by_cluster[[cl]]
  tab_avail <- make_elast_table(
    db=obj$db, beta=obj$beta, fixed=obj$fixed, prob_fun=obj$prob, nlStructure=obj$nl,
    prefix=pref_a, var_prefix=pref_a, delta=0.01, digits=3
  )
  doc <- add_table_to_doc(
    doc, tab_avail,
    paste0("Appendix Table. Extensive- and cross-availability elasticities (Cluster ", cl, ")"),
    note_avail
  )
}

out_path <- file.path("R", "output", "appendix_elasticities_allclusters_price_avail.docx")
print(doc, target = out_path)

out_path



