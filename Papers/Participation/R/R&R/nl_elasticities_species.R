### ============================================================
### SPECIES-LEVEL ELASTICITIES (PRICE + AVAIL) -> DOCX
### c4,c5,c6,c7
### ============================================================

library(dplyr)
library(tibble)
library(officer)
library(flextable)

# --- load your NL + functions ---
source("R/R&R/nl_structure_c4.R"); nlStructure_c4 <- nlStructure
source("R/R&R/nl_structure_c5.R"); nlStructure_c5 <- nlStructure
source("R/R&R/nl_structure_c6.R"); nlStructure_c6 <- nlStructure
source("R/R&R/nl_structure_c7.R"); nlStructure_c7 <- nlStructure


# -----------------------------
# 1) GET Pmat (N x J) from prediction
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
# 2) P(participate) robust
# -----------------------------
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
  if(length(fishing_alts)==0) stop("Could not infer fishing alternatives to build P(part).")
  
  rowSums(Pmat[, fishing_alts, drop=FALSE])
}

# -----------------------------
# 3) helper: alt -> species (suffix after last "_")
# -----------------------------
alt_to_species <- function(alt){
  sub("^.*_", "", alt)
}

# -----------------------------
# 4) aggregate conditional alt probs into species probs
# -----------------------------
Pcond_species <- function(Pmat, nlStructure){
  Ppart <- get_Ppart_from_Pmat(Pmat, nlStructure)
  Pcond <- Pmat / pmax(Ppart, 1e-12)
  
  alts <- setdiff(colnames(Pmat), "no_participation")
  sp <- vapply(alts, alt_to_species, character(1))
  
  spp <- sort(unique(sp))
  Psp <- sapply(spp, function(s){
    cols <- alts[sp == s]
    rowSums(Pcond[, cols, drop=FALSE])
  })
  colnames(Psp) <- spp
  list(Psp = Psp, Ppart = Ppart, species = spp, alts = alts, sp_alt = sp)
}

# -----------------------------
# 5) species-level elasticity matrix (intensive)
#    E_species[j, k] = d ln P(species=j | part) / d ln x_species=k
#    shock applies to ALL vars for that species where x>0
# -----------------------------
elasticity_species_matrix <- function(db, beta, fixed, prob_fun, nlStructure,
                                      var_prefix, delta=0.01, nCores=16){
  
  P0 <- get_Pmat_prediction(db, beta, fixed, prob_fun, nCores=nCores)
  base <- Pcond_species(P0, nlStructure)
  P0_sp <- base$Psp
  species <- base$species
  alts <- base$alts
  sp_alt <- base$sp_alt
  
  E <- matrix(NA_real_, nrow=length(species), ncol=length(species),
              dimnames=list(response_species=species, shocked_species=species))
  
  for(sk in species){
    
    db1 <- db
    alts_k <- alts[sp_alt == sk]
    vars_k <- paste0(var_prefix, "_", alts_k)
    vars_k <- vars_k[vars_k %in% names(db1)]
    if(length(vars_k)==0) next
    
    # shock only where x>0 (union mask across vars in species)
    mask_any <- rep(FALSE, nrow(db1))
    for(v in vars_k){
      x0 <- db1[[v]]
      mask <- is.finite(x0) & x0 > 0
      if(any(mask)){
        db1[[v]][mask] <- x0[mask] * (1 + delta)
        mask_any <- mask_any | mask
      }
    }
    if(!any(mask_any)) next
    
    P1 <- get_Pmat_prediction(db1, beta, fixed, prob_fun, nCores=nCores)
    P1_sp <- Pcond_species(P1, nlStructure)$Psp
    
    for(sj in species){
      e <- ((P1_sp[, sj] - P0_sp[, sj]) / pmax(P0_sp[, sj], 1e-12)) / delta
      E[sj, sk] <- mean(e[mask_any], na.rm=TRUE)
    }
  }
  
  E
}

# -----------------------------
# 6) species-level extensive elasticity (participation)
#    ext_species[k] = d ln P(part) / d ln x_species=k
# -----------------------------
extensive_species <- function(db, beta, fixed, prob_fun, nlStructure,
                              var_prefix, delta=0.01, nCores=16){
  
  P0 <- get_Pmat_prediction(db, beta, fixed, prob_fun, nCores=nCores)
  Ppart0 <- get_Ppart_from_Pmat(P0, nlStructure)
  
  base <- Pcond_species(P0, nlStructure)
  species <- base$species
  alts <- base$alts
  sp_alt <- base$sp_alt
  
  out <- rep(NA_real_, length(species))
  names(out) <- species
  
  for(sk in species){
    
    db1 <- db
    alts_k <- alts[sp_alt == sk]
    vars_k <- paste0(var_prefix, "_", alts_k)
    vars_k <- vars_k[vars_k %in% names(db1)]
    if(length(vars_k)==0) next
    
    mask_any <- rep(FALSE, nrow(db1))
    for(v in vars_k){
      x0 <- db1[[v]]
      mask <- is.finite(x0) & x0 > 0
      if(any(mask)){
        db1[[v]][mask] <- x0[mask] * (1 + delta)
        mask_any <- mask_any | mask
      }
    }
    if(!any(mask_any)) next
    
    P1 <- get_Pmat_prediction(db1, beta, fixed, prob_fun, nCores=nCores)
    Ppart1 <- get_Ppart_from_Pmat(P1, nlStructure)
    
    e <- ((Ppart1 - Ppart0) / pmax(Ppart0, 1e-12)) / delta
    out[sk] <- mean(e[mask_any], na.rm=TRUE)
  }
  
  out
}

# -----------------------------
# 7) make species table (first column extensive)
# -----------------------------
make_species_table <- function(db, beta, fixed, prob_fun, nlStructure,
                               var_prefix, delta=0.01, digits=3, nCores=16){
  
  Esp <- elasticity_species_matrix(db, beta, fixed, prob_fun, nlStructure,
                                   var_prefix=var_prefix, delta=delta, nCores=nCores)
  
  ext <- extensive_species(db, beta, fixed, prob_fun, nlStructure,
                           var_prefix=var_prefix, delta=delta, nCores=nCores)
  
  # build table: rows = shocked species k (so transpose)
  E_rep <- round(t(Esp), digits)
  ext_rep <- round(ext[rownames(E_rep)], digits)
  
  tab <- as.data.frame(E_rep) |>
    mutate(ext_participation = ext_rep, .before = 1) |>
    rownames_to_column("Shocked species (k)")
  
  tab
}

# -----------------------------
# 8) add to doc
# -----------------------------
add_table_to_doc <- function(doc, tab, title_txt, note_txt){
  
  ft <- flextable(tab)
  num_cols <- setdiff(names(tab), "Shocked species (k)")
  ft <- colformat_num(ft, j=num_cols, digits=3)
  ft <- theme_booktabs(ft)
  ft <- autofit(ft)
  ft <- fontsize(ft, part="all", size=9)
  ft <- align(ft, align="left", j=1, part="all")
  ft <- align(ft, align="center", j=2:ncol(tab), part="all")
  ft <- set_table_properties(ft, width=1, layout="autofit")
  
  doc <- body_add_par(doc, title_txt, style="heading 2")
  doc <- body_add_flextable(doc, ft)
  doc <- body_add_par(doc, note_txt, style="Normal")
  doc <- body_add_break(doc)
  
  doc
}

# -----------------------------
# 9) cluster objects (must exist in memory)
# -----------------------------

# notes
note_price <- paste(
  "Rows indicate the shocked species k (1% increase in expected price for all alternatives of species k, applied only when the covariate is > 0).",
  "First column reports the participation (extensive-margin) elasticity: d ln P(participate) / d ln price_k.",
  "Remaining columns report elasticities of conditional species choice probabilities: d ln P(species=j|participate) / d ln price_k.",
  "All quantities are summarized by the sample mean.",
  sep=" "
)

note_avail <- paste(
  "Rows indicate the shocked species k (1% increase in expected availability for all alternatives of species k, applied only when the covariate is > 0).",
  "First column reports the participation (extensive-margin) elasticity: d ln P(participate) / d ln avail_k.",
  "Remaining columns report elasticities of conditional species choice probabilities: d ln P(species=j|participate) / d ln avail_k.",
  "All quantities are summarized by the sample mean.",
  sep=" "
)


