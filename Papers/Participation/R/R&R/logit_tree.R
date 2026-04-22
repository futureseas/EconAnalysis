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



# ---- LOGIT TREE ----

choice_overview <- function(db, alternatives, avail, choiceVar = "choice",
                            drop_alts = NULL, digits = 2){
  
  stopifnot(choiceVar %in% names(db))
  N <- nrow(db)
  
  # Evalúa avail como vectores de largo N
  avail_mat <- sapply(names(alternatives), function(a){
    v <- avail[[a]]
    if(length(v) == 1) rep(v, N) else v
  })
  
  # chosen counts por alternativa (usando IDs de alternatives)
  chosen_counts <- sapply(names(alternatives), function(a){
    sum(db[[choiceVar]] == alternatives[[a]], na.rm = TRUE)
  })
  
  available_counts <- colSums(avail_mat == 1, na.rm = TRUE)
  
  out <- data.frame(
    Alternative = names(alternatives),
    Times_available = as.numeric(available_counts),
    Times_chosen = as.numeric(chosen_counts),
    Percentage_chosen_overall = round(100 * chosen_counts / N, digits),
    Percentage_chosen_when_available = round(100 * chosen_counts / pmax(available_counts, 1), digits),
    row.names = NULL
  )
  
  if(!is.null(drop_alts)){
    out <- out[!out$Alternative %in% drop_alts, , drop=FALSE]
  }
  
  out
}


library(dplyr)
library(stringr)
library(tidyr)
library(flextable)
library(officer)

# --- helper: devuelve todas las alternativas descendientes de un nodo ---
descendant_alts <- function(nlStructure, node){
  kids <- nlStructure[[node]]
  if(is.null(kids)) return(character(0))
  alts <- character(0)
  for(ch in kids){
    if(ch %in% names(nlStructure)){
      alts <- c(alts, descendant_alts(nlStructure, ch))
    } else {
      alts <- c(alts, ch)
    }
  }
  unique(alts)
}

# --- construir tabla 3 niveles: L1 -> L2 -> L3 ---
nl_tree_table_3levels <- function(nlStructure, shares_df, root="root",
                                  digits = 2, include_outside = TRUE,
                                  outside_name = "no_participation"){
  
  # shares_df debe tener: Alternative, Times_chosen, Percentage_chosen_overall
  stopifnot(all(c("Alternative","Times_chosen","Percentage_chosen_overall") %in% names(shares_df)))
  
  # total N elegido (para porcentajes consistentes)
  total_chosen <- sum(shares_df$Times_chosen, na.rm=TRUE)
  
  root_kids <- nlStructure[[root]]
  if(is.null(root_kids)) stop("root not found in nlStructure")
  
  rows <- list()
  
  # función para lookup de chosen y pct de una alternativa
  alt_stats <- function(a){
    r <- shares_df[shares_df$Alternative == a, , drop=FALSE]
    if(nrow(r)==0){
      c(chosen=0, pct=0)
    } else {
      c(chosen = as.numeric(r$Times_chosen[1]),
        pct    = as.numeric(r$Percentage_chosen_overall[1]))
    }
  }
  
  # recorre hijos del root
  for(l1 in root_kids){
    
    # Caso 1: l1 es una alternativa directa (ej: no_participation)
    if(!(l1 %in% names(nlStructure))){
      if(!include_outside && l1 == outside_name) next
      
      st <- alt_stats(l1)
      rows[[length(rows)+1]] <- tibble(
        level1 = l1, level2 = NA_character_, level3 = l1,
        N = st["chosen"], pct = st["pct"],
        row_type = "alt"
      )
      next
    }
    
    # Caso 2: l1 es un nest (ej: part / cwa_dcrb / etc.)
    alts_l1 <- descendant_alts(nlStructure, l1)
    if(!include_outside) alts_l1 <- setdiff(alts_l1, outside_name)
    
    chosen_l1 <- sum(shares_df$Times_chosen[match(alts_l1, shares_df$Alternative)], na.rm=TRUE)
    pct_l1 <- if(total_chosen>0) 100*chosen_l1/total_chosen else 0
    
    # subtotal nivel 1
    rows[[length(rows)+1]] <- tibble(
      level1 = l1, level2 = NA_character_, level3 = NA_character_,
      N = chosen_l1, pct = pct_l1,
      row_type = "subtotal_l1"
    )
    
    # hijos del nivel 1 (nivel 2)
    kids_l2 <- nlStructure[[l1]]
    
    for(l2 in kids_l2){
      
      # si l2 es alternativa directa bajo l1
      if(!(l2 %in% names(nlStructure))){
        if(!include_outside && l2 == outside_name) next
        st <- alt_stats(l2)
        rows[[length(rows)+1]] <- tibble(
          level1 = l1, level2 = l2, level3 = l2,
          N = st["chosen"], pct = st["pct"],
          row_type = "alt"
        )
        next
      }
      
      # si l2 es nest real
      alts_l2 <- descendant_alts(nlStructure, l2)
      if(!include_outside) alts_l2 <- setdiff(alts_l2, outside_name)
      
      chosen_l2 <- sum(shares_df$Times_chosen[match(alts_l2, shares_df$Alternative)], na.rm=TRUE)
      pct_l2 <- if(total_chosen>0) 100*chosen_l2/total_chosen else 0
      
      # subtotal nivel 2
      rows[[length(rows)+1]] <- tibble(
        level1 = l1, level2 = l2, level3 = NA_character_,
        N = chosen_l2, pct = pct_l2,
        row_type = "subtotal_l2"
      )
      
      # nivel 3: alternativas bajo l2
      for(a in nlStructure[[l2]]){
        if(a %in% names(nlStructure)) next  # (por si hubiera >3 niveles)
        if(!include_outside && a == outside_name) next
        st <- alt_stats(a)
        rows[[length(rows)+1]] <- tibble(
          level1 = l1, level2 = l2, level3 = a,
          N = st["chosen"], pct = st["pct"],
          row_type = "alt"
        )
      }
    }
  }
  
  out <- bind_rows(rows) %>%
    mutate(
      N = as.numeric(N),
      pct = round(as.numeric(pct), digits)
    )
  
  # total al final
  out <- bind_rows(
    out,
    tibble(level1 = "TOTAL", level2 = NA_character_, level3 = NA_character_,
           N = total_chosen, pct = 100, row_type = "total")
  )
  
  out
}

# --- flextable bonita con indentación + negrita en subtotales ---
make_tree_flextable <- function(df, digits = 2, title = NULL){
  
  # columna "node" para mostrar indentado (estilo árbol)
  df2 <- df %>%
    mutate(
      node = case_when(
        row_type == "subtotal_l1" ~ paste0(level1),
        row_type == "subtotal_l2" ~ paste0("  └─ ", level2),
        row_type == "alt" & !is.na(level2) ~ paste0("      └─ ", level3),
        row_type == "alt" & is.na(level2)  ~ paste0(level3),
        row_type == "total" ~ "TOTAL",
        TRUE ~ ""
      )
    ) %>%
    select(node, N, pct)
  
  ft <- flextable(df2)
  
  ft <- set_header_labels(ft,
                          node = "Tree node (3 levels)",
                          N    = "N chosen",
                          pct  = "% chosen"
  )
  
  ft <- colformat_num(ft, j = c("N","pct"), digits = digits)
  ft <- autofit(ft)
  ft <- fontsize(ft, size = 8, part = "all")
  
  # negrita subtotales + total
  bold_rows <- which(df$row_type %in% c("subtotal_l1","subtotal_l2","total"))
  if(length(bold_rows)>0) ft <- bold(ft, i = bold_rows, bold = TRUE)
  
  # opcional: línea horizontal arriba del total
  total_i <- which(df$row_type == "total")
  if(length(total_i)==1){
    ft <- hline(ft, i = total_i, border = fp_border(width = 1))
  }
  
  if(!is.null(title)){
    ft <- add_header_lines(ft, values = title)
    ft <- bold(ft, i = 1, part = "header", bold = TRUE)
  }
  
  ft
}


print_nl_tree <- function(nlStructure, root = "root", indent = "", is_last = TRUE){
  # nlStructure: list donde cada nodo apunta a hijos (alternativas o nests)
  # Un nodo es "nest" si aparece como nombre en nlStructure
  # Un nodo es "alternative" si NO aparece como nombre en nlStructure
  
  connector <- if(indent == "") "" else if(is_last) "'- " else "|- "
  
  # imprime root (como nest)
  if(indent == ""){
    cat("Nest:", root, "\n")
  }
  
  kids <- nlStructure[[root]]
  if(is.null(kids)) return(invisible(NULL))
  
  for(i in seq_along(kids)){
    child <- kids[i]
    last_child <- (i == length(kids))
    child_indent <- paste0(indent, if(indent=="" ) "" else if(is_last) "   " else "|  ")
    
    if(child %in% names(nlStructure)){
      # es nest
      cat(indent, connector, "Nest: ", child, "\n", sep="")
      print_nl_tree(nlStructure, root = child, indent = child_indent, is_last = last_child)
    } else {
      # es alternativa
      cat(indent, connector, "Alternative: ", child, "\n", sep="")
    }
  }
  
  invisible(NULL)
}



##### ---- Cluster C4 --- 

db_c4 <- assets$c4$database_wide

alts_c4 <- c(
  sfa_nanc = 1,
  laa_nanc = 2,
  laa_cmck = 3,
  laa_msqd = 4,
  laa_ytna = 5,
  mna_msqd = 6,
  sba_msqd = 7,
  laa_btna = 8,
  sfa_msqd = 9,
  mna_psdn = 10,
  sba_cmck = 11,
  mra_msqd = 12,
  laa_psdn = 13,
  mna_nanc = 14,
  no_participation = 15
)


avail_c4 <- with(db_c4, list(
  sfa_nanc = 1,
  laa_nanc = 1,
  laa_cmck = 1,
  laa_msqd = 1,
  laa_ytna = 1,
  mna_msqd = 1,
  sba_msqd = 1,
  laa_btna = 1 - btnaclosure,
  sfa_msqd = 1,
  mna_psdn = 1,
  sba_cmck = 1,
  mra_msqd = 1,
  laa_psdn = 1,
  mna_nanc = 1,
  no_participation = 1
))

tab_c4 <- choice_overview(
  db          = db_c4,
  alternatives = alts_c4,
  avail        = avail_c4,
  choiceVar    = "choice",
  digits       = 2
)

nlStructure_c4 <- list()
nlStructure_c4[["root"]] <- c("no_participation","part")
nlStructure_c4[["part"]] <- c("cmck","msqd","psdn","nanc","tuna")
nlStructure_c4[["cmck"]] <- c("laa_cmck","sba_cmck")
nlStructure_c4[["msqd"]] <- c("laa_msqd","mna_msqd","mra_msqd","sba_msqd","sfa_msqd")
nlStructure_c4[["psdn"]] <- c("laa_psdn","mna_psdn")
nlStructure_c4[["nanc"]] <- c("laa_nanc","mna_nanc","sfa_nanc")
nlStructure_c4[["tuna"]] <- c("laa_ytna","laa_btna")
print_nl_tree(nlStructure_c4, root="root")

tree_tab_c4 <- nl_tree_table_3levels(nlStructure_c4, tab_c4, root="root", digits=2)
ft_c4 <- make_tree_flextable(tree_tab_c4, digits=2, title="Cluster c4 — NL tree with observed choice shares")


##### ---- Cluster C5 --- 

db_c5 <- assets$c5$database_wide

alts_c5 <- c(
  mna_msqd = 1, sba_msqd = 2, mra_msqd = 3, laa_msqd = 4, npa_msqd = 5, sfa_msqd = 6,  
  cba_msqd = 7, laa_psdn = 8, clo_psdn = 9, cwa_psdn = 10, clw_psdn = 11, sba_cmck = 12,
  laa_cmck = 13, laa_nanc = 14, cwa_albc = 15, cwa_dcrb = 16, clw_dcrb = 17, no_participation = 18)


avail_c5 <- with(db_c5, list(
    mna_msqd = 1 - msqdclosure,
    sba_msqd = 1 - msqdclosure,
    mra_msqd = 1 - msqdclosure,
    laa_msqd = 1 - msqdclosure,
    npa_msqd = 1,
    sfa_msqd = 1 - msqdclosure,
    cba_msqd = 1,
    laa_psdn = 1,
    clo_psdn = 1,
    cwa_psdn = 1 - waclosure_cwa_psdn,
    clw_psdn = 1 - waclosure_clw_psdn,
    sba_cmck = 1,
    laa_cmck = 1,
    laa_nanc = 1,
    cwa_albc = 1,
    cwa_dcrb = 1,
    clw_dcrb = 1,
    no_participation = 1
  ))

tab_c5 <- choice_overview(
  db          = db_c5,
  alternatives = alts_c5,
  avail        = avail_c5,
  choiceVar    = "choice",
  digits       = 2
)

nlStructure_c5 = list()
nlStructure_c5[["root"]] = c("no_participation", "part", "part_crab")
nlStructure_c5[["part"]] = c("msqd", "cmck", "psdn", "laa_nanc", "cwa_albc" )
nlStructure_c5[["msqd"]] = c("mna_msqd", "sba_msqd", "mra_msqd", "laa_msqd", "npa_msqd", "sfa_msqd", "cba_msqd" )
nlStructure_c5[["cmck"]] = c("sba_cmck", "laa_cmck")
nlStructure_c5[["psdn"]] = c("laa_psdn", "clo_psdn", "cwa_psdn", "clw_psdn" )
nlStructure_c5[["part_crab"]] = c("cwa_dcrb", "clw_dcrb")
print_nl_tree(nlStructure_c5, root="root")

tree_tab_c5 <- nl_tree_table_3levels(nlStructure_c5, tab_c5, root="root", digits=2)
ft_c5 <- make_tree_flextable(tree_tab_c5, digits=2, title="Cluster c5 — NL tree with observed choice shares")
ft_c5

##### ---- Cluster C6 --- 

db_c6 <- assets$c6$database_wide

alts_c6 <- c(cba_psdn =1,
             clo_psdn =2,
             clw_psdn =3,
             cwa_psdn =4,
             clo_nanc =5,
             clw_nanc =6,
             cwa_nanc =7,
             clo_cmck =8,
             cwa_dcrb =9,
             nps_sock =10,
             no_participation = 11)


avail_c6 <- with(db_c6, list(
  cba_psdn         = 1 - psdnclosure,
  clo_psdn         = 1 - psdnclosure,
  clw_psdn         = (1 - psdnclosure) * (1 - waclosure_clw_psdn),
  cwa_psdn         = (1 - psdnclosure) * (1 - waclosure_cwa_psdn),
  clo_nanc         = 1,
  clw_nanc         = 1,
  cwa_nanc         = 1,
  clo_cmck         = 1,
  cwa_dcrb         = 1,
  nps_sock         = 1,
  no_participation = 1))

tab_c6 <- choice_overview(
  db          = db_c6,
  alternatives = alts_c6,
  avail        = avail_c6,
  choiceVar    = "choice",
  digits       = 2
)

nlStructure_c6= list()
nlStructure_c6[["root"]] = c("no_participation", "part", "cwa_dcrb", "nps_sock")
nlStructure_c6[["part"]] = c("nanc", "psdn", "clo_cmck")
nlStructure_c6[["nanc"]] = c("clo_nanc", "clw_nanc", "cwa_nanc")
nlStructure_c6[["psdn"]] = c("cba_psdn", "clo_psdn", "clw_psdn", "cwa_psdn")   ### Define settings for NL model
print_nl_tree(nlStructure_c6, root="root")

tree_tab_c6 <- nl_tree_table_3levels(nlStructure_c6, tab_c6, root="root", digits=2)
ft_c6 <- make_tree_flextable(tree_tab_c6, digits=2, title="Cluster c6 — NL tree with observed choice shares")
ft_c6

##### ---- Cluster c7 --- 

db_c7 <- assets$c7$database_wide

alts_c7 <- c(
  laa_cmck = 1,
  mna_cmck = 2,
  laa_jmck = 3,
  mna_jmck = 4,
  laa_msqd = 5,
  mra_msqd = 6,
  sba_msqd = 7,
  sfa_msqd = 8,
  mna_msqd = 9,
  laa_psdn = 10,
  mna_psdn = 11,
  mna_nanc = 12,
  sba_nanc = 13,
  sda_nanc = 14,
  no_participation = 15)


avail_c7 <- with(db_c7, list(
  laa_cmck         = 1,
  mna_cmck         = 1,
  laa_jmck         = 1,
  mna_jmck         = 1,
  laa_msqd         = 1 - msqdclosure,
  mra_msqd         = 1 - msqdclosure,
  sba_msqd         = 1 - msqdclosure,
  sfa_msqd         = 1 - msqdclosure,
  mna_msqd         = 1 - msqdclosure,
  laa_psdn         = 1,
  mna_psdn         = 1,
  mna_nanc         = 1,
  sba_nanc         = 1,
  sda_nanc         = 1,
  no_participation = 1))

tab_c7 <- choice_overview(
  db          = db_c7,
  alternatives = alts_c7,
  avail        = avail_c7,
  choiceVar    = "choice",
  digits       = 2
)

nlStructure_c7= list()
nlStructure_c7[["root"]] = c("no_participation", "part")
nlStructure_c7[["part"]] = c("laa", "mna", "sba", "mra_msqd", "sfa_msqd", "sda_nanc")
nlStructure_c7[["laa"]] = c("laa_cmck", "laa_jmck", "laa_msqd", "laa_psdn")
nlStructure_c7[["mna"]] = c("mna_cmck", "mna_jmck", "mna_msqd", "mna_psdn", "mna_nanc")   ### Define settings for NL model
nlStructure_c7[["sba"]] = c("sba_msqd", "sba_nanc")   ### Define settings for NL model
print_nl_tree(nlStructure_c7, root="root")

tree_tab_c7 <- nl_tree_table_3levels(nlStructure_c7, tab_c7, root="root", digits=2)
ft_c7 <- make_tree_flextable(tree_tab_c7, digits=2, title="Cluster c7 — NL tree with observed choice shares")
ft_c7 


#### ---- Get all cluster in one document ----

library(officer)

doc <- read_docx()

doc <- body_add_par(doc, "NL nesting table — Cluster c4", style="heading 1")
doc <- flextable::body_add_flextable(doc, value = ft_c4)
doc <- body_add_par(doc, "NL nesting table — Cluster c5", style="heading 1")
doc <- flextable::body_add_flextable(doc, value = ft_c5)
doc <- body_add_par(doc, "NL nesting table — Cluster c6", style="heading 1")
doc <- flextable::body_add_flextable(doc, value = ft_c6)
doc <- body_add_par(doc, "NL nesting table — Cluster c7", style="heading 1")
doc <- flextable::body_add_flextable(doc, value = ft_c7)

print(doc, target = "R/output/nl_tree_table.docx")


