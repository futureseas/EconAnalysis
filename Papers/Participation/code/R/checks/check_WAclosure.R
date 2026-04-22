alts <- c("mna_msqd", "sba_msqd", "mra_msqd", "laa_msqd", "npa_msqd", "sfa_msqd", "cba_msqd", "laa_psdn", "clo_psdn", "cwa_psdn", "clw_psdn", "sba_cmck", "laa_cmck", "laa_nanc", "cwa_albc", "cwa_dcrb", "clw_dcrb", "no_participation")
alt_id_map <- setNames(seq_along(alts), alts)

# Define cuáles alternativas son "PSDN-type" (ajusta si corresponde)
psdn_alts <- c("mna_msqd", "sba_msqd", "mra_msqd", "laa_msqd", "npa_msqd", "sfa_msqd", "cba_msqd")


# indicador de closure (sanitizado)
psdn1 <- database$msqdclosure == 1
psdn1[is.na(psdn1)] <- FALSE

psdn_report <- do.call(rbind, lapply(psdn_alts, function(alt){
  alt_id <- alt_id_map[[alt]]
  chosen <- database$choice == alt_id
  
  total_closure <- sum(psdn1, na.rm = TRUE)
  chosen_with_closure <- sum(psdn1 & chosen, na.rm = TRUE)
  chosen_without_closure <- sum((!psdn1) & chosen, na.rm = TRUE)
  
  data.frame(
    alt = alt,
    alt_id = alt_id,
    total_closure = total_closure,
    chosen_with_closure = chosen_with_closure,
    chosen_without_closure = chosen_without_closure,
    share_chosen_with_closure = ifelse((chosen_with_closure + chosen_without_closure) > 0,
                                       chosen_with_closure / (chosen_with_closure + chosen_without_closure),
                                       NA_real_)
  )
}))

psdn_report <- psdn_report[order(psdn_report$chosen_with_closure, psdn_report$total_closure), ]
print(psdn_report)

# exclusión dura: existe closure pero nunca se elige en closure
hard_exclusion_psdn <- subset(psdn_report, total_closure > 0 & chosen_with_closure == 0)
print(hard_exclusion_psdn)
