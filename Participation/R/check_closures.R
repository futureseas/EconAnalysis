alts <- c("cba_psdn", "clo_psdn", "clw_psdn", "cwa_psdn", "clo_nanc", "clw_nanc", "cwa_nanc", "clo_cmck", "cwa_dcrb", "nps_sock", "no_participation")
alt_id_map <- setNames(seq_along(alts), alts)

dc_cols <- grep("^d_cd_", names(database), value = TRUE)

dc_report <- do.call(rbind, lapply(dc_cols, function(v){
  alt <- sub("^d_cd_", "", v)
  if(!(alt %in% names(alt_id_map))) return(NULL)
  alt_id <- alt_id_map[[alt]]
  
  dcd1 <- database[[v]] == 1
  chosen <- database$choice == alt_id
  
  total_dcd <- sum(dcd1, na.rm = TRUE)
  chosen_with_dcd <- sum(dcd1 & chosen, na.rm = TRUE)
  chosen_without_dcd <- sum((!dcd1) & chosen, na.rm = TRUE)
  
  data.frame(
    alt = alt,
    alt_id = alt_id,
    total_dcd = total_dcd,
    chosen_with_dcd = chosen_with_dcd,
    chosen_without_dcd = chosen_without_dcd,
    share_chosen_with_dcd = ifelse((chosen_with_dcd + chosen_without_dcd) > 0,
                                   chosen_with_dcd / (chosen_with_dcd + chosen_without_dcd),
                                   NA_real_)
  )
}))

dc_report <- dc_report[order(dc_report$chosen_with_dcd, dc_report$total_dcd, decreasing = FALSE), ]
print(dc_report)

# Las alternativas "exclusión dura": d_cd aparece pero nunca se eligen con d_cd=1
hard_exclusion <- subset(dc_report, total_dcd > 0 & chosen_with_dcd == 0)
print(hard_exclusion)
