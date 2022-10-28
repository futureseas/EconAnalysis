#' Process dummy variables
#' Function to calculate revenues and distances

#' @param xx Index value
#' @param td2 The tow dates
#' @param dat1 The Data
#' @export

process_dummys2 <- function(xx, td2 = td1, dat1 = dat){
  
# 
#   ### Delete
#   xx <- 100
#   td2 <- td1
#   dat1 <- dat
#   
#   ###
  
  temp_dat <- td2[xx, ]

  
  dat1$set_date <- as_date(dat1$set_date)
  fltz <- temp_dat$fleet_name
  sel  <- temp_dat$selection
  
  ### DO VESSEL HAVE FISH IN THE LAST N_DAYS?
  
  #-----------------------------------------------------------------------------------------------
  #Did vessel fish in past 30 days? 
  dumP <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                              set_date %within% temp_dat$days_inter_part,
                                              VESSEL_NUM == temp_dat$fished_VESSEL_NUM,
                                              selection != "No-Participation",
                                              group_all %in% fltz)
  dumP <- dumP %>% distinct(trip_id, .keep_all = T)
  
  #Add dummy coefficient
  dumP_val <- nrow(dumP)


  ### DO VESSEL HAVE FISH THE SPECIES AT THE PORT IN CONSIDERATION IN THE LAST N_DAYS (AND LAST YEAR)?
  
  
  #-----------------------------------------------------------------------------------------------
  #Did vessel fish in past 30 days at port for species X? 
  dum30 <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                              set_date %within% temp_dat$days_inter,
                                              VESSEL_NUM == temp_dat$fished_VESSEL_NUM,
                                              selection == temp_dat$selection,
                                              selection != "No-Participation",
                                              group_all %in% fltz)
  dum30 <- dum30 %>% distinct(trip_id, .keep_all = T)
  
  #Add dummy coefficient
  dum30_val <- nrow(dum30)
  
#-----------------------------------------------------------------------------------------------
# Vessel fish in the past 30 days of last year at port for species X?
  dum30y <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul, 
                                               set_date %within% temp_dat$prev_year_days_inter,
                                               VESSEL_NUM == temp_dat$fished_VESSEL_NUM,
                                               selection == temp_dat$selection,
                                               selection != "No-Participation",
                                               group_all %in% fltz)
# depth_bin == temp_dat$depth_bin, 
  
dum30y <- dum30y %>% distinct(trip_id, .keep_all = T)
  
#Add dummy coefficient
dum30y_val <- nrow(dum30y)
  
  

  
#-----------------------------------------------------------------------------------------------
  ## Calculate revenues (obtained by all the fleet within 30 days, excluding the actual haul)


  if (sel != "No-Participation") {
    dum_rev <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                                  set_date %within% temp_dat$days_inter,
                                                  group_all %in% fltz,
                                                  selection %in% sel)
      dum_rev <- dum_rev %>% distinct(trip_id, .keep_all = T)

  #Calculate revenue
  mean_rev <- mean(dum_rev$Revenue)
  mean_rev <- replace(mean_rev, is.na(mean_rev), 0)
  }  else {
    mean_rev <- 0
    }
  

  # Include new information in temp_dat
  temp_dat$mean_rev <- mean_rev
  temp_dat$dummy_prev_days <- dum30_val
  temp_dat$dummy_prev_year_days <- dum30y_val
  temp_dat$dummy_prev_days_part <- dumP_val

  return(temp_dat)
}
  


