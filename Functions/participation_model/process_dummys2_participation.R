#' Process dummy variables
#' Function to calculate revenues and distances

#' @param xx Index value
#' @param td1 The tow dates
#' @param dat1 The Data
#' @param 
#' @export

process_dummys2 <- function(xx, td1 = td, dat1 = dat, landPSDN = estPSDN){
  

  # ### Delete
  # xx <- 1
  # td1 <- td
  # dat1 <- dat
  # landPSDN <- estPSDN
  # ###
  
  temp_dat <- td1[xx, ]

  dat1$set_date <- as_date(dat1$set_date)
  fltz <- temp_dat$fleet_name
  sel  <- temp_dat$selection
  species  <- temp_dat$species
  

  ########################################################################################################
  ### DO VESSEL HAVE FISH THE SPECIES AT THE PORT IN CONSIDERATION IN THE LAST N_DAYS (AND LAST YEAR)? ###
  ########################################################################################################
  
  #Did vessel fish in past 30 days at port for species X? 
  dum30 <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                              set_date %within% temp_dat$days_inter,
                                              VESSEL_NUM == temp_dat$fished_VESSEL_NUM,
                                              selection == sel,
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
                                               selection == sel,
                                               selection != "No-Participation",
                                               group_all %in% fltz)

  dum30y <- dum30y %>% distinct(trip_id, .keep_all = T)
  
  #Add dummy coefficient
  dum30y_val <- nrow(dum30y)
  
  
  ##############################################################################################
  ## Calculate revenues (obtained by all the fleet within XX days, excluding the actual haul) ##
  ## Include SDM for species available...                                                      ##
  ##############################################################################################
  
  
  if (sel != "No-Participation") {
    if (species == "PSDN") {
      
      #it should be species average availability last month converted to landings x actual hedonic prices by port/species.
      
      dum_rev <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                                    set_date %within% temp_dat$days_inter,
                                                    group_all %in% fltz,
                                                    selection %in% sel) %>% dplyr::mutate(Closure = 0) 
      
      prediction <- as.data.frame(predict(landPSDN, dum_rev, interval = "prediction"))
      dum_rev$Revenue <- prediction$fit * dum_rev$Price_mton
      dum_rev <- dum_rev %>% distinct(trip_id, .keep_all = T)
      
    } else {
      
      dum_rev <- dat1 %>% ungroup %>% dplyr::filter(trip_id != temp_dat$fished_haul,
                                                  set_date %within% temp_dat$days_inter,
                                                  group_all %in% fltz,
                                                  selection %in% sel)
      dum_rev <- dum_rev %>% distinct(trip_id, .keep_all = T)
    
    }
    
    #Calculate revenue
    mean_rev <- mean(dum_rev$Revenue, na.rm = TRUE)
    mean_rev <- replace(mean_rev, is.na(mean_rev), 0)
    
  } else {
    mean_rev <- 0
  }
  
  # Include new information in temp_dat
  temp_dat$mean_rev <- mean_rev
  temp_dat$dummy_prev_days <- dum30_val
  temp_dat$dummy_prev_year_days <- dum30y_val

  return(temp_dat)
}
  


