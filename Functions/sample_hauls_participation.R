#' Function to Sample Hauls

#' @param xx Index
#' @param hauls1 hauls data frame
#' @param dist_hauls_catch_shares1 df with distance after catch shares
#' @param nhauls_sampled1 number of sampled hauls
#' @param depth_bin_proportions Proportions of tows in each depth bin
#' @param the_seed Seed for sampling tows
#' @export

  sample_hauls <- function(xx, hauls1 = hauls, dist_hauls_catch_shares1 = dist_hauls_catch_shares,
    nhauls_sampled1 = 50, choice_proportions, the_seed){
    
   #---------------------------------------------------------------
   ## Load functions
   source("C:\\GitHub\\EconAnalysis\\Functions\\deg2rad.R")
   source("C:\\GitHub\\EconAnalysis\\Functions\\gcd_slc.R")
   
   #---------------------------------------------------------------

   #Seed uses the row index in the foreach function of sampled_rums
   set.seed(the_seed)
   
   #Check to make sure there are enough samples of each thing to sample without replacement
   the_samples <- lapply(1:nrow(choice_proportions), FUN = function(dd){
                      temp <- dist_hauls_catch_shares1 %>% dplyr::filter(trip_id != hauls1[xx, 'haul_id'],
                                                                         choices == choice_proportions[dd, "selection"])
                      samps <- temp %>% sample_n(size = choice_proportions[dd, "n_samp"], replace = F)
                      return(samps)
                  })
   the_samples <- plyr::ldply(the_samples)
    
    #Now calculate the distances between the points and the actual points
    actual_haul <- hauls1[xx, ]
  
    # #calculate distances in km
    # prev_point <- actual_haul %>% select(prev_up_long, prev_up_lat)
    # the_samples$prev_up_long <- prev_point$prev_up_long
    # the_samples$prev_up_lat <- prev_point$prev_up_lat
    
    ## for the samples
    the_samples[, c('set_long', 'set_lat', 'prev_up_lat', 'prev_up_long')] <- deg2rad(the_samples[, 
      c('set_long', 'set_lat', 'prev_up_lat', 'prev_up_long')])
    the_samples$distance <- gcd_slc(the_samples$prev_up_long, 
        the_samples$prev_up_lat, the_samples$set_long, the_samples$set_lat)
  
    ## for the actual data
    actual_haul[, c('set_long', 'set_lat', 'prev_up_lat', 'prev_up_long')] <- deg2rad(actual_haul[, 
        c('set_long', 'set_lat', 'prev_up_lat', 'prev_up_long')])  
    actual_haul$distance <- gcd_slc(actual_haul$prev_up_long, 
        actual_haul$prev_up_lat, actual_haul$set_long, actual_haul$set_lat)

    #Combine the sampled values and the empirical haul
    actual_haul$prev_haul_num <- NULL
    actual_haul$fished <- TRUE
    actual_haul$fished_haul <- actual_haul$haul_id
    the_samples$fished <- FALSE
    the_samples$fished_haul <- actual_haul$haul_id
  
    the_samples <- rbind(actual_haul, the_samples)
  
    #Define the set_date
    the_samples$set_date <- ymd(paste(actual_haul$set_year, actual_haul$set_month, actual_haul$set_day, sep = "-"))
    
    return(the_samples)
  }