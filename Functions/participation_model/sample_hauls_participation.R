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

   #Seed uses the row index in the foreach function of sampled_rums
   set.seed(the_seed)
   
   # #Check to make sure there are enough samples of each thing to sample without replacement
   # the_samples <- lapply(1:nrow(choice_proportions), FUN = function(dd){
   #                    temp <- dist_hauls_catch_shares1 %>% dplyr::filter(trip_id != hauls1[xx, 'trip_id'],
   #                                                                       selection != as.character(choice_proportions[dd, "selection"]))
   #                    samps <- temp %>% sample_n(size = as.numeric(choice_proportions[dd, "n_samp"]), replace = F)
   #                    return(samps)
   #                })
   # the_samples <- plyr::ldply(the_samples)
   # 
   
    
    temp <- choice_proportions %>% dplyr::filter(selection != as.character(hauls1[xx, "selection"]))
    samps <- temp %>% sample_n(size = nhauls_sampled1, prob = prop, replace = F)
    
    the_samples <- plyr::ldply(samps)
    
    
    
#Now calculate the distances between the points and the actual points
actual_haul <- hauls1[xx, ]
    
 #Combine the sampled values and the empirical haul
 actual_haul$prev_haul_num <- NULL
 actual_haul$fished <- TRUE
 actual_haul$fished_haul <- actual_haul$trip_id
 the_samples$fished <- FALSE
 the_samples$fished_haul <- actual_haul$trip_id
 the_samples <- rbind(actual_haul, the_samples)

  #Define the set_date
  the_samples$set_date <- ymd(paste(actual_haul$set_year, actual_haul$set_month, actual_haul$set_day, sep = "-"))
  return(the_samples)
  }