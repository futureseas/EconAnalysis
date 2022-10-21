
###################
### Sample choice set for the participation model 
###################

#' Format RUM data base on resampled fish tickets
#' Function calls mlogit

#' @param data_in Data going in to the function; default is filt_clusts
#' @param cluster cluster that we are analyzing 
#' @param min_year Minimum year used to filter the data
#' @param max_year Maximum year used to filter the data, also RUM data is filtered to be from the max_year
#' @param ndays Number of previous days data to use in revenue expectations
#' @param nhauls_sampled Number of hauls to sample from the full data set
#' @param seed Seed for sampling tows
#' @param ncores Number of cores to use
#' @param rev_scale Scale the revenue by this factor
#' @param min_year_prob Minimum year used to compute sample probabilities
#' @param max_year_prob Maximum year used to compute sample probabilities

#'
#' @export

sampled_rums <- function(data_in, cluster = 1,
                         min_year_prob = 2010, max_year_prob = 2012,
                         min_year = 2010, max_year = 2012,  
                         ndays = 60, nhauls_sampled = 50, seed = 300, 
                         ncores, rev_scale) {

#Start by sampling 50 tows within the same fleet
#Figure out how close the different clusters are
  
  
  #---------------------------------------------------------------
  ##Filter the data
  dat <- participation_data.save %>% dplyr::filter(set_year >= min_year, set_year <= max_year,
                                   group_all %in% cluster)
  
  #########################  

  
  hauls <- dat %>% 
    distinct(trip_id, .keep_all = T) %>% 
    dplyr::select(trip_id, set_month, VESSEL_NUM, set_day, set_year, Revenue, selection) %>% 
    as.data.frame

  dist_hauls_catch_shares <- hauls %>% dplyr::filter(set_year >= min_year_prob, set_year <= max_year_prob)
  

  # #For each tow in the focus year, sample other tows
  # #Hauls in focus year
  # hauls <- dist_hauls %>% dplyr::filter(set_year == focus_year) %>% arrange(trip_id)
  
  
  
  #---------------------------------------------------------------
  # Create probabilities for sampling choice sets

  
  # Create dataframes
  dbp <- dist_hauls_catch_shares %>% 
    group_by(selection, VESSEL_NUM, set_year, set_month) %>% 
    summarize(sum_rev = sum(Revenue, na.rm = TRUE)) %>%
    group_by(selection, VESSEL_NUM, set_month) %>%
    summarize(mean_rev = mean(sum_rev, na.rm = TRUE)) %>%
    group_by(VESSEL_NUM, set_month) %>%
    mutate(tot_rev = sum(mean_rev, na.rm = TRUE)) %>% ungroup() %>%
    mutate(catch_composition = mean_rev/tot_rev)
    
  full_choice_set <- dist_hauls_catch_shares %>%
    dplyr::select(selection) %>% unique() %>% mutate(merge=1)
  
  all_vessels <- dist_hauls_catch_shares %>% 
    dplyr::select(VESSEL_NUM) %>% unique() %>% mutate(merge=1)
  
  all_month <- dist_hauls_catch_shares %>% 
    dplyr::select(set_month) %>% unique() %>% mutate(merge=1)

  expand <- merge(full_choice_set, all_vessels, 
                  by = c('merge'), all.x = TRUE, all.y = TRUE)
  
  expand <- merge(expand, all_month, 
                  by = c('merge'), all.x = TRUE, all.y = TRUE)
  
  dbp <- merge(expand, dbp, by = c('VESSEL_NUM', 'selection', 'set_month'), all.x = TRUE) %>%
    mutate(catch_composition = ifelse(is.na(catch_composition),0,catch_composition))

  dbp <- dbp %>%
    group_by(selection, set_month) %>%
    summarize(prop = mean(catch_composition))

  
  ### Create factor by month and graph composition
  


cl <- makeCluster(ncores)
registerDoParallel(cl)


dbp3 <- foreach::foreach(ll = 1:12, .packages = c("dplyr", 'plyr', 'lubridate')) %dopar% {
    dbp2 <- dbp %>% dplyr::filter(set_month == ll)  %>%  
      mutate(prop = ifelse(prop == 0,0.001,prop))
      factor <- 1/sum(dbp2$prop)
      
    dbp2 <- dbp2 %>% 
      mutate(prop = prop * factor)
    
    sum(dbp2$prop)
    return(dbp2)
}

dbp_month <- as.data.frame(do.call(rbind.data.frame, dbp3))


# dbp_month <- dbp_month %>% filter(prop > 0)
# ggplot(dbp_month, aes(x=selection, y=prop, fill = selection))+
#   geom_bar(stat='identity')+
#   facet_grid(~set_month,scales="free", space="free_x") +
#   theme(axis.text.x = element_text(size=7, angle=90))

  
dbp_month <- dbp_month[dbp_month$selection != "No-Participation", ]
    
  

  # # dbp <- as.data.frame(dbp)
  # 
  # #Add number of values to sample
  # dbp$n_samp <- dbp$prop * nhauls_sampled
  # 
  # #Round the values to integers
  # dbp$n_samp <- round(dbp$n_samp)
  # 
  # #Top off the highest value
  # max_dbp <- which(dbp$prop == max(dbp$prop))
  # dbp[max_dbp, 'n_samp'] <- dbp[max_dbp, 'n_samp'] + (nhauls_sampled - sum(round(dbp$n_samp)))

  #-----------------------------------------------------------------------------
  #Sample Hauls
  #Set seed
  set.seed(seed)
  seedz <- sample(1:1e7, size = nrow(hauls))

  #Sample hauls and calculate distances
  #For each haul in the focus year, sample nhauls_sampled tows

  
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  source("C:\\GitHub\\EconAnalysis\\Functions\\sample_hauls_participation.R")
  
  sampled_hauls <- foreach::foreach(ii = 1:nrow(hauls),
                                    .export = c("sample_hauls"),
                                    .packages = c("dplyr", 'plyr', 'lubridate')) %dopar% {
                                      set.seed(seedz[ii])
                                 
                                     if (hauls[ii, "selection"] != "No-Participation") {
                                        temp <- dbp_month  %>% dplyr::filter(selection != as.character(hauls[ii, "selection"]),
                                                                      set_month == as.character(hauls[ii, "set_month"])) 
                                        samps <- temp %>% sample_n(size = (nhauls_sampled - 1), weight = prop, replace = F)
                                        the_samples <- as.data.frame(samps[ , "selection"]) 
                                        colnames(the_samples)[1] <- "selection"
                                        the_samples <- the_samples %>%
                                          add_row(selection = "No-Participation")
                                      }
                                      
                                      else {
                                        temp <- dbp_month %>% dplyr::filter(selection != as.character(hauls[ii, "selection"]),
                                                                      set_month == as.character(hauls[ii, "set_month"]))
                                        samps <- temp %>% sample_n(size = nhauls_sampled, weight = prop, replace = F)
                                        the_samples <- as.data.frame(samps[ , "selection"]) 
                                        colnames(the_samples)[1] <- "selection"
                                      }
                                      
                                      actual_haul <- as.data.frame(hauls[ii, "selection"])
                                      colnames(actual_haul)[1] <- "selection"
                                      
                                      #Combine the sampled values and the empirical haul
                                      actual_haul$fished <- TRUE
                                      actual_haul$fished_haul <- hauls[ii, "trip_id"]
                                      actual_haul$fished_VESSEL_NUM <- hauls[ii, "VESSEL_NUM"]
                                      the_samples$fished <- FALSE
                                      the_samples$fished_haul <- hauls[ii, "trip_id"]
                                      the_samples$fished_VESSEL_NUM <- hauls[ii, "VESSEL_NUM"]
                                      the_samples <- rbind(actual_haul, the_samples)
                                     
                                      #Define the set_date
                                      the_samples$set_date <- ymd(paste(hauls[ii, "set_year"], hauls[ii, "set_month"], hauls[ii, "set_day"], sep = "-"))
                                      return(the_samples)
                                      rm(actual_haul, the_samples, temp, samps)
                                    }
  print("Done sampling hauls")
  sampled_hauls <- plyr::ldply(sampled_hauls)
  
  #Obtain previous day, year and day/year date
  sampled_hauls$prev_days_date <- sampled_hauls$set_date - days(ndays)
  sampled_hauls$prev_year_set_date <- sampled_hauls$set_date - days(365)
  sampled_hauls$prev_year_days_date <- sampled_hauls$prev_days_date - days(365)


  #-----------------------------------------------------------------------------
  ### Calculate interval between previews day and year
  
  #What were the average revenues in each location
  tow_dates <- sampled_hauls %>%
    dplyr::select(fished_haul, set_date, prev_days_date, prev_year_set_date, prev_year_days_date,
                  fished_VESSEL_NUM, selection)

  #calculate intervals
  tow_dates$days_inter <- interval(tow_dates$prev_days_date, tow_dates$set_date)
  tow_dates$prev_year_days_inter <- interval(tow_dates$prev_year_days_date, tow_dates$prev_year_set_date)

  #add in the fleet name
  tow_dates$fleet_name <- 1
  td1 <- tow_dates


  #-----------------------------------------------------------------------------
  ### Calculate revenues from each period and process dummy variables

  dummys2 <- foreach::foreach(ii = 1:nrow(td1),
    .packages = c("dplyr", 'lubridate')) %dopar% {
      source("C:\\GitHub\\EconAnalysis\\Functions\\process_dummys2_participation.R")
      process_dummys2(xx = ii, td2 = td1, dat1 = dat)
    }
  print("Done calculating dummys and revenues")
  td1 <- ldply(dummys2)
  # td_select <- td1 %>% select(c(fished_haul, selection, mean_rev))
  
  stopCluster(cl)
  

  #-----------------------------------------------------------------------------
  ## Create additional dummys
  
  #Convert set_date to character string to merge with the sampled_hauls
  tow_dates$set_date_chr <- as.character(tow_dates$set_date)
  td1$set_date_chr <- as.character(td1$set_date)
  sampled_hauls$set_date_chr <- as.character(sampled_hauls$set_date)

  #create dummy for prev days fishing
  td1[which(td1$dummy_prev_days != 0), 'dummy_prev_days'] <- 1
  td1[which(td1$dummy_prev_year_days != 0), 'dummy_prev_year_days'] <- 1

  td1[which(td1$mean_rev != 0), 'dummy_miss'] <- 0
  td1[which(td1$mean_rev == 0), 'dummy_miss'] <- 1
  td1[which(td1$selection == "No-Participation"), 'dummy_miss'] <- 0

  td1$mean_rev_adj <- td1$mean_rev / rev_scale

  sampled_hauls <- cbind(sampled_hauls,
    td1[, c('dummy_prev_days', 'dummy_prev_year_days', "dummy_miss", 'mean_rev', 'mean_rev_adj')] )
  
  
  return(sampled_hauls)
}
  
