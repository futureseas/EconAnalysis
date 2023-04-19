
###################
### Sample choice set for the participation model 
###################

#' Format RUM data base on resampled fish tickets
#' Function calls mlogit

#' @param data_in Data going in to the function; default is filt_clusts
#' @param cluster cluster that we are analyzing 
#' @param min_year Minimum year used to filter the data
#' @param max_year Maximum year used to filter the data, also RUM data is filtered to be from the max_year
#' @param min_year_prob Minimum year used to compute sample probabilities
#' @param max_year_prob Maximum year used to compute sample probabilities
#' @param ndays Number of previous days data to use in revenue expectations
#' @param nhauls_sampled Number of hauls -- trips -- to sample from the full data set, additional to the selection
#' @param seed Seed for sampling tows
#' @param ncores Number of cores to use
#' @param rev_scale Scale the revenue by this factor

#' @export

sampled_rums <- function(data_in, cluster = 4,
                         min_year = 2010, max_year = 2020,
                         min_year_prob = 2012, max_year_prob = 2018,
                         ndays = 30, 
                         nhauls_sampled = 5, seed = 300, 
                         ncores, rev_scale) {

  ###############
  # Delete
  library(doParallel)
  library(tidyr)
  library(plm)
  library(tidyverse)
  library(lubridate)
  data_in <- readRDS("C:\\Data\\PacFIN data\\participation_data.rds")
  cluster <- 4
  min_year_prob <- 2012
  max_year_prob <- 2018
  min_year <- 2010
  max_year <- 2019
  ndays <- 30
  nhauls_sampled <- 5
  seed <- 300
  ncores <- 4
  rev_scale <- 100

  ###############
  
  #---------------------------------------------------------------
  ## Filter the data

  dat <- data_in 
  
  
  #-----------------------------------------------------------------------------
  ## Estimate models for landings
  datPanel <- dat %>% 
    dplyr::filter(selection != "No-Participation") %>% 
    dplyr::filter(set_year >= min_year, set_year <= max_year)

  ### Pacific sardine landing model ###
  datPanel_PSDN <- datPanel %>% filter(Species_Dominant == "PSDN") %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2008-05-29" & set_date < "2008-07-01", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2008-08-08" & set_date < "2008-09-01", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2008-09-23" & set_date < "2009-01-01", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2009-02-20" & set_date < "2009-07-01", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2009-07-18" & set_date < "2009-09-01", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2009-09-23" & set_date < "2010-01-01", 1, 0)) %>% 
    dplyr::mutate(Closure = ifelse(set_date >= "2010-06-12" & set_date < "2010-07-01", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2010-07-22" & set_date < "2010-09-01", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2010-09-24" & set_date < "2011-01-01", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2011-03-05" & set_date < "2011-07-01", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2011-07-12" & set_date < "2011-09-01", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2011-09-21" & set_date < "2012-01-01", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2012-08-23" & set_date < "2012-09-01", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2013-08-22" & set_date < "2013-09-01", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2015-04-28", 1, 0))

  qPSDN <- lm(Landings_mtons ~ lag_PSDN_SDM_90 + factor(VESSEL_NUM) + factor(set_month) + 
                factor(set_year) + factor(Closure), data = datPanel_PSDN)
  
 
  ### Market squid landing model ### (Maybe use lagged prices? ADD WEEKEND!)
  datPanel_MSQD<- datPanel %>% filter(Species_Dominant == "MSQD") %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2010-12-17" & set_date < "2011-03-31", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2011-11-18" & set_date < "2012-03-31", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2012-11-21" & set_date < "2013-03-31", 1, 0))
  
  qMSQD <- lm(Landings_mtons ~ lag_MSQD_SDM_90 + factor(VESSEL_NUM) + factor(set_month) + 
                factor(set_year) + factor(Closure), data = datPanel_MSQD)
  
  summary(qMSQD)

  
  ## Create table for paper (all species)
  # summary(qPSDN)
  # summary(qMSQD)
  # qPSDN <- estimatr::lm_robust(Landings_mtons ~ lag_PSDN_SDM_90 +
  #                                factor(VESSEL_NUM) + factor(set_month) + factor(set_year) + factor(Closure),
  #                              data = datPanel_PSDN, clusters = group_all, se_type = "stata")
  # modelsummary::modelsummary(qPSDN, output = "landings_models.docx")
  
  
  #--------------------------------
  ## Define hauls data used for estimation (in this case, are the trips)
  
  hauls <- dat %>% dplyr::filter(set_year >= min_year, set_year <= max_year,
                                 group_all %in% cluster) %>% 
    distinct(trip_id, .keep_all = T) %>% 
    dplyr::select(trip_id, VESSEL_NUM,  set_year, set_month, set_day, Revenue, selection,
                  PSDN_SDM_30, PSDN_SDM_60, PSDN_SDM_90, PSDN_SDM_220) %>% as.data.frame

  ## Select hauls used to calculate probability for the choice set
  dist_hauls_catch_shares <- hauls %>% dplyr::filter(set_year >= min_year_prob, set_year <= max_year_prob)
    dist_hauls_catch_shares <- dist_hauls_catch_shares[dist_hauls_catch_shares$selection != "No-Participation", ]
  
  #---------------------------------------------------------------
  ## Create probabilities for sampling choice set
  ## For this, we compute the average vessel catch and port composition by month
  ## (The choice set varies depending on the month of the year)

  # nrow(dist_hauls_catch_shares %>% select("selection") %>% unique())

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
      mutate(prop = ifelse(prop == 0,0.00001,prop))
      factor <- 1/sum(dbp2$prop)
    dbp2 <- dbp2 %>% 
      mutate(prop = prop * factor)
    sum(dbp2$prop)
    return(dbp2)
  }

  dbp_month <- as.data.frame(do.call(rbind.data.frame, dbp3))
  
  # dbp_test <- dbp_month %>% group_by(set_month) %>%
  #   summarize(sum_prop = sum(prop)) 
  
  # ## Create example plot with the proportion used to sample the choice set.
  # 
  # dbp_month_plot <- dbp_month %>%
  #   filter(prop > 0.01) %>% 
  #   filter(set_month <= 4) %>%
  #   mutate(selection = as.factor(selection)) %>%
  #   mutate(selection = fct_relevel(selection, "LAA-MSQD", "SBA-MSQD", "MRA-MSQD", "MNA-MSQD", 
  #                                  "LAA-PSDN", "SBA-PSDN", "MNA-PSDN", 
  #                                  "LAA-NANC", "MNA-NANC", 
  #                                  "LAA-CMCK", "LAA-PBNT", "SFA-DCRB"))
  # 
  # months <- as_labeller(c("1" = "January",
  #                         "2" = "February",
  #                         "3" = "March",
  #                         "4" = "Aprill",
  #                         "5" = "May",
  #                         "6" = "June",
  #                         "7" = "July",
  #                         "8" = "August",
  #                         "9" = "September",
  #                         "10" = "October",
  #                         "11" = "November",
  #                         "12" = "December"))
  # 
  # library(viridis)
  # ggplot(dbp_month_plot, aes(x=selection, y=prop, fill = selection)) +
  #   geom_bar(stat='identity')+
  #   facet_grid(~set_month,scales="free", space="free_x", labeller = months) + ylab("Proportion") +
  #   theme(
  #         axis.title.x=element_blank(),
  #         axis.text.x=element_blank(),
  #         axis.ticks.x=element_blank(),
  #         legend.position="bottom"
  #   #    ,axis.text.x = element_text(size=7, angle=90)
  #   ) +
  #   scale_fill_manual(labels=c("LAA-CMCK" = "Chub mackerrel - Los Angeles",
  #                              "LAA-MSQD" = "Market squid - Los Angeles",
  #                              "LAA-NANC" = "Northern anchovy - Los Angeles",
  #                              "LAA-PBNT" = "Pacific Bonito - Los Angeles",
  #                              "LAA-PSDN" = "Pacific sardine - Los Angeles",
  #                              "MNA-MSQD" = "Market squid - Monterey",
  #                              "MNA-NANC" = "Northern anchovy - Monterey",
  #                              "MNA-PSDN" = "Pacific sardine - Monterey",
  #                              "MRA-MSQD" = "Market squid - Morro Bay",
  #                              "SBA-MSQD" = "Market squid - Santa Barbara",
  #                              "SBA-PSDN" = "Pacific sardine - Santa Barbara",
  #                              "SFA-DCRB" = "Dungeness Crab - San Francisco"),
  #                     values=c("#08519c", "#049cdb", "#6baed6","#2171b5", "#85db5b", "#4aa72f", "#248b37",
  #                              "#e1dc0d", "#f0eb00", "#d94701", "#6a51a3", "#969696")) +
  #   guides(fill=guide_legend(title="Species / Port areas: "))
  # 

  #-----------------------------------------------------------------------------
  ## Sample Hauls
  
  #Set seed
  set.seed(seed)
  seedz <- sample(1:1e7, size = nrow(hauls))

  #Sample hauls and calculate distances
  #For each haul in the focus year, sample nhauls_sampled tows

  sampled_hauls <- foreach::foreach(ii = 1:nrow(hauls),
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
                                        } else {
                                        temp <- dbp_month %>% dplyr::filter(selection != as.character(hauls[ii, "selection"]),
                                                                      set_month == as.character(hauls[ii, "set_month"]))
                                        samps <- temp %>% sample_n(size = nhauls_sampled, weight = prop, replace = F)
                                        the_samples <- as.data.frame(samps[ , "selection"]) 
                                        colnames(the_samples)[1] <- "selection"
                                        }
                                      
                                      actual_haul <- as.data.frame(hauls[ii, "selection"])
                                      colnames(actual_haul)[1] <- "selection"
                                      
                                      #Combine the sampled values and the empirical haul
                                      actual_haul$fished <- 1
                                      actual_haul$fished_haul <- hauls[ii, "trip_id"]
                                      actual_haul$fished_VESSEL_NUM <- hauls[ii, "VESSEL_NUM"]
                                      the_samples$fished <- 0
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
  sampled_hauls$prev_day_date <- sampled_hauls$set_date - days(1)
  sampled_hauls$prev_year_set_date <- sampled_hauls$set_date - days(365)
  sampled_hauls$prev_year_days_date <- sampled_hauls$prev_days_date - days(365)

  #-----------------------------------------------------------------------------
  ### Calculate interval between previous day and year
  
  td <- sampled_hauls %>%
    dplyr::select(fished_haul, set_date, prev_days_date, prev_year_set_date, prev_year_days_date, prev_day_date,
                  fished_VESSEL_NUM, selection)

  td$days_inter <- interval(td$prev_days_date, td$prev_day_date)
  td$prev_year_days_inter <- interval(td$prev_year_days_date, td$prev_year_set_date)

  #add in the fleet name
  td$fleet_name <- cluster
  
  #add species 
  td <- td %>% mutate(species = ifelse(selection == "No-Participation", NA, str_sub(td$selection, start= -4)))
  
  #add port 
  td <- td %>% mutate(ports = ifelse(selection == "No-Participation", NA, str_sub(td$selection, end= 3)))


  #-----------------------------------------------------------------------------
  ### Calculate revenues from each period and process dummy variables for past behavior
  
  psdn.sdm <- read.csv(file = 'Participation/SDM_code/sdm_psdn.csv')
  psdn.sdm[is.na(psdn.sdm)] <- 0
  psdn.sdm$set_date <- ymd(paste(psdn.sdm$LANDING_YEAR, psdn.sdm$LANDING_MONTH, psdn.sdm$LANDING_DAY, sep = "-"))
  
  
  dummys2 <- foreach::foreach(ii = 1:nrow(td),
    .packages = c("dplyr", 'lubridate')) %dopar% {
      source("C:\\GitHub\\EconAnalysis\\Functions\\participation_model\\process_dummys2_participation.R")
      process_dummys2(xx = ii, td1 = td, dat1 = dat, qPSDN1 = qPSDN, SDM.PSDN = psdn.sdm)
    }
  print("Done calculating dummys and revenues")
  td2 <- plyr::ldply(dummys2)
  stopCluster(cl)
  

  #-----------------------------------------------------------------------------
  ## Create additional dummys
  
  # Create dummy for prev days fishing
  td2[which(td2$dummy_prev_days != 0), 'dummy_prev_days'] <- 1
  td2[which(td2$dummy_prev_year_days != 0), 'dummy_prev_year_days'] <- 1
  td2[which(td2$dummy_last_day != 0), 'dummy_last_day'] <- 1
  td2[which(td2$mean_rev != 0), 'dummy_miss'] <- 0
  td2[which(td2$mean_rev == 0), 'dummy_miss'] <- 1
  
  # Change dummies to zero for "No-Participation"
  td2[which(td2$selection == "No-Participation"), 'dummy_miss'] <- 0
  td2[which(td2$selection == "No-Participation"), 'dummy_prev_days'] <- 0
  td2[which(td2$selection == "No-Participation"), 'dummy_prev_year_days'] <- 0
  td2[which(td2$selection == "No-Participation"), 'dummy_last_day'] <- 0
  
  td2$mean_rev_adj <- td2$mean_rev / rev_scale

  sampled_hauls <- cbind(sampled_hauls,
    td2[, c('dummy_prev_days', 'dummy_prev_year_days', "dummy_last_day", "dummy_miss", 'mean_rev', 'mean_rev_adj')] )
  
  return(sampled_hauls)
}
  
