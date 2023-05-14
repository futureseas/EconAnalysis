
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
#' @param min_year_est Minimum year used to estimate expected landings
#' @param max_year_est Maximum year used to estimate expected landings
#' @param ndays Number of previous days data to use in revenue expectations -- also for prices and prev behavior dummies
#' @param nhauls_sampled Number of hauls -- trips -- to sample from the full data set, additional to the selection
#' @param seed Seed for sampling tows
#' @param ncores Number of cores to use
#' @param rev_scale Scale the revenue by this factor

#' @export

sampled_rums <- function(data_in, cluster = 4,
                         min_year = 2013, max_year = 2018,
                         min_year_prob = 2013, max_year_prob = 2018,
                         min_year_est = 2005, max_year_est = 2020,
                         ndays = 30, 
                         nhauls_sampled = 5, seed = 300, 
                         ncores, rev_scale) {

  ###############
  # Delete
  gc()
  library(doParallel)
  library(tidyr)
  library(plm)
  library(tidyverse)
  library(lubridate)
  data_in <- readRDS("C:\\Data\\PacFIN data\\participation_data.rds") %>%
    mutate(Vessel.length = as.numeric(Vessel.length),
           Vessel.weight = as.numeric(Vessel.weight),
           Vessel.horsepower = as.numeric(Vessel.horsepower))
  cluster <- 4
  min_year_prob <- 2012
  max_year_prob <- 2017
  min_year_est <- 2012
  max_year_est <- 2019
  min_year <- 2012
  max_year <- 2017
  ndays <- 30
  nhauls_sampled <- 5
  seed <- 300
  ncores <- 4
  rev_scale <- 100
  ################
  
  dat <- data_in 
  
  #-----------------------------------------------------------------------------
  ## Filter the data for estimations
  
  datPanel <- dat %>% 
    dplyr::filter(selection != "No-Participation") %>% 
    dplyr::filter(set_year >= min_year_est, set_year <= max_year_est) %>%
    dplyr::mutate(ln_Landings_mtons = log(Landings_mtons)) %>% 
    dplyr::mutate(ln_Price_mtons = log(Price_mtons)) 
  
  #-----------------------------------------------------------------------------
  ## Calculate landing's center of gravity each vessel and link to closest port
  
  library(raster)    
  Ticket_Coords<-aggregate(Revenue ~ VESSEL_NUM + PORT_AREA_CODE + lat_port + lon_port, 
                           data=datPanel, FUN=sum)
  Permit_ID<-as.data.frame(unique(Ticket_Coords$VESSEL_NUM))
  names(Permit_ID)<-"VESSEL_NUM"
  List <- as.list(as.character(Permit_ID$VESSEL_NUM))
  Permit_COG<-NULL
  
  ### Run function 
  source("C:/GitHub/EconAnalysis/Clustering/CGI_Function.R")
  for (i in 1:length(List)) {
    Permit = List[i]
    Single_Permit<- Ticket_Coords[which(Ticket_Coords$VESSEL_NUM==Permit),]
    Single_COG<-cgi(x=Single_Permit$lon_port, y=Single_Permit$lat_port, z=Single_Permit$Revenue, plot=F)
    Single_COG <- data.frame(
      lon = c(Single_COG$xaxe1[1], Single_COG$xaxe1[2], Single_COG$xaxe2[1], Single_COG$xaxe2[2], Single_COG$xcg),
      lat = c(Single_COG$yaxe1[1], Single_COG$yaxe1[2], Single_COG$yaxe2[1], Single_COG$yaxe2[2], Single_COG$ycg),
      group = c("A", "A", "B", "B","C"))
    Point_Coord<-Single_COG[which(Single_COG$group=="C"),]
    Line_Coord_A<-Single_COG[which(Single_COG$group=="A"),]
    Line_Coord_B<-Single_COG[which(Single_COG$group=="B"),]
    Distance_A<-pointDistance(c(Line_Coord_A[1,1], Line_Coord_A[1,2]), c(Line_Coord_A[2,1],  Line_Coord_A[2,2]), lonlat=TRUE)
    Distance_B<-pointDistance(c(Line_Coord_B[1,1], Line_Coord_B[1,2]), c(Line_Coord_B[2,1],  Line_Coord_B[2,2]), lonlat=TRUE)
    Value<-as.data.frame(c(Permit, Point_Coord$lon, Point_Coord$lat, Distance_A, Distance_B))
    names(Value)<-c("uniqueid", "lon_cg", "lat_cg", "DISTANCE_A", "DISTANCE_B")
    Permit_COG<-rbind(Permit_COG, Value)
  }
  
  ###Any vessel with NaN Values only landed at 1 port, so we change those values to 0
  Permit_COG$DISTANCE_A <- sub(NaN, 0, Permit_COG$DISTANCE_A)
  Permit_COG$DISTANCE_B <- sub(NaN, 0, Permit_COG$DISTANCE_B)
  Permit_COG$DISTANCE_A<-as.numeric(Permit_COG$DISTANCE_A)
  Permit_COG$DISTANCE_B<-as.numeric(Permit_COG$DISTANCE_B)
  
  ###Produce dissimilarity matrix and pairwise comparisons following methods above
  Permit_COG<-Permit_COG[c(1,2,3)]
  names(Permit_COG)[1]<-"VESSEL_NUM"
  rm(Ticket_Coords, List, Permit_ID, Distance_A, Distance_B,
     Point_Coord, Single_Permit, Single_COG, i, Permit, Value, Line_Coord_A, Line_Coord_B)
  
  ### Obtain closest port?
  
  
  #-----------------------------------------------------------------------------
  ## Estimate models for landings and prices 
  
  model_price <- lm(ln_Price_mtons ~ factor(Species_Dominant) + 
                      factor(PORT_AREA_CODE) + 
                      factor(set_month) + 
                      poly(set_year, 3) + 
                      Vessel.length + 
                      Vessel.horsepower, 
                    data = datPanel)
  
  # mod_estimate <- list() 
  # for(ii in min_year_est:max_year_est) {
  #   datPanel_X <- datPanel %>% filter(set_year == ii)
  #   mod_estimate[[ii]] <- lm(Price_mtons ~ factor(Species_Dominant) + factor(PORT_AREA_CODE) + 
  #                                  factor(set_month) + Vessel.length, data = datPanel_X)
  # }
  
  #-----------------------------------------------------------------------------
  ## Estimate models for landings

  model_landings <- lm(ln_Landings_mtons ~ factor(Species_Dominant) +
                           factor(PORT_AREA_CODE) +
                           poly(set_year, 3) +
                           factor(set_month) +
                           Vessel.length +
                           Vessel.horsepower,
                         data = datPanel)
  
  
  # ---------------------------------------------------------------------------- 
  #   models <- list(
  #   "ln(Landings)"  = model_landings,
  #   "ln(Prices)"     = model_price)
  # 
  # gm <- modelsummary::gof_map
  # options(OutDec=".")
  # modelsummary::modelsummary(models, fmt = 2,
  #                            gof_map = c("nobs", "adj.r.squared"),
  #                            statistic = "({std.error}){stars}",
  #                            output = "general_landings_price_models.docx")
  
  
  
  #-----------------------------------------------------------------------------
  ## Estimate models for landings using SDM (conditional vessel have decide to participate)

  ################
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
    dplyr::mutate(Closure = ifelse(set_date >= "2015-04-28", 1, 0)) %>% filter(Closure == 0) %>%
    dplyr::mutate(ln_Landings_mtons = log(Landings_mtons))

  qPSDN <- lm(ln_Landings_mtons ~ lag_PSDN_SDM_60 + factor(set_month) + poly(set_year, 3) + Vessel.length + Vessel.horsepower, data = datPanel_PSDN)
  summary(qPSDN)

  ##############
  ### Market squid landing model ### (Maybe use lagged prices? ADD WEEKEND!)
  datPanel_MSQD <- datPanel %>% filter(Species_Dominant == "MSQD") %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2010-12-17" & set_date < "2011-03-31", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2011-11-18" & set_date < "2012-03-31", 1, 0)) %>%
    dplyr::mutate(Closure = ifelse(set_date >= "2012-11-21" & set_date < "2013-03-31", 1, 0)) %>% 
    dplyr::mutate(weekend = ifelse(chron::is.weekend(set_date), 1, 0)) %>%
    filter(weekend == 0) %>%  filter(Closure == 0) %>%
    dplyr::mutate(ln_Landings_mtons = log(Landings_mtons))
  qMSQD <- lm(ln_Landings_mtons ~ lag_MSQD_SDM_90 + factor(set_month) + poly(set_year, 3) + Vessel.length + Vessel.horsepower, data = datPanel_MSQD)
  summary(qMSQD)

  ##############
  ## Pacific herring landing model
  datPanel_PHRG <- datPanel %>% filter(Species_Dominant == "PHRG") %>%
    dplyr::mutate(ln_Landings_mtons = log(Landings_mtons))
  qPHRG <- lm(ln_Landings_mtons ~ lag_PHRG_SDM_220 + factor(set_month) + poly(set_year, 3) + Vessel.length + Vessel.horsepower, data = datPanel_PHRG)
  summary(qPHRG)

  ##############
  ### Northern anchovy
  datPanel_NANC <- datPanel %>% filter(Species_Dominant == "NANC") %>%
    dplyr::mutate(ln_Landings_mtons = log(Landings_mtons))  
  qNANC <- lm(ln_Landings_mtons ~ lag_NANC_SDM_220 + factor(set_month) + poly(set_year, 3) + Vessel.length + Vessel.horsepower, data = datPanel_NANC)
  summary(qNANC)
  
  # ##############
  # ### Jack Mackerel
  # datPanel_JMCK <- datPanel %>% filter(Species_Dominant == "JMCK") %>%
  # dplyr::mutate(ln_Landings_mtons = log(Landings_mtons)) 
  # qJMCK <- lm(Landings_mtons ~ lag_JMCK_SDM_220 + factor(set_month) + poly(set_year, 3) + Vessel.length + Vessel.horsepower, data = datPanel_JMCK)
  # summary(qJMCK)
  #
  # ##############
  # ### Chub mackerel
  # datPanel_CMCK <- datPanel %>% filter(Species_Dominant == "CMCK") %>%
  # dplyr::mutate(ln_Landings_mtons = log(Landings_mtons)) 
  # qCMCK <- lm(Landings_mtons ~ lag_CMCK_SDM_30 + factor(set_month) + poly(set_year, 3) + Vessel.length + Vessel.horsepower, data = datPanel_CMCK)
  # summary(qCMCK)

  # datPanelcor <- datPanel %>% dplyr::select(c(VESSEL_NUM, Vessel.horsepower, Vessel.length)) %>% drop_na() %>% unique()
  # cor(datPanelcor$Vessel.horsepower, datPanelcor$Vessel.length)
  
  
  # ########################################
  # ## Create table for paper (all species)
  # 
  #   models <- list(
  #   "Pacific sardine"  = qPSDN,
  #   "Market squid"     = qMSQD,
  #   "Northern anchovy" = qNANC,
  #   "Pacific herring"  = qPHRG)
  # 
  # gm <- modelsummary::gof_map
  # options(OutDec=".")
  # modelsummary::modelsummary(models, fmt = 2,
  #                            # coef_omit = "^(?!.*tercept|.*SDM)",
  #                            gof_map = c("nobs", "adj.r.squared"),
  #                            statistic = "({std.error}){stars}",
  #                            coef_rename = c("lag_PSDN_SDM" = "SDM^PSDN_t-1 (<60km)",
  #                                            "lag_MSQD_SDM" = "SDM^MSQD_t-1  (<90km)",
  #                                            "lag_NANC_SDM" = "SDM^NANC_t-1  (<20km)",
  #                                            "lag_PHRG_SDM" = "SDM^PHRG_t-1  (<220km)"),
  #                            output = "landings_models.docx")

  
  #--------------------------------
  ## Define hauls data used for estimation (in this case, are the trips)
  
  hauls <- dat %>% dplyr::filter(set_year >= min_year, set_year <= max_year,
                                 group_all %in% cluster) %>% 
    distinct(trip_id, .keep_all = T) %>% 
    dplyr::select(trip_id, VESSEL_NUM,  set_year, set_month, set_day, Revenue, selection,
                  lag_NANC_SDM_220, lag_PSDN_SDM_60, lag_MSQD_SDM_90, lag_PHRG_SDM_220,
                  NANC_SDM_220, PSDN_SDM_60, MSQD_SDM_90, PHRG_SDM_220) %>% as.data.frame

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
  #                     values=c("#08519c", "#049cdb", "#6baed6","#2171b5", "#85db5b", "#4aa72f",
  #                              "#248b37", "#e1dc0d", "#f0eb00", "#d94701", "#6a51a3", "#969696")) +
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
  
  # add port and species 
  sampled_hauls <- sampled_hauls %>% mutate(species = ifelse(selection == "No-Participation", NA, str_sub(td$selection, start= -4)))
  sampled_hauls <- sampled_hauls %>% mutate(PORT_AREA_CODE = ifelse(selection == "No-Participation", NA, str_sub(td$selection, end= 3)))
  
  
  #-----------------------------------------------
  ### Merge coordinates of port landed and center of gravity with participation data,
  ### Calculate distances and multiply by fuel prices
  port_coord <- read.csv("C:/GitHub/EconAnalysis/Data/Ports/port_areas.csv")
  port_coord <- port_coord[c(-2)]
  
  <<CONTINUE HERE!>>

  
  #-----------------------------------------------------------------------------
  ### Calculate interval between previous day and year
  
  #Obtain previous day, year and day/year date
  sampled_hauls$prev_days_date <- sampled_hauls$set_date - days(ndays)
  sampled_hauls$prev_day_date <- sampled_hauls$set_date - days(1)
  sampled_hauls$prev_year_set_date <- sampled_hauls$set_date - days(365)
  sampled_hauls$prev_year_days_date <- sampled_hauls$prev_days_date - days(365)
  
  # Database to use in iterations
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
  
  ## Load SDMs to be used in function to calculate expected revenue and cost below
  psdn.sdm <- read.csv(file = 'Participation/SDM_code/sdm_psdn.csv')
  psdn.sdm[is.na(psdn.sdm)] <- 0
  psdn.sdm$set_date <- ymd(paste(psdn.sdm$LANDING_YEAR, psdn.sdm$LANDING_MONTH, psdn.sdm$LANDING_DAY, sep = "-"))
  msqd.sdm <- readRDS(file = 'Participation/SDM_code/sdm_msqd.rds')
  msqd.sdm[is.na(msqd.sdm)] <- 0
  msqd.sdm$set_date <- ymd(paste(msqd.sdm$LANDING_YEAR, msqd.sdm$LANDING_MONTH, msqd.sdm$LANDING_DAY, sep = "-"))
  nanc.sdm <- readRDS(file = 'Participation/SDM_code/sdm_nanc.rds')
  nanc.sdm[is.na(nanc.sdm)] <- 0
  nanc.sdm$set_date <- ymd(paste(nanc.sdm$LANDING_YEAR, nanc.sdm$LANDING_MONTH, nanc.sdm$LANDING_DAY, sep = "-"))
  phrg.sdm <- readRDS(file = 'Participation/SDM_code/sdm_phrg.rds')
  phrg.sdm[is.na(phrg.sdm)] <- 0
  phrg.sdm$set_date <- ymd(paste(phrg.sdm$LANDING_YEAR, phrg.sdm$LANDING_MONTH, phrg.sdm$LANDING_DAY, sep = "-"))
  
  # tab.maxdays.psdn <- psdn.sdm %>% 
  #   dplyr::select(c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  #   group_by(LANDING_YEAR, LANDING_MONTH) %>% summarize(max.days = max(LANDING_DAY))
  # tab.maxdays.msqd <- msqd.sdm %>% 
  #   dplyr::select(c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  #   group_by(LANDING_YEAR, LANDING_MONTH) %>% summarize(max.days = max(LANDING_DAY))
  # tab.maxdays.nanc <- nanc.sdm %>% 
  #   dplyr::select(c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  #   group_by(LANDING_YEAR, LANDING_MONTH) %>% summarize(max.days = max(LANDING_DAY))
  # tab.maxdays.phrg <- phrg.sdm %>% 
  #   dplyr::select(c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  #   group_by(LANDING_YEAR, LANDING_MONTH) %>% summarize(max.days = max(LANDING_DAY))
  
  dummys2 <- foreach::foreach(ii = 1:nrow(td),
    .packages = c("dplyr", 'lubridate')) %dopar% {
      source("C:\\GitHub\\EconAnalysis\\Functions\\participation_model\\process_dummys2_participation.R")
      process_dummys2(xx = ii, td1 = td, dat1 = dat, 
                      qPSDN1 = qPSDN, SDM.PSDN = psdn.sdm,
                      qMSQD1 = qMSQD, SDM.MSQD = msqd.sdm,
                      qNANC1 = qNANC, SDM.NANC = nanc.sdm,
                      qPHRG1 = qPHRG, SDM.PHRG = phrg.sdm,
                      model_price1 = model_price,
                      model_landings1 = model_landings)
    }
  print("Done calculating dummys and revenues")
  td2 <- plyr::ldply(dummys2)
  stopCluster(cl)
  
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
  
  
  #-----------------------------------------------
  ## Return data
  return(sampled_hauls)
  
  
  #### FUTURE TASK! ####
  ### Calculated expected distances, and multiply by port fuel prices
  #### Expected distances are the distance traveled by any vessel in the last 30 days to capture species S in port J
  #### Usea catch areas, and calculate distance by row to port of landings
  #### I should calculate also distance from port of choice set to port of gravity. 
  
}
  
