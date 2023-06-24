
###################
### Sample choice set for the participation model 
###################

# WORK TO DO:
# Behavior of the cluster? 
# Use SDM last 30 days (same for CPUE index)

#' @param data_in Data going in to the function; default is filt_clusts
#' @param cluster cluster that we are analyzing 
#' @param min_year Minimum year used to filter the data
#' @param max_year Maximum year used to filter the data, also RUM data is filtered to be from the max_year
#' @param min_year_prob Minimum year used to compute sample probabilities
#' @param max_year_prob Maximum year used to compute sample probabilities
#' @param min_year_est Minimum year used to estimate expected landings. If change, we have to change tables in paper
#' @param max_year_est Maximum year used to estimate expected landings. If change, we have to change tables in paper
#' @param ndays Number of previous days data to use in previous behavior dummies
#' @param nhauls_sampled Number of hauls -- trips -- to sample from the full data set, additional to the selection
#' @param seed Seed for sampling tows
#' @param ncores Number of cores to use
#' @param rev_scale Scale the revenue by this factor

#' @export

sampled_rums <- function(data_in, cluster = 4,
                         min_year = 2013, max_year = 2017,
                         min_year_prob = 2013, max_year_prob = 2017,
                         min_year_est = 2012, max_year_est = 2019,
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
  data_in <- readRDS("C:\\Data\\PacFIN data\\participation_data_filtered.rds") 
  cluster <- 4
  min_year_prob <- 2013
  max_year_prob <- 2017
  min_year_est <- 2012
  max_year_est <- 2019
  min_year <- 2013
  max_year <- 2017
  ndays <- 30
  nhauls_sampled <- 5
  seed <- 300
  ncores <- 4
  rev_scale <- 1000
  ################

  dat <- data_in 
  
  #-------------
  ## Price models
  dat_est <- readRDS("C:/Data/PacFIN data/Tickets_filtered.rds")

  datPanel <- dat_est %>% 
      dplyr::filter(LANDING_YEAR >= min_year_est, LANDING_YEAR <= max_year_est) %>%
      dplyr::mutate(ln_Price_mtons = log(Price_mtons)) %>% 
      mutate(trend = LANDING_YEAR - min_year_est + 1) %>%
      mutate(set_month = LANDING_MONTH) %>%
      filter(Species_Dominant != "FSOL") %>% filter(Species_Dominant != "SCOR") %>%
      filter(Species_Dominant != "STRY") %>% filter(Species_Dominant != "DSOL") %>%
      filter(Species_Dominant != "HTRB") %>% filter(Species_Dominant != "RHRG") %>%
      filter(Species_Dominant != "SLNS") %>% filter(Species_Dominant != "RDB1") %>%
      filter(Species_Dominant != "BSK1") %>% filter(Species_Dominant != "TREE") %>%
      filter(Species_Dominant != "POP2") %>% filter(Species_Dominant != "STR1") %>%
      filter(Species_Dominant != "KFSH") %>% filter(Species_Dominant != "REX")  %>%
      filter(Species_Dominant != "BRZ1") %>% filter(Species_Dominant != "MXR1") %>%
      filter(Species_Dominant != "GSR1") %>% filter(Species_Dominant != "BMCK") %>%
      filter(Species_Dominant != "ART1") %>% filter(Species_Dominant != "RTSK") %>%
      filter(Species_Dominant != "PNK1") %>% filter(Species_Dominant != "GPRW") %>%
      filter(Species_Dominant != "SQID") %>% filter(Species_Dominant != "MXRF") %>%
      filter(Species_Dominant != "RCK7") %>% filter(Species_Dominant != "GBLC") %>%
      filter(Species_Dominant != "HNY1") %>% filter(Species_Dominant != "UTNA") %>%
      filter(Species_Dominant != "MEEL") %>% filter(Species_Dominant != "GGRD") %>%
      filter(Species_Dominant != "MSHP") %>% filter(Species_Dominant != "GBL1") %>%
      filter(Species_Dominant != "PNKR") %>% filter(Species_Dominant != "FLAG") %>%
      filter(Species_Dominant != "YEY1") %>% filter(Species_Dominant != "ROSY") %>%
      filter(Species_Dominant != "SAIL") %>% filter(Species_Dominant != "SCLP") %>%
      filter(Species_Dominant != "RSTN") %>% filter(Species_Dominant != "PLCK") %>%
      filter(Species_Dominant != "BCLM") %>% filter(Species_Dominant != "CKLE") %>%
      filter(Species_Dominant != "GCLM") %>% filter(Species_Dominant != "NUSF")
    
    # model_price <- lm(ln_Price_mtons ~ factor(Species_Dominant):factor(PORT_AREA_CODE) + 
    #                    factor(set_month) + trend, data = datPanel)
    # saveRDS(model_price, file = 'Participation\\R\\model_price_2012_2019.RDS')
    # model_price <- readRDS(file = 'Participation\\R\\model_price_2012_2019.RDS')
    # 
    # # modelsummary::modelsummary(model_price, fmt = 2,
    # #                            gof_map = c("nobs", "adj.r.squared"),
    # #                            statistic = "({std.error}){stars}",
    # #                            output = "Participation\\Results\\price_model.docx")
    # #
  
    species <- as.data.frame(datPanel %>% dplyr::select(Species_Dominant) %>% unique())
    species.list <- c(species$Species_Dominant)
    species.list.number <- as.data.frame(species.list)
    mod_estimate <- list() 
    xx <- 1
    
    for(ii in species.list) {
      datPanel_X <- datPanel %>% filter(Species_Dominant == ii)
      mod_estimate[[xx]] <- 
        lm(ln_Price_mtons ~ factor(PORT_AREA_CODE) + factor(set_month) + trend, data = datPanel_X)
      xx <- min(xx + 1, nrow(species))
    }
    rm(xx, species.list, species, datPanel, dat_est, datPanel_X)
    species.list.number$id_number <- seq(1, nrow(species.list.number))

    # summary(mod_estimate[[29]])
    
  #-----------------------------------------------------------------------------
  ## Define hauls data used for estimation (in this case, are the trips)
  
  hauls <- dat %>% 
    dplyr::filter(set_year >= min_year, 
                  set_year <= max_year, 
                  group_all %in% cluster) %>% 
    distinct(trip_id, .keep_all = T)  %>% 
    dplyr::select(trip_id, VESSEL_NUM, set_year, set_month, set_day, Revenue, selection) %>% 
      as.data.frame

  ## Select hauls used to calculate probability for the choice set
  dist_hauls_catch_shares <- hauls %>% dplyr::filter(set_year >= min_year_prob, set_year <= max_year_prob)
    dist_hauls_catch_shares <- dist_hauls_catch_shares[dist_hauls_catch_shares$selection != "No-Participation", ]
  
    
  #----------------------------------------------------------------------------
  ## Create probabilities for sampling choice set
  ## For this, we compute the average vessel catch and port composition by month
  ## (The choice set varies depending on the month of the year)

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
  
  ############################################################################
  ## Create example plot with the proportion used to sample the choice set.
  #
  # dbp_test <- dbp_month %>% group_by(set_month) %>%
  #   summarize(sum_prop = sum(prop)) 
  # 
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
  ############################################################################

  #-----------------------------------------------------------------------
  ## Sample Hauls
  
  # Set seed
  set.seed(seed)
  seedz <- sample(1:1e7, size = nrow(hauls))

  
  # Include decision previous day!
  hauls$set_date <- as.Date(
    with(hauls, paste(set_year, set_month, set_day, sep="-")), "%Y-%m-%d")
  hauls$prev_day_date <- hauls$set_date - days(1)
  
  selection_prev_days <- hauls %>% 
    dplyr::select(c('selection', 'set_date', 'VESSEL_NUM')) %>%
    unique() %>%
    rename(prev_day_date = set_date) %>%
    rename(prev_selection = selection)
  
  hauls <- merge(hauls, selection_prev_days,
                  by = c('prev_day_date', 'VESSEL_NUM'), 
                  all.x = TRUE, 
                  all.y = FALSE) %>%
    mutate(prev_selection = ifelse(is.na(prev_selection), "No-Participation", prev_selection))
    
  
  #Sample hauls and calculate distances
  #For each haul in the focus year, sample nhauls_sampled tows
  
  sampled_hauls <- foreach::foreach(ii = 1:nrow(hauls),
                                    .packages = c("dplyr", 'plyr', 'lubridate')) %dopar% {
                                      set.seed(seedz[ii])
                                      if (hauls[ii, "selection"] != "No-Participation" & 
                                          hauls[ii, "selection"] != hauls[ii, "prev_selection"]) {
                                          temp <- dbp_month  %>% dplyr::filter(selection != as.character(hauls[ii, "selection"]),
                                                                               selection != as.character(hauls[ii, "prev_selection"]),
                                                                               set_month == as.character(hauls[ii, "set_month"])) 
                                          samps <- temp %>% sample_n(size = (nhauls_sampled - 2), weight = prop, replace = F)
                                          the_samples <- as.data.frame(samps[ , "selection"]) 
                                          colnames(the_samples)[1] <- "selection"
                                          the_samples <- the_samples %>%
                                            add_row(selection = "No-Participation") %>%
                                            add_row(selection = as.character(hauls[ii, "prev_selection"]))
                                        } else if (hauls[ii, "selection"] != "No-Participation" & 
                                                   hauls[ii, "selection"] == hauls[ii, "prev_selection"]) {
                                          temp <- dbp_month %>% dplyr::filter(selection != as.character(hauls[ii, "selection"]),
                                                                              set_month == as.character(hauls[ii, "set_month"]))
                                          samps <- temp %>% sample_n(size = (nhauls_sampled - 1) , weight = prop, replace = F)
                                          the_samples <- as.data.frame(samps[ , "selection"]) 
                                          colnames(the_samples)[1] <- "selection"
                                          the_samples <- the_samples %>%
                                            add_row(selection = "No-Participation")
                                        } else if (hauls[ii, "selection"] == "No-Participation" & 
                                                   hauls[ii, "selection"] != hauls[ii, "prev_selection"]) {
                                          temp <- dbp_month  %>% dplyr::filter(selection != as.character(hauls[ii, "selection"]),
                                                                               selection != as.character(hauls[ii, "prev_selection"]),
                                                                               set_month == as.character(hauls[ii, "set_month"])) 
                                          samps <- temp %>% sample_n(size = (nhauls_sampled - 1), weight = prop, replace = F)
                                          the_samples <- as.data.frame(samps[ , "selection"]) 
                                          colnames(the_samples)[1] <- "selection"
                                          the_samples <- the_samples %>%
                                            add_row(selection = as.character(hauls[ii, "prev_selection"]))
                                        } else if (hauls[ii, "selection"] == "No-Participation" & 
                                                   hauls[ii, "selection"] == hauls[ii, "prev_selection"]) {
                                          temp <- dbp_month  %>% dplyr::filter(selection != as.character(hauls[ii, "selection"]),
                                                                               set_month == as.character(hauls[ii, "set_month"])) 
                                          samps <- temp %>% sample_n(size = (nhauls_sampled), weight = prop, replace = F)
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
  sampled_hauls <- sampled_hauls %>% mutate(PACFIN_SPECIES_CODE = ifelse(selection == "No-Participation", NA, str_sub(sampled_hauls$selection, start= -4)))
  sampled_hauls <- sampled_hauls %>% mutate(PORT_AREA_CODE = ifelse(selection == "No-Participation", NA, str_sub(sampled_hauls$selection, end= 3)))
  sampled_hauls <- sampled_hauls %>% mutate(AGENCY_CODE = 
                    ifelse(PORT_AREA_CODE == "BDA" | PORT_AREA_CODE == "BGA" | PORT_AREA_CODE == "CA2" |                       
                           PORT_AREA_CODE == "CCA" | PORT_AREA_CODE == "ERA" | PORT_AREA_CODE == "LAA" |                                              
                           PORT_AREA_CODE == "MNA" | PORT_AREA_CODE == "MRA" | PORT_AREA_CODE == "SBA" |
                           PORT_AREA_CODE == "SDA" | PORT_AREA_CODE == "SFA", "C", 
                    ifelse(PORT_AREA_CODE == "BRA" | PORT_AREA_CODE == "CBA" | PORT_AREA_CODE == "CLO" |
                           PORT_AREA_CODE == "NPA" | PORT_AREA_CODE == "OR1" | PORT_AREA_CODE == "TLA", "O",
                    ifelse(PORT_AREA_CODE == "CLW" | PORT_AREA_CODE == "CWA" | PORT_AREA_CODE == "NPS" |
                           PORT_AREA_CODE == "SPS" | PORT_AREA_CODE == "WA5", "W", NA))))
  
  #add in the fleet name
  sampled_hauls$fleet_name <- cluster

  #-----------------------------------------------
  ### Merge coordinates of port landed and center of gravity with participation data, and calculate distances from port to COG
  
  port_coord <- read.csv("C:/GitHub/EconAnalysis/Data/Ports/port_areas.csv") %>% 
    rename(PORT_AREA_CODE = port_group_code) %>% 
    rename(lon_port = lon) %>% 
    rename(lat_port = lat)
    port_coord <- port_coord[c(-2)]
    
  ## Calculate landing's center of gravity each vessel and link to closest port
  datCOG <- dat %>% 
      dplyr::filter(selection != "No-Participation") %>% 
      dplyr::filter(set_year >= min_year, set_year <= max_year)
    
  library(raster)    
  Ticket_Coords<-aggregate(Revenue ~ VESSEL_NUM + PORT_AREA_CODE + lat_port + lon_port, 
                           data=datCOG, FUN=sum)
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
  Permit_COG <- Permit_COG %>% 
    rename(fished_VESSEL_NUM = VESSEL_NUM)
  sampled_hauls <- merge(sampled_hauls, port_coord, by = c("PORT_AREA_CODE"), all.x = TRUE, all.y = FALSE)
  sampled_hauls <- merge(sampled_hauls, Permit_COG, by = c("fished_VESSEL_NUM"), all.x = TRUE, all.y = FALSE)
  rm(port_coord, Permit_COG)
  sampled_hauls <- sampled_hauls %>% rowwise() %>%
    mutate(dist_to_cog = ifelse(is.na(lat_port), NA, geosphere::distm(c(lon_port, lat_port), c(lon_cg, lat_cg), fun = geosphere::distHaversine))) %>%
    mutate(dist_to_cog = dist_to_cog/1000) %>% ungroup() %>% dplyr::select(-c(lon_port, lat_port, lon_cg, lat_cg))
  
  #-----------------------------------------------------------------------------
  ### Calculate interval between previous day and year
  
  #Obtain previous day, year and day/year date
  sampled_hauls$prev_days_date <- sampled_hauls$set_date - days(ndays)
  sampled_hauls$prev_30days_date <- sampled_hauls$set_date - days(30)
  sampled_hauls$prev_90days_date <- sampled_hauls$set_date - days(90)
  sampled_hauls$prev_day_date <- sampled_hauls$set_date - days(1)
  sampled_hauls$prev_year_set_date <- sampled_hauls$set_date - days(365)
  sampled_hauls$prev_year_days_date <- sampled_hauls$prev_days_date - days(365)
  
  # Database to use in iterations
  td <- sampled_hauls %>%
    dplyr::select(fished_haul, set_date, prev_days_date, prev_30days_date, prev_90days_date,
                  prev_year_set_date, prev_year_days_date, prev_day_date, fleet_name,
                  fished_VESSEL_NUM, PORT_AREA_CODE, PACFIN_SPECIES_CODE, AGENCY_CODE, selection, dist_to_cog)

  td$days_inter <- interval(td$prev_days_date, td$prev_day_date)
  td$days30_inter <- interval(td$prev_30days_date, td$prev_day_date)
  td$days90_inter <- interval(td$prev_90days_date, td$prev_day_date)
  td$prev_year_days_inter <- interval(td$prev_year_days_date, td$prev_year_set_date)

  

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

  # tab.maxdays.psdn <- psdn.sdm %>% 
  #   dplyr::select(c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  #   group_by(LANDING_YEAR, LANDING_MONTH) %>% summarize(max.days = max(LANDING_DAY))
  # tab.maxdays.msqd <- msqd.sdm %>% 
  #   dplyr::select(c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  #   group_by(LANDING_YEAR, LANDING_MONTH) %>% summarize(max.days = max(LANDING_DAY))
  # tab.maxdays.nanc <- nanc.sdm %>% 
  #   dplyr::select(c(LANDING_YEAR, LANDING_MONTH, LANDING_DAY)) %>%
  #   group_by(LANDING_YEAR, LANDING_MONTH) %>% summarize(max.days = max(LANDING_DAY))

  ### Obtain fuel prices by port and day
  fuel.prices.CA <- readxl::read_excel(here::here("Data", "Fuel_prices", "fuelca.xls"), sheet = "fuelca")
  fuel.prices.WA <- readxl::read_excel(here::here("Data", "Fuel_prices", "fuelwa.xls"), sheet = "fuelwa")
  fuel.prices.OR <- readxl::read_excel(here::here("Data", "Fuel_prices", "fuelor.xls"), sheet = "fuelor")
  Deflactor <- read.csv(file = "C:\\Data\\PacFIN data\\deflactor.csv")
  port_area_codes <- read.csv(here::here('Data', 'Ports', 'ports_area_and_name_codes.csv'), 
                              header = TRUE, stringsAsFactors = FALSE)
  
  fuel.prices <- rbind(fuel.prices.CA,fuel.prices.OR,fuel.prices.WA) %>% 
    dplyr::select(-c('portname', 'DOCKCODE', 'notes', 'pricettl', 'pxquoted')) %>%
    dplyr::rename(PACFIN_PORT_CODE = port) %>%
    dplyr::rename(LANDING_YEAR = YEAR) %>%
    dplyr::rename(LANDING_MONTH = MONTH) %>%
    dplyr::rename(LANDING_DAY = DAY) %>%
    mutate(set_date = lubridate::make_date(LANDING_YEAR, LANDING_MONTH, LANDING_DAY))
    fuel.prices <- merge(fuel.prices, Deflactor, 
                       by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE, all.y = FALSE)
    fuel.prices$pricegal.AFI <- fuel.prices$pricegal * fuel.prices$defl
    fuel.prices[fuel.prices == 0] <- NA
    fuel.prices <- merge(fuel.prices, port_area_codes, 
                       by = ("PACFIN_PORT_CODE"), all.x = TRUE, all.y = FALSE)
    rm(fuel.prices.CA,fuel.prices.OR,fuel.prices.WA)
    fuel.prices <- fuel.prices %>% group_by(set_date, PORT_AREA_CODE) %>% 
      summarize(diesel.price.AFI = mean(pricegal.AFI, na.rm = TRUE)) %>% drop_na()
  
    ## Average prices by state and month
    fuel.prices.state <- 
      readxl::read_excel(here::here("Data", "Fuel_prices", "state_averages.xls"), sheet = "state_averages") %>%
      dplyr::rename(LANDING_YEAR = YEAR) %>%
      dplyr::rename(LANDING_MONTH = MONTH) %>%
      dplyr::rename(AGENCY_CODE = STATE) %>%
      dplyr::rename(diesel.price.state = avgpricegal) %>%
      dplyr::select(-c('avgpricettl')) %>%
      mutate(AGENCY_CODE = ifelse(AGENCY_CODE == 'WA', 'W', ifelse(AGENCY_CODE == 'CA', 'C', ifelse(AGENCY_CODE == 'OR', 'O', 'A'))))
    fuel.prices.state <- merge(fuel.prices.state, Deflactor, by = c("LANDING_YEAR", "LANDING_MONTH"), all.x = TRUE, all.y = FALSE)
    fuel.prices.state$diesel.price.state.AFI <- fuel.prices.state$diesel.price.state * fuel.prices.state$defl 
    fuel.prices.state[fuel.prices.state == 0] <- NA
    fuel.prices.state <- fuel.prices.state %>% drop_na()

    
  ### Calculate revenues
  dummys2 <- foreach::foreach(ii = 1:nrow(td),
    .packages = c("dplyr", 'lubridate')) %dopar% {
      source("C:\\GitHub\\EconAnalysis\\Functions\\participation_model\\process_dummys2_participation.R")
      process_dummys2(xx = ii, td1 = td, dat1 = dat, 
                      qPSDN1 = qPSDN, SDM.PSDN = psdn.sdm,
                      qMSQD1 = qMSQD, SDM.MSQD = msqd.sdm,
                      qNANC1 = qNANC, SDM.NANC = nanc.sdm,
                      model_price1 = model_price,
                      model_landings1 = model_landings,
                      fuel.prices1 = fuel.prices,
                      fuel.prices.state1 = fuel.prices.state,
                      min_year_est1 = min_year_est)
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
  td2[which(td2$cost_port_to_catch_area != 0), 'dummy_miss_cost_ca'] <- 0
  td2[which(td2$cost_port_to_catch_area == 0), 'dummy_miss_cost_ca'] <- 1
  td2[which(td2$travel_cost != 0), 'dummy_miss_cost'] <- 0
  td2[which(td2$travel_cost == 0), 'dummy_miss_cost'] <- 1
                      

  # Change dummies to zero for "No-Participation"
  td2[which(td2$selection == "No-Participation"), 'dummy_miss'] <- 0
  td2[which(td2$selection == "No-Participation"), 'dummy_prev_days'] <- 0
  td2[which(td2$selection == "No-Participation"), 'dummy_prev_year_days'] <- 0
  td2[which(td2$selection == "No-Participation"), 'dummy_last_day'] <- 0
  td2[which(td2$selection == "No-Participation"), 'dummy_miss_cost_ca'] <- 0
  td2[which(td2$selection == "No-Participation"), 'dummy_miss_cost'] <- 0
  
  td2$mean_rev_adj <- td2$mean_rev / rev_scale
  td2$travel_cost <- td2$travel_cost / rev_scale
  td2$cost_port_to_catch_area <- cost_port_to_catch_area / rev_scale
  td2$cost_port_to_cog <- cost_port_to_cog / rev_scale

  sampled_hauls <- cbind(sampled_hauls,
    td2[, c('dummy_prev_days', 'dummy_prev_year_days', "dummy_last_day", 
            "dummy_miss", 'mean_rev', 'mean_rev_adj',
            'cost_port_to_catch_area', 'cost_port_to_cog', 'travel_cost',
            'dummy_miss_cost_ca', 'dummy_miss_cost',
            'diesel_price', 'dist_port_to_catch_area', 'Ddiesel_state')] )
  
  
  
  #-----------------------------------------------
  ## Return data
  return(sampled_hauls)
  
}
  
