data1 <- readRDS(file = 'Participation/SDM_code/sdm_msqd_spawn_v1.rds')
data2 <- readRDS(file = 'Participation/SDM_code/sdm_msqd_spawn_v2.rds')

data1 <- data1 %>% dplyr::filter(LANDING_YEAR < 2012)
data_merge <- rbind(data1, data2)
saveRDS(data_merge, "Participation/SDM_code/sdm_msqd_spawn.rds")