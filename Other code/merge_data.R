data1 <- read.csv(file = 'Participation/SDM_code/sdm_psdn_1.csv')
data2 <- read.csv(file = 'Participation/SDM_code/sdm_psdn_2.csv')

data_merge <- rbind(data1, data2)
write.csv(data_merge, "Participation/SDM_code/sdm_psdn.csv", row.names = FALSE)