######################################################
### Simulated shares figure in participation model ###
######################################################

rm(list = ls(all.names = TRUE)) 
gc()


## Load datase (from Stata work on predicting shares)
Simulated_shares <- read.csv("C:/GitHub/EconAnalysis/Participation/R/Simulated_shares.csv")


# libraries
library(packcircles)
library(ggplot2)
library(viridis)
library(patchwork)
library(dplyr)

# # Create data
# data <- data.frame(group=paste("Group", letters[1:20]), value=sample(seq(1,100),20)) 

# Generate plot SDM = 1 (perc1, original is perc)
Simulated_shares$perc <- Simulated_shares$perc * 100
packing <- circleProgressiveLayout(Simulated_shares$perc, sizetype='area')
packing$radius <- packing$radius
data <- cbind(Simulated_shares, packing)
data1 <- data %>% mutate(selection1 = ifelse(selection == "No-Participation", "No\nParticipation",
                                      ifelse(selection == "SBA-MSQD", "Squid\n(Santa Barbara)", 
                                      ifelse(selection == "MNA-MSQD", "Squid\n(Monterey)", 
                                      ifelse(selection == "LAA-MSQD", "Squid\n(Los Angeles)",
                                      ifelse(selection == "SFA-MSQD", "Squid\n(SF)", NA))))))
dat.gg <- circleLayoutVertices(packing, npoints=50)
gg1 <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data1, aes(x, y, size=perc, label = selection1), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal() + ggtitle("(a) Historical SDM")


# Generate plot SDM = 0
Simulated_shares$perc <- Simulated_shares$perc3 * 100
Simulated_shares <- Simulated_shares %>% dplyr::filter(perc>0)
packing <- circleProgressiveLayout(Simulated_shares$perc, sizetype='area')
packing$radius <- packing$radius
data <- cbind(Simulated_shares, packing)
data2 <- data %>% mutate(selection2 = ifelse(selection == "No-Participation", "No\nParticipation",
                                      ifelse(selection == "LAA-PSDN", "Sardine\n(Los Angeles)", 
                                      ifelse(selection == "MNA-NANC", "Anchovy\n(Monterey)", 
                                      ifelse(selection == "LAA-CMCK", "Chub Mackerel\n(Los Angeles)",
                                      ifelse(selection == "LAA-YTNA", "Yellowfin\n(Los Angeles)", 
                                      ifelse(selection == "LAA-BTNA", "Bluefin\n(Los Angeles)", 
                                      ifelse(selection == "LAA-JMCK", "Jack Mackerel\n(Los Angeles)", 
                                      ifelse(selection == "SFA-NANC", "Anchovy\n(SF)", 
                                      ifelse(selection == "SFA-DCRB", "Crab\n(SF)", 
                                      ifelse(selection == "LAA-PBNT", "Bonito\n(Los Angeles)",NA)))))))))))
dat.gg <- circleLayoutVertices(packing, npoints=50)
gg2 <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data2, aes(x, y, size=perc, label = selection2), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal() + ggtitle("(b) Squid SDM = 0")

gg1 + gg2