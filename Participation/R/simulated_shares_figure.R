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

# # Create data
# data <- data.frame(group=paste("Group", letters[1:20]), value=sample(seq(1,100),20)) 

# Generate plot SDM = 1
Simulated_shares$perc <- Simulated_shares$perc1 * 100
packing <- circleProgressiveLayout(Simulated_shares$perc, sizetype='area')
packing$radius <- packing$radius
data <- cbind(Simulated_shares, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)
gg1 <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data, aes(x, y, size=perc, label = selection), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal() + ggtitle("(a) Squid SDM = 1")

# Generate plot SDM = 0
Simulated_shares$perc <- Simulated_shares$perc3 * 100
Simulated_shares <- Simulated_shares %>% dplyr::filter(perc>0)
packing <- circleProgressiveLayout(Simulated_shares$perc, sizetype='area')
packing$radius <- packing$radius
data <- cbind(Simulated_shares, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)
gg2 <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=id), colour = "black", alpha = 0.6) +
  scale_fill_viridis() +
  geom_text(data = data, aes(x, y, size=perc, label = selection), color="black") +
  theme_void() + 
  theme(legend.position="none")+ 
  coord_equal() + ggtitle("(b) Squid SDM = 0")

gg1 + gg2