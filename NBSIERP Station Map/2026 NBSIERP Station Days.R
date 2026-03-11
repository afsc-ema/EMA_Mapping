library(tidyverse)
#getNOAABathy lines
library(marmap)
#for plotting maps and projections
library(maps)
library(mapdata)
library(mapproj)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

US <- map_data("world2Hires",region="USA")
AK <- subset(US,subregion == "Alaska")
USSR <- map_data("world2Hires",region="USSR")
NBS <- rbind(AK,USSR)
#transforms longitudes (you could also transform the longs in the data file)
NBS$long <- NBS$long-360

bathline <- readRDS("data/bathline.rds")
dat <- read.csv("data/2026 NBSIERP Station Days.csv")

dat$Label <- as.factor(dat$Label)
dat$Leg <- as.factor(dat$Leg)

plot <- ggplot() +
  # geom_tile(data=nbs_19,aes(x=lon,y=lat,fill=btm_temp)) +
  geom_polygon(data=NBS,aes(x=long,y=lat,group=group),fill="grey") +
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-50),size=c(0.3),colour="darkgrey",alpha=0.8) +
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-200),size=c(0.3),colour="black",alpha=0.4) +
  geom_label(data=dat,aes(x=EQ_LONGITUDE,y=EQ_LATITUDE,label=Label,color=Leg)) +
  annotate( geom = "text", x = -174.5, y = 63 , label = "Won't be able to get \n all of these in one day", color = "black", size = 4) +
  coord_sf(crs="EPSG:3476",default_crs=sf::st_crs(4326),xlim=c(-176,-161),ylim=c(59,66)) +
  labs(x="Longitude (°W)",y="Latitude (°N)",color="Leg",title="Proposed 2026 NBSIERP Station Order") +
  scale_y_continuous(breaks=c(56,58,60,62,64)) +
  scale_x_continuous(breaks=c(-175,-172.5,-170,-167.5,-165,-162.5,-160))  +
  theme(axis.ticks=element_blank()) +
  theme(panel.grid=element_line(color=rgb(235, 235, 235, 100, maxColorValue = 255),size=0.5,linetype=1)) +
  theme(panel.background=element_rect(fill="white"))

ggsave(plot=plot,path="figures/",filename="Proposed 2026 NBSIERP Station Order.jpg",width=8,height=8,units=c("in"),dpi=300)
