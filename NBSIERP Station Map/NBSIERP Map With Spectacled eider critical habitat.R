library(tidyverse)
#getNOAABathy lines
library(marmap)
#for plotting maps and projections
library(maps)
library(mapdata)
library(mapproj)
library(sf)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#map
US <- map_data("world2Hires",region="USA")
AK <- subset(US,subregion == "Alaska")
USSR <- map_data("world2Hires",region="USSR")
NBS <- rbind(AK,USSR)
#transforms longitudes (you could also transform the longs in the data file)
NBS$long <- NBS$long-360

bathline <- readRDS("data/bathline.rds")

#data points
dat <- read.csv("data/NBSIERP_Stations2.csv") %>%
  filter(Label  %in% c("NBS Surface Trawl Station","IERP Station"))

shp <- st_read("shapefile/FCH_Somateria_fischeri_20010206.shp")


plot <- ggplot() +
  # geom_tile(data=nbs_19,aes(x=lon,y=lat,fill=btm_temp)) +
  geom_polygon(data=NBS,aes(x=long,y=lat,group=group),fill="grey") +
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-50),size=c(0.3),colour="darkgrey",alpha=0.8) +
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-200),size=c(0.3),colour="black",alpha=0.4) + 
  geom_sf(data = shp, aes(fill = "Spectacled Eider Critical Habitat"), color = NA,alpha=0.6) +
  geom_point(data=dat,aes(x=EQ_LONGITUDE,y=EQ_LATITUDE,shape=Label,color=Label,size=Label)) +
  annotate( geom = "text", x = -161.8, y = 57.4, label = "50m", 
            color = "black", size = 2 )+
  annotate( geom = "text", x = -168.1, y = 56, label = "200m", 
            color = "black", size = 2 )+
  coord_sf(crs="EPSG:3476",default_crs=sf::st_crs(4326),xlim=c(-176,-161),ylim=c(59,66)) +
  labs(x="Longitude (°W)",y="Latitude (°N)",color="Station Type",shape="Station Type",size="Station Type") +
  scale_fill_manual(values = c("Spectacled Eider Critical Habitat" = "orange"), name = "Habitat") +
  scale_y_continuous(breaks=c(56,58,60,62,64)) +
  scale_x_continuous(breaks=c(-175,-172.5,-170,-167.5,-165,-162.5,-160))  +
  scale_color_manual(values=c("NBS Surface Trawl Station"="black","IERP Station" = "royalblue")) +
  scale_shape_manual(values=c("NBS Surface Trawl Station"=19,"IERP Station"=15)) +
  scale_size_manual(values=c("NBS Surface Trawl Station"=5,"IERP Station"=5)) +
  theme(axis.ticks=element_blank()) +
  theme(panel.grid=element_line(color=rgb(235, 235, 235, 100, maxColorValue = 255),size=0.5,linetype=1)) +
  theme(panel.background=element_rect(fill="white"))

ggsave(plot=plot,filename="figures/NBSIERP Map With Spectacled eiter critical habitat.jpg",width=8,height=8,units=c("in"),dpi=300)
