library(tidyverse)
#getNOAABathy lines
library(marmap)
#for plotting maps and projections
library(maps)
library(mapdata)
library(mapproj)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
##installation instructions for coldpool package
#devtools::install_github("afsc-gap-products/akgfmaps", build_vignettes = TRUE)
#devtools::install_github("afsc-gap-products/coldpool")
library(coldpool)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#map
US <- map_data("world2Hires",region="USA")
AK <- subset(US,subregion == "Alaska")
USSR <- map_data("world2Hires",region="USSR")
NBS <- rbind(AK,USSR)
#transforms longitudes (you could also transform the longs in the data file)
NBS$long <- NBS$long-360

#bathymetry
bathline <- readRDS("data/bathline.rds")

#data points
dat <- read.csv("data/NBSIERP_Stations2.csv")

#temp data
nbs_btm <- unwrap(nbs_ebs_bottom_temperature)
nbs_btm_proj <-  project(nbs_btm,method="bilinear","+proj=longlat +datum=WGS84", res = c(0.01, 0.01))
nbs_df <- terra::as.data.frame(nbs_btm_proj,xy=TRUE)
nbs_19 <- subset(nbs_df,select=c("x","y","2019"))
names(nbs_19) <- c("lon","lat","btm_temp")

plot <- ggplot() +
  geom_tile(data=nbs_19,aes(x=lon,y=lat,fill=btm_temp),alpha=0.5) +
 # geom_tile(data=nbs_19,aes(x=lon,y=lat,fill=btm_temp)) +
  geom_polygon(data=NBS,aes(x=long,y=lat,group=group),fill="grey") +
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-50),size=c(0.3),colour="darkgrey",alpha=0.8) +
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-200),size=c(0.3),colour="black",alpha=0.4) +
  geom_text (aes(x=-167.9,y=63.75),label=c("M14")) +
  geom_text (aes(x=-174.688,y=61.9),label=c("M8")) +
  geom_point(data=dat,aes(x=EQ_LONGITUDE,y=EQ_LATITUDE,shape=Label,color=Label,size=Label),alpha=0.6) +
  annotate( geom = "text", x = -161.8, y = 57.4, label = "50m", 
            color = "black", size = 2 )+
  annotate( geom = "text", x = -168.1, y = 56, label = "200m", 
            color = "black", size = 2 )+
  coord_sf(crs="EPSG:3476",default_crs=sf::st_crs(4326),xlim=c(-176,-161),ylim=c(55,66)) +
  labs(x="Longitude (°W)",y="Latitude (°N)",color="Station Type",shape="Station Type",size="Station Type") +
  scale_fill_distiller(palette = "Spectral",na.value="white",limits=range(-0.5,15),breaks=c(0,3,6,9,12,15),name="Bottom Temp (°C)") +
  scale_y_continuous(breaks=c(56,58,60,62,64)) +
  scale_x_continuous(breaks=c(-175,-172.5,-170,-167.5,-165,-162.5,-160))  +
  scale_color_manual(values=c("NBS Surface Trawl Station"="black","Moorings"="gold","IERP Station" = "orangered","SEBS Surface Trawl Station"="purple")) +
  scale_shape_manual(values=c("NBS Surface Trawl Station"=19,"Moorings"=17,"IERP Station"=15,"SEBS Surface Trawl Station"=18)) +
  scale_size_manual(values=c("NBS Surface Trawl Station"=5,"Moorings"=7,"IERP Station"=5,"SEBS Surface Trawl Station"=5)) +
  theme(axis.ticks=element_blank()) +
  theme(panel.grid=element_line(color=rgb(235, 235, 235, 100, maxColorValue = 255),size=0.5,linetype=1)) +
  theme(panel.background=element_rect(fill="white"))

ggsave(plot=plot,filename="figures/NBSIERP Map with Coldpool.jpg",width=8,height=8,units=c("in"),dpi=300)

