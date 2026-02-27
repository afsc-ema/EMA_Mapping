#ggplot, data manipulation
library(tidyverse)
#getNOAABathy lines
library(marmap)
#for plotting maps and projections
library(maps)
library(mapdata)
library(mapproj)
library(ggspatial)
#set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#read in Alaska polygon for mapping
US <- map_data("world2Hires",region="USA")
AK <- subset(US,subregion == "Alaska")
USSR <- map_data("world2Hires",region="USSR")
NBS <- rbind(AK,USSR)
#transforms longitudes (you could also transform the longs in the data file)
NBS$long <- NBS$long-360

#read in bathymetry file.  

#alternative 
bathline <- getNOAA.bathy(-175,-158,55,60)

#read in station coordinate file
event <- read.csv(file.choose())
event$EQ_LONGITUDE <- event$EQ_LONGITUDE *-1
event_ctd <- subset(event,GEAR=="CTD")
event_benth <- subset(event,GEAR=="Benthic Grab")
event_bon <- subset(event,GEAR=="Bongo")
event_3m <- subset(event,GEAR=="3m Beam Trawl")
event_can <- subset(event,GEAR=="CanTrawl")
event_nets <- subset(event,GEAR=="NETS156")

event <- rbind(event_ctd,event_benth,event_bon,event_3m,event_can,event_nets)

catch <- read.csv(file.choose())
catch$CPUE_NUM <- catch$TotalCatchNum/catch$Effort

catch_surface <- catch %>%
  filter(Tow.Type=="S")
catch_mid <- catch %>%
  filter(Tow.Type=="O") %>%
  mutate(CPUE_NUM=CPUE_NUM/1000000000)

catch_single1 <- catch_mid %>%
  filter(CPUE_NUM > 0) %>%
  ungroup() %>%
  mutate(quartile = ntile(CPUE_NUM,5))

catch_single1$quartile <- as.factor(catch_single1$quartile)


catch_breaks <- catch_single1 %>%
  group_by(quartile) %>%
  summarise(min=round(min(CPUE_NUM),digits=3),max=round(max(CPUE_NUM),digits=3)) %>%
  ungroup() %>%
  as.data.frame()
catch_breaks$breaks <- paste(catch_breaks$min,"-",catch_breaks$max,sep="")

nets_map <- ggplot() +
  #alaska polygon
  geom_polygon(data=NBS,aes(x=long,y=lat,group=group),fill="grey") +
  #bathymetry
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-50),size=c(0.3),colour="blue",alpha=0.2) +
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-20),size=c(0.3),colour="red",alpha=0.2) +
  geom_contour(data=bathline,aes(x=x,y=y,z=z),breaks=c(-200),size=c(0.3),colour="seagreen",alpha=0.2) +
  #station coordinates as points
  geom_point(data=catch_single1,aes(x=EQ.Longitude,y=EQ.Latitude,size=quartile,shape=quartile)) +
  #bathymetry labels
  annotate( geom = "text", x = -165.5, y = 59.5, label = "20m", 
            color = "black", size = 2 )+
  annotate( geom = "text", x = -166, y = 58.2, label = "50m", 
            color = "black", size = 2 )+
  annotate( geom = "text", x = -168.1, y = 56, label = "200m", 
            color = "black", size = 2 )+
  scale_size_manual( name = "CPUE (n/m\U00B3)", values= c(1.5, 3, 4.5, 6 , 7.5), labels=catch_breaks$breaks) +
  scale_shape_manual(name = "CPUE (n/m\U00B3)",values=c(16,16,16,16,16),labels=catch_breaks$breaks) +
  #projection and limits using coord_sf
  coord_sf(crs="EPSG:3476",default_crs=sf::st_crs(4326),xlim=c(-174,-158),ylim=c(54,61)) +
  ##alternatively you can use coord_map.  These both work for projections.  The below line is what i use for the CPUE maps and it generally works better.
  #coord_map(proj="azequidistant",orientation=c(90,180,20),xlim=c(-175,-158),ylim=c(55,60))
  #axis and legend label
  labs(x="Longitude (°W)",y="Latitude (°N)",title="NETS156 CPUE") +
  #lat/lon tick labels
  scale_y_continuous(breaks=c(54,56,58,60)) +
  scale_x_continuous(breaks=c(-170,-165,-160)) +
  #changes station color, size, and shape depending on station type.  
  theme(axis.ticks=element_blank()) +
  #lat/long graticule color and transparency
  theme(panel.grid=element_line(color=rgb(235, 235, 235, 100, maxColorValue = 255),size=0.5,linetype=1)) +
  #blank legend background
  theme(panel.background=element_rect(fill="white")) 
  #place legend inside plot, position justification, etc

ctd_map <- ggplot() +
  geom_polygon(data=NBS,aes(x=long,y=lat,group=group),fill="grey") +
  #bathymetry
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-50),size=c(0.3),colour="blue",alpha=0.2) +
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-20),size=c(0.3),colour="red",alpha=0.2) +
  geom_contour(data=bathline,aes(x=x,y=y,z=z),breaks=c(-200),size=c(0.3),colour="seagreen",alpha=0.2) +
  annotate( geom = "text", x = -165.5, y = 59.5, label = "20m", 
            color = "black", size = 2 )+
  annotate( geom = "text", x = -166, y = 58.2, label = "50m", 
            color = "black", size = 2 )+
  annotate( geom = "text", x = -168.1, y = 56, label = "200m", 
            color = "black", size = 2 )+
  geom_point(data=event_ctd,aes(x=EQ_LONGITUDE,y=EQ_LATITUDE),size=3) +
  coord_sf(crs="EPSG:3476",default_crs=sf::st_crs(4326),xlim=c(-174,-158),ylim=c(54,61)) +
  ##alternatively you can use coord_map.  These both work for projections.  The below line is what i use for the CPUE maps and it generally works better.
  #coord_map(proj="azequidistant",orientation=c(90,180,20),xlim=c(-175,-158),ylim=c(55,60))
  #axis and legend label
  labs(x="Longitude (°W)",y="Latitude (°N)",title="CTD Sampling Events") +
  #lat/lon tick labels
  scale_y_continuous(breaks=c(54,56,58,60)) +
  scale_x_continuous(breaks=c(-170,-165,-160)) +
  #changes station color, size, and shape depending on station type.  
  theme(axis.ticks=element_blank()) +
  #lat/long graticule color and transparency
  theme(panel.grid=element_line(color=rgb(235, 235, 235, 100, maxColorValue = 255),size=0.5,linetype=1)) +
  #blank legend background
  theme(panel.background=element_rect(fill="white")) 

bongo_map <- ggplot() +
  geom_polygon(data=NBS,aes(x=long,y=lat,group=group),fill="grey") +
  #bathymetry
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-50),size=c(0.3),colour="blue",alpha=0.2) +
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-20),size=c(0.3),colour="red",alpha=0.2) +
  geom_contour(data=bathline,aes(x=x,y=y,z=z),breaks=c(-200),size=c(0.3),colour="seagreen",alpha=0.2) +
  annotate( geom = "text", x = -165.5, y = 59.5, label = "20m", 
            color = "black", size = 2 )+
  annotate( geom = "text", x = -166, y = 58.2, label = "50m", 
            color = "black", size = 2 )+
  annotate( geom = "text", x = -168.1, y = 56, label = "200m", 
            color = "black", size = 2 )+
  geom_point(data=event_bon,aes(x=EQ_LONGITUDE,y=EQ_LATITUDE),size=3) +
  coord_sf(crs="EPSG:3476",default_crs=sf::st_crs(4326),xlim=c(-174,-158),ylim=c(54,61)) +
  ##alternatively you can use coord_map.  These both work for projections.  The below line is what i use for the CPUE maps and it generally works better.
  #coord_map(proj="azequidistant",orientation=c(90,180,20),xlim=c(-175,-158),ylim=c(55,60))
  #axis and legend label
  labs(x="Longitude (°W)",y="Latitude (°N)",title="Bongo Sampling Events") +
  #lat/lon tick labels
  scale_y_continuous(breaks=c(54,56,58,60)) +
  scale_x_continuous(breaks=c(-170,-165,-160)) +
  #changes station color, size, and shape depending on station type.  
  theme(axis.ticks=element_blank()) +
  #lat/long graticule color and transparency
  theme(panel.grid=element_line(color=rgb(235, 235, 235, 100, maxColorValue = 255),size=0.5,linetype=1)) +
  #blank legend background
  theme(panel.background=element_rect(fill="white")) 

grab_map <- ggplot() +
  geom_polygon(data=NBS,aes(x=long,y=lat,group=group),fill="grey") +
  #bathymetry
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-50),size=c(0.3),colour="blue",alpha=0.2) +
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-20),size=c(0.3),colour="red",alpha=0.2) +
  geom_contour(data=bathline,aes(x=x,y=y,z=z),breaks=c(-200),size=c(0.3),colour="seagreen",alpha=0.2) +
  annotate( geom = "text", x = -165.5, y = 59.5, label = "20m", 
            color = "black", size = 2 )+
  annotate( geom = "text", x = -166, y = 58.2, label = "50m", 
            color = "black", size = 2 )+
  annotate( geom = "text", x = -168.1, y = 56, label = "200m", 
            color = "black", size = 2 )+
  geom_point(data=event_benth,aes(x=EQ_LONGITUDE,y=EQ_LATITUDE),size=3) +
  coord_sf(crs="EPSG:3476",default_crs=sf::st_crs(4326),xlim=c(-174,-158),ylim=c(54,61)) +
  ##alternatively you can use coord_map.  These both work for projections.  The below line is what i use for the CPUE maps and it generally works better.
  #coord_map(proj="azequidistant",orientation=c(90,180,20),xlim=c(-175,-158),ylim=c(55,60))
  #axis and legend label
  labs(x="Longitude (°W)",y="Latitude (°N)",title="CTD Sampling Events") +
  #lat/lon tick labels
  scale_y_continuous(breaks=c(54,56,58,60)) +
  scale_x_continuous(breaks=c(-170,-165,-160)) +
  #changes station color, size, and shape depending on station type.  
  theme(axis.ticks=element_blank()) +
  #lat/long graticule color and transparency
  theme(panel.grid=element_line(color=rgb(235, 235, 235, 100, maxColorValue = 255),size=0.5,linetype=1)) +
  #blank legend background
  theme(panel.background=element_rect(fill="white")) 
  

stn_map <- plot_grid(ctd_map,bongo_map,grab_map,can_map,nets_map)

ggsave(plot=stn_map,filename="Alex Survey Map.jpg",width=24,height=12,units=c("in"),dpi=300)
