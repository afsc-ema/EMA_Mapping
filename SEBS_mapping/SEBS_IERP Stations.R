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

#read in bathymetry file.  
stations <- read.csv("data/AppendixXX_DY26-05_BASISStationCoordinates.csv")
#alternative 
bathline <- getNOAA.bathy(-180,-158,50,65)

##SEBS Method
stn <- ggplot() +
  #alaska polygon
  geom_polygon(data=NBS,aes(x=long,y=lat,group=group),fill="tan") +
  #bathymetry
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-50),size=c(0.3),colour="seagreen") +
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-20),size=c(0.3),colour="seagreen") +
  geom_contour(data=bathline,aes(x=x,y=y,z=z),breaks=c(-200),size=c(0.3),colour="seagreen") +
  geom_contour(data=bathline,aes(x=x,y=y,z=z),breaks=c(-100),size=c(0.3),colour="seagreen")+
  #station coordinates as points
  geom_point(data=stations, aes(x = Londd, y = Latdd, shape = Labels,size=Labels,color=Labels),alpha=1) +
  #bathymetry labels
  annotate( geom = "text", x = -165, y = 59.5, label = "20m", 
            color = "black", size = 4 )+
  annotate( geom = "text", x = -165.5, y = 58.2, label = "50m", 
            color = "black", size = 4 )+
  annotate( geom = "text", x = -168.1, y = 55.75, label = "200m", 
            color = "black", size = 4 ) +
  annotate(geom="text",x= -167, y=56.75,label="100m",color="black",size=4) +
  scale_shape_manual(name = "Station type",values=c("Core Trawl" = 17, "Optional Trawl"= 2, "Oceanography" = 18, "NBEST Surface Trawl" = 8, "IERP" = 13),
                     limits=c("Core Trawl","Optional Trawl","Oceanography","NBEST Surface Trawl","IERP")) +
  scale_size_manual(name = "Station type", values =c( "Core Trawl"= 4, "Optional Trawl"= 4, "Oceanography" = 4, "NBEST Surface Trawl" = 5, "IERP" = 5),
                    limits=c("Core Trawl","Optional Trawl","Oceanography","NBEST Surface Trawl","IERP")) +
  scale_color_manual(name = "Station type", values =c("Core Trawl" = "black", "Optional Trawl"= "black", "Oceanography" = "black", "NBEST Surface Trawl" = "blue", "IERP" = "blue"),
                     limits=c("Core Trawl","Optional Trawl","Oceanography","NBEST Surface Trawl","IERP")) +
  #projection and limits using coord_sf
  coord_sf(crs="EPSG:3476",default_crs=sf::st_crs(4326),xlim=c(-180,-158),ylim=c(54,64)) +
  ##alternatively you can use coord_map.  These both work for projections.  The below line is what i use for the CPUE maps and it generally works better.
  #coord_map(proj="azequidistant",orientation=c(90,180,20),xlim=c(-175,-158),ylim=c(55,60))
  #axis and legend label
  labs(x="Longitude (°W)",y="Latitude (°N)") +
  #lat/lon tick labels
  scale_y_continuous(breaks=c(54,56,58,60,62, 64)) +
  scale_x_continuous(breaks=c(-180, -175,-170,-165,-160)) +
  annotation_scale(text_cex = 1, line_width = 2, style = "ticks", location = "bl") +
  #blank legend background
  #lat/long graticule color and transparency
  theme(panel.background=element_rect(fill="transparent"),
        panel.grid=element_line(color="gray",size=0.5,linetype=1),
        axis.ticks=element_blank(),
        text = element_text(size = 20)) 


ggsave(stn, filename="images/BASIS_NBSIERP_Map.jpg",width=12,height=8,units=c("in"),dpi=300)


##NBSIERP Method
stn2 <- ggplot() +
  #alaska polygon
  geom_polygon(data=NBS,aes(x=long,y=lat,group=group),fill="grey") +
  #bathymetry
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-50),size=c(0.3),colour="black") +
  geom_contour (data=bathline,aes(x=x,y=y,z=z),breaks=c(-20),size=c(0.3),colour="black") +
  geom_contour(data=bathline,aes(x=x,y=y,z=z),breaks=c(-200),size=c(0.3),colour="black") +
  #station coordinates as points
  geom_point(data=stations, aes(x = Londd, y = Latdd, shape = DesignType,size=DesignType,color=DesignType),alpha=1) +
  #bathymetry labels
  annotate( geom = "text", x = -165.5, y = 59.5, label = "20m", 
            color = "black", size = 2 )+
  annotate( geom = "text", x = -166, y = 58.2, label = "50m", 
            color = "black", size = 2 )+
  annotate( geom = "text", x = -168.1, y = 56, label = "200m", 
            color = "black", size = 2 ) +
  scale_shape_manual(name = "Station type",values=c("CoreTrawl" = 17, "AdaptTrawl"= 2, "Ocean" = 18, "NBEST Station" = 8, "IERP Station" = 13),
                     limits=c("CoreTrawl","AdaptTrawl","Ocean","NBEST Station","IERP Station")) +
  scale_size_manual(name = "Station type", values =c("CoreTrawl" = 4, "AdaptTrawl"= 4, "Ocean" = 4, "NBEST Station" = 5, "IERP Station" = 5),
                    limits=c("CoreTrawl","AdaptTrawl","Ocean","NBEST Station","IERP Station")) + 
  scale_color_manual(name = "Station type", values =c("CoreTrawl" = "black", "AdaptTrawl"= "black", "Ocean" = "black", "NBEST Station" = "blue", "IERP Station" = "blue"),
                     limits=c("CoreTrawl","AdaptTrawl","Ocean","NBEST Station","IERP Station")) + 
  #projection and limits using coord_sf
  coord_sf(crs="EPSG:3476",default_crs=sf::st_crs(4326),xlim=c(-180,-158),ylim=c(54,64)) +
  ##alternatively you can use coord_map.  These both work for projections.  The below line is what i use for the CPUE maps and it generally works better.
  #coord_map(proj="azequidistant",orientation=c(90,180,20),xlim=c(-175,-158),ylim=c(55,60))
  #axis and legend label
  labs(x="Longitude (°W)",y="Latitude (°N)") +
  #lat/lon tick labels
  scale_y_continuous(breaks=c(54,56,58,60,62, 64)) +
  scale_x_continuous(breaks=c(-180, -175,-170,-165,-160)) +
  annotation_scale(text_cex = 1, line_width = 2, style = "ticks", location = "bl") +
  #blank legend background
  #lat/long graticule color and transparency
  theme(axis.ticks=element_blank()) +
  theme(panel.grid=element_line(color=rgb(235, 235, 235, 100, maxColorValue = 255),size=0.5,linetype=1)) +
  theme(panel.background=element_rect(fill="white"))

ggsave(stn2, filename="images/BASIS_NBSIERP_Map_NBSIERPstype.jpg",width=12,height=8,units=c("in"),dpi=300)
