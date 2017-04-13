rm(list=ls())
#setwd('/Users/johnlloyd/GitHub/StateOfTheMountainBirds')

dfmap<-read.csv('WTSP_mapdata.csv',header=T,na.strings = "NA",stringsAsFactors=TRUE)

str(dfmap)
par(mfrow=c(1,1),mar=c(7,7,3,2)+0.1,cex.axis=1.1,cex.lab=1)
options(stringsAsFactors = T) ## need to run this --- weird ggplot bug=!
library(RColorBrewer)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(rgdal)
mapdat <- map_data('state',stringsAsFactors=TRUE)
states <- map_data("state",stringsAsFactors=TRUE)
NewEngland <- subset(states, region %in% c("maine", "vermont", "new york","new hampshire"))

Neighborhoods <- readOGR(".","NewEnglandBoundaries")
Neighborhoods <- spTransform(Neighborhoods, CRS("+proj=longlat +datum=WGS84"))
Neighborhoods <- fortify(Neighborhoods)

WTSP.MBW.map = get_map(location = c(-72,44), color="color",maptype="satellite",source="google",zoom=5)

scale_x_longitude <- function(xmin=-180, xmax=180, step=1, ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(lapply(xbreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*W")), ifelse(x > 0, parse(text=paste0(x,"^o", "*E")),x))))
  return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}
scale_y_latitude <- function(ymin=-90, ymax=90, step=0.5, ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(lapply(ybreaks, function(x) ifelse(x < 0, parse(text=paste0(x,"^o", "*S")), ifelse(x > 0, parse(text=paste0(x,"^o", "*N")),x))))
  return(scale_y_continuous("Latitude", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
}   
q <- ggmap(WTSP.MBW.map) 
q <- q + 
  geom_point(data=dfmap, size=2, aes(y=Latitude, x=Longitude, color=Max.Count)) + 
  geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='darkolivegreen2', data=Neighborhoods, alpha=0) + 
  scale_x_longitude(xmin=-69, xmax=-75, step=-2) +
  scale_y_latitude(ymin=42, ymax=46, step=2) + 
  labs(y="Latitude", x="Longitude") +
  theme(axis.title.x=element_text(size=rel(2),color="ROYALBLUE"),
        axis.text.x = element_text(size = rel(1.5))) + coord_map(projection="mercator",xlim=c(-76.9, -66.9), ylim=c(40.8,47.8)) +
  theme(axis.title.y=element_text(size=rel(2),color="ROYALBLUE",angle = 90),
        axis.text.y = element_text(size = rel(1.5)))  + 
  scale_color_gradientn(colours = rev(heat.colors(n = 6)), name = "Maximum\ncount")
q

ggsave (filename = "WTSP_Counts.tiff", dpi = 200,device="tiff")


# I found this to be an easy way to get color names from brew.pal palettes

brewer.pal(n = 6, name = "YlOrRd")
#palette(brewer.pal(n = 8, name = "Set2"))



