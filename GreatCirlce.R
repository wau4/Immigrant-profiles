# http://spatialanalysis.co.uk/2012/06/mapping-worlds-biggest-airlines/

library(ggplot2)
library(maps)
library(maptools)
library(rgeos)
gpclibPermit()

# Load in your great circles 
# see http://www.stanford.edu/~cengel/cgi-bin/anthrospace/great-circles-on-a-recentered-worldmap-in-ggplot 
# You need a file that has long, lat, airline and group. 
# The group variable is produced as part of the Anthrospace tutorial.
gcircles<- read.csv("flights_gcircles.csv")

# Get a world map
worldmap <- map_data ("world")

# Load in your urban areas shapefile from Natural Earth Data
urbanareasin<- readShapePoly("10m-urban-area/10m_urban_areas.shp")

# Simplify these using the gsimplify function from the rgeos package
simp<- gSimplify(urbanareasin, 10, topologyPreserve=T)

# Fortify them for use with ggplot2
urbanareas<-fortify(simp)

# This step removes the axes labels etc when called in the plot.
xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL, limits=c(70,-70))
quiet<-list(xquiet, yquiet)

# Create a base plot
base<- ggplot(worldmap, aes(x = long, y = lat))

# Then build up the layers
wrld<-c(geom_polygon(aes(long,lat,group=group), size = 0.1, colour= "#090D2A", fill="#090D2A", alpha=1, data=worldmap))
# urb<- c(geom_polygon(aes(long,lat,group=group), size = 0.1, colour= "#FCFFF1", fill="#FCFFF1", alpha=1, data=urbanareas))

# Bring it all together with the great circles
base + wrld + urb + coord_equal()+ quiet
opts(panel.background = theme_rect(fill='#00001C',colour='#00001C'))
+geom_path(data=gcircles, aes(long,lat, group=group, colour=airline),alpha=0.1, lineend="round",lwd=0.1)