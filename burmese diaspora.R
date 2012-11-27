
source("gadm_sp.R")
# maps = getCountries(c("MYS","MMR","THA"), level=1)
malaysia = getCountries(c("MYS"), level=1)
thailand = getCountries(c("THA"), level=1)
myanmar = getCountries(c("MMR"), level=1)


# Simplify these using the gsimplify function from the rgeos package
library(rgeos)
# simp<- gSimplify(maps, 10, topologyPreserve=T)
tolerance = .1
malaysia<- gSimplify(malaysia, tolerance, topologyPreserve=T)
thailand<- gSimplify(thailand, tolerance, topologyPreserve=T)
myanmar<- gSimplify(myanmar, tolerance, topologyPreserve=T)

# GET CENTROIDS
library(sp)
# Myanmar
myanmar.centroids = coordinates(myanmar)
myanmar.c = as.data.frame(myanmar.centroids)
# create column based on rownames for admin label
myanmar.c$region = substr(rownames(myanmar.c), 5, length(rownames(myanmar.c)))
# myanmar.c$region[c(3,5,6,10)]= c("Chin", "karenni", "Karin", "Rohinga")
# myanmar.c = myanmar.c[c(3,5,6,10),]

# malaysia
malaysia.centroids = coordinates(malaysia)
malaysia.c = as.data.frame(malaysia.centroids)
malaysia.c$region = substr(rownames(malaysia.c), 5, length(rownames(malaysia.c))) # created column based on rownames for admin label
# malaysia.c$region[c(12)]= c("Kuala Lumpur")
# malaysia.c = malaysia.c[c(12),]

# thailand
thailand.centroids = coordinates(thailand)
thailand.c = as.data.frame(thailand.centroids)
thailand.c$region = substr(rownames(thailand.c), 5, length(rownames(thailand.c))) # created column based on rownames for admin label
# thailand.c$region[c(17,70,15,52)]= c("Mae Hon Son", "Tak", "Katchiburi", "Ratchiburi")
# thailand.c = thailand.c[c(17,70,15,52),]



# fortify data--i.e. make it usable by ggplot
library(ggplot2)
myanmar = fortify(myanmar)
thailand = fortify(thailand)
malaysia = fortify(malaysia)

# world = map_data("world")
# Get country boundaries and then highlight selected states

g = ggplot( ) 
# g = g + geom_point(aes(x=lon, y=lat, color=dn) )
# g = g + scale_colour_gradient(low="black", high="white", trans="sqrt")
g = g + theme_bw() + xlim(c(90,108)) 
g = g + theme( axis.text.y = element_blank() )
# g = g + theme( axis.text.x = element_blank(), axis.text.y = element_blank() )
g = g + geom_polygon(data= myanmar, aes(x=long,y=lat,group=group), fill="red", color="white", alpha=0.5)
g = g + geom_polygon(data= thailand, aes(x=long,y=lat,group=group), fill="blue", color="white", alpha=0.5)
g = g + geom_polygon(data= malaysia, aes(x=long,y=lat,group=group), fill="green", color="white", alpha=0.5)
g = g + geom_text(data= myanmar.c, aes(label = region, x=V1, y=V2), size=3)
g = g + geom_text(data= thailand.c, aes(label = region, x=V1, y=V2), size=3)
g = g + geom_text(data= malaysia.c, aes(label = region, x=V1, y=V2), size=3)

#record time to execute--higher tolerance will decrease time
system.time(print(g))

# ggsave("burmese_diaspora_draft.png")

