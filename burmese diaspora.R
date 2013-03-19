
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
myanmar.c$StateID = substr(rownames(myanmar.c), 5, length(rownames(myanmar.c)))
myanmar.c$State[c(3,4,5,6,9,10,12,14)]= c("Chin", "Kachin", "karenni", "Karen", "Mon", "Rakhaing", "Shan", "Yangoon")
myanmar.c = myanmar.c[c(3,4,5,6,9,10,12,14),]

# malaysia
malaysia.centroids = coordinates(malaysia)
malaysia.c = as.data.frame(malaysia.centroids)
malaysia.c$StateID = substr(rownames(malaysia.c), 5, length(rownames(malaysia.c))) 
malaysia.c$State[c(12)]= c("Kuala Lumpur")
malaysia.c = malaysia.c[c(12),]

# thailand
thailand.centroids = coordinates(thailand)
thailand.c = as.data.frame(thailand.centroids)
thailand.c$StateID = substr(rownames(thailand.c), 5, length(rownames(thailand.c))) 
thailand.c$State[c(22,70,15,52)]= c("Mae Hong Son", "Tak", "Kanchana Buri", "Ratchaburi")
thailand.c = thailand.c[c(22,70,15,52),]



# fortify data--i.e. make it usable by ggplot
library(ggplot2)
myanmar.fortify = fortify(myanmar)
thailand.fortify = fortify(thailand)
malaysia.fortify = fortify(malaysia)

# world = map_data("world")
# Get country boundaries and then highlight selected states

g = ggplot( ) 
# g = g + geom_point(aes(x=lon, y=lat, color=dn) )
# g = g + scale_colour_gradient(low="black", high="white", trans="sqrt")
g = g + theme_bw() + xlim(c(90,108)) 
g = g + theme( axis.text.y = element_blank() )
# g = g + theme( axis.text.x = element_blank(), axis.text.y = element_blank() )
g = g + geom_polygon(data= myanmar.fortify, aes(x=long,y=lat,group=group), fill="red", color="white", alpha=0.5)
g = g + geom_polygon(data= thailand.fortify, aes(x=long,y=lat,group=group), fill="blue", color="white", alpha=0.5)
g = g + geom_polygon(data= malaysia.fortify, aes(x=long,y=lat,group=group), fill="green", color="white", alpha=0.5)
g = g + geom_text(data= myanmar.c, aes(label = state, x=V1, y=V2), size=3)
g = g + geom_text(data= thailand.c, aes(label = state, x=V1, y=V2), size=3)
g = g + geom_text(data= malaysia.c, aes(label = state, x=V1, y=V2), size=3)

#record time to execute--higher tolerance will decrease time
# system.time(print(g))

# ggsave("burmese_diaspora_draft.png")

# add routes
# calculate routes 
library(geosphere)
rts <- gcIntermediate( as.vector(myanmar.c[,c("V1", "V2")]), 
                       as.vector(malaysia.c[,c("V1", "V2")]), 
                       50, breakAtDateLine=FALSE, addStartEnd=TRUE) 
rts.df = data.frame(StateID = rowname)
library(plyr)
source ("fortify-spatial.r")
rts.ff <- fortify.SpatialLinesDataFrame(rts) # convert into something ggplot can plot
rts.ff = merge(rts, myanmar.c, all.x=True) # merge to get State  ????Merge rts.ff(jp jan 15, '12)
state.origin$state.percent = state.origin$Refugees / sum(state.origin$Refugees)
rts.ff = merge(rts.ff, state.origin, )
routes = geom_path(aes(long,lat, 
                       group=State, 
                       colour= State), 
                   alpha= 1, 
                   data= rts.ff  # test: use gcircles instead of gcircles.rg
)
ggplot() + routes
g + routes
