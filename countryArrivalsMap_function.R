

countryArrivals <- function(country = "Vietnam", 
                            visa = c("I", "R"), 
                            xlim = c(95,310), 
                            ylim = c(-10, 90), 
                            alpha = .1){

# Recenter data ----
  center <- 275 # positive values only - US centered view is 260
  
# shift coordinates to recenter great circles
  gcircles$long.recenter <-  ifelse(gcircles$long  < center - 180 , gcircles$long + 360, gcircles$long) 
  
visa.rows = which( gcircles$AlienType == visa) # test: use group instead of group.regroup
country.rows = which( gcircles$Country == country) # test: use group instead of group.regroup
country.visa.rows = which( gcircles$Country == country & gcircles$AlienType == visa) # test: use group instead of group.regroup
country.map = which( worldmap.cp$region == country)

# plot ----
require(ggplot2)

g= ggplot() +
  theme(panel.background = element_rect(fill='grey95',colour='grey95')) + 
  geom_polygon(aes(long.recenter, lat, group=group.regroup), 
               size = 0.2, 
               fill="#f9f9f9", 
               colour = "grey", 
               data=worldmap.cp
  ) +
  geom_polygon(aes(long.recenter,lat, group=group.regroup), 
                 size = 0.2, 
                 fill="White", 
                 colour = "grey35", 
                 alpha = .1 , 
                 data=states.cp
  ) +  
  geom_polygon(aes(long.recenter, lat, group=group.regroup),  # highlight country of origin
                   size = 0.2, 
                   fill="grey35", 
                   colour = "grey35", 
                   data=worldmap.cp[country.map,]
  ) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),  
              axis.ticks = element_blank(), 
              axis.title.x = element_blank(), axis.title.y = element_blank(), 
              axis.text.x = element_blank(), axis.text.y = element_blank()
              , legend.position = "none"
  ) +
  ylim( ylim ) + # zoom to area containing USA and country 
  xlim( xlim ) + # zoom to phililppines
  coord_equal()

routes = geom_line(aes(long.recenter,lat, 
                       group=group, # test: use group instead of group.regroup
                       colour=AlienType, 
                       type=Class), 
                   alpha= alpha, 
                   lineend="round", 
                   lwd=.1, 
                   data= gcircles[country.visa.rows,]  # test: use gcircles instead of gcircles.rg
                  )

# Create title
arrival.range = range(gcircles[country.visa.rows, "ArrivalDate"])
min.arr = format(arrival.range[1],"%m/%Y")
max.arr = format(arrival.range[2],"%m/%Y")
num.arr = format(length(unique(gcircles[country.visa.rows,"id"])))
title = sprintf("%s EDN Notifications from %s during %s to %s", num.arr, country, min.arr, max.arr)

g + 
  routes + 
  scale_color_manual(values=c("red", "blue")) +
  labs(title=title)

}

# TEST
#### retreive map data ----
load("worldmap.cp.RData")
load("states.cp.RData")
load("gcircles.RData") # test: use group instead of group.regroup

countryArrivals()
countryArrivals(country="Philippines")
countryArrivals(country="Nepal", visa="R", xlim=c(95,310) )
countryArrivals(country="Haiti", xlim = c(180,310) , ylim = c(10,75), alpha=.35 )
countryArrivals(country="Dominican Republic", visa="I", xlim = c(180,310) , ylim = c(10,75), alpha=.5 )
countryArrivals(country="Mexico", visa="I", xlim = c(180,310) , ylim = c(10,75), alpha=.5 )
countryArrivals(country="Cuba", xlim = c(180,310) , ylim = c(10,75), alpha=.35 )