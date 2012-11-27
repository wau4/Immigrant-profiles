#### retreive map data ----
load("worldmap.cp.RData")
load("states.cp.RData")
load("gcircles.rg.RData")

# Define elements to display
country = "Vietnam"
visa = "I"

visa.rows = which( gcircles.rg$AlienType == visa)
country.rows = which( gcircles.rg$Country == country)
country.visa.rows = which( gcircles.rg$Country == country & gcircles.rg$AlienType == visa)
country.map = which( worldmap.cp$region == country)

# plot ----
require(ggplot2)
plotStartTime = date()

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
               alpha=.1, 
               data=states.cp
               ) +  
  geom_polygon(aes(long.recenter, lat, group=group.regroup),  # highlight country of origin
                size = 0.2, 
                fill="#f9f9f9", 
                colour = "grey35", 
                data=worldmap.cp[country.map,]
                 ) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(),  
        axis.ticks = element_blank(), 
        axis.title.x = element_blank(), axis.title.y = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank()
        , legend.position = "none"
  ) +
    ylim(-10, 90) + # zoom to phililppines (default is (-60, 90))
    xlim(95,310) + # zoom to phililppines
    coord_equal()

routes = geom_line(aes(long.recenter,lat, 
                       group=group.regroup, 
                       colour=AlienType, 
                       type=Class), 
                   alpha=.25, 
                   lineend="round", 
                   lwd=.1, 
                   data= gcircles.rg[country.visa.rows,]  # test: use gcircles instead of gcircles.rg
)  
g + routes

#  How long did it take to run? ----
plotStopTime = date()
plotStartTime
plotStopTime