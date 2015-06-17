# from: http://rud.is/b/2015/05/14/geojson-hexagonal-statebins-in-r/

# download us_states_hexgrid.geojson 
# from https://team.cartodb.com/u/andrew/tables/andrew.us_states_hexgrid/public/map

# Here’s a way to use the GeoJSON version in R. I like GeoJSON since it’s a single file vs a directory of files and is readable vs binary. If you’re in a TL;DR hurry, you can just review the code in this gist. Read on for expository.
# When you download the GeoJSON, it should be in a file called us_states_hexgrid.geojson. We can see what’s in there with R pretty easily:

  
hexbin_map <- function (fill_data = data, 
                        fill_data_id = "state.abb",
                        legend_title= "Persons",
                        text_size = 4) {
  library(rgeos)
  library(rgdal)
   
  ogrInfo("us_states_hexgrid.geojson", "OGRGeoJSON")
  
  us <- readOGR("us_states_hexgrid.geojson", "OGRGeoJSON")
  
  centers <- cbind.data.frame(data.frame(gCentroid(us, byid=TRUE), id=us@data$iso3166_2))
  
  library(ggplot2)
  library(dplyr)
  
  us_hex <- fortify(us, region="iso3166_2")
  
  fill_data = fill_data %>% filter(!is.na(id))
  
  source("getClassBreaks.R")
  breaks = getClassBreaks(fill_data$count, catMethod = "fixedWidth", 
                          numCats = 4, round = TRUE)
  breaks = unique(breaks)
  
  fill_data = fill_data %>% 
       mutate(fill = cut(count, breaks = breaks, dig.lab=10, right = TRUE) )
  
  
  gg <- ggplot() +
     # base map
     geom_map(data=us_hex, map=us_hex,
                      aes(x=long, y=lat, map_id=id),
                      color="grey90", fill = "white", size=0.5) + 
     # optional fill
        geom_map(data=fill_data, map=us_hex, 
                 # choropleth data
                 aes(fill=fill, map_id=id), color = "grey90") +
     # color palette
        scale_fill_brewer(legend_title, palette=2, drop = FALSE) + # 3 is blue, 8 is orange
     # remove white stripe in legend
        guides(fill = guide_legend(override.aes = list(colour = NA))) +
     # label states
     geom_text(data=centers, aes(label=id, x=x, y=y), color="grey60", size= text_size) + 
     # scale chart by dimension of map, not viewer
     coord_map() + 
     # clean themes
     labs(x=NULL, y=NULL) + 
     theme_bw() + 
     theme(
        panel.border=element_blank(),
        panel.grid=element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank()
                       )
  gg
}

# example

# load("edn.geo.rda")
#   str(edn.geo)
# 
# library(lubridate)  
# library(dplyr)
# 
#   data <- edn.geo %>% 
#        mutate( year = as.integer(year(ArrivalDate)) ) %>%
#        filter( year == 2014) %>% 
#        filter(ClassB == 1) %>% 
#        filter( AlienType == 'I' ) %>%
#        count( State)
#        
#   
#   names(data) = c("id","count")
#   sum(data$count)
#   
#   
# hexbin_map()
