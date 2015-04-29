# another random change
# === to avoid 'out of memory' error, restart R session (session menu) and then.... 
# options(java.parameters = "-Xmx4g")
# xlcMemoryReport()
# xlcFreeMemory()

###Annual number of arrivals
# library(XLConnect)
# wb = "C:/Users/bzp3/Desktop/dna/Immigrant-profiles/immigrant-profile.xlsx"
# data = readWorksheetFromFile(wb, sheet = "VisaExamAgeSexEDN-DHS", 
#                                 startRow=5, header = TRUE)
# head(arrivals)
# save(arrivals, file = "arrivals.rda")


load("arrivals.all.rda")

AgeSexTornado <- function ( .country = "philippines",
                            .year = 2010,
                            .visa = c("LPR","Refugee/Asylee"),
                            arrival.data = arrivals.all,
                            text.scale=4,
                            .theme = NA
                            ) {

     # select Country and Year
       if ( !is.na(.country)){
            arrivals = arrival.data[arrival.data$Country.Name %in% toupper(.country),]
       } else {
            arrivals = arrival.data
       }
  
       .year = 2010
       if ( !is.na(.year)){
            arrivals = arrivals[arrivals$Year %in% paste("Calendar", .year),]
       } 
  
  # visa categories
#        table(arrivals$Visa.Type)
       
#        .visa = c("LPR","Refugee/Asylee")
       if ( !is.na(.visa[1])){
            arrivals = arrivals[arrivals$Visa.Type %in% .visa,]
       }
  
  # reverse order of age groups
       arrivals$age.group = factor(arrivals$Age.Group4.DHS.Data, 
                                   levels = rev(unique(arrivals$Age.Group4.DHS.Data))
                                   ) 
  
  # combine LPR and EDN counts
       arrivals$count = ifelse(is.na(arrivals$CYLPR),
                               arrivals$EDN,
                               arrivals$CYLPR)
  # text label for bars
        t = arrivals
  
       # calculate percent total and percent males.  
       library(plyr)
       t.sum = ddply(t, .(Visa.Type, age.group, Sex), summarise, 
                     count=sum(count))
       t.total = ddply(t.sum, .(Visa.Type, age.group), summarise, 
                     max.count=max(abs(count)), # for sizing of text
                     percent.male = round( 
                          sum(count * (Sex %in% "M"))*100/sum(abs(count)) 
                          ),
                     total = sum( abs(count) )
                          )
       t = merge(t.sum, t.total, all.x = TRUE)
       t$percent.total = round( t$total *100  / sum(abs(t$count)) ) 
       t = ddply(t, .(Visa.Type), transform, 
                  visa.percent = round(100*abs(total)/sum(abs(count))) 
                  )
  # remove 'unknown' age group and sex
       t = t[!(t$age.group == "Unknown" | t$Sex == "Unknown") ,]
  
  # organize sex data
       t$Sex = factor(t$Sex)
       # convert female totals to negative numbers
       t$count = ifelse(t$Sex %in% 'F', -1*t$count, 
                               t$count)  
     # scale text by number of visa types shown in facet
       num.visa = length(unique(t$Visa.Type))
       t$text.scale = t$total/(text.scale*num.visa)
       
       
  # tornado plot
  library(ggplot2)
  library(scales)
       

  
       ggTornado = 
       ggplot(t, aes(x=age.group, y=count, fill=Sex)) + 
            geom_bar(stat="identity", position="identity", width = .95) + 
            
#             # Percent total label  # show value on female side only
            geom_text(aes(label = ifelse(Sex %in% "F" ,   
                                         paste(" ", visa.percent, "% ", sep=""),
                                         ""),
                          size=text.scale,
                          y=count,
                          hjust = ifelse(visa.percent>20, 0, 1),
                          color = ifelse(visa.percent>20, 
                                         "white", "black")
                          ),
                      vjust=0
                      ) +
            geom_text(aes(label = ifelse(Sex %in% "F" & visa.percent>20,
                                         " of total",
                                         ""),
                          size=text.scale/4,
                          y=count,
                          hjust = ifelse(visa.percent>20, 0, 1),
                          color = ifelse(visa.percent>20, 
                                         "white", "black")
                          ),
                      vjust=1 ) +
            
            # Percent male label  # show value on male side only
            geom_text(aes(
                 label = ifelse(Sex %in% "M" , 
                                         paste(" ", percent.male, "% ", sep=""),
                                         ""
                                         ),
                         hjust = ifelse(visa.percent>33, 1, 0),
                          vjust=0 ,
                          y=count,
                          size=text.scale
                          ), 
                      color="black") +
            geom_text(aes(label = ifelse(Sex %in% "M" , 
                                         " male  ",
                                         ""
                                         ),
                          hjust = ifelse(visa.percent>33, 1, 0),
                          vjust= 1,
                          y=count,
                          size= text.scale/4
                          ), 
                      color="black") +
            
            
            scale_size_continuous(range=c(4,15), guide=FALSE) + 
            scale_color_manual(values=c("black", "white"), guide=FALSE) +
            scale_y_continuous(label=comma) + #  comma format for populations
            scale_fill_hue(l=40) + 
            xlab("Age\n") +
            ylab("\nPopulation") + 
            coord_flip() + 
            theme( axis.title.x = element_text(face="bold", size=20),
                   axis.title.y = element_text(face="bold", size=20),
                   axis.text.x = element_text(face="bold", size=14),
                   axis.text.y = element_text(face="bold", size=14),
                   panel.background =  element_rect(fill = NA, colour = NA), 
                   panel.border =      element_rect(fill = NA, colour=NA), 
                   panel.grid.major =  element_line(colour = NA, size = 0.2),
                   panel.grid.minor =  element_line(colour = NA, size = 0.5)
            ) 
  
     # Facet by visa type
     #        print(head(t))
#        print(num.visa)
       if (num.visa>1){
            plot = ggTornado + 
                 facet_grid(Visa.Type ~ .)
       } else { plot = ggTornado }
       if (.theme %in% "tufte"){
            plot  + scale_fill_hue(l=70)
       } else {
               plot + theme_bw() 
       }
       
     }

