
# === to avoid 'out of memory' error, restart R session (session menu) and then.... 
# options(java.parameters = "-Xmx4g")
# xlcMemoryReport()
# xlcFreeMemory()

library(XLConnect)

###Annual number of arrivals
wb = "C:/Users/bzp3/Desktop/dna/Immigrant-profiles/immigrant-profile.xlsx"
arrivals.all = readWorksheetFromFile(wb, sheet = "VisaExamAgeSexEDN-DHS", 
                                startRow=5, header = TRUE)
head(arrivals)
save(arrivals, file = "arrivals.rda")

# select Country and Year
     .country = "philippines"
     if ( !is.na(.country)){
          arrivals = arrivals.all[arrivals.all$Country.Name %in% toupper(.country),]
     }

     .year = 2010
     if ( !is.na(.year)){
          arrivals = arrivals[arrivals$Year %in% paste("Calendar", .year),]
     } 

# visa categories
     table(arrivals$Visa.Type)
     
     .visa = c("LPR","Refugee/Asylee")
     if ( !is.na(.year)){
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
     t.sum = ddply(t, .(Visa.Type, age.group), summarise, 
                   max.count=max(abs(count)), # for sizing of text
                   percent.male = round( 
                        sum(count * (Sex %in% "M"))*100/sum(abs(count)) 
                        ),
                   total = sum( abs(count) )
                        )
     t = merge(t, t.sum, all.x = TRUE)
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
     
# tornado plot
library(ggplot2)
library(scales)

     ggTornado = 
     ggplot(t, aes(x=age.group, y=count, fill=Sex)) + 
          geom_bar(stat="identity", position="identity", width = .95) + 
          geom_text(aes(label = ifelse(Sex %in% "F" & visa.percent>9, 
                                       # show value on female side only
                                       paste(" ", visa.percent, "% ", sep=""),
                                       ""),
                        size=t$max.count,
                        y=count),
                    hjust=0,
                    vjust=0,
                    color="white") +
          geom_text(aes(label = ifelse(Sex %in% "F" & visa.percent>9,
                                       # show value on female side only
                                       "  total", ""),
                        size=t$max.count/2,
                        y=count),
                    hjust=0,
                    vjust=1,
                    color="white") +
          geom_text(aes(label = ifelse(Sex %in% "M" & visa.percent>9, 
                                       # show value on male side only
                                       paste(percent.male, "% ", sep=""),
                                       ""
                                       ),
                        hjust = 1,
                        vjust=0 ,
                        lineheight=1 ,
                        y=count,
                        size=t$max.count/2
                        ), 
                    color="black") +
          geom_text(aes(label = ifelse(Sex %in% "M" & visa.percent>9, 
                                       # show value on male side only
                                       "male  ",
                                       ""
                                       ),
                        hjust = 1,
                        vjust= 1,
                        y=count,
                        size=t$max.count/3
                        ), 
                    color="black") +
          scale_size_continuous(range=c(2,15), guide=FALSE) + 
          scale_y_continuous(label=comma) + #  comma format for populations
          scale_fill_hue(l=40) + 
          xlab("Age") +
          ylab("Population") + 
          coord_flip() + 
          theme( axis.title.x = element_text(face="bold", size=20),
                 axis.title.y = element_text(face="bold", size=20),
                 axis.text.x = element_text(face="bold", size=18),
                 axis.text.y = element_text(face="bold", size=18)
                 ) 

# Facet by visa type
     ggTornado + facet_grid(.~Visa.Type, scale="free")

