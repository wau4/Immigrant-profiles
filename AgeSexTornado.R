
# options(java.parameters = "-Xmx4g" ) # May run out of memory allocated for java (default is .5Gb)
# gc()  # clear memory--garbage collector

library(XLConnect)
wb <-loadWorkbook("C:/Users/bzp3/Desktop/dna/Immigrant-profiles/immigrant-profile.xlsx")

###Annual number of arrivals
sex.age <- readWorksheet (wb, sheet = "VisaExamAgeSexEDN-DHS", startRow=5, startCol=1, header = TRUE)

# tornado plot
library(ggplot2)

# country = "Haiti"

tornado = sex.age[sex.age$Year == "Calendar 2010" & sex.age$Sex %in% c("F","M"),]
tornado$Sex = factor(tornado$Sex)
tornado$EDN = ifelse(tornado$Sex %in% 'F', -1*tornado$EDN, tornado$EDN)  # convert female totals to negative numbers
tornado$CYLPR = ifelse(tornado$Sex %in% 'F', -1*tornado$CYLPR, tornado$CYLPR)  # convert female totals to negative numbers

# remove unwanted visa categories
tornado = tornado[which(!tornado$Visa.Type %in% c("VISITOR","Nonimmigrant", "Unknown", "Other")),] # remove visitors 
# tornado = tornado[!is.na(tornado$Visa.Type),]

# reverse order of age groups
tornado$age.group = factor(tornado$Age.Group4.DHS.Data, levels = rev(unique(tornado$Age.Group4.DHS.Data))) 

# EDN bars
ggplot(tornado, aes(x=age.group, y=EDN, fill=Sex)) + 
     geom_bar(stat="identity", position="identity") +  # error if stat= not included.  don't know why?!
     coord_flip() + 
     scale_fill_hue(l=40) + 
     xlab("Age") + theme(axis.title.y = element_text(face="bold", size=20)) +
     ylab("Population") + theme(axis.title.x = element_text(face="bold", size=20)) 

# text label in bar--refugee only
t = tornado[tornado$Visa.Type %in% "Refugee/Asylee",]
library(plyr)
t.sum = ddply(t, .(age.group), summarise, 
              max.edn=max(abs(EDN)), 
              percent.male = round( sum(EDN * (Sex %in% "M"))*100/sum(abs(EDN)) ),
              total = sum( abs(EDN) )
                   )
t = merge(t, t.sum, all.x = TRUE)
t[t$Sex %in% "F", "percent.male"] = NA
t$percent.total = round( t$total *100  / sum(abs(t$EDN)) ) 

library(scales)
ggplot(t, aes(x=age.group, y=EDN, fill=Sex)) + 
     geom_bar(stat="identity", position="identity", width = .95) + 
     geom_text(aes(label = ifelse(Sex %in% "F",
                                  paste(" ", percent.total, "% ", 
                                        sep=""),
                                  ""),
                   size=t$max.edn,
                   y=EDN),
               hjust=0,
               vjust=0,
               color="white") +
     geom_text(aes(label = ifelse(Sex %in% "F", "  total", ""),
                   size=t$max.edn/2,
                   y=EDN),
               hjust=0,
               vjust=1,
               color="white") +
     geom_text(aes(label = ifelse(!is.na(percent.male),
                                  paste(percent.male, "% ", sep=""),
                                  ""
                                  ),
                   hjust = 1,
                   lineheight=1 ,
                   y=EDN,
                   size=t$max.edn/2
                   ), 
               color="black") +
     geom_text(aes(label = ifelse(!is.na(percent.male),
                                  "male",
                                  ""
                                  ),
                   hjust = 1,
                   vjust= 1,
                   y=EDN,
                   size=t$max.edn/4
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
            axis.text.y = element_text(face="bold", size=18 )
            ) 

# Facet by visa type
ggplot(tornado, aes(x=age.group, y=EDN, fill=Sex)) + 
     geom_bar(stat="identity", position="identity") +  # error if stat= not included.  don't know why?!
     coord_flip() + 
     scale_fill_hue(l=40) + 
     xlab("Age") + theme(axis.title.y = element_text(face="bold", size=20)) +
     ylab("Population") + theme(axis.title.x = element_text(face="bold", size=20)) + 
     facet_grid(.~Visa.Type, scale="free")

# dot plot
tornado2=tornado
tornado2$EDN = ifelse(tornado2$Sex %in% 'F', -1*tornado2$EDN, tornado2$EDN)  # convert female back to positive numbers
tornado2$CYLPR = ifelse(tornado2$Sex %in% 'F', -1*tornado2$CYLPR, tornado2$CYLPR) 
tornado2$CYLPR = -1*tornado2$CYLPR  # convert lpr totals to negative numbers

ggplot(tornado2, aes(x=age.group, y=EDN, fill=Sex, color=Sex)) + 
     geom_point(stat="identity", position="jitter", size=3) +  # error if stat= not included.  don't know why?!
     coord_flip() + 
     scale_fill_hue(l=40) + 
     xlab("Age") + theme(axis.title.y = element_text(face="bold", size=20)) +
     ylab("Population") + theme(axis.title.x = element_text(face="bold", size=20)) + 
     facet_grid(.~Visa.Type, scale="free")
