
options(java.parameters = "-Xmx4g" ) # May run out of memory allocated for java (default is .5Gb)
gc()  # clear memory--garbage collector

library(XLConnect)
wb <-loadWorkbook("C:/Users/bzp3/Desktop/dna/Immigrant-profiles/immigrant-profile.xlsx")

###Annual number of arrivals
sex.age <- readWorksheet (wb, sheet = "VisaExamAgeSexEDN-DHS", startRow=5, startCol=1, header = TRUE)

# tornado plot
library(ggplot2)

# country = "Haiti"

tornado = sex.age[sex.age$Year == "Calendar 2010" and sex.age$Sex %in% c("F","M"),]
tornado$Sex = factor(tornado$Sex)
tornado$EDN = ifelse(tornado$Sex=='F', -1*tornado$EDN, tornado$EDN)  # convert female totals to negative numbers
tornado$CYLPR = ifelse(tornado$Sex=='F', -1*tornado$CYLPR, tornado$CYLPR)  # convert female totals to negative numbers


# EDN
ggplot(tornado, aes(x=Age.Group4.DHS.Data, y=EDN, fill=factor(Visa.Type))) + 
     geom_bar(stat="identity") +  # error if stat= not included.  don't know why?!
     coord_flip() + scale_fill_hue(l=40) + 
     xlab("Age") + theme(axis.title.y = element_text(face="bold", size=20)) +
     ylab("Sex") + theme(axis.title.x = element_text(face="bold", size=20)) + facet_grid(.~Sex, scale="free")


#DHS

tornado = tornado[!tornado$Visa.Type %in% c("VISITOR","Nonimmigrant", "Unknown"),] # remove visitors 

ggplot(tornado, aes(x=Age.Group4.DHS.Data, y=CYLPR, fill=Visa.Type)) + 
     geom_bar(stat="identity", position="identity") +  # error if stat= not included.  don't know why?!
     coord_flip() + scale_fill_hue(l=40) +
     xlab("Age") + theme(axis.title.y = element_text(face="bold", size=20)) +
     ylab("Sex") + theme(axis.title.x = element_text(face="bold", size=20))




