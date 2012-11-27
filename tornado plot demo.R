# test tornado plot
library(ggplot2)

dat <- data.frame(
     variable=c("A","B","A","B"),
     Level=c("Top-2","Top-2","Bottom-2","Bottom-2"),
     value=c(.2,.3,-.2,-.3)
)

ggplot(dat, aes(variable, value, fill=Level)) + geom_bar(position="dodge")
## plots offset, as expected


# tornado
ggplot(dat, aes(variable, value, fill=Level)) + 
     geom_bar(position="identity") +
     coord_flip()


#  Adapt for our data....Age by Sex
Agegroups = c("15-24","25-34","35-44","45-54","55-64","65+")

dat2 <- data.frame(
     Age=c("15-24","25-34","35-44","45-54","55-64","65+", "15-24","25-34","35-44","45-54","55-64","65+") ,
     # ?rep(Agegroups, 2),  # repeat ages for each level of sex
     Sex= c("Male","Female","Male","Female","Male","Female","Male","Female","Male","Female", "Male", "Female"),  
     # rep(c("Male","Female"), length(Agegroups))?
     value=c(.2,.3,.4,.1,.05, .04, -.24,-.28,-.29,-.12,-.06, -.03)
)

ggplot(dat2, aes(x=Age, y=value, fill=Sex)) + 
     geom_bar(stat="identity", position="identity") +  # error if stat = not included.  don't know why?!
     coord_flip()

# add vertical line at zero.  
# fix colors and labels

   

