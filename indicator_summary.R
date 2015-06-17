# indicators

library(readxl); library(dplyr); library(tidyr); 
library(ggplot2); library(knitr)

# choose directory

dir = 'TB Indicators 2013 Annual'; .year = 2013
dir = 'TB Indicators 2014 Annual'; .year = 2014

# retrieve list of files
if (.year==2013){
  files1 <- list.files(path=dir, pattern="*CDCcomplete.xls", full.names=T, recursive=FALSE)
  files2 <- list.files(path=dir, pattern="*REVISED.xls", full.names=T, recursive=FALSE)
  files3 <- list.files(path=dir, pattern="*RESOLUTION.xls", full.names=T, recursive=FALSE)
  files <- unique(c(files1, files2, files3))
} else {
  files <- list.files(path=dir, pattern="*.xls", full.names=T, recursive=FALSE)
}
# extract data

for (i in 1:length(files[1:length(files)])){
  x = files[i]
  
  t <- read_excel(x, sheet = 1)
  if (ncol(t)<5) next
  
  names(t)[1] <- "Indicator"
  data_start = which( tolower(t$Indicator) == 'indicator' ) 
  if (length(data_start) == 0) data_start <- 1
  t <- read_excel(x, sheet = 1, skip = data_start[1]) # reload file
  
  names(t)[1] <- "Indicator"
  names(t)[2:13] <- c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec")
  
  data_rows = which(!( 
       ( is.na(t[, 1] )  | t[,1] == 'error' | t[,1] == '' | t[,1] == 'Indicator' ) 
#        &
#             if (ncol(t)>13) (is.na(t[, 14] ) | t[14] == 0 ) else F
       ))
  
  if (length(data_rows)==0) next
  
  if (ncol(t)>13) {
        names(t)[14] = 'total'
        t = t[data_rows, 1:14]
        t[, 2:14] <- apply(t[, 2:14], 2, as.integer)
        t$total = apply(t[, 2:13], 1, FUN = sum, na.rm = T)
  } else {
       t[, 2:13] <- apply(t[, 2:13], 2, as.integer)
       t$total = apply(t[, 2:13], 1, FUN = sum, na.rm = T)
  }
  
  t$indicator_num = sapply(strsplit(t$Indicator, ".", fixed = TRUE), '[[', 1)
 
     # returns string w/o leading or trailing whitespace
     trim <- function (x) gsub("^\\s+|\\s+$", "", x)
     t$indicator_num = trim(t$indicator_num)  
  
  x =  gsub(pattern = "(.*/)(.*)(annual.*)",
               replacement = "\\2",
               x = x)
  
  panel = gsub(pattern = "(.*/)(.*)(Annual.*)",
               replacement = "\\2",
               x = x)
  
  year = .year
  
  cat(panel, year)
  
  df = data_frame(panel = panel, year = year) %>% cbind(t)
  
  if (i==1){ panels = df } else {
        panels = bind_rows(panels, df)
  }
 
}

# summarize

panels %>% count(panel) %>% View
panels %>% arrange(panel) %>% View
panels %>% count(Indicator) %>% View

str(panels)

# create database
library(RODBC)
db = odbcConnectAccess2007("indicators.accdb")
sqlDrop(db, paste0("panel", .year), errors = FALSE)
sqlSave(db, panels, tablename = paste0("panel", .year))
odbcCloseAll()

# remove duplicate sites...
if (.year == 2013) { dupe_panels = c("Bangladesh_Green Crescent_Hoq_", "Benin_Cabinet Medical_") }

smears = panels %>% 
     filter( !(panel %in% dupe_panels)) %>%
     filter(indicator_num %in% c('5b', '5c', '5h', '5d')) %>% 
     select(indicator_num, total, panel) %>% 
     spread(indicator_num, total) %>% 
     group_by(panel) %>%
     summarise(smears = sum(`5b`,`5c`,`5h`, na.rm=T) , 
            cultures = sum(`5b`,`5d`, na.rm=T)
            ) %>% 
     arrange(desc(smears)) 
smears
# simple plot

ggplot(smears, aes(x=smears, y=cultures)) + 
  geom_point()

# annotated plot

ggplot(smears, aes(x=smears, y=cultures, 
                   color = ifelse(smears>4 & cultures<2*smears, 'red', 
                                  ifelse(smears>4 & cultures>=2*smears, 'blue', 'black'))
)
       ) + 
  geom_point( ) +
  scale_y_log10(breaks = c(5,10,50,100,500)) + scale_x_log10( breaks = c(5,10,50,100,500)) +
  scale_color_manual(values = c('red' = 'red', 'black' = 'black', 'blue' = 'blue'), guide="none") +
  geom_text( aes( label = ifelse(smears>4 & cultures<2*smears, panel, 
                                 ifelse(smears>4 & cultures>=2*smears, panel, '')), hjust=-.1)  ) +
     geom_abline(intercept=log10(2), slope=1, color = "grey70") +
     annotate('text', x=70, y=25, 
              label="line represents points where \nculture > 2x smear", color = "grey50", hjust=1) +
  xlab("Number of Smear Positive Applicants (LOG10)") +
  ylab("Number of Culture Positive Applicant (LOG10)") +
  ggtitle( paste(.year, "TB Indicators")) +
  theme_set(theme_gray(base_size = 20))

ggsave(paste0("smearVculture", .year, ".pdf") )

# table
smears %>% filter(smears<5 ) %>% kable() 
