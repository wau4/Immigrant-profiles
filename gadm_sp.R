# Get GADM rdata files

library('sp')  # alternative from 
# http://stackoverflow.com/questions/5126745/gadm-maps-cross-country-comparision-graphics

## load a file from GADM (you just have to specify the countries "special part" of the file name, like "ARG" for Argentina. Optionally you can specify which level you want to have
loadGADM <- function (fileName, level = 1, ...) {
  # load files downloaded from gadm
  load(paste(fileName, "_adm", level, ".RData", sep     = ""))
  gadm
}

## the maps objects get a prefix (like "ARG_" for Argentina)
changeGADMPrefix <- function (GADM, prefix) {
  GADM <- spChFIDs(GADM, paste(prefix, row.names(GADM), sep = "_"))
  GADM
}

## load file and change prefix
loadChangePrefix <- function (fileName, level = 0, ...) {
  theFile <- loadGADM(fileName, level)
  theFile <- changeGADMPrefix(theFile, fileName)
  theFile
}

## this function creates a SpatialPolygonsDataFrame that contains all maps you specify in "fileNames".
## E.g.: 
## spdf <- getCountries(c("ARG","BOL","CHL"))
## plot(spdf) # should draw a map with Brasil, Argentina and Chile on it.

getCountries <- function (fileNames, level = 0, ...) {
  polygon <- sapply(fileNames, loadChangePrefix, level)
  polyMap <- do.call("rbind", polygon)
  polyMap
}

# test:
# getCountries(c("MYS","MMR","THA"), level=1)