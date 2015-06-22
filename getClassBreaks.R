getClassBreaks = function (dataVector, catMethod = 'fixedWidth', numCats = 5, verbose = TRUE, midpoint = 0,
               round = FALSE, 
               effective.zero = 1  # everything less than this will be grouped with zero
) 
{
     # restrict data to being equal or above effective.zero
     if (effective.zero>0){
     dataVector = dataVector[dataVector >= effective.zero]}
     
     functionName <- as.character(sys.call()[[1]])
     catMethodList <- c("fixedWidth", "diverging", "quantiles", 
                        "pretty", "logFixedWidth", "categorical")
     if (!catMethod %in% catMethodList) {
          warning("classification method should be set to one of :", 
                  paste(catMethodList, ""), "\nsetting to fixedWidth as default\n")
          catMethod = "fixedWidth"
     }
     if (catMethod == "fixedWidth") {
          minVal <- min(dataVector, na.rm = TRUE)
          maxVal <- max(dataVector, na.rm = TRUE)
          
          ### add function to round the values (jp May 2015) ####
          if (round) {
              sig_digits = 1
              if (signif(maxVal, digits = sig_digits) >= maxVal) {
                    sig_digits = sig_digits + 1
                    maxVal = signif(maxVal, digits = sig_digits) } else {
                         if ( signif(maxVal, digits = sig_digits) >= maxVal){
                              sig_digits = sig_digits + 1
                              maxVal = signif(maxVal, digits = sig_digits) } else {
                                   if ( signif(maxVal, digits = sig_digits) >= maxVal){
                                     sig_digits = sig_digits + 1
                                     maxVal = signif(maxVal, digits= sig_digits) } else {
                         if ( signif(maxVal, digits = sig_digits) >= maxVal){
                               sig_digits = sig_digits + 1
                               maxVal = signif(maxVal, digits = sig_digits) }
                                        }}}
               }
          
          cutVector <- minVal + (((0:numCats)/numCats) * (maxVal - 
                                                               minVal))
          
          ### add function to round the values (jp May 2015) ####
          if (round) {
               cutVector[1:(length(cutVector)-1)] = signif(cutVector[1:(length(cutVector)-1)], 
                                                           digits = sig_digits )
               minVal <- min(dataVector, na.rm = TRUE)
               maxVal <- max(dataVector, na.rm = TRUE)
               cutVector[1] = minVal
               cutVector[max(length(cutVector))] = maxVal
          }
          
     }
     else if (catMethod == "diverging") {
          minVal <- min(dataVector, na.rm = TRUE)
          maxVal <- max(dataVector, na.rm = TRUE)
          above <- abs(maxVal - midpoint)
          below <- abs(midpoint - minVal)
          sideCats <- numCats/2
          interval <- max(c(above, below))/sideCats
          if (numCats%%2 == 0) {
               fromAbove <- midpoint + interval
               fromBelow <- midpoint - interval
          }
          else {
               fromAbove <- midpoint + interval/2
               fromBelow <- midpoint - interval/2
          }
          cutsAbove <- seq(from = fromAbove, to = midpoint + (sideCats * 
                                                                   interval), by = interval)
          cutsBelow <- seq(from = fromBelow, to = midpoint - (sideCats * 
                                                                   interval), by = -interval)
          if (numCats%%2 == 0) {
               cutVector <- c(rev(cutsBelow), midpoint, cutsAbove)
          }
          else {
               cutVector <- c(rev(cutsBelow), cutsAbove)
          }
     }
     else if (catMethod == "quantiles") {
          testNumCats <- numCats
          uniqueBreaksFlag <- FALSE
          while (uniqueBreaksFlag == FALSE && testNumCats > 0) {
               testQuantiles <- quantile(dataVector, probs = seq(0, 
                                                                 1, 1/testNumCats), na.rm = TRUE)
               if (length(testQuantiles) == length(unique(testQuantiles))) {
                    uniqueBreaksFlag <- TRUE
               }
               else {
                    testNumCats <- testNumCats - 1
               }
          }
          if (testNumCats != numCats && verbose) 
               message(paste("You asked for", numCats, "quantiles, only", 
                             testNumCats, "could be created in quantiles classification"))
          cutVector <- quantile(dataVector, probs = seq(0, 1, 1/testNumCats), 
                                na.rm = TRUE)
     }
     else if (catMethod == "pretty") {
          cutVector <- pretty(dataVector, n = numCats)
          
          # reset cut vector to minimum and maximum values of data.
          # use the round option to select whether or not to do this
          if (round){
            minVal <- min(dataVector, na.rm = TRUE)
            maxVal <- max(dataVector, na.rm = TRUE)
            cutVector[1] = minVal
            cutVector[max(length(cutVector))] = maxVal
          }
          
          actualNumberOfBreaks <- length(cutVector) - 1
          if (actualNumberOfBreaks != numCats && verbose) 
               message(paste("You asked for", numCats, "categories,", 
                             actualNumberOfBreaks, "were used due to pretty() classification"))
     }
     else if (catMethod == "logFixedWidth") {
          if (min(dataVector, na.rm = TRUE) < 0) {
               stop("negative values in your data cannot be classified using catMethod=logFixedWidth")
               return(FALSE)
          }
          else if (min(dataVector, na.rm = TRUE) == 0) {
               if (verbose) 
                    message("zero values are replaced with NA as they can't be logged in catMethod=logFixedWidth")
               dataVector[which(dataVector == 0)] <- NA
               dataVectorLogged <- log(dataVector)
          }
          else {
               dataVectorLogged <- log(dataVector)
          }
          minVal <- min(dataVectorLogged, na.rm = TRUE)
          maxVal <- max(dataVectorLogged, na.rm = TRUE)
          maxValNotLogged <- max(dataVector, na.rm = TRUE)
          cutVector <- minVal + (((0:numCats)/numCats) * (maxVal - 
                                                               minVal))
          cutVector <- exp(cutVector)
          cutVector[length(cutVector)] <- maxValNotLogged
          
          ### add function to round the values (jp Jan 2014) ####
          if (round) {
               log.cutVector = floor(log10(cutVector))
               cutVector = 
                    round( cutVector/(10^log.cutVector))*(10^log.cutVector)
               cutVector = round(cutVector/10)*10
               cutVector[1] = exp(minVal)  # for 1st value, keep min
               cutVector[length(cutVector)] = exp(maxVal)  # for last value, keep max
          }
     }
     if (length(catMethod) == 1 && catMethod == "categorical") {
          stop(functionName, " shouldn't be called when catMethod == 'categorical'")
          return(0)
     }
     else {
          return(cutVector)
     }
}