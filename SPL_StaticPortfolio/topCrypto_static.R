if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
if(!require("stringi")) install.packages("stringi"); library("stringi")
if(!require("rdrop2")) install.packages("rdrop2"); library("rdrop2")

if(!require("RCurl")) install.packages("RCurl"); library("RCurl")
if(!require("jsonlite")) install.packages("jsonlite"); library("jsonlite")


token <- readRDS("./token.rds")


topCrypto <- function(startDate =  as.Date("2016-01-01"), endDate = (Sys.Date() - 3), trainingDay = (Sys.Date() - 120), mcDay, numberofConst, method, averageFrom, averageTo, ponder){
  
  
  
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)
  trainingDay <- as.Date(trainingDay)
  
  temp0 <- tempdir()
  drop_download("/SPL.WS1819/bigFileList.RData", local_path = temp0, overwrite = TRUE, dtoken = token)
  load(paste0(temp0, "/bigFileList.RData"))
  coin.Names <- array(names(bigFileList))
  
  if(!is.numeric(numberofConst) || numberofConst <= 0 || numberofConst > length(coin.Names)){
    stop("Number of constituents misspecified")
  }
  
  # Method: Day ------
  
  if (method == "day"){
    mcDay <- as.Date(mcDay)
    if (mcDay > trainingDay | mcDay < startDate | numberofConst > length(coin.Names)){
      stop("Error: parameters misspecified")  
    } 
    
    
    
    marketcap.Crypto <- data.frame("crypto" = gsub(pattern = "\\.csv", replacement =  "", coin.Names), "marketcap" = NA, stringsAsFactors = FALSE)
    
    for (crypt0 in coin.Names){
      df1 <- bigFileList[[crypt0]]
      mc <- df1$marketCap[df1$date == mcDay]
      marketcap.Crypto$marketcap[marketcap.Crypto$crypto == crypt0] <- mc
      
    }
    marketcap.Crypto$marketcap <- as.numeric(marketcap.Crypto$marketcap)
    omc <- order(marketcap.Crypto$marketcap, decreasing = TRUE)
    ind <- omc[1:numberofConst]
    ind <- sort.default(ind)
    top <- marketcap.Crypto[ind, ]
    row.names(top) <- seq(1:numberofConst)
    
    return(top)
    
  } else  if (method == "exp_average"){  
    averageFrom <- as.Date(averageFrom)
    averageTo <- as.Date(averageTo)
    if(numberofConst > length(coin.Names) || averageFrom < startDate || averageFrom > trainingDay || averageTo > trainingDay || ponder > 1 || ponder <= 0){
      stop("Error: Parameters misspecified")  
    }   
    marketcap.Crypto <- data.frame("crypto" = gsub(pattern = "\\.csv", replacement =  "", coin.Names), "marketcap" = NA, stringsAsFactors = FALSE)
    for (crypt0 in coin.Names){
      
      df1 <- bigFileList[[crypt0]]
      df1$marketCap <- as.numeric(df1$marketCap)
      startAvrg <- which(df1$date == averageFrom)
      stopAvrg <- which(df1$date == averageTo)
      mc.List <- c(0)
      for(d in 0:((stopAvrg - startAvrg) - 1)){
        mc.List[d + 1] <- (ponder^(d)*df1$marketCap[(stopAvrg - startAvrg) - d])/(stopAvrg - startAvrg + 1)
        
      }
      mc <- sum(mc.List, na.rm = T)
      marketcap.Crypto$marketcap[marketcap.Crypto$crypto == crypt0] <- mc
    }
    marketcap.Crypto$marketcap <- as.numeric(marketcap.Crypto$marketcap)
    omc <- order(marketcap.Crypto$marketcap, decreasing = TRUE)
    ind <- omc[1:numberofConst]
    ind <- sort.default(ind)
    top <- marketcap.Crypto[ind, ]
    row.names(top) <- seq(1:numberofConst)
    
    
    return(top)
    
  } else {stop("Error: The method isn't correctly specified")}
}
