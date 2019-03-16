if(!require("foreach")) install.packages("foreach"); library("foreach")
if(!require("psych")) install.packages("psych"); library("psych")
if(!require("tseries")) install.packages("tseries"); library("tseries")
if(!require("RiskPortfolios")) install.packages("RiskPortfolios"); library("RiskPortfolios")
if(!require("rdrop2")) install.packages("rdrop2"); library("rdrop2")
if(!require("RCurl")) install.packages("RCurl"); library("RCurl")
if(!require("jsonlite")) install.packages("jsonlite"); library("jsonlite")




token <- readRDS("./token.rds")

#################################################
#################################################
#####Dynamic Portfolio Optimization##############
#################################################
#################################################



dynamicPortfolio <- function(startDate =  as.Date("2016-01-01"), endDate = (Sys.Date() - 3),numberofConst, method, ponder, testWindow, trainWindow, windowChange){
    
    startDate <-  as.Date(startDate)
    endDate <- as.Date(endDate)
    
    if (trainWindow + testWindow > as.numeric(endDate - startDate) + 1){
    stop("Error: Train and test windows too large")
  } 
  
    rndsInt <- (as.numeric(endDate - startDate) + 1 - trainWindow) %/% windowChange
    resultsAlltogether <- NULL
  
    temp0 <- tempdir()
    drop_download("/SPL.WS1819/bigFileList.RData", local_path = temp0, overwrite = TRUE, dtoken = token)
    load(paste0(temp0, "/bigFileList.RData"))
    coin.Names <- array(names(bigFileList))
    marketcapCrypto <- data.frame("crypto" = coin.Names, "marketcap" = NA, stringsAsFactors = FALSE)
  
    for (iter in 0:(rndsInt-1)){

      print(iter)
    
      if (method == "day"){

        if(numberofConst > length(coin.Names)){
          stop("Error: parameters misspecified")  
        } 
    
    
        for (crypt0 in coin.Names){
      
          df1 <- bigFileList[[crypt0]]
          df1 <- df1[which(df1$date == startDate) : which(df1$date == endDate), ]
          rownames(df1) <- seq(1:nrow(df1))
          mc <- as.numeric(df1$marketCap[(trainWindow + iter*windowChange)])
          #mc_day <- df1$date[trainWindow+iter*windowChange]
      if (length(which(is.na(df1$marketCap[(1 + iter*windowChange):(trainWindow+iter*windowChange)])))/trainWindow < 0.8) {
        marketcapCrypto$marketcap[marketcapCrypto$crypto == crypt0] <- mc
      }    else {
        marketcapCrypto$marketcap[marketcapCrypto$crypto == crypt0] <- 0
      } 
    }
    marketcapCrypto$marketcap <- as.numeric(marketcapCrypto$marketcap)
    omc <- order(marketcapCrypto$marketcap, decreasing = TRUE)
    ind <- omc[1:numberofConst]
    ind <- sort.default(ind)
    top <- marketcapCrypto[ind, ]
    row.names(top) <- seq(1:numberofConst)

    
  } else if(method == "exp_average"){
        
      if(numberofConst > length(coin.Names) || ponder > 1 || ponder <= 0){
        stop("Error: Parameters misspecified")  
      }
      
        
        for (crypt0 in coin.Names){
          
          df1 <- bigFileList[[crypt0]]
          df1 <- df1[which(df1$date == startDate) : which(df1$date == endDate), ]
          df1$marketCap <- as.numeric(df1$marketCap)
          rownames(df1) <- seq(1:nrow(df1))
          startAvrg <- 1 + iter * windowChange 
          stopAvrg <- trainWindow + iter * windowChange
          mc.List <- c(0)
          
          for(d in 0:(stopAvrg - startAvrg) - 1){
            mc.List[d + 1] <- (ponder^(d)*df1$marketCap[(stopAvrg - startAvrg) - d])/(stopAvrg - startAvrg + 1)
          }
          mc <- sum(mc.List, na.rm = T)
          
          if(length(which(is.na(df1$marketCap[(1 + iter*windowChange):(trainWindow+iter*windowChange)])))/trainWindow < 0.8){
              
              marketcapCrypto$marketcap[marketcapCrypto$crypto == crypt0] <- mc
          } else {
              
              marketcapCrypto$marketcap[marketcapCrypto$crypto == crypt0] <- 0
              
          }
            
            
          }
        
        marketcapCrypto$marketcap <- as.numeric(marketcapCrypto$marketcap)
        omc <- order(marketcapCrypto$marketcap, decreasing = TRUE)
        ind <- omc[1:numberofConst]
        ind <- sort.default(ind)
        top <- marketcapCrypto[ind, ]
        row.names(top) <- seq(1:numberofConst)

        
        
   } 
    
  
#cryptofolio function----  
  
  cryptolist <- as.list(top$crypto) 
  
  
  returnsDaily <- function(x){
    returnRow <- length(x) - 1
    dailyreturns <- list()
    dailyreturns[1] <- 0
    for (j in 1:returnRow){
      dailyreturns[j+1] <- (log(x[j+1]) - log(x[j]))
    }
    
    return(as.numeric(dailyreturns))
  }
  
  returnsBinded <- NULL
  
  for (crypt0 in cryptolist){
    df <- bigFileList[[crypt0]]
    df$price <- as.numeric(df$price)
    returns <-  returnsDaily(df$price)
    returnsBinded <- cbind(returnsBinded, returns)
  }
  
  returnsBinded <- as.data.frame(returnsBinded)
  colnames(returnsBinded) <- top$crypto
  returnsBinded <- as.matrix(returnsBinded)
  
  returnsBinded.Insample <- returnsBinded[(which(df$date == startDate) + iter * windowChange):(which(df$date == startDate) + iter*windowChange + trainWindow - 1), ]

  
  if(iter != rndsInt-1){
  returnsBinded.Outsample <- returnsBinded[(which(df$date == startDate) + iter*windowChange + trainWindow) : (which(df$date == startDate) + iter*windowChange+trainWindow + testWindow - 1), ]
  
  } else {returnsBinded.Outsample <- returnsBinded[(which(df$date == startDate) + (rndsInt - 1)*windowChange+trainWindow + 1) : which(df$date == endDate), ]
  }
  #calculate optimal weights
  
  returnsBinded.Insample[is.na(returnsBinded.Insample)] <- 0
  returnsBinded.Outsample[is.na(returnsBinded.Outsample)] <- 0
  
  returnsBinded.Insample[is.infinite(returnsBinded.Insample)] <- 0
  returnsBinded.Outsample[is.infinite(returnsBinded.Outsample)] <- 0
  
  mu <-  meanEstimation(returnsBinded.Insample)
  Sigma <-  covEstimation(returnsBinded.Insample)
  semiDev <-  semidevEstimation(returnsBinded.Insample)  
  
  
  
  mv <- optimalPortfolio(mu = mu, Sigma = Sigma,
                         control = list(type = 'mv',  constraint = 'lo'))
  minvol <- optimalPortfolio(Sigma = Sigma,
                             control = list(type = 'minvol',  constraint = 'lo'))
  invvol <- optimalPortfolio(Sigma = Sigma,
                             control = list(type = 'invvol',  constraint = 'lo'))
  erc <- optimalPortfolio(Sigma = Sigma,
                          control = list(type = 'erc',  constraint = 'lo'))
  maxdiv <- optimalPortfolio(Sigma = Sigma,
                             control = list(type = 'maxdiv',  constraint = 'lo'))
  
  riskeff <- NA
  riskeff <- try(optimalPortfolio(Sigma = Sigma, semiDev = semiDev,
                                  control = list(type = 'riskeff',  constraint = 'lo')))
  maxdec <- optimalPortfolio(Sigma = Sigma,
                             control = list(type = 'maxdec',  constraint = 'lo'))
  
  
  #now we make a df with a column of crypto tickers and then for every one of the methods
  #a column that says how much weight it gives to which crypto 
  
  weights <- cbind(mv, minvol, invvol, erc, maxdiv, riskeff, maxdec)
  results <- returnsBinded.Outsample %*% cbind(weights, 1/nrow(top))

    cumReturns <- matrix(nrow= NROW(results), ncol= NCOL(results))
  for(q in 1:NCOL(results)) {
    for( w in 1: NROW(results) ) {
      cumReturns[w, q] <- prod( (1+results[1:w,q]) ) - 1
    } }
  
    cumReturns <- as.data.frame(cumReturns) 
    colnames(cumReturns) <- c("mv", "minvol", "invvol", 
                               "erc", "maxdiv", "riskeff", "maxdec", "naive")
    resultsAlltogether <- rbind(resultsAlltogether, cumReturns)
  }  

  dp.resultList <- list("results_dynamic"= resultsAlltogether)
  return(dp.resultList)
  
}
