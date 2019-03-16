if(!require("foreach")) install.packages("foreach"); library("foreach")
if(!require("psych")) install.packages("psych"); library("psych")
if(!require("tseries")) install.packages("tseries"); library("tseries")
if(!require("RiskPortfolios")) install.packages("RiskPortfolios"); library("RiskPortfolios")
if(!require("rdrop2")) install.packages("rdrop2"); library("rdrop2")


token <- readRDS("./token.rds")

staticPortfolio <- function(startDate =  as.Date("2016-01-01"), endDate = (Sys.Date() - 3), trainingDay = (Sys.Date() - 120), topCC){
  
  trainingDay <- as.Date(trainingDay)
  startDate <-  as.Date(startDate)
  endDate <- as.Date(endDate)
  
  if (trainingDay < startDate || startDate > endDate || trainingDay > endDate){
    stop("Error: Dates misspecified")
  } else {
  
  temp0 <- tempdir()
  drop_download("/SPL.WS1819/bigFileList.RData", local_path = temp0, overwrite = TRUE, dtoken = token)
  load(paste0(temp0, "/bigFileList.RData"))
  
  cryptolist <- as.list(topCC$crypto) 
  
  
  returnsDaily <- function(x){
    returnRow <- length(x) - 1
    dailyreturns <- list()
    dailyreturns[1] <- NA
    for (j in 1:returnRow){
      dailyreturns[j+1] <- (log(x[j+1]) - log(x[j]))
    }
    return(as.numeric(dailyreturns))
  }
  
  returnsBinded <- NULL
  
  for (crypt0 in cryptolist){
    df <- bigFileList[[crypt0]]
    
    df$date <- as.Date(df$date)
    df$price <- as.numeric(df$price)
    returns <-  returnsDaily(df$price)
    returnsBinded <- cbind(returnsBinded, returns)
  }
  
  returnsBinded <- as.data.frame(returnsBinded)
  colnames(returnsBinded) <- topCC$crypto
  
  
  returnsBinded <- as.matrix(returnsBinded)
  
  
  returnsBinded.Insample <- returnsBinded[1:which(df$date == trainingDay), ]
  
  #CHECK THIS
  returnsBinded.Outsample <- returnsBinded[(which(df$date == trainingDay + 1):which(df$date == endDate)), ]
  
  returnsBinded.Insample[is.na(returnsBinded.Insample)] <- 0
  returnsBinded.Outsample[is.na(returnsBinded.Outsample)] <- 0
  
  returnsBinded.Insample[is.infinite(returnsBinded.Insample)] <- 0
  returnsBinded.Outsample[is.infinite(returnsBinded.Outsample)] <- 0
  
    # Mean estimation
    mu <-  meanEstimation(returnsBinded.Insample)
    # Covariance estimation
     Sigma = covEstimation(returnsBinded.Insample)
     semiDev = semidevEstimation(returnsBinded.Insample)  
  
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
     
           
      
  weights <- cbind(mv, minvol, invvol, erc, maxdiv, riskeff, maxdec)
  
  results <- returnsBinded.Outsample %*% cbind(weights, 1/nrow(topCC))
  cumReturns <- matrix(nrow= NROW(results), ncol= NCOL(results))
  
  for (q in 1:NCOL(results)) {
    for (w in 1: NROW(results) ) {
      cumReturns[w, q] <- prod((1+results[1:w,q])) - 1
    } 
  }
  
  cumReturns <- as.data.frame(cumReturns) 
  colnames(cumReturns) <- c("mv", "minvol", "invvol", "erc", 
                             "maxdiv", "riskeff", "maxdec", "naive")
  
  sp.resultList <- list("weights" = weights, "cumReturns" = cumReturns)
  
  return(sp.resultList)
  
}}
