#if(!require("foreach")) install.packages("foreach"); library("foreach")
#if(!require("psych")) install.packages("psych"); library("psych")
#if(!require("tseries")) install.packages("tseries"); library("tseries")
if(!require("RiskPortfolios")) install.packages("RiskPortfolios"); library("RiskPortfolios")
if(!require("rdrop2")) install.packages("rdrop2"); library("rdrop2")

source("./20_topCrypto_static_submission.R")
source("./cryptoFolioStaticNewPackage_submission.R")


token = readRDS("./token.rds")

topCC = topCrypto(startDate     = "2017-01-01", 
                  endDate       = "2019-03-15", 
                  trainingDay   = "2019-01-01", 
                  mcDay         = "2019-01-01", 
                  numberofConst = 10, 
                  method        = "day")


sP = staticPortfolio(startDate   = "2017-01-01", 
                     endDate     = "2019-03-15", 
                     trainingDay = "2019-01-01",
                     topCC       = topCC)

dp <-  dynamicPortfolio(startDate     = "2017-01-01", 
                        endDate       = "2019-03-15",
                        numberofConst = 19, 
                        method        = "day", 
                        testWindow    = 35, 
                        trainWindow   = 50, 
                        windowChange  = 35)

