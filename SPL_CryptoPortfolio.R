source("./topCrypto_static.R")
source("./StaticPortfolio.R")
source("./DynamicPortfolio.R")


token = readRDS("./token.rds")

#Top
topCC = topCrypto(startDate     = "2017-01-01", 
                  endDate       = "2019-03-15", 
                  trainingDay   = "2019-01-01", 
                  mcDay         = "2019-01-01", 
                  numberofConst = 10, 
                  method        = "day")


sP    = staticPortfolio(startDate   = "2017-01-01", 
                        endDate     = "2019-03-15", 
                        trainingDay = "2019-01-01",
                        topCC       = topCC)

saveRDS(sP, "./staticPortfolio_results.RDS")

dP    =  dynamicPortfolio(startDate     = "2017-01-01", 
                          endDate       = "2019-03-15",
                          numberofConst = 10, 
                          method        = "exp_average", 
                          ponder        = 0.5,
                          testWindow    = 35, 
                          trainWindow   = 50, 
                          windowChange  = 35)

saveRDS(dP, "./dynamicPortfolio_results.RDS")