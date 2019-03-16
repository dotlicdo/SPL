
source("./SPL_StaticPortfolio/topCrypto_static.R")
source("./SPL_StaticPortfolio/Static_Portfolio.R")


token = readRDS("./SPL_StaticPortfolio/token.rds")

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


saveRDS(sP, "./SPL_StaticPortfolio/spResult.RDS")