source("./SPL_DynamicPortfolio/DynamicPortfolio.R")
readRDS("./SPL_DynamicPortfolio/token.rds")


dp <-  dynamicPortfolio(startDate     = "2017-01-01", 
                        endDate       = "2019-03-15",
                        numberofConst = 19, 
                        method        = "day", 
                        testWindow    = 35, 
                        trainWindow   = 50, 
                        windowChange  = 35)

saveRDS(dp, "./SPL_DynamicPortfolio/dpResult.RDS")
