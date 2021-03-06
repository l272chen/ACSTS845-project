#    845 Project
#
#   Leiguang Chen
#  Andrew van den Hoeven



# begin by clearing our workspace
rm(list=ls())


# -- Load in required libraries -- #
library(quantmod) #used to fetch the data
library(rugarch)
library(parallel) #used to run the code in parallel

# -- Load in the data -- #
setwd("/home/andrew/Documents/school/845/project/ACSTS845-project")

ticker<- "COM-SOYB_MEAL"
#prices<-getSymbols(Symbols=c(ticker),
#        from="2004-01-01",
#        to="2016-12-01",
#        src="yahoo",
#        env=NULL)[,6]
price.df <-read.csv(paste0(ticker, ".csv"))
prices <- xts(price.df$Value, order.by = as.POSIXct( price.df$Date)  )

# Also we're concerned with the log returns of the adjusted close
returns <- dailyReturn(prices,type="log")



#-- Initialize our variables --#
window.size <- 250 # roughly one trading year
steps.ahead <- c(1,2) # the various horizons at which to look ahead
alpha.levels <- c(0.005,0.01,0.05,0.10)


models.norm <- list( ugarchspec(variance.model = list(garchOrder=c(1,0)), mean.model = list(armaOrder=c(0,0))) , # ARCH(1)
                     ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0))) , #GARCH(1,1)
                     ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder=c(1,1))) , #ARMA(1,1)-GARCH(1,1)
                     ugarchspec(variance.model = list(model = "eGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0)))  #EGARCH(1,1)
)
forecasts.list.norm <- as.list(rep(NA,4))

models.student <- list( ugarchspec(variance.model = list(garchOrder=c(1,0)), mean.model = list(armaOrder=c(0,0)),distribution.model="std") , # ARCH(1)
                        ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0)),distribution.model="std") , #GARCH(1,1)
                        ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder=c(1,1)),distribution.model="std") , #ARMA(1,1)-GARCH(1,1)
                        ugarchspec(variance.model = list(model = "eGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0)),distribution.model="std")  #EGARCH(1,1)
)
forecasts.list.student <- as.list(rep(NA,4))



#-- Rolling forecasts  --#

#first set up the parallel code
no_cores <- detectCores() - 1 # save a core for the rest of your computer
# Initiate cluster
cl <- makeCluster(no_cores)

for( i in 1:(length(models.norm))){
  forecasts.list.norm[[i]]<- ugarchroll(spec=models.norm[[i]],data=returns,n.ahead = 1,
                                        refit.every = 1, refit.window ="moving",n.start =window.size,
                                        window.size=window.size,VaR.alpha = alpha.levels,
                                        cluster=cl)
  print(paste("Completed normal forecast for model", i))
}

for( i in 1:(length(models.student))){
  forecasts.list.student[[i]]<- ugarchroll(spec=models.student[[i]],data=returns,n.ahead = 1,
                                           refit.every = 1, refit.window ="moving",n.start =window.size,
                                           window.size=window.size,VaR.alpha = alpha.levels,
                                           cluster=cl)
  print(paste("Completed student's t forecast for model", i))
}

#remember to stop the cluster 
stopCluster(cl)

#-- Save the results --##
saveRDS(forecasts.list.norm, paste0("normal-",ticker, ".rds"))
saveRDS(forecasts.list.student, paste0("student-",ticker, ".rds"))











