#    845 Project
#
#   Leiguang Chen
#  Andrew van den Hoeven



# begin by clearing our workspace
rm(list=ls())


# -- Load in required libraries -- #
library(quantmod) #used to fetch the data
library(rugarch)


# -- Load in the data -- #
# for now we'll just use a single set for testing out the r code

ticker<- "MSFT"
getSymbols(Symbols=c(ticker),
        from="2000-01-01",
        to="2016-12-01",
        src="yahoo")


# Also we're concerned with the log returns of the adjusted close
returns <- dailyReturn(MSFT$MSFT.Adjusted,type="log")



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

for( i in 1:(length(models.norm))){
  forecasts.list.norm[[i]]<- ugarchroll(spec=models.norm[[i]],data=returns,n.ahead = 1,
                                refit.every = 1, refit.window ="moving",n.start =window.size,
                                window.size=window.size,VaR.alpha = alpha.levels,
                                keep.coef = FALSE)
  print(paste("Completed normal forecast for model", i))
}

for( i in 1:(length(models.student))){
  forecasts.list.student[[i]]<- ugarchroll(spec=models.student[[i]],data=returns,n.ahead = 1,
                                        refit.every = 1, refit.window ="moving",n.start =window.size,
                                        window.size=window.size,VaR.alpha = alpha.levels,
                                        keep.coef = FALSE)
  print(paste("Completed student's t forecast for model", i))
}

#-- Save the results --##
saveRDS(forecasts.list.norm, paste0("normal-",ticker, ".rds"))
saveRDS(forecasts.list.student, paste0("student-",ticker, ".rds"))











