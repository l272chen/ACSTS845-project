#    845 Project
#
#   Leiguang Chen
# Andrew van den Hoeven



# begin by clearing our workspace
rm(list=ls())

#all additional files are assumed to be in the smae directory as the current r script
setwd(dirname(sys.frame(1)$ofile))


# -- Load in required libraries -- #
library(quantmod) #used to fetch the data
library(rugarch)


# -- Load in the data -- #
# for now we'll just use a single set for testing out the r code

getSymbols(Symbols=c("MSFT"),
        from="2000-01-03",
        to="2001-01-11",
        src="yahoo")


# Also we're concerned with the log returns of the adjusted close
returns <- dailyReturn(MSFT$MSFT.Adjusted,type="log")



#-- Initialize our variables --#
window.size <- 250 # roughly one trading year
steps.ahead <- c(1,2) # the various horizons at which to look ahead
alpha.levels <- c(0.005,0.01,0.05,0.10)


models <- list( ugarchspec(variance.model = list(garchOrder=c(1,0)), mean.model = list(armaOrder=c(0,0))) , # ARCH(1)
                ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0))) , #GARCH(1,1)
                ugarchspec(variance.model = list(garchOrder=c(1,1)), mean.model = list(armaOrder=c(1,1))) , #ARM(1,1)-GARCH(1,1)
                ugarchspec(variance.model = list(model = "eGARCH", garchOrder=c(1,1)), mean.model = list(armaOrder=c(0,0)))  #EGARCH(1,1)
                )
forecasts.list <- as.list(rep(NA,4))
#-- Rolling forecasts  --#

for( i in 1:(length(models))){
  forecasts.list[[i]]<- ugarchroll(spec=models[[i]],data=returns,n.ahead = 1,
                                refit.every = 1, refit.window ="moving",n.start =window.size,
                                window.size=window.size,VaR.alpha = alpha.levels,
                                keep.coef = FALSE)
}


#-- Analysis of forecasts --#

violations <- forecasts.list[[1]]@forecast$VaR >forecasts.list[[1]]@forecast$VaR[,"realized"]
colSums(violations)/ dim(violations)[1]



report(forecasts.list[[1]], type="VaR", VaR.alpha = alpha.levels[2], conf.level = 0.95) 











