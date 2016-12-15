# 845  project 
#
#   Leiguang Chen
# Andrew van den Hoeven
# summary stats

#clean our workspace
rm(list=ls())


# -- Load in required libraries -- #
library(quantmod) #used to fetch the data
library(DistributionUtils) # for the kurtosis function

#-- load in data --#

equity.tickers<- c("MSFT","AAPL","BAC")
currency.tickers<- c("CAD=X","JPY=X", "GBP=X")
commodity.tickers <- c("COM-AU_EFP","COM-CHEESE_BRL","COM-SOYB_MEAL")


getSymbols(Symbols=equity.tickers,
           from="2000-01-01",
           to="2016-12-01",
           src="yahoo")

getSymbols(Symbols=currency.tickers,
           from="2004-01-01",
           to="2016-12-01",
           src="yahoo")


commodity.returns <- list(rep(NA,3))

for (i in 1:3){
  price.df =read.csv(paste0(commodity.tickers[i], ".csv"))
  prices   =xts(price.df$Value, order.by = as.POSIXct( price.df$Date)  )
  commodity.returns[[i]] <- dailyReturn(prices,type="log")
}


summary.stats <- function(returns.series, name="series"){
  r.n= length(returns.series)
  r.mean= mean(returns.series)
  r.sd= sd(returns.series)
  r.min= min(returns.series)
  r.max= max(returns.series)
  r.kurt= kurtosis(returns.series)
  r.skew =skewness(returns.series)
  print(paste(name, "summary statistics:"))
  #print("r.n,r.mean,r.sd,r.min,r.max,r.kurt,r.skew")
  #using a tab lets you paste it into spreadsheet programs!
  print(cat(c(r.n,r.mean,r.sd,r.min,r.max,r.kurt,r.skew),sep ="\t"))
}



tickers<-c(equity.tickers,commodity.tickers,currency.tickers)
returns <- list( dailyReturn(MSFT$MSFT.Adjusted,type="log"),
                 dailyReturn(AAPL$AAPL.Adjusted,type="log"),
                 dailyReturn(BAC$BAC.Adjusted,type="log"),
                 commodity.returns[[1]],
                 commodity.returns[[2]],
                 commodity.returns[[3]],
                 dailyReturn(`CAD=X`$`CAD=X.Adjusted`,type="log"),
                 dailyReturn(`JPY=X`$`JPY=X.Adjusted`,type="log"),
                 dailyReturn(`GBP=X`$`GBP=X.Adjusted`,type="log")
)

for (i in 1:(length(tickers))){
  summary.stats(returns.series=returns[[i]],name=tickers[i])
}

