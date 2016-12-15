#    845 Project
#
#   Leiguang Chen
# Andrew van den Hoeven

#-- Analysis of forecasts --#


# begin by clearing our workspace
rm(list=ls())

#set our current working directory
#setwd("/home/andrew")

#-- load require libraries --#
library(rugarch)


# load in our previously computed forecast
forecasts.list<- readRDS("normal-MSFT.rds")

#initialize variables
alpha.levels <- c(0.005,0.01,0.05,0.10)


violations <- forecasts.list[[3]]@forecast$VaR >forecasts.list[[3]]@forecast$VaR[,"realized"]
colSums(violations)/ dim(violations)[1]


for (i in 1:(length(alpha.levels))){
  report(forecasts.list[[4]], type="VaR", VaR.alpha = alpha.levels[i], conf.level = 0.95) 
}

forecasts.list[[4]]@model$coef
