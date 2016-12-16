#    845 Project
#
#   Leiguang Chen
# Andrew van den Hoeven

#-- Analysis of forecasts --#


# begin by clearing our workspace
rm(list=ls())

#set our current working directory
setwd("/home/andrew")
setwd("/home/andrew/Documents/school/845/project/ACSTS845-project")

#-- load require libraries --#
library(rugarch)


# load in our previously computed forecast
forecasts.list<- readRDS("student-COM-SOYB_MEAL.rds")

#initialize variables
alpha.levels <- c(0.005,0.01,0.05,0.10)


#violations <- forecasts.list[[1]]@forecast$VaR >forecasts.list[[1]]@forecast$VaR[,"realized"]
#colSums(violations)/ dim(violations)[1]

#Note 1: ARCH(1), 2:GARCH(1,1) , 3:ARMAGARCH , 4: EGARCH
for (i in 1:(length(alpha.levels))){
  report(forecasts.list[[4]], type="VaR", VaR.alpha = alpha.levels[i], conf.level = 0.95) 
}




report(forecasts.list[[4]], type="VaR", VaR.alpha = alpha.levels[1], conf.level = 0.95) 
forecasts.list[[1]]<-resume(forecasts.list[[1]], solver="lbfgs")
forecasts.list[[2]]<-resume(forecasts.list[[2]], solver="lbfgs")
forecasts.list[[3]]<-resume(forecasts.list[[3]], solver="lbfgs")
forecasts.list[[4]]<-resume(forecasts.list[[4]], solver="lbfgs")

forecasts.list[[1]]<-resume(forecasts.list[[1]], solver="hybrid",solver.control=list(rel.tol=1e-4,delta=1e-5))
forecasts.list[[2]]<-resume(forecasts.list[[2]], solver="hybrid",solver.control=list(rel.tol=1e-4,delta=1e-5))
forecasts.list[[3]]<-resume(forecasts.list[[3]], solver="hybrid",solver.control=list(rel.tol=1e-4,delta=1e-5))
forecasts.list[[4]]<-resume(forecasts.list[[4]], solver="hybrid",solver.control=list(rel.tol=1e-4,delta=1e-5))

forecasts.list[[3]]<-resume(forecasts.list[[3]], solver="hybrid",solver.control=list(rel.tol=1e-4,delta=1e-5,trace =TRUE),fit.control = list(stationarity=FALSE, fixed.se = 0, scale=0))
forecasts.list[[3]]<-resume(forecasts.list[[3]], solver="hybrid",solver.control=list(rel.tol=1e-3,delta=1e-5,trace =TRUE),fit.control = list(stationarity=FALSE, fixed.se = 0, scale=0))


for (i in 1:4){
  #lol this could be done with the convergence() function apparently
  if(!is.null(forecasts.list[[i]]@forecast[[1]]$converge)){
    print(paste(sum(sapply(1:(length(forecasts.list[[i]]@forecast)),function (x) !forecasts.list[[i]]@forecast[[x]]$converge)), "window(s) do(es) not converge!"))
  }
  else{
    print("all windows converge")
  }
}



summary(forecasts.list[[4]]@forecast[[1]])
test <- forecasts.list[[2]]
test <- forecasts.list[[2]]@forecast[setdiff(1:(length(forecasts.list[[2]]@forecast)) ,attributes(convergence(forecasts.list[[2]]))$nonconverged)]

forecasts.list[[2]]@forecast
test

test@model$noncidx <-as.vector(NULL)
test@forecast[[2981]]
report(test, type="VaR", VaR.alpha = alpha.levels[i], conf.level = 0.95)
convergence(test)
typeof()
as.vector(convergence(forecasts.list[[2]]))
attributes(convergence(forecasts.list[[2]]))$nonconverged
sum(sapply(1:3,function (x) not(forecasts.list[[1]]@forecast[[x]]$converge)))
summary(forecasts.list[[3]]@forecast[1:4])

!is.null(forecasts.list[[1]]@forecast[[1]]$converge)
!is.null(forecasts.list[[4]]@forecast[[1]]$converge)
typeof(forecasts.list[[4]]@forecast[[1]])
