# 845  project 
#
#   Leiguang Chen
# Andrew van den Hoeven
# summary stats

#clean our workspace
rm(list=ls())


#set our current working directory
#setwd("/home/andrew")
setwd("/home/andrew/Documents/school/845/project/ACSTS845-project")

#-- load require libraries --#
library(rugarch)


# load in our previously computed forecast
forecasts.list<- readRDS("student-COM-CHEESE_BRL.rds")



#check to see that all the windows converge
for (i in 1:4){
  #lol this could be done with the convergence() function apparently
  if(!is.null(forecasts.list[[i]]@forecast[[1]]$converge)){
    print(paste(sum(sapply(1:(length(forecasts.list[[i]]@forecast)),function (x) !forecasts.list[[i]]@forecast[[x]]$converge)), "window(s) do(es) not converge!"))
  }
  else{
    print("all windows converge")
  }
}





for (model_n in 1:4){
  total <- forecasts.list[[model_n]]@model$coef[[1]]$coef[,1]
  for ( i in 2:length(forecasts.list[[model_n]]@model$coef)){
    total<- total +forecasts.list[[model_n]]@model$coef[[i]]$coef[,1]
  }
  print(paste("Model", model_n))
  print(round(total/length(forecasts.list[[model_n]]@model$coef),5))
}



#use the following if needed

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
forecasts.list[[3]]<-resume(forecasts.list[[3]], solver="hybrid",solver.control=list(rel.tol=1e-2,delta=1e-5,trace =TRUE),fit.control = list(stationarity=FALSE, fixed.se = 0, scale=0))

forecasts.list[[1]]<-resume(forecasts.list[[1]], solver="hybrid",solver.control=list(rel.tol=1e-4,delta=1e-5,trace =TRUE),fit.control = list(stationarity=FALSE, fixed.se = 0, scale=0))
forecasts.list[[2]]<-resume(forecasts.list[[2]], solver="hybrid",solver.control=list(rel.tol=1e-4,delta=1e-5,trace =TRUE),fit.control = list(stationarity=FALSE, fixed.se = 0, scale=0))

