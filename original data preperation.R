#    845 Project
#
#   Leiguang Chen
#  Andrew van den Hoeven
install.packages("MASS")
require(MASS)
plotcompare<-function(a) 
{
  # plotcompare is the function which plot the density plot of 
  # original data compared with normal distribution and student t distribution
  # a is the returns
  a<-(a-mean(a))/sd(a)
  d<-density(a)
  mydt <- function(x, m, s, df) dt((x-m)/s, df)/s
  t<-fitdistr(a, mydt, list(m = 0, s = 1), df = 9, lower = c(-Inf, 0))
  plot(d,col=3,main="Density plot comparison between normal,student t and original data")
  curve(dnorm(x, mean=mean(a), sd=sd(a)),col=9,lty=2, 
        add=TRUE, yaxt="n")
  curve(dt(x, t$estimate[1], t$estimate[2],df = 9),col=4,lty=5, 
        add=TRUE, yaxt="n")
  legend("topright", legend=c("normalized data", "normal distribution","student t distribution"),
         col=c(3, 9,4), lty=c(1,2,5), cex=0.6)
  
}

tailplot_r<-function(a,b,c)
{
  # tailplot_l gives us a tail comparison 
  # a is the returns, (b,c) are the xlim parameter, d is the position of legend
  a<-(a-mean(a))/sd(a)
  d<-density(a)
  mydt <- function(x, m, s, df) dt((x-m)/s, df)/s
  t<-fitdistr(a, mydt, list(m = 0, s = 1), df = 9, lower = c(-Inf, 0))
  plot(d,col=3,main="Tail comparison ",xlim=c(b,c),ylim=c(0,0.06))
  curve(dnorm(x, mean=mean(a), sd=sd(a)),col=9,lty=2, 
        add=TRUE, yaxt="n")
  curve(dt(x, t$estimate[1], t$estimate[2],df = 9),col=4,lty=5, 
        add=TRUE, yaxt="n")
  legend("topright",legend=c("normalized data", "normal distribution","student t distribution"),
         col=c(3, 9,4), lty=c(1,2,5), cex=0.6)
}

tailplot_l<-function(a,b,c)
{
  # tailplot_l gives us a tail comparison 
  # a is the returns, (b,c) are the xlim parameter, d is the position of legend
  a<-(a-mean(a))/sd(a)
  d<-density(a)
  mydt <- function(x, m, s, df) dt((x-m)/s, df)/s
  t<-fitdistr(a, mydt, list(m = 0, s = 1), df = 9, lower = c(-Inf, 0))
  plot(d,col=3,main="Tail comparison ",xlim=c(b,c),ylim=c(0,0.06))
  curve(dnorm(x, mean=mean(a), sd=sd(a)),col=9,lty=2, 
        add=TRUE, yaxt="n")
  curve(dt(x, t$estimate[1], t$estimate[2],df = 9),col=4,lty=5, 
        add=TRUE, yaxt="n")
  legend("topleft",legend=c("normalized data", "normal distribution","student t distribution"),
         col=c(3, 9,4), lty=c(1,2,5), cex=0.6)
}


violationplot<-function(v,r)
{ # violationplot gives us a plot that can see the var estimation compared with 
  # real data directly
  # r is the return, v is the estimated var
  n=length(v)
  x=seq(1:n)
  plot(r,main="VaR forecsating compared with real data",ylab="VaR and real data",ylim=c(-0.25,0.15))
  lines(x,v,col=3)
  legend("topright", legend=c("returns", "forecasting VaR"),
         col=c(1,3), lty=c(1,2),cex=0.7)
  
}


# a closer look of plotcompare
violatepoint1<-function(v,r)
{
  n=length(v)
  x=seq(1:n)
  plot(r,main=" Closer look of VaR with returns",ylab="VaR and real data",xlim=c(1500,2500),ylim=c(-0.25,0.15))
  lines(x,v,col=3)
  legend("topright", legend=c("returns", "forcasting VaR"),
         col=c(1,3), lty=c(1,2), cex=0.5)
}

##############plots#################
library(quantmod) #used to fetch the data
library(rugarch)
library(parallel) #used to run the code in parallel
ticker<- "JYP=X"
prices<-getSymbols(Symbols=c(ticker),
                   from="2004-01-01",
                   to="2016-12-01",
                   src="yahoo",
                   env=NULL)[,6]
returns <- dailyReturn(prices,type="log")
a<-as.numeric(returns)
forecasts.list<-readRDS("normal-MSFT.rds")
v1<-forecasts.list[[1]]@forecast$VaR
v2<-forecasts.list[[2]]@forecast$VaR
v3<-forecasts.list[[3]]@forecast$VaR
v4<-forecasts.list[[4]]@forecast$VaR
v11<-v1$`alpha(0%)` ; v21<-v2$`alpha(0%)`
v12<-v1$`alpha(1%)` ; v22<-v2$`alpha(1%)`
v13<-v1$`alpha(5%)` ; v23<-v2$`alpha(5%)`
v14<-v1$`alpha(10%)` ; v24<-v2$`alpha(10%)`
v31<-v3$`alpha(0%)` ; v41<-v4$`alpha(0%)`
v32<-v3$`alpha(1%)` ; v42<-v4$`alpha(1%)`
v33<-v3$`alpha(5%)` ; v43<-v4$`alpha(5%)`
v34<-v3$`alpha(10%)` ; v44<-v4$`alpha(10%)`;

r<-v1$realized

par(mfrow=c(1,1))
plot(returns)
plotcompare(returns)
# we compare 95% VaR here
violationplot(v13,r) #arch(1)
violatepoint1(v13,r)
violationplot(v23,r) #garch(1,1)
violatepoint1(v23,r)
violationplot(v33,r) #arma(1,1)-garch(1,1)
violatepoint1(v33,r)
violationplot(v43,r) #egarch(1,1)
violatepoint1(v43,r)
par(mfrow=c(1,2))
tailplot_r(returns,2,6)
tailplot_l(returns,-6,-2)
