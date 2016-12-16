#    845 Project
#
#   Leiguang Chen
#  Andrew van den Hoeven

plotcompare<-function(a) 
{
  # plotcompare is the function which plot the density plot of 
  # original data compared with normal distribution and student t distribution
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
         col=c(3, 9,4), lty=c(1,2,5), cex=0.8)
  
}

plotcompare(a)

