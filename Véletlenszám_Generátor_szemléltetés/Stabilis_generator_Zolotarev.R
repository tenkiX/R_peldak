#Stabilis generator

install.packages("stabledist") #MENÜ
library(stabledist) #LOAD

stabilis_gen<-function(n){
  alpha=2; #alpha ]0;2] és alpha!=1
  lambda=1;
  beta = 1; #ferdeség: [-1; 1]
  z = array(n)
   for (i in 1:n){
    xszi<-runif(1,-pi/2,pi/2)   #1 db egyenletes
    eta<-rexp(1,lambda)
    z[i]<-(sin(alpha*xszi)/(cos(xszi)^(1/alpha)))*((cos((1-alpha)*xszi)/eta))^((1-alpha)/alpha) 
  }
   
  par(mfrow=c(1,2))
  hist(z,freq=F,main="Zolotarev-féle")
  points(seq(-6,6,0.01),dstable(seq(-6,6,0.01),alpha,beta),type="l",col="red")

  y<-rstable(n,alpha,beta) #gamma a scale, delta shift lenne még
  hist(y,freq=F,main="Stabilis R hívással")
  points(seq(-6,6,0.01),dstable(seq(-6,6,0.01),alpha,beta),type="l",col="red")
}

stabilis_gen(10000)






