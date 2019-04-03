#Normáis eloszlás generálása egyenletesbõl

#CHT
u<-runif(12,0,1);u
z<-sum(u)-6;z

norm_gen<-function(n){
  z<-matrix(0,ncol=1,nrow=n)
  for (i in 1:n){
    u<-runif(12,0,1)   #12 db egyenletest generálunk
    z[i]<-sum(u)-6   #1 db normálisért
  }
  
  par(mfrow=c(1,2))
  hist(z,freq=F,main="Normal from CHT")
  points(seq(-3,3,0.01),dnorm(seq(-3,3,0.01)),type="l",col="red")
  y<-rnorm(n)
  hist(y,freq=F,main="Normal from R")
  points(seq(-3,3,0.01),dnorm(seq(-3,3,0.01)),type="l",col="red") 
}

norm_gen(1000)
