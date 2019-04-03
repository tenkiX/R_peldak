#Normális eloszlás m- várható érték és s - szórás paraméterének becslése
n<-1000
m<-3 #várható érték, ezt fogjuk becsülni!!!
s<-2 #vszórás, ezt fogjuk becsülni!!!

x<-rnorm(n,mean=m,sd=s)

#Log-likelihood függvény:
norm_loglik<-function(y,par){
  loglik<-sum(dnorm(y,mean=par[1], sd=par[2],log=TRUE))
  return(-loglik)
}

opt<-nlm(f=norm_loglik, p=c(1,1), y=x);opt #kiinduló érték m=1, sd=1
opt$estimate #paraméter becslése

-m-s-3
m+s+3

#Mit csináltunk?
par(mfrow=c(1,2))
hist(x,freq = F,main="Norm(m,s) sürüségfv")
points(seq(-8,8,0.1),dnorm(seq(-8,8,0.1),mean=m,sd=s),type="l")
hist(x,freq = F,main="Norm(ML becsléssel kapott m és s) sürüségfv")
points(seq(-8,8,0.1),dnorm(seq(-8,8,0.1),mean=opt$estimate[1],sd=opt$estimate[2]),type="l", col="red")