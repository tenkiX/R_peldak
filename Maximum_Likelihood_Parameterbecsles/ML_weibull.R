#Weibull eloszlás c=0 -eltolás, a-alak (shape), b -skála (scale), paraméterrel

#1. eset: Exponenciális
a<-0.5 #alak (shape), ezt fogjuk becsülni!!!
b<-1 # skála (scale), ezt fogjuk becsülni!!!

#2. eset: Rayleigh
a<-2 #alak (shape), ezt fogjuk becsülni!!!
b<-1 # skála (scale), ezt fogjuk becsülni!!!

#1. eset: Normális
a<-3.57 #alak (shape), ezt fogjuk becsülni!!!
b<-1 # skála (scale), ezt fogjuk becsülni!!!
x<-rweibull(n,shape=a,scale=b)

#Log-likelihood függvény:
weibull_loglik<-function(y,par){
  loglik<-sum(dweibull(y,shape=par[1], scale=par[2],log=TRUE))
  return(-loglik)
}

opt<-nlm(f=weibull_loglik, p=c(2,2), y=x);opt #kiinduló érték l=1, s=1
opt$estimate #paraméter becslése

#Mit csináltunk?
par(mfrow=c(1,2))
hist(x,freq = F,main="Weibull(a,b) sürüségfv")
points(seq(0,25,0.1),dweibull(seq(0,25,0.1),shape=a,scale=b),type="l")
hist(x,freq = F,main="Weibull(ML becsléssel kapott a és b) sürüségfv")
points(seq(0,25,0.1),dweibull(seq(0,25,0.1),shape=opt$estimate[1],scale=opt$estimate[2]),type="l", col="red")