#Gamma eloszlás a - hely, lambda - skála paraméterének becslése
n<-1000
a<-5 #alak (shape), ezt fogjuk becsülni!!!
l<-3 #skála (scale), ezt fogjuk becsülni!!!

x<-rgamma(n,shape=a,scale=l)

#Log-likelihood függvény:
gamma_loglik<-function(y,par){
  loglik<-sum(dgamma(y,shape=par[1], scale=par[2],log=TRUE))
  return(-loglik)
}

opt<-nlm(f=gamma_loglik, p=c(1,1), y=x);opt #kiinduló érték a=1, l=1
opt$estimate #paraméter becslése

#Mit csináltunk?
par(mfrow=c(1,2))
hist(x,freq = F,main="Gamma(a,l) sürüségfv")
points(seq(0,a^2,0.1),dgamma(seq(0,a^2,0.1),shape=a,scale=l),type="l")
hist(x,freq = F,main="Gamma(ML becsléssel kapott a és l) sürüségfv")
points(seq(0,a^2,0.1),dgamma(seq(0,a^2,0.1),shape=opt$estimate[1],scale=opt$estimate[2]),type="l", col="red")