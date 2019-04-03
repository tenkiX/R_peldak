#Exponenciális eloszlás lambda paraméterének becslése
n<-1000
l<-3 #lambda, ezt fogjuk becsülni!!!
x<-rexp(n,rate=l)

#Log-likelihood függvény:
exp_loglik<-function(y,par){
  loglik<-sum(dexp(y,rate=par,log=TRUE))
  return(-loglik)
}
#Mgj: azért vesszük a (-1)-szeresét, mert az optimalizáló algoritmus
#minimum keresésre van megirva
#Optimalizálás: nlm(f= optimalizálandó függvény, p= paraméter kezdeti értéke)
#Mgj: Érzékeny a kiinduló érték választásra, hogy választunk??

opt<-nlm(f=exp_loglik, p=1, y=x);opt #kiinduló érték lambda=1
opt$estimate #paraméter becslése

#Mit csináltunk?
par(mfrow=c(1,3))
hist(x,freq = F,main="Exp(l) sürüségfv")
points(seq(0,2,0.05),dexp(seq(0,2,0.05),rate=l),type="l")
hist(x,freq = F,main="Exp(ML becsléssel kapott lambda) sürüségfv")
points(seq(0,2,0.05),dexp(seq(0,2,0.05),rate=opt$estimate),type="l", col="red")
hist(x,freq = F,main="Kettõ együtt")
points(seq(0,2,0.05),dexp(seq(0,2,0.05),rate=l),type="l")
points(seq(0,2,0.05),dexp(seq(0,2,0.05),rate=opt$estimate),type="p", col="red")