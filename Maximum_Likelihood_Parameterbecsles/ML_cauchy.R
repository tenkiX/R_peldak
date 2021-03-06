#Cauchy eloszlas
n<-10000

l<-0 #location ezt fogjuk becs�lni
s<-1 #scale ezt fogjuk becs�lni

x<-rcauchy(n,l,s)
z<-x[x>-10 & x<10] #sz�r�s

#Log-likelihood f�ggv�ny:
cauchy_loglik<-function(y,par){
  loglik<-sum(dcauchy(y,location=par[1], scale=par[2],log=TRUE))
  return(-loglik)
}

opt<-nlm(f=cauchy_loglik, p=c(0,1), y=z);opt #kiindul� �rt�k l=1, s=1
opt$estimate #param�ter becsl�se

#Mit csin�ltunk?
par(mfrow=c(1,2))
hist(z,freq = F,main="Cauchy(l,s) s�r�s�gfv")
points(seq(-8,8,0.1),dcauchy(seq(-8,8,0.1),l,s),type="l")
hist(z,freq = F,main="Cauchy(ML becsl�ssel kapott l �s s) s�r�s�gfv")
points(seq(-8,8,0.1),dcauchy(seq(-8,8,0.1),opt$estimate[1],opt$estimate[2]),type="l", col="red")

