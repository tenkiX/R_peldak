#Gamma eloszl�s a - hely, lambda - sk�la param�ter�nek becsl�se
n<-1000
a<-5 #alak (shape), ezt fogjuk becs�lni!!!
l<-3 #sk�la (scale), ezt fogjuk becs�lni!!!

x<-rgamma(n,shape=a,scale=l)

#Log-likelihood f�ggv�ny:
gamma_loglik<-function(y,par){
  loglik<-sum(dgamma(y,shape=par[1], scale=par[2],log=TRUE))
  return(-loglik)
}

opt<-nlm(f=gamma_loglik, p=c(1,1), y=x);opt #kiindul� �rt�k a=1, l=1
opt$estimate #param�ter becsl�se

#Mit csin�ltunk?
par(mfrow=c(1,2))
hist(x,freq = F,main="Gamma(a,l) s�r�s�gfv")
points(seq(0,a^2,0.1),dgamma(seq(0,a^2,0.1),shape=a,scale=l),type="l")
hist(x,freq = F,main="Gamma(ML becsl�ssel kapott a �s l) s�r�s�gfv")
points(seq(0,a^2,0.1),dgamma(seq(0,a^2,0.1),shape=opt$estimate[1],scale=opt$estimate[2]),type="l", col="red")