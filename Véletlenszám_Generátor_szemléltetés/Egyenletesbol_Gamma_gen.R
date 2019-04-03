#Egyenletes eloszlásból generáljunk Gamma(N,lambda)-t

gamma_gen<-function(n,N,lambda){
  Z = matrix(0, ncol=2, nrow=n)
  for (i in 1:n) {
    U=runif(N)   #N db egyenletest generálunk
    Z[i,1]=-sum(log(1-U)/lambda)}  #1 db gammáért
  Z[,2]<-rgamma(n,shape=N,rate=lambda)
  
  par(mfrow=c(1,2)) 
  hist(Z[,1],freq=F,main="Gamma from Uniform")
  points(seq(0,10,0.01),dgamma(seq(0,10,0.01),shape=N,rate=lambda),type="l",col="red")
  hist(Z[,2],freq=F,main="Gamma from R")
  points(seq(0,10,0.01),dgamma(seq(0,10,0.01),shape=N,rate=lambda),type="l",col="red")
}

gamma_gen(5000,10,2)