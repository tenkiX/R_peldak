#Egyenletes eloszlásból generáljunk Exp(lambda)-t

lambda<-2
N<-1000

exp_gen<-function(N,lambda){
  U=runif(N)  #N db egyenletes generálása
  X=-1/lambda*log(1-U)  #Elkészítjük az N db exponenciálist
  Y=rexp(N,lambda)  #Exp generálása
  par(mfrow=c(1,2)) 
  hist(X,freq=F,main="Exp from Uniform")
  points(seq(0,10,0.01),dexp(seq(0,10,0.01),lambda),type="l",col="red")
  hist(Y,freq=F,main="Exp from R")
  points(seq(0,10,0.01),dexp(seq(0,10,0.01),lambda),type="l",col="blue")
}

exp_gen(10000,1)