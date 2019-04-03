#Inverz fgv módszer általánosan

#1. álltás Exp(lambda) #ennek normál eloszlásra kell hasonlítani
n<-1000
lambda<-2
rexp(n, rate=lambda)
x<-pexp(rexp(n, rate=lambda), rate=lambda)
par(mfrow=c(1,2))
plot(1:n,x)
hist(x)

#1. állítás: norm(mean,sd) #ennek normál eloszlásra kell hasonlítani
n<-1000
m<-2
s<-1
x<-pnorm(rnorm(n,m,s),m,s)
par(mfrow=c(1,2))
plot(1:n,x)
hist(x,freq=F)

#2. állítás exp(lambda)
m<-1000
lambda<-2
x<-qexp(runif(n),rate=lambda)
par(mfrow=c(1,2))
hist(x,freq=F)
points(seq(0,4,0.05),dexp(seq(0,4,0.05),rate=lambda), type="l", col="red")

x<-qexp(runif(n),rate=lambda)
y<-qexp(runif(n),rate=lambda)
plot(x,y)

#2. állítás: Cauchy(1,s)
n<-1000
l<-0
s<-1
runif(n)
x<-qcauchy(runif(n),location=1,scale=s)
y<-x[x>-10 & x<10]
plot(y)
hist(y,freq=F)

points(seq(-10,10,0.05),dcauchy(seq(-10,10,0.05),location=l,scale=s),type="l", col="red")


#2. állítás: norm(mean,sd)
n<-1000
m<-2
s<-1
rnorm(n,m,s)
par(mfrow=c(1,2))
x<-qnorm(runif(n),m,s)
hist(x,freq=F)
plot(x)
#énadtamhozzá csak ezt
#standardizálás

Z<-(x-mean(x))/sd(x)  #Standardizálás

hist(Z,freq=F)  #Standard normal
lines(seq(-3,3,0.05),dnorm(seq(-3,3,0.05)),col="red")  #Standard normalis sfv

