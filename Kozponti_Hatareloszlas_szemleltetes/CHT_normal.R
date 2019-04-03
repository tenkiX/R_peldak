#CHT szeml�ltet�se norm�l eloszl�ssal 
n<-10000 

m<-2 #v�rhat� �rt�k
sd<-10  #sz�r�s
x<-rnorm(n,mean,sd);

X<-matrix(x,ncol=k)
head(X)
col_sum<-apply(X,1,sum)

par(mfrow=c(1,2))
hist(col_sum,freq = F) #norm�lisra hasonl�

#Alkalmazzuk a CHT-t
mean(col_sum)
sd(col_sum)

Z<-(col_sum-mean(col_sum))/sd(col_sum)  #Standardiz�l�s
mean(Z)  
sd(Z)    

hist(Z,freq=F)  #Standard normal
lines(seq(-3,3,0.05),dnorm(seq(-3,3,0.05)),col="red")  #Standard normalis sfv