#CHT szemléltetése gamma eloszlással 
n<-10000 
k<-10
b<-1  #skála 
a<-1  #alak
x<-rgamma(n,a,b);

X<-matrix(x,ncol=k)
head(X)
col_sum<-apply(X,1,sum)

par(mfrow=c(1,2))
hist(col_sum,freq = F) #normálisra hasonló

#Alkalmazzuk a CHT-t
mean(col_sum)
sd(col_sum)

Z<-(col_sum-mean(col_sum))/sd(col_sum)  #Standardizálás
mean(Z)  
sd(Z)    

hist(Z,freq=F)  #Standard normal
lines(seq(-3,3,0.05),dnorm(seq(-3,3,0.05)),col="red")  #Standard normalis sfv
