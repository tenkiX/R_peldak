#CHT Cauchy szemléltetés
n<-10000 

l<-0 #location
s<-1 #scale
x<-rcauchy(n,l,s);x
y<-x[x>-10 & x<10]
X<-matrix(y,ncol=k)
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
