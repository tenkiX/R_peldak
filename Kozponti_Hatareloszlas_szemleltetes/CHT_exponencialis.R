#Exponenci�lis eloszl�s eset�n CHT szeml�ltet�s
n<-1000  #kis�rlet sz�ma
k<-10   # 1 k�s�rleten bel�l a dob�sok

lambda<-2
x<-rexp(n*k,rate=lambda);x

X<-matrix(x,ncol=k)
head(X)
col_sum<-apply(X,1,sum)

par(mfrow=c(1,2))
hist(col_sum,freq = F) #norm�lisra hasonl�

#Alkalmazzuk a CHT-t
mean(col_sum) #4.939104 >>elm�leti: 5=10*1/2
sd(col_sum) #4.846696   >>elm�leti 1.581139=1/2*sqrt(10)

Z<-(col_sum-mean(col_sum))/sd(col_sum)  #Standardiz�l�s
mean(Z)  #2.338928e-17  >>elm�leti: 0
sd(Z)    #1     >>elm�leti: 1

hist(Z,freq=F)  #Standard normal
lines(seq(-3,3,0.05),dnorm(seq(-3,3,0.05)),col="red")  #Standard normalis sfv
