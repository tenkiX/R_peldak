#Exponenciális eloszlás esetén CHT szemléltetés
n<-1000  #kisérlet száma
k<-10   # 1 kísérleten belül a dobások

lambda<-2
x<-rexp(n*k,rate=lambda);x

X<-matrix(x,ncol=k)
head(X)
col_sum<-apply(X,1,sum)

par(mfrow=c(1,2))
hist(col_sum,freq = F) #normálisra hasonló

#Alkalmazzuk a CHT-t
mean(col_sum) #4.939104 >>elméleti: 5=10*1/2
sd(col_sum) #4.846696   >>elméleti 1.581139=1/2*sqrt(10)

Z<-(col_sum-mean(col_sum))/sd(col_sum)  #Standardizálás
mean(Z)  #2.338928e-17  >>elméleti: 0
sd(Z)    #1     >>elméleti: 1

hist(Z,freq=F)  #Standard normal
lines(seq(-3,3,0.05),dnorm(seq(-3,3,0.05)),col="red")  #Standard normalis sfv
