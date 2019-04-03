#Kockadobás szimuláción keresztul CHT szemléltetés
n<-1000  #kisérlet száma
k<-10   # 1 kísérleten belül a dobások

x<-round(runif(n*k,1,6))
x[1:100]

X<-matrix(x,ncol=k)  #n: sor (kísérletek), k:oszlop (dobások)
head(X)

col_sum<-apply(X,1,sum);col_sum  #minden kísérlet dobásösszege (1: sor, 2: oszlop)

par(mfrow=c(1,2))
hist(col_sum,freq = F) #normálisra hasonló

#Alkalmazzuk a CHT-t
mean(col_sum) #34.856 >> elméleti: 35=10*3.5
sd(col_sum) #4.764448 >> elméleti: 4.564355=sqrt((6-11))^2/12)*sqrt(10)

Z<-(col_sum-mean(col_sum))/sd(col_sum);Z  #Standardizálás

mean(Z)  #3.479812e-16 >> elméleti: 0
sd(Z)    #1 >> elméleti: 1

hist(Z,freq=F)  #Standard normal
lines(seq(-3,3,0.05),dnorm(seq(-3,3,0.05)),col="red")  #Standard normalis >>> CHT
