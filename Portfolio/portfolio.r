#Portfólió tervezés
Legyen x0 kezdeti tõkénk, melyet n db befektetési eszközbe fektethetünk
Ezek megtérülési rátája: R1,...,Rn (valségi változó)
	Várható hozam:  E(Ri)=ri

Tekintsük a befektetési eszközök covarianciamátrixát
	Cij=cov(Ri,Rj)

Megjegyzés: ha a befektetési eszközök függetlenek:
	cov(Ri,Rj)=0
	cov(Ri,Ri)= szigma^2 (szórásnégyzet)
Ilyen esetben csak a fõátlóba vannak értékek

Cél: hozam->MAX
     kockázat->MIN

  kérdés: Hogyan fektessük be a pénzünket?(Melyik eszközbe mennyit? -> súlyok)
############################################################
************************************************************


#Adatok generálása
X<-matrix(0,ncol=12,nrow=11)

#Kis hozam, Kis kockázat - Nagy hozam, Nagy kockázat
for (i in 1:10){
  set.seed(i)
  X[i,]<-rnorm(12,mean=i,sd=i)}

#Kockázatmentes banki kötvény
X[11,]<-rep(0.05,n=12)  

X<-t(X)
head(X)
colnames(X)<-paste(1:11,'termék',sep='.')
row.names(X)<-paste(1:12,"hó",sep=".")
head(X)

#Éves hozam
r<-colMeans(X);r
par(mfrow=c(1,2))
plot(1:11,r,xlab="Befektetési eszközök", ylab="Éves hozam")

#Szórás
sigma<-NULL
for (i in 1:11){
  sigma[i]<-sd(X[,i])}
names(sigma)<-names(r)
plot(1:11,sigma,xlab="Befektetési eszközök", ylab="Szórás")

#Fektessünk be!
par(mfrow=c(1,1))
plot(0:10,0:10,type="n",xlab="Kockázat",ylab="Hozam",main="Lehetséges portfóliók")


#1. Csak a legnagyobb hozamúba fektessük a pénzünket.
which.max(r)
which.max(sigma)  #egyben a legnagyobb kockázatú is
w1<-c(0,0,0,0,0,0,1,0,0,0,0) #7. eszközbe tesszük az 1 egységnyi pénzünket
h1<-sum(r*w1);h1 #várható hozam
k1<-sqrt(w1%*%cov(X)%*%w1);k1 #kockázat
points(k1,h1,col="blue",pch=16)
text(k1,h1,"1",c(1,0))

#2. Egyformán fektessük be pénzünket
w2<-rep(1/11,11)
h2<-sum(r*w2);h2
k2<-sqrt(w2%*%cov(X)%*%w2);k2
points(k2,h2,col="blue",pch=16)
text(k2,h2,"2",c(1,0))

#3. Csak kötvénybe fektessük be pénzünket
w3<-c(rep(0,10),1)
h3<-sum(r*w3);h3
k3<-sqrt(w3%*%cov(X)%*%w3);k3
points(k3,h3,col="blue",pch=16)
text(k3,h3,"3",c(1,0))

#4. Kaptunk egy tippet
sort(r) 
w4<-c(0, 0, 0, 0.1, 0, 0.2, 0.25, 0, 0.25, 0.2, 0 ) #5 legnagyobb hozamúba rakunk
sum(w4)
h4<-sum(r*w4);h4
k4<-sqrt(w4%*%cov(X)%*%w4);k4
points(k4,h4,col="blue",pch=16)
text(k4,h4,"4",c(1,0))

#5. Egyéni ötlet:
w5<-c(0,0.3,0,0.2,0.1,0.2,0,0,0,0,0.2)
sum(w5)
h5<-sum(r*w4);h4
k5<-sqrt(w5%*%cov(X)%*%w5);k5
points(k5,h5,col="green",pch=16)
text(k5,h5,"5",c(1,0))

install.packages('quadprog')
library(quadprog)

#Mi lesz az optimális portfólió? 
#Azaz a legnagyobb hozamú és legkisebb kockázatú?
portfolio_fv <- function(data,r)
{
  n <- ncol(data)   #adatsor oszlopainak a száma
  Dmat <-  cov(data)   ### kovarianciamátrix
  dvec <-  rep(0, times=n)  ### nullvektor n elemû
  Amat <-  cbind(colMeans(data), rep(1, n),diag(n))   #oszloponként összerakja a mátrixot
  bvec <-  c(r, 1, rep(0, times=n))    #vektort állít elõ
  meq <-  2
  portfolio = solve.QP(Dmat, dvec, Amat, bvec, meq)   #optimalizáció!!!
  weights = round(portfolio$solution, digits = 4)  #megoldás kerekítése-->> SÚLYOK
  names(weights) = colnames(data)
  list(weights = weights,risk = portfolio$value,return = r)
}

data<-X[,1:10]  #Kivesszük akötvényt
r <- mean(r[-11]) #kivesszük a kötvény hozamát
opt_portfolio<-portfolio_fv(data,r)

#Optimális portfólió súlyai
w_opt<-round(opt_portfolio$weights*100,2);w_opt

#Optimális portfóiló hozama
r_opt<-opt_portfolio$return;r_opt 

#Optimális portfóiló kockázata
risk_opt<-sqrt(opt_portfolio$weights%*%cov(data)%*%opt_portfolio$weights);risk_opt

points(risk_opt,r_opt,col="red",pch=16)
text(risk_opt,r_opt,"opt",c(1,0))

## Hatékony portfóliók görbéje
#Azaz az adott kockázati szinten a legnagyobb hozamú portóflió!
eff_port_fv <- function(data)
{
  n <-  ncol(data)
  ri <-  colMeans(data)
  p<- 3*n
  r <- seq(min(ri), max(ri), length=p)
  weights <-  rep(0, n)
  weights[which.min(ri)] <-  1
  for (i in 2:(p-1)) {
    newWeights <- portfolio_fv(data, r[i])$weights
    weights <-  rbind(weights, newWeights)}  #soronként
  newWeights <-  rep(0, n)
  newWeights[which.max(ri)]<-  1
  weights <- rbind(weights, newWeights)
  weights <-  round(weights, 4)
  colnames(weights) = colnames(data)
  rownames(weights) = 1:p
  list(weights = weights, r = r)
  }

eff_port<-eff_port_fv(data)

#A különbözõ hozamokhoz tartozó optimális portfóliók súlyai
weights <- eff_port$weights;weights
apply(weights,1,sum) #ellenõrzés, sorösszeg 1
r_eff<-eff_port$r;r_eff

#A különbözõ hozamokhoz tartozó optimális portfóliók kockázatai
risks <- NULL
for (i in 1:nrow(weights)) {
  new_risk <- sqrt(weights[i, ] %*% cov(data) %*% weights[i, ])
  risks <- c(risks, new_risk)
}
risks

#Hatékony portfóliók görbéje
points(risks, r_eff,type="l")





##PÉLDA### Swiss pension fund assets returns benchmark - Hozamok a Svájci nyugdíjalapoknál
install.packages('fBasics')
library(fBasics)
data <- 100 * LPP2005REC[, 1:6]; data[1:10,] #100* az LPP2005REC adatsor 1-6 oszlopai
plot(LPP2005REC[, 1:6])

# PORTFÓLIÓ ELVÁRT HOZAMA (oszopátlagok átlaga)
r <- mean(colMeans(data)) 

#AZ OPTIMÁLIS PORTFÓLIÓ
portfolio <- portfolio_fv(data,r);portfolio   

#EREDMÉNY
weights <- portfolio$weights*100;weights  #súlyok kiiratása
sum(weights)   #súlyok összege 100
c(weightedReturn = round((weights %*% colMeans(data))[[1]],3), r = round(100 * r, 3))  #ELLenõrzés
Weights <- weights[weights > 0]   #csak a pozitív súlyúak
pie(Weights, labels = names(Weights),main="LPP2005 Portfolio Weights")

r_opt<-portfolio$return
risk_opt<-sqrt(portfolio$weights%*%cov(data)%*%portfolio$weights)

eff_port<-eff_port_fv(data)
weights <- eff_port$weights;weights
r<-eff_port$r;r

risks <- NULL
for (i in 1:nrow(weights)) {
  new_risk <- sqrt(weights[i, ] %*% cov(data) %*% weights[i, ])
  risks <- c(risks, new_risk)
}
risks
plot(risks, r,type="l",xlab="Kockázat",ylab="Hozam",main="Hatékony portfóliók görbéje")
points(risk_opt,r_opt,col="red",pch=16)
text(0.18,0.05,"(0.24,0.043)")
