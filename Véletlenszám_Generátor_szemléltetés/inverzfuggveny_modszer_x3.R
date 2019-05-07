#x^3 gen inverzzel
#this software comes with not warranty, mert máté pl máshogy csinálta
n <- 1000
x <- runif(n);
y <-  x^(1/3)

hist(y,freq=F)

z<-seq(0,1,1/n)
points(z,(z ^ 2)*3 , type="l", col="red")
