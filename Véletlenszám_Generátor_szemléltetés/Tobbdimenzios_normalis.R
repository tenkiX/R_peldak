#n dimenziós normál eloszlás generálása
install.packages("mvtnorm")
library(mvtnorm)

mean=c(1,2)
sigma = diag(length(mean))
x <- rmvnorm(n=500, mean=mean, sigma=sigma)
plot(x)


#mj.: sigma <- matrix(c(4,2,2,3), ncol=2) #szimm mátrixnak kell lennie


