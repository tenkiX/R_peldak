#########################################################

#Egyenletes eloszlasbol generaljunk Weibull(a=shape,b=scale)-t

weibull_gen<-function(N,a,b){
   U=runif(N)  			# N db egyenletes generalasa
   X=b*(-log(1-U))^(1/a)  	# N db weibull-hoz
   par(mfrow=c(1,2)) 
   hist(X,freq=F,main="Weibull from Uniform")
   points(seq(-N,N,0.01),dweibull(seq(-N,N,0.01),shape=a,scale=b),type="l",col="red")
   Y=rweibull(N,shape=a,scale=b)  		# weibull generalasa
   hist(Y,freq=F,main="Weibull from R")
   points(seq(-N,N,0.01),dweibull(seq(-N,N,0.01),shape=a,scale=b),type="l",col="red")
}

weibull_gen(1000,2,1)