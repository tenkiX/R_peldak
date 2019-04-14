#Egyenletes eloszlasbol generaljunk cauchy(l,s)-t

cauchy_gen<-function(N,l,s){
   U=runif(N)  			# N db egyenletes generalasa
   X=s*tan(pi*(U-1/2))+l  	# N db cauchy-hoz
   par(mfrow=c(1,2)) 
   x<-X[X>-10&X<10]
   hist(x,freq=F,main="Cauchy from Uniform")
   points(seq(-10,10,0.05),dcauchy(seq(-10,10,0.05),location=l,scale=s),type="l",col="red")
   Y=rcauchy(N,location=l, scale=s)  		# cacuhy generalasa
   y<-Y[Y>-10&Y<10]
   hist(y,freq=F,main="Cauchy from R")
   points(seq(-10,10,0.05),dcauchy(seq(-10,10,0.05),location=l,scale=s),type="l",col="red")
}

cauchy_gen(1000,2,1)

