#### R-ben Lineáris regresszió  #####
x<-runif(10,-5,5)  #Magyarázó változó (independent)
y<-x+rnorm(10,2,1)#Függõ változó (dependent)

#Fitting a linear model 
model<-lm(y~x);model  #egyenes, kiírt dolgok: tengelymetszet (intercept), meredekség (x)

#Summary of the model
summary(model)  
#kiírt dolgok: 
#tengelymetszet (intercept) 	H0 nullhipotézis	t,pr pedig a hipo-k paraméterei. ha p<0.025 elvetjük H0-t és H1-et fogadjuk el	
#meredekség (x)			H1			
#minél több csillagunk van annál jobb

#Grafika
plot(x,y)  #pontpárok kirajzolása
abline(model,col="red",lwd=2,lty=2)  #illesztett egyenes rajzolása

#Diagnostic plots: NOT a plot of the model itself!
#plot(model)  #Residual plot; QQplot  

#Elõrejelzés
model$coefficients
model$coefficients[1]+model$coefficients[2]* 2 #elõrejelzés Y=Intercept+Slope*30
#VAGY
predict(model,newdata=data.frame(x=2))  #elõrejelzés X=30-ra
#ábrán
plot(x,y)  
abline(model,col="red",lwd=2,lty=2)  
points(2,model$coefficients[1]+model$coefficients[2]*2,col="blue",lwd=3,pch=2)


#Hiba
predict(model)  #b[1]+b[2]*x: becsült értékek
y-predict(model)  # tényleges - becslés: a  hibáják
sum((y-predict(model))^2)  #hibák négyzetösszege

#modell hibája: Residual standard error
summary(model)
anova(model)
sqrt(sum((y-predict(model))^2)/(10-2))  #10-2:length(x)-tagok száma (konst,x)


plot(x,y,xlim=c(-5,5),ylim=c(-5,5))
abline(model,col="red",lwd=2,lty=2) 
p<-model$coefficients[1]+model$coefficients[2]*x
e<-abs(y-p)
rect(x,y,x+e,p) 

#Konfidencia intervallum
confint(model,level=0.95)

#Egy új x értékre becslést és konf int adni:
predict(model,newdata=data.frame(x=2),interval = "confidence", level = 0.95) 

#For a given value of x, the interval estimate for the mean of the dependent variable, ?y , is called the confidence interval.
newx <- seq(-5, 5, by=0.1);newx
confint<-predict(model,newdata=data.frame(x=newx),interval = "confidence");confint
par(mfrow=c(1,1))
plot(x,y, main="Confidence interval")
lines(newx, confint[,1], col="red", lty=1)
lines(newx, confint[,2], col="blue", lty=2)
lines(newx, confint[,3], col="blue", lty=2)

#Egyéb kétváltozós modellek

#1.Kvadratikus
x<-runif(10,-5,5)  
y<-x^2+rnorm(10,2,1)

plot(x,y)

#Fitting a kvadratic model 
model<-lm(y~x+I(x^2));model  #parabola
summary(model)

#Reduced model
model<-lm(y~I(x^2));model  #parabola
summary(model)

#Parabola illesztés
f<-model$coefficients[1]+model$coefficients[2]*seq(-5,5,0.1)^2
points(seq(-5,5,0.1),f,type="l",col="red")



#2. Logaritmikus
x<-runif(10,0,10)  
y<-x+log(x)+rnorm(10,3,2)

plot(x,y)

#Fitting a kvadratic model 
model<-lm(y~x+I(log(x)));model  #parabola
summary(model)

#Parabola illesztés
f<-model$coefficients[1]+model$coefficients[2]*seq(0,10,0.1)+model$coefficients[3]*log(seq(0,10,0.1))
points(seq(0,10,0.1),f,type="l",col="red")



#Többváltozós regresszió
longley
attach(longley)
head(longley)


#Full model: Residual standard error: 0.3049; Adjusted R-squared:  0.9925
model<-lm(Employed ~ GNP.deflator + GNP + Unemployed + Armed.Forces + Population + Year)
summary(model)


#redukálási cél: kiszedegetni a legnagyobb Pr értékû coefficientet -> ekkor res. stdr error csökken, R-squared nõ és ez nekünk jó
#Reduced model1:Residual standard error: 0.2897; Adjusted R-squared:  0.9932 
model<-lm(Employed ~  GNP + Unemployed + Armed.Forces + Population + Year)
summary(model)

#Reduced model2:Residual standard error: 0.2794; Adjusted R-squared:  0.9937 
model<-lm(Employed ~  GNP + Unemployed + Armed.Forces +  Year)
summary(model)

#Reduced model3: Residual standard error: 0.3321; Adjusted R-squared:  0.9911
model<-lm(Employed ~   Unemployed + Armed.Forces +  Year)
summary(model)

#Reduced model4: Residual standard error: 0.5017; Adjusted R-squared:  0.9796 
model<-lm(Employed ~   Unemployed +  Year)
summary(model)


#Residual standard error (x): HIBA  >> MIN
#Adjusted R-squared (y): magyarázó erõ >> MAX
x<-c(0.3049,0.2897,0.2794,0.3321,0.5017)
y<-c(0.9925,0.9932,0.9937,0.9911,0.9796)
plot(x,y,type="b",xlab="Residual standard error",ylab=" Adjusted R-squared")
text(x[1],y[1],"FM",c(1,1))
text(x[2],y[2],"RM1",c(1,1))
text(x[3],y[3],"RM2",c(0,0))
text(x[4],y[4],"RM3",c(1,1))
text(x[5],y[5],"RM4",c(1,1))
#BEST!!! >> Reduced model2: 
points(min(x),max(y),col="red",pch="*",cex=2)



