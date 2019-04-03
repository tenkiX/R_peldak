iskolazottsag<-c(12.6,12.4,11.6,10.4,4.4)  #Magyarázó változó (independent)
varhato_elettartam<-c(81.1,78.5,75.4,74,65.4) #Függõ változó (dependent)

#Fitting a linear model 
model<-lm(varhato_elettartam~iskolazottsag);model  #egyenes

#Intercept:  x=0-hoz tartozó y érték, azaz ha valaki
#0 évet tanul, akkor ennyi a várható élettartama  (beta0)
#Másik érték: ha 1 évvel többet tanul, akkor annyi a növekedett
#élettartam. Azaz ha az x értékét egyel növeljük akkor az
#y annyival növekszik

#Summary of the model
summary(model) 

elorejelzes<-predict(model,newdata=data.frame(iskolazottsag=12))  #elõrejelzés

#Grafika
plot(iskolazottsag,varhato_elettartam)  #pontpárok kirajzolása
abline(model,col="red")  #ilesztett egyenes rajzolása
points(12,elorejelzes,col="blue",lwd=3,pch=2) #elõrejelzett érték jelölése
