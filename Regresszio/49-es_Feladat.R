iskolazottsag<-c(12.6,12.4,11.6,10.4,4.4)  #Magyar�z� v�ltoz� (independent)
varhato_elettartam<-c(81.1,78.5,75.4,74,65.4) #F�gg� v�ltoz� (dependent)

#Fitting a linear model 
model<-lm(varhato_elettartam~iskolazottsag);model  #egyenes

#Intercept:  x=0-hoz tartoz� y �rt�k, azaz ha valaki
#0 �vet tanul, akkor ennyi a v�rhat� �lettartama  (beta0)
#M�sik �rt�k: ha 1 �vvel t�bbet tanul, akkor annyi a n�vekedett
#�lettartam. Azaz ha az x �rt�k�t egyel n�velj�k akkor az
#y annyival n�vekszik

#Summary of the model
summary(model) 

elorejelzes<-predict(model,newdata=data.frame(iskolazottsag=12))  #el�rejelz�s

#Grafika
plot(iskolazottsag,varhato_elettartam)  #pontp�rok kirajzol�sa
abline(model,col="red")  #ilesztett egyenes rajzol�sa
points(12,elorejelzes,col="blue",lwd=3,pch=2) #el�rejelzett �rt�k jel�l�se
