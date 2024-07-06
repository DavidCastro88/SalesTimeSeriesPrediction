rm(list=ls(all=TRUE))
library(forecast)
library(car)
library(carData)
library(TSA)
library(fANCOVA)
library(pdR)
library(timsac)
library(lmtest) 
library(uroot)
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funcion-regexponencial.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funciones-BP.LB.test-pruebaDW1.R")
source("https://raw.githubusercontent.com/NelfiGonzalez/Funciones-de-Usuario-Estadistica-III/main/Funciones-Criterios.Informacion-Calidad.Intervalos.R")

Data=read.table(file.choose(),header=T,sep=";")
serie=ts(Data$nominal_sales,freq=12,start=c(2001,1))
win.graph()
par(mfrow = c(1, 2))
#GRAPH
#win.graph() 
plot(serie,xlab = "Time",
     ylab='Serie',
     main = "Nominal Footwear Sales Indexes", 
     type = "l",                        
     lwd = 2,                   
     cex.lab = 1.5,             
     cex.main = 1.2)
grid(col = "gray", lty = "dotted")
boxplot(serie~cycle(serie),names=month.abb,family = "A",ylab='',xlab='')
#We cut the series
m=12
n=length(serie)-m
yt=ts(serie[1:n],freq=m,start=c(2001,1))



#First regular difference
difReg=diff(yt)
win.graph() 
plot(difReg,xlab = "Time",ylab='∇Yt', main = "First regular difference ∇Yt ", 
     type = "l",                        
     lwd = 2,                   
     cex.lab = 1.5,             
     cex.main = 1.8)
abline(h=mean(difReg),lwd=1)

#First seasonal difference
difEst=diff(yt,lag=12)
win.graph() 
plot(difEst,xlab = "Time",ylab='∇12Yt', main = "First seasonal difference ∇12Yt ", 
     type = "l",                        
     lwd = 2,                   
     cex.lab = 1.5,             
     cex.main = 1.8)
abline(h=mean(difEst),lwd=1)

#First seasonal difference and First regular difference
difRegEst=diff(diff(yt,12),1)
win.graph() 
plot(difRegEst,xlab = "Time",ylab='∇∇12Yt', main = "First seasonal and regular difference ∇∇12Yt ", 
     type = "l",                        
     lwd = 2,                   
     cex.lab = 1.3,             
     cex.main = 1.2)
abline(h=mean(difRegEst),lwd=1)


#ACF'S
#yt
win.graph() 
acf(as.numeric(yt),main="",lag.max=36,ci.type="ma",ci.col=2,lwd=2,family = "A",cex.axis = 1)
title(main="ACF Yt",cex.main=1)
#∇∇12Yt
win.graph()
par(mfrow = c(2, 1))
acf(as.numeric(difRegEst),main="",lag.max=36,ci.type="ma",lwd=2)
title(main="ACF regular and seasonal difference(d=D=1)",cex.main=1)
abline(v=c(12,24,36),lty=2,col=2)
#win.graph()
pacf(as.numeric(difRegEst),lag.max=36,lwd=2,main="")
title(main="PACF regular and seasonal difference(d=D=1)",cex.main=1)
abline(v=c(12,24,36),lty=2,col=2)

#Test Hegy sobre serie recortada
HEGY.test(wts=yt,itsd=c(0,0,c(0)),selectlags=list(mode="aic", Pmax=12))$stats

#Auto.arima()
auto.arima(yt,ic="aic",seasonal.test="ocsb")
auto.arima(yt,ic="aic",seasonal.test="ch")
auto.arima(yt,ic="aic",seasonal.test="seas")
auto.arima(yt,ic="bic",seasonal.test="ocsb")
auto.arima(yt,ic="bic",seasonal.test="ch")
auto.arima(yt,ic="bic",seasonal.test="seas")

#armasubsets()
win.graph()
plot(armasubsets(difRegEst,nar=24,nma=24,y.name='Y',ar.method='ols'))


#-----MODEL 1:ARIMA(0,1,11)(1,1,2)[12] SOLO ma(1,3,11) SAR(1) SMA(2)
modelo1=Arima(yt,order=c(0,1,11),seasonal=list(order=c(1,1,2)),fixed=c(NA,0,NA,rep(0,7),NA,NA,0,NA),method="ML")
k1=length(modelo1$coef[modelo1$coef!=0]);k1 #Calcular k el total de par?ametros del modelo
coeftest(modelo1)
summary(modelo1)
#-----MODEL 2:ARIMA(0,1,11)(1,1,2)[12] SOLO ma(1,3,5,11) SAR(1) SMA(2) ------------------------------------------------------
modelo2=Arima(yt,order=c(0,1,11),seasonal=list(order=c(1,1,2)),fixed=c(NA,0,NA,0,NA,rep(0,5),NA,NA,0,NA),method="ML")
k2=length(modelo2$coef[modelo2$coef!=0]);k2
coeftest(modelo2)
summary(modelo2)

#Modelo 1
win.graph()
ythat1=modelo1$fitted #Este objeto ya tiene fechas
plot(serie,ylab='Serie',lwd=2)
lines(ythat1,col=2,lwd=2)
legend("topleft",legend=c("Serie","Fitted Model 1"),lty=1,col=(1:4),lwd=2, cex=1.2)

#AIC-BIC
Criteriosmodelo1=exp.crit.inf.resid(residuales=residuals(modelo1),n.par=k1); Criteriosmodelo1
Criteriosmodelo2=exp.crit.inf.resid(residuales=residuals(modelo2),n.par=k2); Criteriosmodelo2 

#Ljun-Box
BP.LB.test(residuals(modelo1),maxlag=36,type="Ljung")
BP.LB.test(residuals(modelo2),maxlag=36,type="Ljung")

#Normalidad
shapiro.test(residuals(modelo1))
shapiro.test(residuals(modelo2))
#Pronostico
predmod1=ts(as.data.frame(forecast(modelo1,h=12,level=95)),freq=12,start=c(2023,5));predmod1
ytpron1=predmod1[,1] #Pron?ostico puntual
predmod2=ts(as.data.frame(forecast(modelo2,h=12,level=95)),freq=12,start=c(2023,5));predmod2
ytpron2=predmod2[,1] #Pron?ostico puntual

#Definiendo variables para pron?sticos
tnuevo=(n+1):length(serie) #?ndice de tiempo en los pron?sticos
ytest=ts(serie[tnuevo],freq=12,start=c(2023,5))
#Medidas de pronostico
unloadNamespace("Metrics")
accuracy(ytpron1,ytest)
amplitud.cobertura(real=ytest,LIP=predmod1[,2],LSP=predmod1[,3]) 
accuracy(ytpron2,ytest)
amplitud.cobertura(real=ytest,LIP=predmod2[,2],LSP=predmod2[,3]) 

#r2
SSE <- sum((ytest - ytpron1)^2)  
SST <- sum((ytest - mean(ytpron1))^2)  
R2 <- 1 - (SSE / SST)

#Graph Predictions
win.graph()
p=cbind(ytest,ytpron1)
plot(main="Predictions SARIMA model",p,plot.type="single",type="b",pch=c(19,1:2),col=c("black","blue"),lty=1,lwd=2,ylab="Ytf",xaxt="n")
axis(1,at=time(p),labels=c("May-23","Jun-23","Jul-23","Aug-23","Sep-23","Oct-23","Nov-23","Dec-23","Jan-24","Feb-24","Mar-24","Apr-24"))
legend("topleft",legend=c("Real","Predictions"),
       pch=c(19,1:2),lty=c(1:2),col=c("black","blue"),lwd=2,cex=1.2)

#Predictions May-2024  to April-2025
#Fitted Model with all years
yt_full=ts(serie,freq=m,start=c(2001,1))
#-----MODEL 1:ARIMA(0,1,11)(1,1,2)[12] SOLO ma(1,3,11) SAR(1) SMA(2)
model=Arima(yt_full,order=c(0,1,11),seasonal=list(order=c(1,1,2)),fixed=c(NA,0,NA,rep(0,7),NA,NA,0,NA),method="ML")
k1=length(model$coef[model$coef!=0]);k1 #Calcular k el total de par?ametros del modelo
coeftest(model)
summary(model)

#Predictions 
predictions=ts(as.data.frame(forecast(model,h=12,level=95)),freq=12,start=c(2024,5));predictions
ytpron=predictions[,1] #Pron?ostico puntual
lo95 <- predictions[,2]
hi95 <- predictions[,3]

#Graph
win.graph() 
plot(ytpron, type = "l", col = "black", lwd = 2, xlab = "Time",ylab="Value",xaxt="n",
     main = "Forecasts with 95% Confidence Intervals", ylim = range(c(lo95, hi95)))
axis(1, at = time(predictions), labels = c("May-24","Jun-24","Jul-24","Aug-24","Sep-24","Oct-24","Nov-24","Dec-24","Jan-25","Feb-25","Mar-25","Apr-25"))
lines(lo95, col = "green", lwd = 2, lty = 2)  
lines(hi95, col = "green", lwd = 2, lty = 2)  
polygon(c(time(ytpron), rev(time(ytpron))), c(lo95, rev(hi95)), col = rgb(0, 1, 0, 0.2), border = NA)
legend("topleft", legend = c("Forecasts", "95% Confidence Intervals"), 
       col = c("black", "green"), lwd = 2, lty = c(1, 2), fill = c(NA, rgb(0, 1, 0, 0.2)))