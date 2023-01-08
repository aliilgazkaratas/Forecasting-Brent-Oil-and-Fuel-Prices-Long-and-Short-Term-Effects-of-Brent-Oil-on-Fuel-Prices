library(forecast)
library(TSstudio)
library(tidyverse)
library(ggfortify)
library(tseries)
library(imputeTS)
library(ggplot2)
library("xts")
library("TSstudio")
library("performance")
library(dLagM) # ARDL model in R
library(ARDL)
library(vars)

data = read.csv("data_fuel.csv", header=TRUE)
data=data[,c(-1,-4,-7)] #the data which is not used are removed
data[215,5]=2.88 #The last data was NA, it is added.
head(data)
summary(data)
str(data)

par(mfrow=c(2,2))
#investigation of trend seasonality
ts_decompose(gasoTS)
ts_decompose(dieselTS)
ts_decompose(brentTS)
ts_decompose(cpiTS)

#stationary tests
adf.test(data$UnleadedGasoline)
adf.test(data$DieselOil)
adf.test(data$Brent.TRY)
adf.test(data$CPI.2)

adf.test(diff(data$UnleadedGasoline))
adf.test(diff(data$DieselOil))
adf.test(diff(data$Brent.TRY))
adf.test(diff(data$CPI.2))


#ARIMA MODELS
#brentTS
brentTS=ts(data$Brent.TRY, start=c(2005,1),end=c(2022,11), frequency=12)
par(mfrow=c(1,2))
acf(brentTS,lag.max = 60)
pacf(brentTS,lag.max =60)
brentTS1=diff(brentTS)
par(mfrow=c(1,2))
plot(brentTS)
plot(brentTS1)
acf(brentTS1,lag.max = 60)
pacf(brentTS1,lag.max =60)

arima(brentTS,order=c(0,1,1),seasonal = c(0,0,2))
arima(brentTS,order=c(0,1,2),seasonal = c(0,0,2)) #selected model
arima(brentTS,order=c(1,1,1),seasonal = c(0,0,2))
arima(brentTS,order=c(1,2,2),seasonal = c(0,0,2)) 

best.brentmodel = arima(brentTS,order=c(0,1,2),seasonal = c(0,0,2)) 
summary(best.brentmodel)
checkresiduals(best.brentmodel)
forecast.brent  = forecast(best.brentmodel, h = 25)
accuracy(forecast.brent)
plot(forecast.brent, main ="Forecast for Brent with SARIMA")

#gasoTS
gasoTS=ts(data$UnleadedGasoline, start=c(2005,1),end=c(2022,11), frequency=12)
par(mfrow=c(1,2))
acf(gasoTS,lag.max = 60)
pacf(gasoTS,lag.max =60)
gasoTS1=diff(gasoTS)
par(mfrow=c(1,2))
plot(gasoTS)
plot(gasoTS1)
acf(gasoTS1,lag.max = 60)
pacf(gasoTS1,lag.max =60)

arima(gasoTS,order=c(0,1,1),seasonal = c(0,0,2))
arima(gasoTS,order=c(0,1,2),seasonal = c(0,0,2))
arima(gasoTS,order=c(1,1,1),seasonal = c(0,0,2))
arima(gasoTS,order=c(2,1,1),seasonal = c(0,0,2)) #selected model

best.gasomodel = arima(gasoTS,order=c(2,1,1),seasonal = c(0,0,2))
summary(best.gasomodel)
checkresiduals(best.gasomodel)
forecast.gaso  = forecast(best.gasomodel, h = 25)
accuracy(forecast.gaso )
plot(forecast.gaso,main ="Forecast for Gasoline with SARIMA" )


#dieselTS
dieselTS=ts(data$DieselOil, start=c(2005,1),end=c(2022,11), frequency=12)
par(mfrow=c(1,2))
plot(dieselTS)
acf(dieselTS,lag.max = 60)
pacf(dieselTS,lag.max =60)
dieselTS1=diff(dieselTS)
par(mfrow=c(1,2))
plot(dieselTS)
plot(dieselTS1)
acf(dieselTS1,lag.max = 60)
pacf(dieselTS1,lag.max =60)

arima(dieselTS,order=c(2,1,3),seasonal = c(0,0,1)) #selected model
arima(dieselTS,order=c(2,1,2),seasonal = c(0,0,1))
arima(dieselTS,order=c(1,1,3),seasonal = c(0,0,1))
arima(dieselTS,order=c(1,1,2),seasonal = c(0,0,1)) 

best.dieselmodel = arima(dieselTS,order=c(2,1,3),seasonal = c(0,0,1))
summary(best.dieselmodel)
checkresiduals(best.dieselmodel)
forecast.diesel  = forecast(best.dieselmodel, h = 25)
accuracy(forecast.diesel )
plot(forecast.diesel, main ="Forecast for Diesel Oil with SARIMA")


#cpiTS
cpiTS=ts(data$CPI.2, start=c(2005,1),end=c(2022,11), frequency=12)
par(mfrow=c(1,2))
acf(cpiTS,lag.max = 60)
pacf(cpiTS,lag.max =60)
cpiTS1=diff(cpiTS)
par(mfrow=c(1,2))
plot(cpiTS)
plot(cpiTS1)
acf(cpiTS1,lag.max = 60)
pacf(cpiTS1,lag.max =60)

arima(cpiTS,order=c(0,1,2),seasonal = c(0,0,2)) #selected model
arima(cpiTS,order=c(0,1,1),seasonal = c(0,0,2))
arima(cpiTS,order=c(1,1,0),seasonal = c(0,0,2))
arima(cpiTS,order=c(1,1,1),seasonal = c(0,0,2)) 


best.cpimodel = arima(cpiTS,order=c(0,1,2),seasonal = c(0,0,2))
summary(best.cpimodel)
checkresiduals(best.cpimodel)
forecast.cpi = forecast(best.cpimodel, h = 25)
accuracy(forecast.cpi)
plot(forecast.cpi, main ="Forecast for CPI with SARIMA")


#HOLTWINTER'S MODELS

#for brent
hw_brent=HoltWinters(brentTS,seasonal = "multiplicative")
hw_pre_brent=predict(hw_brent, 25, prediction.interval = TRUE)
plot(hw_brent)
plot(brentTS, ylab="Price of Brent Petrol",xlab="Years")
lines(hw_brent$fitted[,1], lty=2, col="green") #hw fitted values
lines(hw_pre_brent[,1], col="red") #prediction
lines(hw_pre_brent[,2], lty=2, col="orange") #confidence intervals
lines(hw_pre_brent[,3], lty=2, col="orange")

checkresiduals(hw_brent)
hw_for_brent=forecast(hw_brent,h=25)
accuracy(hw_for_brent)
plot(hw_for_brent)

#for gasoline
hw_gaso=HoltWinters(gasoTS,seasonal = "multiplicative")
hw_pre_gaso=predict(hw_gaso, 25, prediction.interval = TRUE)
plot(hw_gaso)
plot(gasoTS, ylab="Price of Unleaded Gasoline",xlab="Years")
lines(hw_gaso$fitted[,1], lty=2, col="green") #hw fitted values
lines(hw_pre_gaso[,1], col="red") #prediction
lines(hw_pre_gaso[,2], lty=2, col="orange") #confidence intervals
lines(hw_pre_gaso[,3], lty=2, col="orange")

checkresiduals(hw_gaso)
hw_for_gaso=forecast(hw_gaso,h=25)
accuracy(hw_for_gaso)
plot(hw_for_gaso)

#for diesel
hw_diesel=HoltWinters(dieselTS,seasonal = "multiplicative")
hw_pre_diesel=predict(hw_diesel, 25, prediction.interval = TRUE)
plot(hw_diesel)
plot(dieselTS, ylab="Price of Diesel Oil",xlab="Years")
lines(hw_diesel$fitted[,1], lty=2, col="green") #hw fitted values
lines(hw_pre_diesel[,1], col="red") #prediction
lines(hw_pre_diesel[,2], lty=2, col="orange") #confidence intervals
lines(hw_pre_diesel[,3], lty=2, col="orange")

checkresiduals(hw_diesel)
hw_for_diesel=forecast(hw_diesel,h=25)
accuracy(hw_for_diesel)
plot(hw_for_diesel)

#for cpi
hw_cpi=HoltWinters(cpiTS,seasonal = "multiplicative")
hw_pre_cpi=predict(hw_cpi, 25, prediction.interval = TRUE)
plot(hw_cpi)
plot(cpiTS, ylab="CPI",xlab="Years")
lines(hw_cpi$fitted[,1], lty=2, col="green") #hw fitted values
lines(hw_pre_cpi[,1], col="red") #prediction
lines(hw_pre_cpi[,2], lty=2, col="orange") #confidence intervals
lines(hw_pre_cpi[,3], lty=2, col="orange")

checkresiduals(hw_cpi)
hw_for_cpi=forecast(hw_cpi,h=25)
accuracy(hw_for_cpi)
plot(hw_for_cpi)


#ARDL

#Optimal Lag Selection
VARselect(data$Brent.TRY, lag.max = 10 ) # lag = 7, at HQ 
VARselect(data$UnleadedGasoline, lag.max = 10) # lag = 8 at HQ
VARselect(data$DieselOil, lag.max = 10) # lag = 9 at HQ
VARselect(data$CPI.2, lag.max = 10) # lag = 5 at HQ

data_2=as.data.frame(data)
#ARDL for Brent
ardlBoundOrders(data = data, formula =Brent.TRY~UnleadedGasoline+DieselOil+CPI.2 , ic ="AIC",max.p = 10,max.q = 10)

ardlBound(data = data, formula =Brent.TRY~UnleadedGasoline+DieselOil+CPI.2, case = 3,max.p = 10,max.q = 10) #testing Cointegration

ardlmodel_brent = ardlDlm(formula =Brent.TRY~UnleadedGasoline+DieselOil+CPI.2,data=data_2, p =9, q =7)
summary(ardlmodel_brent)

checkresiduals(ardlmodel_brent) #model diagnostics

ardlforecast_brent=forecast(ardlmodel_brent,x=data_2$Brent.TRY, h=25)

#ARDL for Gasoline
ardlBoundOrders(data = data, formula =UnleadedGasoline~Brent.TRY+DieselOil+CPI.2 , ic ="AIC",max.p = 10,max.q = 10)

ardlBound(data = data, formula =UnleadedGasoline~Brent.TRY+DieselOil+CPI.2, case = 3,max.p = 10,max.q = 10) #testing Cointegration

ardlmodel_gaso = ardlDlm(formula =UnleadedGasoline~Brent.TRY+DieselOil+CPI.2,data=data_2, p =9, q =8)
summary(ardlmodel_gaso )

checkresiduals(ardlmodel_gaso ) #model diagnostics

ardlforecast_gaso=forecast(ardlmodel_gaso ,x=data$Brent.TRY, h=25)

#ARDL for Diesel
ardlBoundOrders(data = data, formula =DieselOil~Brent.TRY+UnleadedGasoline+CPI.2 , ic ="AIC",max.p = 10,max.q = 10)

ardlBound(data = data, formula =DieselOil~Brent.TRY+UnleadedGasoline+CPI.2, case = 3,max.p = 10,max.q = 10) #testing Cointegration

ardlmodel_diesel = ardlDlm(formula =DieselOil~Brent.TRY+UnleadedGasoline+CPI.2,data=data_2, p =8, q =9)
summary(ardlmodel_diesel )

checkresiduals(ardlmodel_diesel ) #model diagnostics

ardlforecast_diesel=forecast(ardlmodel_diesel ,x=data$Brent.TRY, h=25)

#ARDL for CPI
ardlBoundOrders(data = data, formula =CPI.2~Brent.TRY+UnleadedGasoline+DieselOil , ic ="AIC",max.p = 10,max.q = 10)

ardlBound(data = data, formula =CPI.2~Brent.TRY+UnleadedGasoline+DieselOil , case = 3,max.p = 10,max.q = 10) #testing Cointegration

ardlmodel_cpi = ardlDlm(formula =CPI.2~Brent.TRY+UnleadedGasoline+DieselOil,data=data_2, p =9, q =5)
summary(ardlmodel_cpi)

checkresiduals(ardlmodel_cpi) #model diagnostics

ardlforecast_cpi=forecast(ardlmodel_cpi ,x=data$Brent.TRY, h=25)
