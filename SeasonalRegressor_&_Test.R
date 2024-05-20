#pulizia iniziale di ambiente
rm( list=ls() ); graphics.off(); cat("\014")

#Librerie
library(readr)
library(xts)
library(forecast)
library(urca)
library(ggplot2)
library(lubridate)

#Import dati
dataset <- read_csv("No_null_project_data.csv", col_types = cols(Date = col_character()))
which(is.na(dataset))
which(is.na(dataset$Date))
which(is.na(dataset$CO))
which(is.na(dataset$Hour))
dataset<-dataset[, -4]

data<-dataset$CO
dates<-seq(as.POSIXct("2004-03-10 18:00:00"), as.POSIXct("2005-02-28 23:00:00"),
           by="hour")
tms<-xts(x=data, order.by=dates)
attr(tms, 'frequency')<-24
which(is.na(tms))
plot(tms)

#Train test split:
train_date <- ceiling(nrow(tms)*0.8)
train <- tms[1:train_date,]
test <- ts(tms[-c(1:train_date), ], frequency=24)
pred_date<-seq(as.POSIXct("2004-12-19 23:00;00"), as.POSIXct("2005-02-28 23:00:00"),
                         by="hour")

#Lambda per trasformazione di BoxCox trovata prima per rendere stazionaria la serie
lambda<- -0.8999268
bct<-BoxCox(train, lambda = lambda)

#Costruisco sinusoidi per stagionalità (stagionalità settimanale con dati orari è 7*24=168. Quindi: S=168, j=S/2=168/2=84)
time<- 1:8526
omegat<-outer(time, 1:8, )*2*pi/168 
co<-cos(omegat)
si<-sin(omegat) 

#SI PRENDONO I MODELLI MIGLIORI E SI UTILIZZANO COME BASE PER MODELLI CON REGRESSORE STAGIONALE
#INFINE SI PREVEDE IL TEST SET E SI SCEGLIE IL MIGLIORE

#MIGLIORE DI SARIMA: SARIMA(5,1,1)(1, 1, 2)[24] (IN BASE AD AICc)
#Modello Arima con stagionalità P=1, D=1 e Q=3
mod6 <- Arima(train, c(5, 1, 1), 
              list(order=c(1, 1, 2), period=24),
              lambda=lambda)
mod6
#Acf e Pacf dei residui
Acf(mod6$residuals, 96)
Pacf(mod6$residuals, 96)

#Valutazione dei residui del modello
#Test di Box Ljung
Box.test(mod6$residuals, 26, "Ljung-Box", 9)
#Plot dei residui
plot(mod6$residuals)
#Density plot dei residui
den<-(density(mod6$residuals))
plot(den, frame=FALSE, col="blue", main="Density plot")
#Density plot con box plot
hist(mod6$residuals,
     col="green",
     prob=TRUE)
lines(den,
      lwd=2,
      col="red")
#QQ plot
qqnorm(mod6$residuals, pch=1, frame=FALSE)
qqline(mod6$residuals, col="steelblue", lwd=2)

#Previsione:
pred_mod6 <- forecast(mod6, length(test))

#Plot predicted
plot(pred_mod6)

#Plot train+fitted+predicted
plot(pred_mod6) 
lines(pred_mod6$fitted, col="burlywood")

#Creo dataframe con previsioni
previsioni6<-pred_mod6$mean[1:1705]
df_previsioni6<-xts(x=previsioni6, order.by=pred_date)
attr(df_previsioni6, 'frequency')<-24
colnames(df_previsioni6)[1]<-"CO"

#Train+predicted+test
plot(tms)
lines(df_previsioni6, col="blue")

#Errori di previsione
err_mod6  <- as.ts(test, frequency(24)) - as.ts(df_previsioni6, frequency(24)) 
rmse6 <- err_mod6^2 %>% mean() %>% sqrt()
mae6  <- abs(err_mod6) %>% mean()
mape6 <- mean(abs(err_mod6)/length(test)*100) 

#MIGLIORE DI SARIMA CON CSS: SARIMA(5, 1, 1)(3, 1, 3)[24]  (IN BASE A MAPE SU TRAINING)
modn <- Arima(train, c(5, 1, 1), 
              list(order=c(3, 1, 3), period=24),
              method="CSS",
              lambda=lambda) #Non confrontabile con AICc
modn
summary(modn)

#Acf e Pacf dei residui
Acf(modn$residuals, 96)
Pacf(modn$residuals, 96)

#Valutazione dei residui del modello
#Test di Box Ljung
Box.test(modn$residuals, 26, "Ljung-Box", 6)
#Plot dei residui
plot(modn$residuals)
#Density plot dei residui
den<-(density(modn$residuals))
plot(den, frame=FALSE, col="blue", main="Density plot")
#Density plot con box plot
hist(modn$residuals,
     col="green",
     prob=TRUE)
lines(den,
      lwd=2,
      col="red")
#QQ plot
qqnorm(modn$residuals, pch=1, frame=FALSE)
qqline(modn$residuals, col="steelblue", lwd=2)

#Previsione:
pred_modn <- forecast(modn, length(test))

#Plot predicted
plot(pred_modn)

#Plot train+fitted+predicted
plot(pred_modn) 
lines(pred_modn$fitted, col="burlywood")

#Creo dataframe con previsioni 
previsionin<-pred_modn$mean[1:1705]
df_previsionin<-xts(x=previsionin, order.by=pred_date)
attr(df_previsionin, 'frequency')<-24
colnames(df_previsionin)[1]<-"CO"

#Train+predicted+test
plot(tms)
lines(df_previsionin, col="red")

#Errori di previsione
err_modn  <- as.ts(test, frequency(24)) - as.ts(df_previsionin, frequency(24)) 
rmsen <- err_modn^2 %>% mean() %>% sqrt()
maen  <- abs(err_modn) %>% mean()
mapen <- mean(abs(err_modn)/length(test)*100) 


#SARIMAX (5, 1, 1)(1, 1, 3) CON REGRESSORE STAGIONALITA' SETTIMANALE SINUSOIDALE 
mod6x <- Arima(train, c(5, 1, 1), 
              list(order=c(1, 1, 3), period=24),
              lambda=lambda,
              xreg=cbind(co,si)[1:6821]) 
mod6x

#Acf e Pacf dei residui
Acf(mod6x$residuals, 96)
Pacf(mod6x$residuals, 96)

#Valutazione dei residui del modello
#Test di Box Ljung
Box.test(mod6x$residuals, 26, "Ljung-Box", 6)
#Plot dei residui
plot(mod6x$residuals)
#Density plot dei residui
den<-(density(mod6x$residuals))
plot(den, frame=FALSE, col="blue", main="Density plot")
#Density plot con box plot
hist(mod6x$residuals,
     col="green",
     prob=TRUE)
lines(den,
      lwd=2,
      col="red")
#QQ plot
qqnorm(mod6x$residuals, pch=1, frame=FALSE)
qqline(mod6x$residuals, col="steelblue", lwd=2)

#Previsione:
pred_mod6x <- forecast(mod6x, length(test), xreg = cbind(co, si)[6822:8526])

#Plot predicted
plot(pred_mod6x)

#Plot train+fitted+predicted
plot(pred_mod6x) 
lines(pred_mod6x$fitted, col="burlywood")

#Creo dataframe con previsioni 
previsioni6x<-pred_mod6x$mean[1:1705]
df_previsioni6x<-xts(x=previsioni6x, order.by=pred_date)
attr(df_previsioni6x, 'frequency')<-24
colnames(df_previsioni6x)[1]<-"CO"

#Train+predicted+test
plot(tms)
lines(df_previsioni6x, col="red")

#Errori di previsione
err_mod6x  <- as.ts(test, frequency(24)) - as.ts(df_previsioni6x, frequency(24)) 
rmse6x <- err_mod6x^2 %>% mean() %>% sqrt()
mae6x  <- abs(err_mod6x) %>% mean()
mape6x <- mean(abs(err_mod6x)/length(test)*100) 

#SARIMAX (5, 1, 1)(3, 1, 3) CSS CON REGRESSORE STAGIONALITA' SETTIMANALE SINUSOIDALE
modnx <- Arima(train, c(5, 1, 1), 
              list(order=c(3, 1, 3), period=24),
              xreg=cbind(co,si)[1:6821],
              method="CSS",
              lambda=lambda) #Non confrontabile con AIC
modnx
summary(modnx)

#Acf e Pacf dei residui
Acf(modnx$residuals, 96)
Pacf(modnx$residuals, 96)

#Valutazione dei residui del modello
#Test di Box Ljung
Box.test(modnx$residuals, 26, "Ljung-Box", 6)
#Plot dei residui
plot(modnx$residuals)
#Density plot dei residui
den<-(density(modnx$residuals))
plot(den, frame=FALSE, col="blue", main="Density plot")
#Density plot con box plot
hist(modnx$residuals,
     col="green",
     prob=TRUE)
lines(den,
      lwd=2,
      col="red")
#QQ plot
qqnorm(modnx$residuals, pch=1, frame=FALSE)
qqline(modnx$residuals, col="steelblue", lwd=2)

#Previsione:
pred_modnx <- forecast(modnx, length(test), xreg = cbind(co, si)[6822:8526])

#Plot predicted
plot(pred_modnx)

#Plot train+fitted+predicted
plot(pred_modnx) 
lines(pred_modnx$fitted, col="burlywood")

#Creo dataframe con previsioni 
previsioninx<-pred_modnx$mean[1:1705]
df_previsioninx<-xts(x=previsioninx, order.by=pred_date)
attr(df_previsioninx, 'frequency')<-24
colnames(df_previsioninx)[1]<-"CO"

#Train+predicted+test
plot(tms)
lines(df_previsioninx, col="red")

#Errori di previsione
err_modnx  <- as.ts(test, frequency(24)) - as.ts(df_previsioninx, frequency(24)) 
rmsenx <- err_modnx^2 %>% mean() %>% sqrt()
maenx  <- abs(err_modnx) %>% mean()
mapenx <- mean(abs(err_modnx)/length(test)*100) 

#Barplot per confrontare i MAPE dei modelli
mape_list<-c(mape6, mape6x, mapen, mapenx)

names(mape_list)<-c("mod6", "mod6x", "modn", "modnx")
b<-barplot(mape_list, las=2, col="blue", ylim=c(0, 18))
text(
  x=b,
  y=mape_list,
  labels=round(mape_list, 3),
  pos=3,
  srt=90,
  offset=1.5
)
