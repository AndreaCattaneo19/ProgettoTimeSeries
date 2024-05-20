#pulizia iniziale di ambiente
rm( list=ls() ); graphics.off(); cat("\014")

#Librerie
library(readr)
library(xts)
library(forecast)
library(urca)
library(ggplot2)

#Import dati
dataset <- read_csv("No_null_project_data.csv", col_types = cols(Date = col_character()))
which(is.na(dataset))
which(is.na(dataset$Date))
which(is.na(dataset$CO))
which(is.na(dataset$Hour))
dataset<-dataset[, -4]

dataset$Time<-paste(dataset$Date, dataset$Hour, sep="-")
which(is.na(dataset$Time))

dataset$Time <- as.POSIXct(dataset$Time,
                           format="%Y-%m-%d-%H", tz="GMT", frequency=24)
which(is.na(dataset))
which(is.na(dataset$Time))
dataset<-dataset[,-1]
dataset<-dataset[,-1]

tms <- xts(dataset$CO, order.by=dataset$Time, frequency = 24)
attr(tms, 'frequency')<-24

#Train test split:
train_date <- ceiling(nrow(tms)*0.8)
train <- tms[1:train_date,]
test <- tms[-c(1:train_date),]

plot(train)
seasonplot(as.ts(train))
#Presenza di stagionalità giornaliera e settimanale

#Avendo dati annuali non è possibile valutare la presenza di ciclicità legate al mese, anche se sembra evidente un calo di media e varianza da giugno ad agosto
dec<-decompose(as.ts(train))
plot(dec)
#Non sembra presente un trend

#Dal plot la serie non risulta stazionaria nè in media nè in varianza
medie <- period.apply(train, endpoints(train, "days"), mean) 
devst <- period.apply(train, endpoints(train, "days"), sd)
plot(medie, devst, type="p")
#Non risulta una relazione lineare tra media e deviazione standard di CO, la non stazionarietà in varianza non è risolvibile con trasformazione logaritmica
ltrain<-log(train)
plot(ltrain)
#La trasformazione logaritmica infatti non rende stazionaria in varianza la serie

#Utilizzo trasformazione di box cox:
bctrain<-BoxCox(train, lambda = "auto")
attributes(bctrain)
#Salvo lambda per poter invertire la trasformazione
lambda<- -0.8999268

plot(bctrain)

#Test ADF
summary(ur.df(bctrain, "none", 24, "AIC"))
#La serie non risulta stazionaria dal test ADF

#ACF e PACF di cbtms
Acf(bctrain)
Pacf(bctrain)

#Differenzio ogni 24 per eliminare stagionalità
sbctrain<-diff(bctrain, 24)
sbctrain<-na.omit(sbctrain)
plot(sbctrain)
#Dal plot la serie non sembra del tutto stazionaria
#Test ADF
summary(ur.df(sbctrain, "none", 24, "AIC"))
#Dal test rifiuto ipotesi nulla e serie risulta stazionaria

#Differenziazione di ordine 1 per stazionarietà in media
dsbctrain <- diff(sbctrain, 1)
dsbctrain<-na.omit(dsbctrain)
plot(dsbctrain)
#Dal plot la serie sembra definitivamente stazionaria

#Test ADF
summary(ur.df(dsbctrain, "none", 24, "AIC"))
#Dal test rifiuto ipotesi nulla e serie risulta stazionaria


#ACF e PACF dopo differenziazioni
Acf(dsbctrain, 96)
Pacf(dsbctrain, 96)

#AUTOARIMA PER TROVARE COMBINAZIONE MIGLIORE DI PARAMETRI RISPETTO AD AICc
auto.arima(train, seasonal=FALSE, ic="aicc", trace=TRUE)
#Trova come modello migliore ARIMA(5,1,1)
#Utilizzo questo modello come base per il modello stagionale

#Modello Arima(5,1,1)
mod1 <- Arima(train, c(5, 1, 1), c(0,0,0), include.drift=FALSE) 
mod1

#Acf e Pacf di residui
Acf(mod1$residuals, 96)
Pacf(mod1$residuals, 96)
#Dal plot P=1 o P=2

#Modello SARIMA(5, 1, 1)(1, 1, 0)[24]
mod2 <- Arima(train, c(5, 1, 1), list(order=c(1, 1, 0), period=24), lambda=lambda) 
mod2

#Acf e Pacf di residui
Acf(mod2$residuals, 96)
Pacf(mod2$residuals, 96)

#Proviamo anche Arima con stagionalità P=2, D=1, Q=0
#Modello SARIMA(5, 1, 1)(2, 1, 0)[24]
mod3 <- Arima(train, c(5, 1, 1), list(order=c(2, 1, 0), period=24), lambda=lambda) 
mod3
summary(mod3)

#Acf e Pacf di residui
Acf(mod3$residuals, 96)
Pacf(mod3$residuals, 96)

#Analisi dei residui
#Test di Box Ljung
Box.test(mod3$residuals, 26, "Ljung-Box", 8)
#Plot dei residui
plot(mod3$residuals)
#Density plot dei residui
den<-(density(mod4$residuals))
plot(den, frame=FALSE, col="blue", main="Density plot")
#Density plot con bar plot
hist(mod43$residuals,
     col="green",
     prob=TRUE)
lines(den,
      lwd=2,
      col="red")
#QQ plot
qqnorm(mod3$residuals, pch=1, frame=FALSE)
qqline(mod3$residuals, col="steelblue", lwd=2)


#Modello SARIMA(5, 1, 1)(1, 1, 1)[24] 
mod4 <- Arima(train, order=c(5, 1, 1), seasonal=list(order=c(1, 1, 1), period=24), lambda=lambda) 
mod4

#Acf e Pacf di residui
Acf(mod4$residuals, 96)
Pacf(mod4$residuals, 96)

#Analisi dei residui
#Test di Box Ljung
Box.test(mod4$residuals, 26, "Ljung-Box", 8)
#Plot dei residui
plot(mod4$residuals)
#Density plot dei residui
den<-(density(mod4$residuals))
plot(den, frame=FALSE, col="blue", main="Density plot")
#Density plot con bar plot
hist(mod4$residuals,
     col="green",
     prob=TRUE)
lines(den,
      lwd=2,
      col="red")
#QQ plot
qqnorm(mod4$residuals, pch=1, frame=FALSE)
qqline(mod4$residuals, col="steelblue", lwd=2)

#Provo anche modello con Q=2
#Modello SARIMA(5, 1, 1)(1, 1, 2)[24] 
mod5 <- Arima(train, c(5, 1, 1), list(order=c(1, 1, 2), period=24), lambda=lambda) 
mod5
summary(mod5)
#Acf e Pacf di residui
Acf(mod5$residuals, 96)
Pacf(mod5$residuals, 96)

#Test di Box Ljung
Box.test(mod5$residuals, 26, "Ljung-Box", 9)

#Plot dei residui
plot(mod5$residuals)
#Density plot dei residui
den<-(density(mod5$residuals))
plot(den, frame=FALSE, col="blue", main="Density plot")
#Density plot con bar plot
hist(mod5$residuals,
     col="green",
     prob=TRUE)
lines(den,
      lwd=2,
      col="red")
#QQ plot
qqnorm(mod5$residuals, pch=1, frame=FALSE)
qqline(mod5$residuals, col="steelblue", lwd=2)

#Provo anche modello con Q=2
#Modello SARIMA(5, 1, 1)(1, 1, 3)[24]
mod6 <- Arima(train, c(5, 1, 1), list(order=c(1, 1, 3), period=24), lambda=lambda) 
mod6
summary(mod6)
#Acf e Pacf di residui
Acf(mod6$residuals, 96)
Pacf(mod6$residuals, 96)

#Valutazione dei residui del modello
#Test di Box Ljung
Box.test(mod6$residuals, 26, "Ljung-Box", 10)
#Plot dei residui
plot(mod6$residuals)
#Density plot dei residui
den<-(density(mod6$residuals))
plot(den, frame=FALSE, col="blue", main="Density plot")
#Density plot con bar plot
hist(mod6$residuals,
     col="green",
     prob=TRUE)
lines(den,
      lwd=2,
      col="red")
#QQ plot
qqnorm(mod6$residuals, pch=1, frame=FALSE)
qqline(mod6$residuals, col="steelblue", lwd=2)


#Modello SARIMA(5, 1, 1)(2, 1, 2)[24]
mod7 <- Arima(train, c(5, 1, 1), list(order=c(2, 1, 2), period=24), method="CSS", lambda=lambda) #Non confrontabile con AIC
mod7
summary(mod7)
#Acf e Pacf di residui
Acf(mod7$residuals, 96)
Pacf(mod7$residuals, 96)

#Valutazione dei residui del modello
#Test di Box Ljung
Box.test(mod7$residuals, 26, "Ljung-Box", 10)


#Provo anche modello con P=2 e Q=3
#Modello SARIMA(5, 1, 1)(2, 1, 3)[24]
mod8 <- Arima(train, c(5, 1, 1), list(order=c(2, 1, 3), period=24), method="CSS", lambda=lambda) #Non confrontabile con AIC
mod8
summary(mod8)
#Acf e Pacf di residui
Acf(mod8$residuals, 96)
Pacf(mod8$residuals, 96)

#Valutazione dei residui del modello
#Test di Box Ljung
Box.test(mod8$residuals, 26, "Ljung-Box", 11)

#Provo anche modello con P=3 e Q=3
#Modello SARIMA(5, 1, 1)(3, 1, 3)
mod9 <- Arima(train, c(5, 1, 1), list(order=c(3, 1, 3), period=24), method="CSS", lambda=lambda) #Non confrontabile con AIC
mod9
summary(mod9)
#Acf e Pacf di residui
Acf(mod9$residuals, 96)
Pacf(mod9$residuals, 96)

#Valutazione dei residui del modello
#Test di Box Ljung
Box.test(mod9$residuals, 26, "Ljung-Box", 12)
#Plot dei residui
plot(mod9$residuals)
#Density plot dei residui
den<-(density(mod9$residuals))
plot(den, frame=FALSE, col="blue", main="Density plot")
#Density plot con bar plot
hist(mod9$residuals,
     col="green",
     prob=TRUE)
lines(den,
      lwd=2,
      col="red")
#QQ plot
qqnorm(mod9$residuals, pch=1, frame=FALSE)
qqline(mod9$residuals, col="steelblue", lwd=2)
