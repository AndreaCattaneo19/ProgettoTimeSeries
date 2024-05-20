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

lambda<- -0.8999268 #lambda trovata in ARIMA

####################MIGLIOR MODELLO ARIMA#########################################
#Addestro il modello su tutto il dataset e prevedo per i successivi 31 giorni (Marzo)
mod6 <- Arima(tms, c(5, 1, 1), 
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
pred_mod6 <- forecast(mod6, h=744)
#Plot di previsioni
plot(pred_mod6)

#######DATAFRAME CON PREVISIONI################
previsioni6<-pred_mod6$mean[1:743]
#Creo date per mese di marzo
pred_date<-seq(as.POSIXct("2005-03-01 00:00;00"), as.POSIXct("2005-03-31 23:00:00"),
                 by="hour")

#Dataframe
df_previsioni<-xts(x=previsioni6, order.by=pred_date)
attr(df_previsioni, 'frequency')<-24
colnames(df_previsioni)[1]<-"ARIMA"

####################MIGLIOR UCM#####################################
#Dataset per ucm
#Poichè i modelli UCM sono modelli additivi è meglio fornire loro una serie stazionaria in varianza
#BoxCox transformation
data<-BoxCox(dataset$CO, lambda = lambda)

data[8527:9269]<-NA
dates<-seq(as.POSIXct("2004-03-10 18:00:00"), as.POSIXct("2005-03-31 23:00:00"),
           by="hour") #crea dati fino a giorno da prevedere
UCM_data<-xts(x=data, order.by=dates)
attr(UCM_data, 'frequency')<-24
which(is.na(UCM_data))
colnames(UCM_data)[1]<-"CO"

varCO <- var(UCM_data$CO, na.rm = TRUE)

#Modello con Trend deterministico
mod7 <- SSModel(CO ~ 0 +
                  SSMtrend(2, list(0, 0)) +
                  SSMseasonal(24, NA, "dummy") +
                  SSMseasonal(168, 0, "trig", harmonics = 1:10),
                data = UCM_data,
                H = NA
)


mod7$a1["level", ] <- mean(UCM_data$CO[1:2160]) #media di primi 3 mesi di dati
diag(mod7$P1inf) <- 0
diag(mod7$P1) <- varCO


par_init7 <- c(
  log_var_omega = log(varCO/1000),
  log_var_eps = log(varCO/10)
)


fit7 <- fitSSM(mod7, par_init7)
fit7$optim.out$convergence

#Kalman filter smoothing
smo7 <- KFS(fit7$model,
            filtering = "signal",
            smoothing = c("state", "disturbance"))

#Plot con previsioni
plot(ts(UCM_data$CO))
lines(8527:9269, ts(smo7$m[8527:9269,]), col="red")

#invertire previsioni da trasformazione BoxCox e metterle nel dataset con le previsioni
invBoxCox <- function(x, lambda){
  y<-(lambda*x + 1)**(1/lambda)
  y
}


UCM<-invBoxCox(smo7$m[8527:9269,], lambda)
#mettere le previsioni nel dataset finale
df_previsioni$UCM <- UCM

#Sistemare dataset finale in formato corretto
Hour<-format(pred_date, "%H")
Date<-format(pred_date, "%Y-%m-%d")
df_previsioni$Data<-Date
df_previsioni$Hour<-Hour
df_previsioni$Data<-Date

df_previsioni$ARIMA2<-df_previsioni$ARIMA
df_previsioni$UCM2<-df_previsioni$UCM


df_previsioni<-df_previsioni[,-1]
df_previsioni<-df_previsioni[,-1]

colnames(df_previsioni)[3]<-"ARIMA"
colnames(df_previsioni)[4]<-"UCM"
colnames(df_previsioni)[1]<-"Date"

df_previsioni$Date<-as.character(df_previsioni$Date)


df_previsioni<-data.frame(df_previsioni$Date, df_previsioni$Hour, df_previsioni$ARIMA, df_previsioni$UCM)
df_previsioni$Hour<-as.integer(df_previsioni$Hour)
df_previsioni$ARIMA<-as.numeric(df_previsioni$ARIMA)
df_previsioni$UCM<-as.numeric(df_previsioni$UCM)
which(is.na(df_previsioni))


#Converto in csv
write.csv(df_previsioni, "Previsioni_arima_ucm.csv", row.names=FALSE, sep=",")

