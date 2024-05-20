#pulizia iniziale di ambiente
rm( list=ls() ); graphics.off(); cat("\014")

#Librerie
library(readr)
library(xts)
library(KFAS)
library(forecast)

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

#Poichè i modelli UCM sono modelli additivi è meglio fornire loro una serie stazionaria in varianza
#BoxCox transformation
lambda<- -0.8999268 #lambda trovata in ARIMA 
dataset$CO<-BoxCox(dataset$CO, lambda = lambda)

tms <- xts(dataset$CO, order.by=dataset$Time, frequency = 24)
attr(tms, 'frequency')<-24
colnames(tms)[1] <- "CO"

#Train test split:
train_date <- ceiling(nrow(tms)*0.8)
test <- tms[-c(1:train_date),]
colnames(test)[1] <- "CO"

#Creo dataset con NA al posto dei valori di CO di test per fare le previsioni
dataset$y<-dataset$CO
dataset$y[(nrow(dataset)-1705):nrow(dataset)]<-NA
y<-xts(dataset$y, dataset$Time)
attr(y, 'frequency')<-24
colnames(y)[1] <- "CO"
#ly<-log(y)

#Funzione per calcolo di MAE, MAPE e RMSE
losses <- function(y, yhat) {
  aerr <- abs(y - yhat)
  c(MAE = mean(aerr),
    MAPE = mean(aerr/y)*100,
    RMSE = sqrt(mean(aerr^2)))
}

#Primo modello 
#Nella serie non sembra presente un trend e se c'è è molto debole
#Sappiamo dall'esplorazione e dagli arima che sono presenti una stagionalità oraria ed una settimanale
#Le modelleremo con dummy o sinusoidi (deterministica o stocastica)
#Si è scelto di non considerare il ciclo perchè una serie storica di un anno non sembra sufficiente a considerare ciclicità di medio/lungo periodo

#Trend random walk, stagionalità giornaliera a dummy stocastiche e settimanale trigonometrica deterministica con 10 sinusoidi
mod1 <- SSModel(CO ~ 0 +
                  SSMtrend(1, NA) +
                  SSMseasonal(24, NA, "dummy") +
                  SSMseasonal(168, 0, "trig", harmonics = 1:10),
                data = y,
                H = NA
)

varCO <- var(y$CO, na.rm = TRUE)

mod1$a1["level", ] <- mean(y$CO[1:2160]) #media di primi 3 mesi di dati
diag(mod1$P1inf) <- 0
diag(mod1$P1) <- varCO


par_init <- c(
  log_var_eta = log(varCO/100),
  log_var_omega <- log(varCO/1000),
  log_var_eps <- log(varCO/20)
)

fit1 <- fitSSM(mod1, par_init)
fit1$optim.out$convergence
exp(fit1$optim.out$par)

#Kalman filter smoothing
smo1 <- KFS(fit1$model,
            filtering = "signal",
            smoothing = c("state", "disturbance"))

#Plot trend su ts
plot(ts(tms$CO))
lines(ts(smo1$alphahat[,"level"]), col="red") #stampa trend

#Plot stagionalità settimanale
seas168 <- rowSums(smo1$alphahat[, seq(24, 44, 2)]) #per componente stagionale sinusoidi
plot(ts(seas168))

#Plot con previsioni
plot(ts(tms$CO))
lines(ts(smo1$m), col="red")

loss1 <- losses(test$CO, smo1$m[6822:8526,])
loss1


#Trend rw con stagionalità giornaliera a dummy stocastiche e settimanale trigonometrica stocastica con 10 sinusoidi
mod3 <- SSModel(CO ~ 0 +
                  SSMtrend(1, NA) +
                  SSMseasonal(24, NA, "dummy") +
                  SSMseasonal(168, NA, "trig", harmonics = 1:10),
                data = y,
                H = NA
)
mod3$a1
mod3$Q

mod3$a1["level", ] <- mean(y$CO[1:2160]) #media di primi 3 mesi di dati
diag(mod3$P1inf) <- 0
diag(mod3$P1) <- varCO

par_init3 <- c(
  log_var_eta = log(varCO/100),
  log_var_omega <- log(varCO/1000),
  log_var_omega_trig <- log(varCO/1200),
  log_var_eps <- log(varCO/20)
)

updt3 <- function(pars, model) {
  mod3$Q[1, 1, 1] <- exp(pars[1])
  mod3$Q[2, 2, 1] <- exp(pars[2]) 
  diag(mod3$Q[3:22, 3:22, 1]) <- exp(pars[3]) 
  mod3$H[1, 1, 1] <- exp(pars[4])
  mod3
}

fit3 <- fitSSM(mod3, par_init3, updt3)
fit3$optim.out$convergence
exp(fit3$optim.out$par)

#Kalman filter smoothing
smo3 <- KFS(fit3$model,
            filtering = "signal",
            smoothing = c("state", "disturbance"))

#Plot trend su ts
plot(ts(tms$CO))
lines(ts(smo3$alphahat[,"level"]), col="red") #stampa trend

#Plot stagionalità settimanale
seas168 <- rowSums(smo3$alphahat[, seq(25, 44, 2)]) #per componente stagionale sinusoidi
plot(ts(seas168))

#Plot con previsioni
plot(ts(tms$CO))
lines(ts(smo3$m), col="red")

loss3 <- losses(test$CO, smo3$m[6822:8526,])
loss3


#Modello con Local Linear Trend 
mod6 <- SSModel(CO ~ 0 +
                  SSMtrend(2, list(NA, NA)) +
                  SSMseasonal(24, NA, "dummy") +
                  SSMseasonal(168, 0, "trig", harmonics = 1:10),
                data = y,
                H = NA
)


mod6$a1["level", ] <- mean(y$CO[1:2160]) #media di dati train set
diag(mod6$P1inf) <- 0
diag(mod6$P1) <- varCO


par_init6 <- c(
  log_var_eta = log(varCO/100), 
  log_var_zeta = log(varCO/1000),
  log_var_omega = log(varCO/1000),
  log_var_eps = log(varCO/10)
)


fit6 <- fitSSM(mod6, par_init6)
fit6$optim.out$convergence
exp(fit6$optim.out$par)

#Kalman filter smoothing
smo6 <- KFS(fit6$model,
            filtering = "signal",
            smoothing = c("state", "disturbance"))

#Plot trend su ts
plot(ts(tms$CO))
lines(ts(smo6$alphahat[,"level"]), col="red") #stampa trend

#Plot stagionalità settimanale
seas168 <- rowSums(smo6$alphahat[, seq(25, 44, 2)]) #per componente stagionale sinusoidi
plot(ts(seas168))

#Plot con previsioni
plot(ts(tms$CO))
lines(ts(smo6$m), col="red")

loss6 <- losses(test$CO, smo6$m[6822:8526,])
loss6

#Modello con Trend deterministico 
mod7 <- SSModel(CO ~ 0 +
                  SSMtrend(2, list(0, 0)) +
                  SSMseasonal(24, NA, "dummy") +
                  SSMseasonal(168, 0, "trig", harmonics = 1:10),
                data = y,
                H = NA
)


mod7$a1["level", ] <- mean(y$CO[1:2160]) #media di primi 3 mesi di dati
diag(mod7$P1inf) <- 0
diag(mod7$P1) <- varCO


par_init7 <- c(
  log_var_omega = log(varCO/1000),
  log_var_eps = log(varCO/10)
)


fit7 <- fitSSM(mod7, par_init7)
fit7$optim.out$convergence
exp(fit7$optim.out$par)

#Kalman filter smoothing
smo7 <- KFS(fit7$model,
            filtering = "signal",
            smoothing = c("state", "disturbance"))

#Plot trend su ts
plot(ts(tms$CO))
lines(ts(smo7$alphahat[,"level"]), col="red") #stampa trend

#Plot stagionalità settimanale
seas168 <- rowSums(smo7$alphahat[, seq(25, 44, 2)]) #per componente stagionale sinusoidi
plot(ts(seas168))

#Plot con previsioni
plot(ts(tms$CO))
lines(ts(smo7$m), col="red")

loss7 <- losses(test$CO, smo7$m[6822:8526,])
loss7

#Confronto i modelli:
mape_list<-c(loss1[2], loss3[2], loss6[2], loss7[2])

names(mape_list)<-c("ucm1", "ucm3", "ucm6", "ucm7")
b<-barplot(mape_list, las=2, col="maroon", ylim=c(0, 0.3))
text(
  x=b,
  y=mape_list,
  labels=round(mape_list, 4),
  pos=3,
  srt=90,
  offset=1.5
)
