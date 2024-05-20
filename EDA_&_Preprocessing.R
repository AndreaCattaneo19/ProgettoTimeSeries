#pulizia iniziale di ambiente
rm( list=ls() ); graphics.off(); cat("\014")

#Librerie
library(readr)
library(ggplot2)
library(dplyr)

#Import dati
dataset <- read_csv("Project_data_2021_2022 (TRAINSET).csv", col_types = cols(Date = col_character()))

#Si osservano 3 colonne nel dataset: date, hour e CO
#Date è di tipo character, mentre hour e CO sono di tipo numeric
#Il dataset presenta 8562 righe
#Parte da marzo 2004 e arriva a febbraio 2005

dataset$Hour <- as.character(dataset$Hour)

#crea colonna Time, data e ora insieme
dataset$Time <- paste(dataset$Date, dataset$Hour, sep="-")

#converte in datetime:

dataset$Date <- as.Date(dataset$Date, "%Y-%m-%d")


dataset$Time <- as.POSIXct(dataset$Time,
                           format="%Y-%m-%d-%H")
#PRIMA ESPLORZIONE

#Controllo sulla presenza di valori nulli
which(is.na(dataset$Date))
which(is.na(dataset$Hour))
which(is.na(dataset$CO))
#Solo la colonna CO presenta valori nulli
sum(is.na(dataset$CO)) #365 nulli
#Nel dataset sono presenti 365 valori nulli nella colonna CO
which(dataset$CO==0)
#Nel dataset non sono presenti valori 0 per CO

#Esplorazione 
df<-dataset
df<-df[,-2]
#Estrae mese, anno e giorno della settimana
df$Month <- format(df$Time, "%m")
df$Year <- format(df$Time, "%Y")
df$Day <- format(df$Time, "%d")
df$Hour <- format(df$Time, "%H")
df$WDay <- weekdays(df$Time)

#Setta temporaneamente a 0 i NA
df$CO[is.na(df$CO)] <- 0

#SISTEMARE COLORI E TITOLI DEI GRAFICI 
#Plot ts completa
plot(dataset$Time, dataset$CO, type="l", lwd=3, xlab="Time", ylab="CO")

#Ordino giorni della settimana in df
df$WDay<- factor(df$WDay, levels= c("lunedì", "martedì", 
                                      "mercoledì", "giovedì", 
                                      "venerdì", "sabato", "domenica"))

#Box plot per giorno della settimana
ggplot(df, aes(x=WDay, y=CO, fill=WDay)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")

#Plot_orario
Hourly_mean<-aggregate(df$CO, by=list(Category=df$Hour), FUN=mean)
plot(Hourly_mean$Category,Hourly_mean$x, type="l", lwd=3, xlab="Hour", ylab="mean_CO")

#Box plot per ora del giorno
ggplot(df, aes(x=Hour, y=CO, fill=Hour)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")

#Plot andamento medio orario per giorno della settimana
Hourly_mean_day<-aggregate(df$CO, by=list(df$Hour, df$WDay), FUN=mean)

Hourly_mean_day$Group.2<- factor(Hourly_mean_day$Group.2, levels= c("lunedì", "martedì", "mercoledì", "giovedì", 
                                                                    "venerdì", "sabato", "domenica"))
Hourly_mean_day<-Hourly_mean_day[order(Hourly_mean_day$Group.2), ]

Hourly_mean_day$Group.1 <-as.numeric(Hourly_mean_day$Group.1)

ggplot(Hourly_mean_day, aes(Group.1, x, col=Group.2))+
  geom_line() + scale_color_brewer(palette="Dark2")+
  labs(y= "CO", x = "Hour")

#Plot andamento mensile (non è ordine di ts ma quello di mesi)
#Box plot per mese
ggplot(df, aes(x=Month, y=CO, fill=Month)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")

#Plot CO annuale
#Box plot per anno
ggplot(df, aes(x=Year, y=CO, fill=Year)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")


#Correzione dei dati nulli
#Dall'esplorazione si intuisce che esistono differenze negli andamenti orari, giornalieri e mensili
#Pertanto si è deciso di rimpiazzare ciascun nullo con la media delle rilevazioni di CO 
#alla stessa ora nello stesso giorno della settimana precedente e successiva
#se una delle due rilevazioni dovesse essere assente si utilizzerà il valore dell'unica osservazione non nulla

null_data <- subset(dataset, is.na(CO))
null_data$Date <- as.Date(null_data$Date, "%Y-%m-%d")
null_data$Hour <- as.character((null_data$Hour))

#I valori mancanti sono distribuiti su tutto il dataset
#A volte mancano i valori delle rilevazioni orarie per un giorno intero o per più giorni consecutivi
#altre mancano le rilevazioni per alcune ore in un singolo giorno.
#Potrei rimpiazzare i valori mancanti facendo la media tra i valori dello stesso giorno della settimana precedente e di quella successiva
#Per ogni valore nullo voglio:
#spostarmi indietro di 7 giorni e prendere il valore corrispondente alla stessa ora
#spostarmi avanti di 7 giorni e prendere il valore corrispondente alla stessa ora
#fare la media tra i due
before_null<-null_data
before_null$Date<-as.Date(before_null$Date)
before_null$Date<-before_null$Date-7

after_null<-null_data
after_null$Date<-as.Date(after_null$Date)
after_null$Date<-after_null$Date+7

before_null<- inner_join(dataset, before_null, by=c("Date"="Date", "Hour"="Hour"))
before_null<-before_null[,-5]
before_null$CO.x[is.na(before_null$CO.x)] <- 0


after_null<- inner_join(dataset, after_null, by=c("Date"="Date", "Hour"="Hour"))
after_null<-after_null[,-5]
after_null$CO.x<-after_null$CO.x[is.na(after_null$CO.x)]<-0
#Funzione media_nulli:
#dato un valore nullo prende li valore 7 giorni prima e 7 giorni dopo alla stessa ora e ne fa la media
#se uno dei due valori con cui fare la media è anch'esso nullo prende il secondo valore
media_nulli <- function(before, after){
  m<-vector(mode="numeric", length=0)
  for (i in 1:length(before)){
    if(before[i]!=0 & after[i]!=0){
      m[i]<-(before[i] + after[i])/2
    }else{
      if(before[i]==0){
        m[i]<-after[i]
      }else{
        m[i]<-before[i]
      }
    }
  }
  return(m)
}

null_data$media<-media_nulli(before_null$CO.x, after_null$CO.x)
which(null_data$media==0)
#controllo che non ci siano nulli nelle medie
which(is.na(null_data$media))
#arrotonda eliminando decimali
null_data$media<-floor(null_data$media)

#rimpiazza nulli con la media
null_data<-null_data[,-3]
dataset<-left_join(dataset, null_data, by=c("Date"="Date", "Hour"="Hour"))
dataset$CO <- ifelse(is.na(dataset$CO), dataset$media, dataset$CO)
dataset<-dataset[,-5]

#Risulta un valore a 0 di CO nel dataset 
which(dataset$CO==0)

#lo rimpiazzo con il valore 7 giorni prima alla stessa ora
dataset[6938,]
which(dataset$Date=='2004-12-17' & dataset$Hour==19)
dataset[6770,]
dataset[6938,3]<-dataset[6770,3]
#controllo che non ci siano più nulli nel dataset
which(is.na(dataset$CO))
dataset$Time<-dataset$Time.x
dataset<-dataset[,-4]
dataset<-dataset[,-4]

#Esplorazione di dataset senza nulli

#Plot ts completa
plot(dataset$Time, dataset$CO, type="l", lwd=3, xlab="Time", ylab="CO")

noNA_df<-dataset
noNA_df$Month <- format(noNA_df$Time, "%m")
noNA_df$Year <- format(noNA_df$Time, "%Y")
noNA_df$Day <- format(noNA_df$Time, "%d")
noNA_df$Hour <- format(noNA_df$Time, "%H")
noNA_df$WDay <- weekdays(noNA_df$Time)
noNA_df$WDay<- factor(df$WDay, levels= c("lunedì", "martedì", 
                                    "mercoledì", "giovedì", 
                                    "venerdì", "sabato", "domenica"))

#Box plot per giorno della settimana
ggplot(noNA_df, aes(x=WDay, y=CO, fill=WDay)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Plot_orario
noNa_Hourly_mean<-aggregate(noNA_df$CO, by=list(Category=noNA_df$Hour), FUN=mean)
plot(noNa_Hourly_mean$Category,noNa_Hourly_mean$x, type="l", lwd=3, xlab="Hour", ylab="mean_CO")

#Box plot per ora
ggplot(noNA_df, aes(x=Hour, y=CO, fill=Hour)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


#Plot andamento medio orario per giorno della settimana

noNA_Hourly_mean_day<-aggregate(noNA_df$CO, by=list(noNA_df$Hour, noNA_df$WDay), FUN=mean)

noNA_Hourly_mean_day$Group.2<- factor(noNA_Hourly_mean_day$Group.2, levels= c("lunedì", "martedì", "mercoledì", "giovedì", 
                                                                    "venerdì", "sabato", "domenica"))
noNA_Hourly_mean_day<-noNA_Hourly_mean_day[order(noNA_Hourly_mean_day$Group.2), ]

noNA_Hourly_mean_day$Group.1 <-as.numeric(noNA_Hourly_mean_day$Group.1)

ggplot(noNA_Hourly_mean_day, aes(Group.1, x, col=Group.2))+
  geom_line() + scale_color_brewer(palette="Set1")+
  labs(y= "CO", x = "Hour")

#Box plot per mese
ggplot(noNA_df, aes(x=Month, y=CO, fill=Month)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")

#Box plot per anno
ggplot(noNA_df, aes(x=Year, y=CO, fill=Year)) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")


#export del file con i nulli rimossi
write.csv(dataset, "No_null_project_data.csv", row.names=FALSE)
