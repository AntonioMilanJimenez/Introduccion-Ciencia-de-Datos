## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ------------------------------------------------------------------------
#Carga de los 'datasets'
library(ggplot2)
heart <- read.csv("/home/antonio/Documentos/IntroCienciaDatos/TrabajoFinal/heart/heart.dat",
          comment.char = "@", header = FALSE)

names(heart) <- c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral", 
"FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak",
"Slope", "MajorVessels", "Thal", "Class")

deltaAil <- read.csv("/home/antonio/Documentos/IntroCienciaDatos/TrabajoFinal/delta_ail/delta_ail.dat",
             comment.char = "@", header = FALSE)

names(deltaAil) <- c("RollRate", "PitchRate", "currPitch", "currRoll", "diffRollRate", "Sa")
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Dimensiones del dataset heart
dim(heart)

#Nombre de las variables
colnames(heart)

#Tipo atómico de cada variable
unlist(lapply(heart,class))

#Estructura del dataset
class(heart)
unique(heart$Class)

#Conversión de la variable de salida a un factor
heart$Class <- factor(heart$Class, levels = c(1, 2), labels = c("Si", "No"))
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Comprobación de missing values
which(is.na(heart) == TRUE)

#Comprobación de valores duplicados
which(duplicated(heart) == TRUE)
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Proporción de la variable de salida
round(prop.table(table(heart$Class)) * 100, digits = 1)

#Proporción de las variables discretas predictoras
round(prop.table(table(heart$Sex)) * 100, digits = 1)
round(prop.table(table(heart$ChestPainType)) * 100, digits = 1)
round(prop.table(table(heart$FastingBloodSugar)) * 100, digits = 1)
round(prop.table(table(heart$ResElectrocardiographic)) * 100, digits = 1)
round(prop.table(table(heart$ExerciseInduced)) * 100, digits = 1)
round(prop.table(table(heart$Slope)) * 100, digits = 1)
round(prop.table(table(heart$MajorVessels)) * 100, digits = 1)
round(prop.table(table(heart$Thal)) * 100, digits = 1)
## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
#Distribución de la variable Age
ggplot(heart, aes(x=Age)) + geom_histogram(binwidth = 5,color="black",fill="lightblue")+labs(
  title="Distribucion de Age")

#Distribución de la variable RestBloodPressure
ggplot(heart, aes(x=RestBloodPressure))+geom_density(color="darkblue",fill="lightblue")+geom_vline(
  aes(xintercept=mean(RestBloodPressure)),color="green")+labs(title="Distribucion de RestBloodPressure"
  )+geom_text(aes(x=136,y=0.01),label="mean",color="green")+geom_vline(aes(xintercept=median(RestBloodPressure
  )),color="red")+geom_text(aes(x=124,y=0.01),label="median",color="red")+geom_text(aes(x=180,y=0.02),
  label=paste("mean: ",toString(round(mean(heart$RestBloodPressure),2))),color="green") +geom_text(aes(x=179,y=0.018),
  label=paste("median: ",toString(median(heart$RestBloodPressure))),color="red") + geom_text(aes(x=179,y=0.016),
  label=paste("std: ",toString(round(sd(heart$RestBloodPressure),2))),color="blue")

#Distribución de la variable SerumCholestoral
ggplot(heart, aes(x=SerumCholestoral))+geom_density(color="darkblue",fill="lightblue")+geom_vline(
  aes(xintercept=mean(SerumCholestoral)),color="green")+labs(title="Distribucion de SerumCholestoral"
  )+geom_text(aes(x=272,y=0.004),label="mean",color="green")+geom_vline(aes(xintercept=median(SerumCholestoral
  )),color="red")+geom_text(aes(x=224,y=0.004),label="median",color="red")+geom_text(aes(x=400,y=0.007),
  label=paste("mean: ",toString(round(mean(heart$SerumCholestoral),2))),color="green") +geom_text(aes(x=400,y=0.0065),
  label=paste("median: ",toString(median(heart$SerumCholestoral))),color="red") + geom_text(aes(x=400,y=0.006),
  label=paste("std: ",toString(round(sd(heart$SerumCholestoral),2))),color="blue")

#Distribución de la variable MaxHeartRate
ggplot(heart, aes(x=MaxHeartRate))+geom_density(color="darkblue",fill="lightblue")+geom_vline(
  aes(xintercept=mean(MaxHeartRate)),color="green")+labs(title="Distribucion de MaxHeartRate"
  )+geom_text(aes(x=140,y=0.004),label="mean",color="green")+geom_vline(aes(xintercept=median(MaxHeartRate
  )),color="red")+geom_text(aes(x=160,y=0.004),label="median",color="red")+geom_text(aes(x=190,y=0.014),
  label=paste("mean: ",toString(round(mean(heart$MaxHeartRate),2))),color="green") +geom_text(aes(x=190,y=0.012),
  label=paste("median: ",toString(median(heart$MaxHeartRate))),color="red") + geom_text(aes(x=190,y=0.01),
  label=paste("std: ",toString(round(sd(heart$MaxHeartRate),2))),color="blue")

#Distribución de la variable Oldpeak
ggplot(heart, aes(x=Oldpeak))+geom_density(color="darkblue",fill="lightblue")+geom_vline(
  aes(xintercept=mean(Oldpeak)),color="green")+labs(title="Distribucion de Oldpeak")+geom_text(
  aes(x=12,y=0.004),label="mean",color="green")+geom_vline(aes(xintercept=median(Oldpeak)),
  color="red")+geom_text(aes(x=2,y=0.004),label="median",color="red")+geom_text(aes(x=40,y=0.044),
  label=paste("mean: ",toString(round(mean(heart$Oldpeak),2))),color="green") +geom_text(aes(x=40,y=0.04),
  label=paste("median: ",toString(median(heart$Oldpeak))),color="red") + geom_text(aes(x=40,y=0.036),
  label=paste("std: ",toString(round(sd(heart$Oldpeak),2))),color="blue")
## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
#Normalización del dataset
heartNormalizado <- data.frame(heart)
heartNormalizado$RestBloodPressure <- scale(heartNormalizado$RestBloodPressure)
heartNormalizado$SerumCholestoral <- scale(heartNormalizado$SerumCholestoral)
heartNormalizado$MaxHeartRate <- scale(heartNormalizado$MaxHeartRate)
heartNormalizado$Oldpeak <- scale(heartNormalizado$Oldpeak)

#Maxima correlacion entre las variables predictoras
max(abs(cor(heart[-14]))%%1)
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Dimensiones del dataset delta_Ail
dim(deltaAil)
## ------------------------------------------------------------------------



#####################################################
#Análisis dataset deltaAil
####################################################

## ------------------------------------------------------------------------
#Nombre de las variables del dataset
colnames(deltaAil)

#Tipo atomico de cada variable
unlist(lapply(deltaAil,class))

#Estructura del dataset y de la variable de salida
class(deltaAil)
min(deltaAil$Sa)
max(deltaAil$Sa)

#Correlación respecto a la variable de salida
cor(deltaAil)[6,]
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Comprobación de missing values
which(is.na(deltaAil) == TRUE)

#Comprobación de valores duplicados
which(duplicated(deltaAil) == TRUE)
## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
#Distribución de la variable de salida Sa
ggplot(deltaAil, aes(x=Sa))+geom_density(color="darkblue",fill="lightblue")+labs(
  title="Distribucion de la variable de salida Sa")

#Distribución de la variable RollRate
ggplot(deltaAil, aes(x=RollRate))+geom_density(color="darkblue",fill="lightblue")+geom_vline(
  aes(xintercept=mean(RollRate)),color="green")+labs(title="Distribucion de RollRate")+geom_text(
  aes(x=-0.001,y=15),label="mean",color="green")+geom_vline(aes(xintercept=median(RollRate)),
  color="red")+geom_text(aes(x=0.004,y=15),label="median",color="red")+geom_text(aes(x=0.012,y=60),
  label=paste("mean: ",toString(round(mean(deltaAil$RollRate),5))),color="green") +geom_text(
  aes(x=0.012,y=55),label=paste("median: ",toString(median(deltaAil$RollRate))),color="red"
  )+geom_text(aes(x=0.012,y=50),label=paste("std: ",toString(round(sd(deltaAil$RollRate),5))),color="blue")

#Distribución de la variable PitchRate
ggplot(deltaAil, aes(x=PitchRate))+geom_density(color="darkblue",fill="lightblue")+geom_vline(
  aes(xintercept=mean(PitchRate)),color="green")+labs(title="Distribucion de PitchRate")+geom_text(
  aes(x=0.0015,y=50),label="mean",color="green")+geom_vline(aes(xintercept=median(PitchRate)),color="red"
  )+geom_text(aes(x=0,y=50),label="median",color="red")+geom_text(aes(x=0.006,y=160),label=paste(
  "mean: ",toString(round(mean(deltaAil$PitchRate),5))),color="green") +geom_text(aes(x=0.006,y=150),
  label=paste("median: ",toString(median(deltaAil$PitchRate))),color="red") + geom_text(aes(x=0.006,y=140),
  label=paste("std: ",toString(round(sd(deltaAil$PitchRate),5))),color="blue")

#Distribución de la variable currPitch
ggplot(deltaAil, aes(x=currPitch))+geom_density(color="darkblue",fill="lightblue")+geom_vline(
  aes(xintercept=mean(currPitch)),color="green")+labs(title="Distribucion de currPitch")+geom_text(
  aes(x=0.013,y=30),label="mean",color="green")+geom_vline(aes(xintercept=median(currPitch)),
  color="red")+geom_text(aes(x=0.007,y=30),label="median",color="red")+geom_text(aes(x=0.025,y=55),
  label=paste("mean: ",toString(round(mean(deltaAil$currPitch),5))),color="green") +geom_text(
  aes(x=0.025,y=50),label=paste("median: ",toString(median(deltaAil$currPitch))),color="red")+geom_text(
  aes(x=0.025,y=45),label=paste("std: ",toString(round(sd(deltaAil$currPitch),5))),color="blue")

#Distribución de la variable currRoll
ggplot(deltaAil, aes(x=currRoll))+geom_density(color="darkblue",fill="lightblue")+geom_vline(aes(
  xintercept=mean(currRoll)),color="green")+labs(title="Distribucion de currRoll")+geom_text(aes(
  x=-0.003,y=10),label="mean",color="green")+geom_vline(aes(xintercept=median(currRoll)),color="red"
  )+geom_text(aes(x=0.01,y=10),label="median",color="red")+geom_text(aes(x=0.03,y=22),label=paste("mean: ",
  toString(round(mean(deltaAil$currRoll),5))),color="green") +geom_text(aes(x=0.03,y=20),label=paste(
  "median: ",toString(median(deltaAil$currRoll))),color="red") + geom_text(aes(x=0.03,y=18),label=paste(
  "std: ",toString(round(sd(deltaAil$currRoll),5))),color="blue")

#Distribución de la variable diffRollRate
ggplot(deltaAil, aes(x=diffRollRate))+geom_density(color="darkblue",fill="lightblue")+geom_vline(
  aes(xintercept=mean(diffRollRate)),color="green")+labs(title="Distribucion de diffRollRate")+geom_text(
  aes(x=-0.0001,y=500),label="mean",color="green")+geom_vline(aes(xintercept=median(diffRollRate)),
  color="red")+geom_text(aes(x=0.0001,y=500),label="median",color="red")+geom_text(aes(x=0.0005,y=1200),
  label=paste("mean: ",toString(round(mean(deltaAil$diffRollRate),10))),color="green") +geom_text(
  aes(x=0.0005,y=1050),label=paste("median: ",toString(median(deltaAil$diffRollRate))),color="red"
  )+geom_text(aes(x=0.0005,y=900),label=paste("std: ",toString(round(sd(deltaAil$diffRollRate),10))),color="blue")
## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
#Normalización del dataset
deltaAilNormalizado <- data.frame(deltaAil)

deltaAilNormalizado$RollRate <- scale(deltaAil$RollRate)
deltaAilNormalizado$PitchRate <- scale(deltaAil$PitchRate)
deltaAilNormalizado$currPitch <- scale(deltaAil$currPitch)
deltaAilNormalizado$currRoll <- scale(deltaAil$currRoll)
deltaAilNormalizado$diffRollRate <- scale(deltaAil$diffRollRate)
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Correlación entre variable predictoras
max(abs(cor(deltaAil[-6]))%%1)
## ------------------------------------------------------------------------

######################################################
#Problema de Regresión
######################################################


## ------------------------------------------------------------------------
#Correlación de las variables respecto a la variable de salida
cor(deltaAil)[6,]
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Correlación entre Sa y RollRate
plot(Sa~RollRate,deltaAil)

#Construcción modelo simple con RollRate
fit1=lm(Sa~RollRate,data=deltaAil)
summary(fit1)

#Construcción modelo simple con PitchRate
fit2=lm(Sa~PitchRate,data=deltaAil)
summary(fit2)

#Construcción modelo simple con currPitch
fit3=lm(Sa~currPitch,data=deltaAil)
summary(fit3)

#Construcción modelo simple con currRoll
fit4=lm(Sa~currRoll,data=deltaAil)
summary(fit4)

#Construcción modelo simple con diffRollRate
fit5=lm(Sa~diffRollRate,data=deltaAil)
summary(fit5)

#Modelo simple construido con RollRate
plot(Sa~RollRate,data=deltaAil)
abline(fit1,col="blue")
confint(fit1)
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Ejemplo de predicción del modelo simple
predict(fit1,data.frame(RollRate=c(0.01,-0.01,1)))

#Cálculo del RMSE para el modelo simple construido
yprime=predict(fit1,data.frame(RollRate=deltaAil$RollRate))
sqrt(sum(abs(deltaAil$Sa-yprime)^2)/length(yprime))
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Modelo simple con datos normalizados
fit1n=lm(Sa~RollRate,data=deltaAilNormalizado)
summary(fit1n)
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Correlaciones entre las variables predictoras
cor(deltaAil)[-6,-6]

#Construcción de modelo lineal múltiple
fit6 = lm(Sa~.,data=deltaAil)
summary(fit6)

#Construcción de modelo lineal múltiple
fit7 = lm(Sa~.-currPitch,data=deltaAil)
summary(fit7)

#Construcción de modelo lineal múltiple
fit8 = lm(Sa~.-currPitch-PitchRate,data=deltaAil)
summary(fit8)

#Cálculo de RMSE para el modelo lineal múltiple 8
yprime=predict(fit8,deltaAil)
sqrt(sum(abs(deltaAil$Sa-yprime)^2)/length(yprime))
## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
#Comportamiento del modelo lineal múltiple construido para la variable RollRate
plot(deltaAil$Sa~deltaAil$RollRate)
points(deltaAil$RollRate,fitted(fit8),col="blue",pch=20)

#Comportamiento del modelo lineal múltiple construido para la variable currRoll
plot(deltaAil$Sa~deltaAil$currRoll)
points(deltaAil$currRoll,fitted(fit8),col="blue",pch=20)

#Comportamiento del modelo lineal múltiple construido para la variable diffRollRate
plot(deltaAil$Sa~deltaAil$diffRollRate)
points(deltaAil$diffRollRate,fitted(fit8),col="blue",pch=20)
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Construcción de modelo múltiple con interacciones
fit9 = lm(Sa~RollRate+I(RollRate^2),data=deltaAil)
summary(fit9)

#Construcción de modelo múltiple con interacciones
fit10 = lm(Sa~RollRate+diffRollRate+I(RollRate^2)+I(diffRollRate^2)+I((RollRate+diffRollRate)^2),
           data=deltaAil)
summary(fit10)

#Construcción de modelo múltiple con interacciones
fit11 = lm(Sa~RollRate+diffRollRate+currRoll+I((RollRate+diffRollRate+currRoll)^2),
           data=deltaAil)
summary(fit11)

#Construcción de modelo múltiple con interacciones
fit12 = lm(Sa~RollRate+PitchRate+currPitch+currRoll+diffRollRate+I((RollRate+PitchRate+currPitch
          +currRoll+diffRollRate)^2),data=deltaAil)
summary(fit12)

#Construcción de modelo múltiple con interacciones
fit13 = lm(Sa~RollRate+currRoll+diffRollRate+RollRate*currRoll+RollRate*diffRollRate,data=deltaAil)
summary(fit13)

#Construcción de modelo múltiple con interacciones
fit14 = lm(Sa~RollRate+currRoll+diffRollRate+RollRate*currRoll+RollRate*diffRollRate+I(RollRate^2)
           +I(currRoll^2)+I(diffRollRate^2),data=deltaAil)
summary(fit14)

#Construcción de modelo múltiple con interacciones
fit15 = lm(Sa~RollRate+currRoll+diffRollRate+((RollRate+currRoll+diffRollRate)^2)+I(RollRate^2)
           +I(currRoll^2)+I(diffRollRate^2),data=deltaAil)
summary(fit15)

#Construcción de modelo múltiple con interacciones
fit16 = lm(Sa~RollRate+currRoll+diffRollRate+currPitch+PitchRate+(.^2)+I(RollRate^2)+I(currRoll^2)
           +I(diffRollRate^2),data=deltaAil)
summary(fit16)

#Construcción de modelo múltiple con interacciones
fit17 = lm(Sa~RollRate+currRoll+diffRollRate+currPitch+PitchRate+(.^3)+I(RollRate^3)+I(currRoll^3)
           +I(diffRollRate^3)+I(PitchRate^3)+I(currPitch^3),data=deltaAil)
summary(fit17)
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Construcción de modelo utlizando kNN y obtención de RMSE
require(kknn)
fitknn1 <- kknn(Sa ~ .-currPitch-PitchRate, deltaAil, deltaAil)
yprime = fitknn1$fitted.values
sqrt(sum((deltaAil$Sa-yprime)^2)/length(yprime))

#Construcción de modelo utlizando kNN y obtención de RMSE
fitknn2 <- kknn(Sa ~ ., deltaAil, deltaAil)
yprime = fitknn2$fitted.values
sqrt(sum((deltaAil$Sa-yprime)^2)/length(yprime))

#Construcción de modelo utlizando kNN y obtención de RMSE
fitknn3 <- kknn(Sa ~ RollRate, deltaAil, deltaAil)
yprime = fitknn3$fitted.values
sqrt(sum((deltaAil$Sa-yprime)^2)/length(yprime))

#Construcción de modelo utlizando kNN y obtención de RMSE
fitknn4 <- kknn(Sa ~ RollRate+diffRollRate+currRoll+RollRate*diffRollRate, deltaAil, deltaAil)
yprime = fitknn4$fitted.values
sqrt(sum((deltaAil$Sa-yprime)^2)/length(yprime))

#Construcción de modelo utlizando kNN y obtención de RMSE
fitknn5 <- kknn(Sa ~ RollRate+diffRollRate+currRoll+RollRate*diffRollRate+I(RollRate^2)
                +I(diffRollRate^2), deltaAil, deltaAil)
yprime = fitknn5$fitted.values
sqrt(sum((deltaAil$Sa-yprime)^2)/length(yprime))

#Construcción de modelo utlizando kNN y obtención de RMSE
fitknn6 <- kknn(Sa ~ RollRate+diffRollRate+currRoll+currPitch+PitchRate+(.^2),
                deltaAil, deltaAil)
yprime = fitknn6$fitted.values
sqrt(sum((deltaAil$Sa-yprime)^2)/length(yprime))

#Construcción de modelo utlizando kNN y obtención de RMSE
fitknn7 <- kknn(Sa ~ RollRate+diffRollRate+currRoll+currPitch+PitchRate+(.^3), deltaAil,
                deltaAil)
yprime = fitknn7$fitted.values
sqrt(sum((deltaAil$Sa-yprime)^2)/length(yprime))

#Construcción de modelo utlizando kNN con datos normalizados y obtención de RMSE
fitknnNorm <- kknn(Sa ~ RollRate+diffRollRate+currRoll+currPitch+PitchRate+(.^2), deltaAilNormalizado, deltaAilNormalizado)
yprime = fitknnNorm$fitted.values
sqrt(sum((deltaAil$Sa-yprime)^2)/length(yprime))
## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
#Cálculo de MSE para el algoritmo "lm" utilizando validación cruzada
setwd("./delta_ail")

nombre <- "delta_ail"
run_lm_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  fitMulti=lm(Y~.-X2-X3,x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Cálculo de MSE para el algoritmo "knn" utilizando validación cruzada
setwd("./delta_ail")

nombre <- "delta_ail"
run_knn_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-5-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-5-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  In <- length(names(x_tra)) - 1
  names(x_tra)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tra)[In+1] <- "Y"
  names(x_tst)[1:In] <- paste ("X", 1:In, sep="")
  names(x_tst)[In+1] <- "Y"
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  fitMulti=kknn(Y~.+(.^2),x_tra,test)
  yprime=fitMulti$fitted.values
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Resultados de train y test para los algoritmos lm y kNN
lmMSEtrain
lmMSEtest
knnMSEtrain
knnMSEtest
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Preparación tablas para tests de comparación
setwd("./delta_ail")

resultados <- read.csv("regr_test_alumnos.csv")
tablatst <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatst) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatst) <- resultados[,1]

resultados <- read.csv("regr_train_alumnos.csv")
tablatra <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatra) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatra) <- resultados[,1]
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Test de Wilcoxon
difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)

LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic

#Resultados de test de Wilcoxon
Rmas
Rmenos
pvalue
#Confianza
(1-pvalue)*100
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Test de Wilcoxon para resultados de entrenamiento
difs <- (tablatra[,1] - tablatra[,2]) / tablatra[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatra)[1], colnames(tablatra)[2])
head(wilc_1_2)

LMvsKNNtra <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtra$statistic
pvalue <- LMvsKNNtra$p.value
LMvsKNNtra <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtra$statistic

Rmas
Rmenos
pvalue
#Confianza
(1-pvalue)*100
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Test de Friedman
test_friedman <- friedman.test(as.matrix(tablatst))
test_friedman

#Test de post-hoc Holm
tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)
## ------------------------------------------------------------------------

###################################################
#Problema de Clasificación
###################################################

## ------------------------------------------------------------------------
#División del dataset en entrenamiento y test
set.seed(10)
shuffled <- sample(dim(heart)[1])
eightypct <- (dim(heart)[1] * 80) %/% 100
heart_train <- heart[shuffled[1:eightypct], 1:13]
heart_test <- heart[shuffled[(eightypct+1):dim(heart)[1]], 1:13]

heart_train_labels <- heart[shuffled[1:eightypct], 14]
heart_test_labels <- heart[shuffled[(eightypct+1):dim(heart)[1]], 14]

#División de los datos normalizados en entrenamiento y test
set.seed(10)
shuffledN <- sample(dim(heartNormalizado)[1])
heartN_train <- heartNormalizado[shuffledN[1:eightypct], 1:13]
heartN_test <- heartNormalizado[shuffledN[(eightypct+1):dim(heartNormalizado)[1]], 1:13]

heartN_train_labels <- heartNormalizado[shuffledN[1:eightypct], 14]
heartN_test_labels <- heartNormalizado[shuffledN[(eightypct+1):dim(heartNormalizado)[1]], 14]
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Construcción de modelos utilizando kNN con diferentes "k"
library(class)
require(caret)
set.seed(10)
getKnn <- function(miK){

  heart_test_pred <- knn(train = heart_train, test = heart_test, cl = heart_train_labels, k=miK)
  
  postResample(pred = heart_test_pred, obs = heart[shuffled[(eightypct+1):dim(heart)[1]], 14])  
}
result1 <- lapply(1:20,getKnn)
result1 <- unlist(result1)[1:20*2-1]
#Dado que existe aleatoriedad en este proceso se realiza 5 veces y se trata con la media
result2 <- lapply(1:20,getKnn)
result2 <- unlist(result2)[1:20*2-1]
result3 <- lapply(1:20,getKnn)
result3 <- unlist(result3)[1:20*2-1]
result4 <- lapply(1:20,getKnn)
result4 <- unlist(result4)[1:20*2-1]
result5 <- lapply(1:20,getKnn)
result5 <- unlist(result5)[1:20*2-1]

result<-unlist(Map("+",unlist(Map("+",unlist(Map("+",unlist(Map("+",result1,result2)),
                                                 result3)),result4)),result5))

result<-result/5
## ------------------------------------------------------------------------

## ---- echo=TRUE----------------------------------------------------------
#Gráfica de resultados obtenidos en diferentes "k"
df <- data.frame(k=1:20,accuracy=result)
ggplot(df, aes(x=k,y=accuracy)) + geom_histogram(stat="identity",color="black",fill="deepskyblue")+coord_cartesian(ylim=c(0.65,0.8))+ labs(title="Acierto de kNN con diferentes k")  

#Mejor resultado obtenido
max(result)
## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
#Construcción de modelos con kNN con datos normalizados y diferentes "k"
set.seed(10)
getKnn <- function(miK){

  heart_test_pred <- knn(train = heartN_train, test = heartN_test, cl = heartN_train_labels, k=miK)

  postResample(pred = heart_test_pred, obs = heartNormalizado[shuffledN[(eightypct+1):dim(heartNormalizado)[1]], 14])  
}

result1 <- lapply(1:20,getKnn)
result1 <- unlist(result1)[1:20*2-1]
#Dado que existe aleatoriedad en este proceso se realiza 5 veces y se trata con la media de los resultados
result2 <- lapply(1:20,getKnn)
result2 <- unlist(result2)[1:20*2-1]
result3 <- lapply(1:20,getKnn)
result3 <- unlist(result3)[1:20*2-1]
result4 <- lapply(1:20,getKnn)
result4 <- unlist(result4)[1:20*2-1]
result5 <- lapply(1:20,getKnn)
result5 <- unlist(result5)[1:20*2-1]

result<-unlist(Map("+",unlist(Map("+",unlist(Map("+",unlist(Map("+",result1,result2)),
                                                 result3)),result4)),result5))
print(result)

result<-result/5
## ------------------------------------------------------------------------

## ---- echo=TRUE----------------------------------------------------------
#Gráfica con resultados de kNN y sus diferentes "k"
df <- data.frame(k=1:20,accuracy=result)

ggplot(df, aes(x=k,y=accuracy)) + geom_histogram(stat="identity",color="black",fill="deepskyblue")+coord_cartesian(ylim=c(0.7,0.85))+ labs(title="Acierto de kNN con diferentes k en datos normalizados")

#Mejor resultado obtenido
max(result)
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#División de los datos en entrenamiento y test
set.seed(10)
shuffled <- sample(dim(heart)[1])
eightypct <- (dim(heart)[1] * 80) %/% 100
heart_train <- heart[shuffled[1:eightypct], ]
heart_test <- heart[shuffled[(eightypct+1):dim(heart)[1]], ]
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Construcción del modelo con LDA
library(MASS)
library(ISLR)
ldaFit <- lda(Class ~.,data=heart_train)
ldaFit

#Resultados del modelo construido con LDA
ldaPred <- predict(ldaFit,heart_test)

table(ldaPred$class,heart_test$Class)
resultLDA <- mean(ldaPred$class==heart_test$Class)
resultLDA
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Construcción y resultados de un modelo LDA sobre datos normalizados
set.seed(10)
shuffledN <- sample(dim(heartNormalizado)[1])
heartN_train <- heartNormalizado[shuffledN[1:eightypct], ]
heartN_test <- heartNormalizado[shuffledN[(eightypct+1):dim(heartNormalizado)[1]], ]

ldaFitN <- lda(Class ~.,data=heartN_train)
ldaFitN

ldaPredN <- predict(ldaFitN,heartN_test)
table(ldaPredN$class,heartN_test$Class)
resultLDAN <- mean(ldaPredN$class==heartN_test$Class)
resultLDAN
## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
#Construcción del modelo con QDA
qdaFit <- qda(Class ~.,data=heart_train)
qdaFit

#Resultados del modelo construido con QDA
qdaPred <- predict(qdaFit,heart_test)

table(qdaPred$class,heart_test$Class)
resultQDA <- mean(qdaPred$class==heart_test$Class)
resultQDA
## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
#Construcción y resultados del modelo QDA sobre datos normalizados
qdaFitN <- qda(Class ~.,data=heartN_train)
qdaFitN

qdaPredN <- predict(qdaFitN,heartN_test)
table(qdaPredN$class,heartN_test$Class)
resultQDAN <- mean(qdaPredN$class==heartN_test$Class)
resultQDAN
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Obtención de resultados de kNN utilizando validación cruzada y diferentes "k"
setwd("./heart")

nombre <- "heart"
run_knn_fold <- function(i, x, tt = "test",miK) {
  file <- paste(x, "-10-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-10-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  
  
  #Normalizacion
  heartN_train <- data.frame(x_tra)
  names(heartN_train) <- c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral",
  "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced",
  "Oldpeak", "Slope", "MajorVessels", "Thal", "Class")
  
  heartN_train$RestBloodPressure <- scale(heartN_train$RestBloodPressure)
  heartN_train$SerumCholestoral <- scale(heartN_train$SerumCholestoral)
  heartN_train$MaxHeartRate <- scale(heartN_train$MaxHeartRate)
  heartN_train$Oldpeak <- scale(heartN_train$Oldpeak)
  
  
  heartN_test <- data.frame(test)
  names(heartN_test) <- c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral",
  "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak",
  "Slope", "MajorVessels", "Thal", "Class")
  
  heartN_test$RestBloodPressure <- scale(heartN_test$RestBloodPressure)
  heartN_test$SerumCholestoral <- scale(heartN_test$SerumCholestoral)
  heartN_test$MaxHeartRate <- scale(heartN_test$MaxHeartRate)
  heartN_test$Oldpeak <- scale(heartN_test$Oldpeak)
  
  #Preparacion datos
  heartN_train$Class <- factor(heartN_train$Class, levels = c(1, 2), labels = c("Si", "No"))
  heartN_test$Class <- factor(heartN_test$Class, levels = c(1, 2), labels = c("Si", "No"))
  
  heartN_train_labels <- heartN_train$Class
  heartN_test_labels <- heartN_test$Class
  
  heartN_train <- heartN_train[,1:13]
  heartN_test <- heartN_test[,1:13]
  
  #Construcción modelo kNN y resultado
  heart_test_pred <- knn(train = heartN_train, test = heartN_test, cl = heartN_train_labels, k=miK)
  postResample(pred = heart_test_pred, obs = heartN_test_labels) 
  
}

#Función utilizada para construir modelos con diferentes "k"
mejorK <- function(miK){
  
  knnAcctrain<-mean(sapply(1:10,run_knn_fold,nombre,"train",miK))
  knnAcctest<-mean(sapply(1:10,run_knn_fold,nombre,"test",miK))
  list(knnAcctrain,knnAcctest)
}

resultados <- lapply(1:20,mejorK)
## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
#Obtención de la mejor "k"
which.max(unlist(resultados)[1:20*2])

#Resultados obtenidos
knnAcctrain <- unlist(resultados[7])[1]
knnAcctest <- unlist(resultados[7])[2]
knnAcctrain
knnAcctest
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Resultado de modelo LDA utilizando validación cruzada
setwd("./heart")

nombre <- "heart"
run_lda_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-10-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-10-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  
  
  #Preparación datos
  heartN_train <- data.frame(x_tra)
  names(heartN_train) <- c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral",
  "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak",
  "Slope", "MajorVessels", "Thal", "Class")
  
  
  heartN_test <- data.frame(test)
  names(heartN_test) <- c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral",
  "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak",
  "Slope", "MajorVessels", "Thal", "Class")
  
  
  heartN_train$Class <- factor(heartN_train$Class, levels = c(1, 2), labels = c("Si", "No"))
  heartN_test$Class <- factor(heartN_test$Class, levels = c(1, 2), labels = c("Si", "No"))
  
  #Construcción modelo y resultado
  ldaFit <- lda(Class ~.,data=heartN_train)
  ldaPred <- predict(ldaFit,heartN_test)
  
  table(ldaPred$class,heartN_test$Class)
  resultLDA <- mean(ldaPred$class==heartN_test$Class)
  resultLDA
  
}

#Resultados obtenidos
ldaAcctrain<-mean(sapply(1:10,run_lda_fold,nombre,"train"))
ldaAcctest<-mean(sapply(1:10,run_lda_fold,nombre,"test"))
ldaAcctrain
ldaAcctest
## ------------------------------------------------------------------------


## ------------------------------------------------------------------------
#Resultados del modelo QDA utilizando validación cruzada
setwd("./heart")

nombre <- "heart"
run_qda_fold <- function(i, x, tt = "test") {
  file <- paste(x, "-10-", i, "tra.dat", sep="")
  x_tra <- read.csv(file, comment.char="@")
  file <- paste(x, "-10-", i, "tst.dat", sep="")
  x_tst <- read.csv(file, comment.char="@")
  if (tt == "train") {
    test <- x_tra
  }
  else {
    test <- x_tst
  }
  
  
  #Preparación datos
  heartN_train <- data.frame(x_tra)
  names(heartN_train) <- c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral",
  "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak",
  "Slope", "MajorVessels", "Thal", "Class")
  
  
  heartN_test <- data.frame(test)
  names(heartN_test) <- c("Age", "Sex", "ChestPainType", "RestBloodPressure", "SerumCholestoral",
  "FastingBloodSugar", "ResElectrocardiographic", "MaxHeartRate", "ExerciseInduced", "Oldpeak",
  "Slope", "MajorVessels", "Thal", "Class")
  
  
  heartN_train$Class <- factor(heartN_train$Class, levels = c(1, 2), labels = c("Si", "No"))
  heartN_test$Class <- factor(heartN_test$Class, levels = c(1, 2), labels = c("Si", "No"))
  
  #Construcción de modelo
  qdaFit <- qda(Class ~.,data=heartN_train)
  qdaPred <- predict(qdaFit,heartN_test)
  
  table(qdaPred$class,heartN_test$Class)
  resultQDA <- mean(qdaPred$class==heartN_test$Class)
  resultQDA
  
}


#Resultados obtenidos
qdaAcctrain<-mean(sapply(1:10,run_qda_fold,nombre,"train"))
qdaAcctest<-mean(sapply(1:10,run_qda_fold,nombre,"test"))
qdaAcctrain
qdaAcctest
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Preparación tablas para comparación de algoritmos
setwd("./heart")

resultados <- read.csv("clasif_test_alumnos.csv")
tablatst <- cbind(resultados[,2:dim(resultados)[2]])
colnames(tablatst) <- names(resultados)[2:dim(resultados)[2]]
rownames(tablatst) <- resultados[,1]
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Test de Wilcoxon
#kNN vs LDA
difs <- (tablatst[,1] - tablatst[,2]) / tablatst[,1]
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c(colnames(tablatst)[1], colnames(tablatst)[2])
head(wilc_1_2)

LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue
#Confianza
(1-pvalue)*100
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Test de Wilcoxon
#kNN vs QDA
difs <- (tablatst[,1] - tablatst[,3]) / tablatst[,1]
wilc_1_3 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_3) <- c(colnames(tablatst)[1], colnames(tablatst)[3])
head(wilc_1_3)

LMvsKNNtst <- wilcox.test(wilc_1_3[,1], wilc_1_3[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_3[,2], wilc_1_3[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue
#Confianza
(1-pvalue)*100
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Test de Wilcoxon
#LDA vs QDA
difs <- (tablatst[,2] - tablatst[,3]) / tablatst[,2]
wilc_2_3 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_2_3) <- c(colnames(tablatst)[2], colnames(tablatst)[3])
head(wilc_2_3)

LMvsKNNtst <- wilcox.test(wilc_2_3[,1], wilc_2_3[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_2_3[,2], wilc_2_3[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue
#Confianza
(1-pvalue)*100
## ------------------------------------------------------------------------

## ------------------------------------------------------------------------
#Test de Friedman
test_friedman <- friedman.test(as.matrix(tablatst))
test_friedman

#Test de post-hoc Holm
tam <- dim(tablatst)
groups <- rep(1:tam[2], each=tam[1])
pairwise.wilcox.test(as.matrix(tablatst), groups, p.adjust = "holm", paired = TRUE)
## ------------------------------------------------------------------------
