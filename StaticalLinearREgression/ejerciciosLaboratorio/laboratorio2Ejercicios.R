require(MASS)
require(kknn)

#Obtenemos el modelo

California <- read.csv("/home/antonio/Descargas/california.dat", comment.char="@")

#Asignación manual
names(California) <- c("Longitude", "Latitude", "HousingMedianAge",
                       "TotalRooms", "TotalBedrooms", "Population", "Households",
                       "MedianIncome", "MedianHouseValue")

fitknn1 <- kknn(MedianHouseValue ~ ., California, California)


#Aqui tenemos informacion del objeto

names(fitknn1)

#Podemos ahora visualizar este modelo

plot(California$MedianHouseValue~California$MedianIncome)
points(California$MedianIncome,fitknn1$fitted.values,col="blue",pch=20)

#Y a continuación realizar predicciones y calcular la raíz de ECM

yprime = fitknn1$fitted.values
sqrt(sum((California$MedianHouseValue-yprime)^2)/length(yprime)) #RMSE

#Se obtiene un RMSE de 39132.79
#Recordemos que con regresión lineal múltiple se obtuvo con el mejor modelo un error de 65130.05, muy superior a este.

#Probando ahora con el mejor modelo que se tuvo para regresión lineal múltiple:

fitknn2 <- kknn(MedianHouseValue~(.^2), California, California)
yprime = fitknn2$fitted.values
sqrt(sum((California$MedianHouseValue-yprime)^2)/length(yprime)) #RMSE

#Se obtiene un RMSE de 37943.05, que es el mejor resultado hasta ahora. Sin embargo, el mejor modelo con regresión lineal no tiene porqué ser el mismo para knn.

fitknn3 <- kknn(MedianHouseValue~((.-Longitude)^2), California, California)
yprime = fitknn3$fitted.values
sqrt(sum((California$MedianHouseValue-yprime)^2)/length(yprime)) #RMSE

#Un RMSE de 40606.61

fitknn4 <- kknn(MedianHouseValue~.-Households, California, California)
yprime = fitknn4$fitted.values
sqrt(sum((California$MedianHouseValue-yprime)^2)/length(yprime)) #RMSE

#Se obtiene un RMSE de 39445.74

#Por lo tanto, para los diferentes modelos que hemos probado, el mejor coincide con el que lo fue también para la regresión lineal. Podemos visualizarlo:

plot(California$MedianHouseValue~California$MedianIncome)
points(California$MedianIncome,fitknn2$fitted.values,col="blue",pch=20)



#Pasamos ahora a trabajar con la validación cruzada

setwd("/home/antonio/Descargas")

nombre <- "california"
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
  fitMulti=lm(Y~(.^2),x_tra)
  yprime=predict(fitMulti,test)
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
lmMSEtrain<-mean(sapply(1:5,run_lm_fold,nombre,"train"))
lmMSEtest<-mean(sapply(1:5,run_lm_fold,nombre,"test"))
lmMSEtrain
lmMSEtest

#lmMSEtrain --> 4232811871
#lmMSEtest --> 4285665165

#Pasamos ahora a compararlo con knn

nombre <- "california"
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
  fitMulti=kknn(Y~(.^2),x_tra,test)
  yprime=fitMulti$fitted.values
  sum(abs(test$Y-yprime)^2)/length(yprime) ##MSE
}
knnMSEtrain<-mean(sapply(1:5,run_knn_fold,nombre,"train"))
knnMSEtest<-mean(sapply(1:5,run_knn_fold,nombre,"test"))
knnMSEtrain
knnMSEtest

#knnMSEtrain --> 1464815703
#knnMSEtest --> 4303958978

#Si sólo nos hubiésemos centrado en los resultados para "train", hubiesemos concluido que "knn" se comporta mejor para este dataset que la regresión lineal. Sin embargo,
#gracias a que se ha realizado la validación cruzada, observamos los resultados para test y descubrimos que son muy similares a los de regresión, incluso algo superiores.
#Con esto descubrimos que "knn" se ha sobreajustado a los datos de entrenamiento.


#A continuación vamos a comparar estos algoritmos para saber realmente cómo de diferente ha sido su rendimiento para este conjunto de datos. Utilizaremos el test de Wilcoxon.

#Para los resultados de test
difs <- (lmMSEtest - knnMSEtest) / lmMSEtest
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c("lm test", "knn test")
head(wilc_1_2)


LMvsKNNtst <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtst$statistic
pvalue <- LMvsKNNtst$p.value
LMvsKNNtst <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtst$statistic
Rmas
Rmenos
pvalue

#Con un p-value de 1 nos indica el test con toda seguridad que los resultados de estos algoritmos para el conjunto de datos son los mismos



#Para los resultados de train
difs <- (lmMSEtrain - knnMSEtrain) / lmMSEtrain
wilc_1_2 <- cbind(ifelse (difs<0, abs(difs)+0.1, 0+0.1), ifelse (difs>0, abs(difs)+0.1, 0+0.1))
colnames(wilc_1_2) <- c("lm train", "knn train")
head(wilc_1_2)


LMvsKNNtrain <- wilcox.test(wilc_1_2[,1], wilc_1_2[,2], alternative = "two.sided", paired=TRUE)
Rmas <- LMvsKNNtrain$statistic
pvalue <- LMvsKNNtrain$p.value
LMvsKNNtrain <- wilcox.test(wilc_1_2[,2], wilc_1_2[,1], alternative = "two.sided", paired=TRUE)
Rmenos <- LMvsKNNtrain$statistic
Rmas
Rmenos
pvalue


#Con un p-value de 1, nos vuelve a indicar que son algoritmos similares en cuanto a resultados.



