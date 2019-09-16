require(ISLR)
require(MASS)

#Lo primero es cargar el dataset 'California'

California <- read.csv("/home/antonio/Descargas/california.dat", comment.char="@")

#Asignación manual
names(California) <- c("Longitude", "Latitude", "HousingMedianAge",
                 "TotalRooms", "TotalBedrooms", "Population", "Households",
                 "MedianIncome", "MedianHouseValue")


#Para este ejercicios tendremos que hacer una predicción de la última variable "Median House Value"

#Empezamos realizando una comparación "1 vs 1" de todas las variables con la que queremos predecir y descubrir si podemos ya desechar alguna que no nos interese

temp <- California
plotY <- function (x,y) {
  plot(temp[,y]~temp[,x], xlab=paste(names(temp)[x]," X",x,sep=""),
       ylab=names(temp)[y])
}
par(mfrow=c(3,4))
x <- sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2])
par(mfrow=c(1,1))

#Aunque en un principio ninguna de ellas muestra una tendencia clara, podríamos prescindir, por ejemplo, de la variable "Population" pues parece que el valor de la casa
#es más independiente de la población de la ciudad.

#Ahora vamos a estudiar las correlaciones de las variables

calculateCor <- function(var1){
  cor(California$MedianHouseValue,var1)
}
xCor <- lapply(list(California$Longitude, California$Latitude, California$HousingMedianAge, California$TotalRooms, California$TotalBedrooms, 
                    California$Population, California$Households,California$MedianIncome),calculateCor)
unlist(xCor)

#Con este cálculo comprobamos que la variable más correlacionada es "MedianIncome" con una correlación de 0.688 .Ya con mucha diferencia las siguientes serían Latitude, 
#HousingMedianAge y TotalRooms.


#Procedemos ahora a construir un modelo con "MedianIncome" para el ajuste lineal

fit1=lm(MedianHouseValue~MedianIncome,data=California)
summary(fit1)

#Observando el campo de "Adjusted R-squared", vemos que tenemos un acierto del 47.34% sólo con esa variable en un ajuste lineal.
#Podemos dibujar este ajuste:
par(mfrow=c(1,1))
plot(MedianHouseValue~MedianIncome,California)
abline(fit1,col="blue")


#Si lo probamos con otra variable como "TotalRooms":
fit2=lm(MedianHouseValue~TotalRooms,data=California)
summary(fit2)

#Tenemos un resultado mucho más pobre de un 1%

#Calculamos la raíz del ECM para el primer modelo:

sqrt(sum(fit1$residuals^2)/length(fit1$residuals))

#Trabajando ahora con la predicción de nuevos datos:

predict(fit1,data.frame(MedianIncome=c(2.5,5,7.5)))

#También podemos calcular el RMSE para datos de test:

yprime=predict(fit1,data.frame(MedianIncome=California$MedianIncome))

sqrt(sum(abs(California$MedianHouseValue-yprime)^2)/length(yprime))


#Ahora vamos a trabajar con modelos lineales multiples, utilizando varias variables

fit3=lm(MedianHouseValue~MedianIncome+TotalRooms,data=California)
summary(fit3)

#Vemos que se obtiene el mismo R ajustado de 47.34% y que se obtiene un p-valor muy alto para la variable "TotalRooms", lo cuál nos indica que deberíamos descartarla.

#Contiuamos probando otras combinaciones de variables buscando mejorar el resultado y descubrir qué variable resultan irrelevantes o incluso empeoran el resultado:

fit4=lm(MedianHouseValue~MedianIncome+Latitude,data=California)
summary(fit4)

#Se consigue una ligera mejora al 48.14%. Además, el p-valor tan bajo para la variable de Latitud indica que es interesante tener en cuenta esa variable en el modelo.

fit5=lm(MedianHouseValue~MedianIncome+Latitude+HousingMedianAge,data=California)
summary(fit5)

#Nuevamente conseguimos una mejora hasta el 51.7% por lo que la edad de la casa es otro factor a considerar
#Podemos ahora nuevamente probar con la variable TotalRooms para saber si conseguimos una mejora al estar trabajando también con estas otras variables

fit6=lm(MedianHouseValue~MedianIncome+Latitude+HousingMedianAge+TotalRooms,data=California)
summary(fit6)

#Se obtiene una ligera mejora, consiguiendo un 52.14%

#Comprobemos ahora al incluir todas las variables en el modelo:

fit7=lm(MedianHouseValue~.,data=California)
summary(fit7)

#Se obtiene una considerable mejora, alcanzando un 63.7%
#Observando los p-valores de las variables se tiene que "Households" es ligeramente superior por lo que podemos probar a eliminarla del modelo

fit8=lm(MedianHouseValue~.-Households,data=California)
summary(fit8)

#Se tiene ahora un acierto del 63.63% y ya no se indican que haya más variables de las que podamos prescindir. Podríamos ahora valorar si merece la pena sacrificar
#ligeramente el acierto por un modelo con menos variables y más interpretable. En este caso, dado que no se tienen demasiadas variables, se decide escoger cómo modelo
#el que incluía todas las variables.

#Interacciones

#Ahora consiste en que las variables del modelo "interactuen" entre ellas con el objetivo de conseguir mejores modelos. Empezamos multiplicando las dos
#primeras variables con las que trabajamos:

fit9=lm(MedianHouseValue~MedianIncome*Latitude,data=California)
summary(fit9)

#Se obtiene tan solo un 48.3% .Probamos añadiendo otra variable

fit10=lm(MedianHouseValue~MedianIncome*HousingMedianAge,data=California)
summary(fit10)

#50.9%

fit11=lm(MedianHouseValue~MedianIncome*Latitude*HousingMedianAge,data=California)
summary(fit11)

#52%

#Obteniendo resultados muy similares a los que se obtuvieron anteriormente, parece que con estas interacciones no se está consiguiendo un mejor comportamiento
#del modelo.


#Vamos a probar ahora a trabajar con la no-linealidad

fit12=lm(MedianHouseValue~MedianIncome + I(MedianIncome^2),data=California)
summary(fit12)

#Tan solo un 47.8%. Nuevamente no se está consiguiendo mejorar el modelo

fit13=lm(MedianHouseValue~MedianIncome + HousingMedianAge + I(MedianIncome^2)+I(HousingMedianAge^2),data=California)
summary(fit13)

#51.78%

fit14=lm(MedianHouseValue~MedianIncome + HousingMedianAge + I(MedianIncome^2)*I(HousingMedianAge^2),data=California)
summary(fit14)

#51.78%

fit15=lm(MedianHouseValue~MedianIncome + HousingMedianAge + I(MedianIncome^3)+I(HousingMedianAge^3),data=California)
summary(fit15)

#52.16

#Dado que cuando se utilizó todas las variables se obtuvo el mejor resultado, probamos ahora no-linealidad con todas las variables
fit16=lm(MedianHouseValue~(.^2),data=California)
summary(fit16)

#Un acierto de 68.09. Probamos ahora a prescindir de algunas variables que se nos indica

fit17=lm(MedianHouseValue~(.^2)-Population*Households,data=California)
summary(fit17)

#68.03% Seguimos quitando algunas variables

fit18=lm(MedianHouseValue~((.-Longitude)^2)-Population*Households,data=California)
summary(fit18)

#Se cae de nuevo a 61.37% por lo que finalmente vamos a tomar "MedianHouseValue~(.^2)" como el mejor modelo con 68.09%.

#Procedemos a visualizarlo
plot(California$MedianHouseValue~California$MedianIncome)
points(California$MedianIncome,fitted(fit16),col="blue",pch=20)

#Y calculamos el RMSE para el conjunto de test
yprime=predict(fit16,California)
sqrt(sum(abs(California$MedianHouseValue-yprime)^2)/length(yprime))

#Se obtiene un error de 65130.05

