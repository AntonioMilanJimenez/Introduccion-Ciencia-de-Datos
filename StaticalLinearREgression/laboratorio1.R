require(ISLR)
require(MASS)

#Comprobamos que ya esta cargado el dataset
Boston$lstat

#Podriamos añadir el objeto al entorno para acceder a él directamente:

attach(Boston)
lstat
detach(Boston)


#Asi comparamos dos variables del dataset
plot(medv~age,Boston)


#Asi las visualizaremos todas las comparaciones
temp <- Boston
plotY <- function (x,y) {
  plot(temp[,y]~temp[,x], xlab=paste(names(temp)[x]," X",x,sep=""),
       ylab=names(temp)[y])
}
par(mfrow=c(3,4))
x <- sapply(1:(dim(temp)[2]-1), plotY, dim(temp)[2])
par(mfrow=c(1,1))

#Podemos ya de un simple vistazo descartar algunas y quedarnos con las mas relevantes:

par(mfrow=c(3,3))
x <- sapply(c(1, 5, 6, 7, 8, 10, 11, 12, 13), plotY, dim(temp)[2])
par(mfrow=c(1,1))

#Bien graficamente o calculando la correlacion, descubrimos que las varaibles
# 6 y 13, es decir, rm y lstat, son las varibales candidatas para el
#ajuste lineal

#VAmos a obtener los modelos de ello

fit1=lm(medv~lstat,data=Boston)
#or fit1=lm(Boston$medv~Boston$lstat)
fit1

fit2=lm(medv~rm,data=Boston)
fit2

#Vamos a obtener diferentes valores estadisticos para estas dos variables:

summary(fit1)
par(mfrow=c(2,1))
plot(medv~lstat,Boston)
abline(fit1,col="red")
confint(fit1)

summary(fit2)
plot(medv~rm,Boston)
abline(fit2,col="blue")
par(mfrow=c(1,1))
confint(fit2)

#Viendo que fi1 ha dado mejores resultados, nos centramos en él

sqrt(sum(fit1$residuals^2)/length(fit1$residuals))

sqrt(sum(fit1$residuals^2)/(length(fit1$residuals)-2))


#Prediccion

predict(fit1,data.frame(lstat=c(5,10,15)))

#Calculando manualmente el RMSE

yprime=predict(fit1,data.frame(lstat=Boston$lstat))

sqrt(sum(abs(Boston$medv-yprime)^2)/length(yprime))



#Modelos lineales multiples:

#Ir obteniendo el modelo añadiendo las variables
fit3=lm(medv~lstat+age,data=Boston)
summary(fit3)

fit4=lm(medv~lstat+rm,data=Boston)
summary(fit4)

#En esto ultimo comprobamos que "Adjusted R-squared" es de 0.637, consiguiendo
# un gran incremento ya que hemos utilizado las dos mejores variables, lstat y rm

#Para ahorrarnos tiempo en determinar las mejores variables, las mas interesantes,
#podemos dibujarlas en esta escala de grises que indicaran variables interesantes
#en aquellas gráficas que se siga una escala de grises ordenada

temp <- Boston
plot(temp[,-dim(temp)[2]],pch=16,col=gray(1-(temp[,dim(temp)[2]]/max(temp[,dim(temp)[2]]))))

#Ahora vamos a hacerlo con todas las variables

fit5=lm(medv~.,data=Boston)
summary(fit5)

#Lo primero es comprobar en F-statistic el p-value si indica confianza. Si no fuese así, un p-valor alto, directamente se descarta el modelo.
#Si sí muestra confianza, significara que al menos una de las variables de entrada muestra relacion lineal con la variable que buscamos de salida;
#obsevamos ya y comparamos el "Adjusted R-squared"

#Tenemos un Adjusted R-squared de 0.733 y descubrimos variables con p-valores muy altos.
#Esto nos invita a ir eliminando de una a una las variables que vayan teniendo los p-valores más altos

fit6=lm(medv~.-age,data=Boston)
summary(fit6)

#Se ha mejorado muy levemente el resultado y hemos conseguido prescindir de una variable. Seguimos con el proceso y
#fijandonos en los p-valores de las variables que se van obteniendo ya que pueden ir cambiando

fit7=lm(medv~.-age-indus,data=Boston)
summary(fit7)

fit8=lm(medv~.-age-indus-chas,data=Boston)
summary(fit8)

fit9=lm(medv~.-age-indus-chas-crim,data=Boston)
summary(fit9)

#Por ejemplo aqui se empeora un poco pero conseguimos mejor interpretabilidad. Pararemos donde conseguiremos oportuno.



#Interacciones:

#Se trata de hacer interacciones (multiplicar) entre variables

attach(Boston)
par(mfrow=c(1,1))
fit8=lm(medv~lstat*rm,Boston)
summary(fit8)
plot(medv~lstat)
points(lstat,fitted(fit8),col="green",pch=20)

#La tabla de coeficientes aparece ordenada por jerarquia, no podriamos ahi prescindir de lstat o rm si tenemos la interacción entre ambas. Se traduce en que "no nos importa" en este caso el p-valor para lstat y para rm


#No linealidad

fit9=lm(medv~I(lstat^2),Boston)
summary(fit9)
plot(medv~lstat)
points(lstat,fitted(fit9),col="red",pch=20)

#Es necesario incluir los terminos de menor grado por jerarquia

fit9=lm(medv~lstat +I(lstat^2),Boston)
summary(fit9)
plot(medv~lstat)
points(lstat,fitted(fit9),col="red",pch=20)

#Se observa una mejora muy importante al haber incluido ese termino


#Prueba

fitprueba=lm(medv~lstat +rm +I(lstat * rm) +I(lstat^2) +I(lstat^2 * rm),Boston)
summary(fitprueba)
plot(medv~lstat)
points(lstat,fitted(fitprueba),col="red",pch=20)


#Calculo manual de RMSE

yprime=predict(fit8,Boston)
sqrt(sum(abs(Boston$medv-yprime)^2)/length(yprime))


#Formato KEEL

#Lectura del dataset CAlifornia en formato Keel. Ejecutar script de pag 20 para tener ya preparado el dataset california y poder trabajar con él.
