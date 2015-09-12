Sintaxis Diagnóstico del modelo "Mass and Physical Measurements for Male Subjects"
USMass <- read.delim("http://www.statsci.org/data/oz/physical.txt")
head(USMass)
attach(USMass)
plot(USMass) 
#La inspección gráfica no nos aporta información suficiente para conformar los modelos

cor(USMass) 
#Este análisis de correlación nos permite identificar las mejores variables para la construcción de los modelos

# Agrupamiento de Datos: se conformaron los grupos con base al análisis de correlación
grupo1<-data.frame(Mass,Shoulder,Fore,Waist,Thigh,Calf)
grupo2<-data.frame(Mass,Fore,Waist,Bicep,Chest,Head)
grupo3<-data.frame(Mass,Neck,Height,Chest,Shoulder,Calf)
grupo4<-data.frame(Mass,Chest,Neck,Waist,Thigh,Calf)
grupo5<-data.frame(Mass,Shoulder,Head,Fore,Bicep,Thigh)
grupo6<-data.frame(Mass,Waist,Height,Fore,Thigh,Neck)
grupo7<-data.frame(Mass,Waist,Height,Fore,Thigh,Shoulder)
grupo8<-data.frame(Mass,Waist,Height,Fore,Thigh,Calf)

# Planteamiento de modelos:
modelo1<-lm(Mass~.,data=grupo1);summary(modelo1)
modelo2<-lm(Mass~.,data=grupo2);summary(modelo2)
modelo3<-lm(Mass~.,data=grupo3);summary(modelo3)
modelo4<-lm(Mass~.,data=grupo4);summary(modelo4)
modelo5<-lm(Mass~.,data=grupo5);summary(modelo5)   
modelo6<-lm(Mass~.,data=grupo6);summary(modelo6)
modelo7<-lm(Mass~.,data=grupo7);summary(modelo7)
modelo8<-lm(Mass~.,data=grupo8);summary(modelo8)

# Diagnóstico del modelo:
# Librerías a usar, es necesario instalarlas previamente:
library(lmtest)
library(faraway)

# Selección de Modelo
BIC(modelo1,modelo2,modelo3,modelo4,modelo5,modelo6,modelo7,modelo8) # Seleccionamos el modelo con el BIC mas bajo
modelo 1= 117,9675
modelo 2= 123,1320
modelo 3= 143,1657
modelo 4= 126,4212
modelo 5= 141,3650
modelo 6= 113,6597
modelo 7= 113,7684
modelo 8= 11,7721
AIC(modelo1,modelo2,modelo3,modelo4,modelo5,modelo6,modelo7,modelo8) # Seleccionamos el modelo con el AIC mas bajo
modelo 1= 110,3302
modelo 2= 115,4947
modelo 3= 135,5284
modelo 4= 118,7839
modelo 5= 133,7277
modelo 6= 106,0225
modelo 7= 106,1311
modelo 8= 104,1348

# Segun las pruebas BIC y AIC selecionamos el modelo 8

# Análisis Gráfico del modelo seleccionado
par(mfrow=c(2,2))
plot(modelo8)
par(mfrow=c(1,1))
# Esta primera inspección gráfica del modelo nos indica un buen comportamiento de los supuestos que se comprobaran mas adelante con la validación de los mismos

# Pruebas de Especificación:
resettest(modelo1);resettest(modelo2);resettest(modelo3);resettest(modelo4);resettest(modelo5);resettest(modelo6);resettest(modelo7);resettest(modelo8) 
modelo 1: p-valor= 0,9254
modelo 2: p-valor= 0,5412
modelo 3: p-valor= 0,1585
modelo 4: p-valor= 0,3912
modelo 5: p-valor= 0,5158
modelo 6: p-valor= 0,6639
modelo 7: p-valor= 0,84
modelo 8: p-valor= 0,4844
# Los p-valor nos dicen que no rechazamos las pruebas de hipotesis nula, por lo tanto este test nos indica que los modelos están bien especificados.

# Diagnóstico de multicolinealidad:
vif(modelo8)
 Waist   Height     Fore    Thigh     Calf 
2.758095 1.213924 4.188512 3.260068 3.413778 
1/(1-summary(modelo8)$r.squared)
 32.57216
# se compara cada vif de las variables con el vif de cada modelo, si es mayor el vif de la variable significa que esa variable genera multicolinealidad
# Según la comparación que se realizó mediante la prueba vif, se determina que ninguna variable genera multicolinealidad

# Para la validación de los supuestos realizamos las siguientes pruebas

-----># Linealidad:
par(mfrow=c(2,3))
prplot(modelo8,1)
prplot(modelo8,2)
prplot(modelo8,3)
prplot(modelo8,4)
prplot(modelo8,5)
par(mfrow=c(1,1))
# Se observa linealidad en todos los casos.

------># Normalidad:
r<-residuals(modelo8)
par(mfrow=c(1,2))
qqnorm(modelo8$resid)
qqline(modelo8$resid)
hist(r,freq = F,nclass=10)
xfit<-seq(min(r),max(r))
yfit<-dnorm(xfit,mean=mean(r),sd=sd(r))
lines(xfit, yfit, col="blue", lwd=2) 
par(mfrow=c(1,1))
# Mediante la inspección gráfica se puede determinar el cumplimiento del supuesto.
shapiro.test(r) 
W= 0,96208
p-value= 0,5324
# se comprueba la existencia de normalidad en nuestro modelo mediante el test de Shapiro - Wilks, ya que el p-valor nos indica que no rechazamos la prueba de hipotesis nula que confirma la normalidad del modelo.

-------># Homocedasticidad:
rs<-rstandard(modelo8)
opar<-par(mfrow=c(1,2))
# los residuos solos (Para realizar el test de Breusch-Pagan)
plot(rs,ylab='Residuos estudentizados')
title(sub="(a)")
# los residuos versus los valores ajustados (Para realizar el test de Breusch-Pagan)
plot(fitted(modelo8),rs,xlab='Valores ajustados',ylab='Residuos estudentizados')
title(sub="(b)")
par(opar)
bptest(modelo8) 
BP= 2,5318
p-valor= 0,7717
# Con el test de Breusch-Pagan se ratifica el supuesto de homocedasticidad dentro de nuestro modelo, ya que el p-valor nos indica que no rechazamos la prueba de hipotesis nula que confirma el supuesto de homocedasticidad.

--------># Incorrelación:
opar<-par(mfrow=c(1,2))
n<-length(rs)
plot(rs[2:n],rs[1:(n-1)],xlab=expression(r[i-1]),ylab=expression(r[i]))
plot(rs,type="l",ylab="residuals")
par(opar)
# Con base al anterior análisis gráfico se puede determinar que no hay evidencia de autocorrelación
dwtest(modelo8) 
DW= 2,0182
p-valor= 0,4987
# Con el Test de Durbin Watson se confirma el supuesto de incorrelación en el modelo, ya que el p-valor nos indica que no rechazamos la prueba de hipotesis nula que confirma el supuesto de incorrelacion.





