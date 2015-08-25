# BORRAR MEMORIA:
rm(list=ls())

############################
#  CONSTRUIMOS LOS DATOS   #
############################

# Pizza Shack
sales<-c(43.6,38,30.1,35.3,46.4,34.2,30.2,40.7,38.5,22.6,37.6,35.2)
ads<-c(12,11,9,7,12,8,6,13,8,6,8,10)
cost<-c(13.9,12,9.3,9.7,12.3,11.4,9.3,14.3,10.2,8.4,11.2,11.1)
pizza<-data.frame(sales,ads,cost)

# Bosque

DBH <- c(10.2,13.72,15.43,14.37,15,15.02,15.12,15.24,15.24,15.28, 13.78,15.67,15.67,15.98,16.5,16.87,17.26,17.28,17.87,19.13)
D16 <-c(9.3,12.1,13.3,13.4,14.2,12.8,14,13.5,14,13.8,13.6,14, 13.7,13.9,14.9,14.9,14.3,14.3,16.9,17.3)
HT <-c(89,90.07,95.08,98.03,99,91.05,105.6,100.8,94,93.09,89, 102,99,89.02,95.09,95.02,91.02,98.06,96.01,101)
VOL <-c(25.93,45.87,56.2,58.6,63.36,46.35,68.99,62.91,58.13, 59.79,56.2,66.16,62.18,57.01,65.62,65.03,66.74,73.38,82.87,95.71)
bosque<-data.frame(VOL=VOL,DBH=DBH,D16=D16,HT=HT)

############################
#  MODELAMOS PIZZA         #
############################

# ajustamos el modelo de regresion multiple:
fit<-lm(sales~ads+cost,data=pizza,x=T)
# que da como resultado
summary(fit)

############################
#  MODELAMOS BOSQUE        #
############################

fit.3<-lm(VOL~DBH+D16+HT)
fit.2<-update(fit.3,.~.-DBH)

# COMPARACIÓN DE MODELOS:
# Obtenemos el AIC para sendos modelos:
AIC(fit.3) #=107.4909
AIC(fit.2) #=108.4066
# y el BIC con:
AIC(fit.3,k=log(nrow(bosque)))
AIC(fit.2,k=log(nrow(bosque)))
# o bien con:
library(stats4)
BIC(fit.3) #=112.4696
BIC(fit.2) #=112.3895

################################
#   ERRORES DE ESPECIFICACIÓN  #
################################

#         RAMSEY-RESET         

resettest(fit, power=2:3, type="fitted")

#  J DE DAVIDSON-MACKINNON

fm1<-lm(sales~ads+cost,data=pizza,x=T)
fm2<-lm(sales~ads,data=pizza,x=T)

jtest(fm1,fm2)

# ESTUDIANTES CON BOSQUE

################################
#   MULTICOLINEALIDAD          #
################################

# Procedemos a una seleccion automatica de variables
fit.aic<-step(fit); fit.aic
# se excluye 'ads': sales ~ cost
# Si hacemos la seleccion con el test F:
drop1(fit,test="F")
# tambien eliminamos 'ads' por tener mayor p-valor
# ads p-valor=0.5907
# cost p-valor=0.1797
# con lo que nos quedamos con el ajuste
fit.cost<-lm(sales ~ cost); summary(fit.cost)

fit.cost<-update(fit.cost,.~.-1)
summary(fit.cost)
# y probamos si podemos incluir ahora 'ads'
anova(fit.cost,update(fit.cost,.~.+ads),test="F")

# BOSQUE

# Creamos nuevas variables ortogonales por componentes principales:
pr<-princomp(~DBH+HT+D16,data=bosque);pr
pr$loadings
# Las mas relevantes son:
pr1<--0.1758344*DBH-0.9695333*HT-0.1705505*D16
pr2<- 0.7451155*DBH-0.2443003*HT+0.6205806*D16
# y ajustamos un nuevo modelo con ellas
# (componentes principales incompletas):
fit.pr<-lm(VOL~pr1+pr2)
summary(fit.pr)

library(MASS)
# Dibujamos el grafico ridge para decidir el valor de k (lambda):
plot(lm.ridge(VOL~.,data=bosque,lambda=seq(0,3,0.01)))
title(main="Ridge Plot")
# y empleamos estimadores de dicho valor implementados en R:
select(lm.ridge(VOL~.,data=bosque))
#modified HKB estimator is 0.09691231
#modified L-W estimator is 0.05336196
#smallest value of GCV at 0.65
# Ajustamos el modelo ridge con la estimacion HKB:
fit.ridge.HKB<-lm.ridge(VOL~.,data=bosque,lambda=0.09691231)
fit.ridge.HKB
# DBH D16 HT
#-108.406928 1.721365 5.541354 0.695394
# Y decidimos otro valor lambda a partir del grafico ridge:
fit.ridge<-lm.ridge(VOL~.,data=bosque,lambda=1.5)
fit.ridge
DBH D16 HT
-104.7096682 2.3458025 4.5627709 0.6976587

# DIAGNÓSTICO: ERRORES (RESIDUALES)

# Nos aseguramos de tener cargados los datos del Apendice.
fit<-lm(VOL~.,data=bosque,x=T)
# La matriz gorro completa se obtiene con:
x<-fit$x
hat<-x%*%solve(t(x)%*%x)%*%t(x); hat
i<-diag(rep(1,length(VOL))) # matriz identidad
# con la que se calculan los residuos comunes
(i-hat)%*%VOL
# obtenidos tambien directamente con:
residuals(fit)
e<-fit$res; e
# los residuos estandarizados, se consiguen de los
# residuos comunes y la estima del error:
s<-summary(fit)$sigma
d<-e/s
# los residuos (inter.) estudentizados, con:
rstandard(fit)
# obtenidos de los residuos comunes, la matriz gorro
# y la estima del error:
h<-hatvalues(fit) # es la diagonal de la matriz gorro
hat(fit$x)
r<-e/(s*sqrt(1-h));r
# los residuos ext. estudentizados son:
rt<-rstudent(fit);rt
# los residuos PRESS:
ei<-e/(1-h);ei
# Los pintamos todos:
par(mfrow=c(2,1))
plot(e,ylim=c(-7,7))
points(d,pch=2)
points(ei,pch=3)
legend(5,6.5,c("R.comun","R.estandar","R.press"),pch=c(1,2,3),cex=0.8)
plot(r,ylim=c(-3,3))
points(rt,pch=2)
legend(5,3,c("R.int.stud","R.ext.stud"),pch=c(1,2),cex=0.8)

# LINEALIDAD

# Partimos del ajuste del modelo
fit<-lm(VOL~.,data=bosque)
library(faraway)
opar<-par(mfrow=c(2,2))
# Residuos parciales para DBH
prplot(fit,1)
# Residuos parciales para D16
prplot(fit,2)
# Residuos parciales para HT
prplot(fit,3)
par(opar)

# HOMOCEDASTICIDAD

# Cargamos los datos
data(InsectSprays)
# Ajustamos un modelo de ANOVA para predecir el numero de insectos
# que quedan tras la vaporizacion con el insecticida:
fit<-lm(count~spray,data=InsectSprays)
# Investigamos primeramente con un analisis grafico de residuos
# representando los residuos estudentizados:
r<-rstandard(fit)
opar<-par(mfrow=c(2,2))
# los residuos solos
plot(r,ylab='Residuos estudentizados')
title(sub="(a)")
# los residuos versus los valores ajustados
plot(fitted(fit),r,xlab='Valores ajustados',
ylab='Residuos estudentizados')
title(sub="(b)")
# y los residuos versus la variable predictora 'spray'
plot(r~spray,data=InsectSprays,xlab='Insecticida',
ylab='Residuos estudentizados')
title(sub="(c)")
par(opar)

library(lmtest)
# El test de Breusch-Pagan se obtiene con
bptest(fit)
# El test de Bartlett se consigue con
bartlett.test(r ~ spray, data = InsectSprays)

# NORMALIDAD

# Cargamos los datos y ajustamos el modelo
fit<-lm(VOL~.,data=bosque)
opar<-par(mfrow=c(1,2))
# Los graficos de normalidad para los residuos son
qqnorm(fit$resid)
qqline(fit$resid)
# Podemos hacer tambien un histograma y superponer una densidad normal
# para los residuos estandarizados o estudentizados
r<-rstandard(fit)
hist(r,prob=T,xlim=c(-3,3),xlab="Res.estudentizados",main="Histograma")
lines(xseq<-seq(-3,3,length=100), dnorm(xseq,mean(r),sd(r)))
par(opar)
# Y podemos extraer informacion sobre el reparto de los residuos
# estudentizados por debajo de -2 y encima de 2
summary(rstandard(fit))
quantile(rstandard(fit),prob=c(0.025,0.975))
# Los tests formales de Shapiro-Wilks y Kolmogorov-Smirnov dan:
shapiro.test(r) #p-valor=0.3922
ks.test(r,pnorm) #p-valor=0.8606

# INCORRELACIÓN:

# Partimos del ajuste
fit<-lm(VOL~.,data=bosque)
# y hacemos los test sobre los residuos estudentizados
r<-rstandard(fit)
opar<-par(mfrow=c(1,2))
# El grafico siguiente nos da el grafico lag
n<-length(r)
plot(r[2:n],r[1:(n-1)],xlab=expression(r[i-1]),ylab=expression(r[i]))
# y este permite identificar autocorrelacion
plot(r,type="l",ylab="residuals")
par(opar)
# El test de Durbin-Watson:
library(lmtest)
dwtest(fit,alternative="two.sided")

###########################
# CORRECCIÓN DE PROBLEMAS #
###########################

# MINIMOS CUADRADOS GENERALIZADOS

# Cargamos los datos
data(InsectSprays)
attach(InsectSprays)

# Calculamos los residuos del modelo inicial sin correccion:
res<-residuals(lm(count~spray,data=InsectSprays))

# y verifiquemos con Bartlett los problemas de heterocedasticidad:
bartlett.test(rstandard(lm(count~spray)) ~ spray, data = InsectSprays)
# p-valor=9.085e-05

# la estimacion de la varianza por grupos:
sigma2.s<-unlist(lapply(split(res^2,spray),mean))

# y definimos los pesos:
pesos<-1/rep(sigma2.s,tabulate(spray))

# Ya ajustamos el modelo por mcp:
fit1<-lm(count~spray,data=InsectSprays)
fit2<-lm(count~spray,weights=pesos)

# y dibujamos los residuos para apreciar si hemos corregido el problema de heterocedasticidad:
r<-rstandard(fit2)
opar<-par(mfrow=c(2,2))

# los residuos solos
plot(r,ylab='Res.estudentizados')

# los residuos versus los valores ajustados
plot(fitted(fit2),r,xlab='Valores ajustados', ylab='Res.estudentizados')

# y los residuos versus la variable predictora 'spray'
plot(r~spray,xlab='Insecticida',ylab='Res.estudentizados')

par(opar)
detach(InsectSprays)

bartlett.test(r ~ spray, data = InsectSprays)

# TRANSFORMACIÓN DE LA VARIABLE RESPUESTA:

# Cargamos los datos en R:
data(trees)

# y ajustamos el modelo de regresion multiple
fit<-lm(Volume ~ Height + Girth, data = trees)
summary(fit)
# R2=0.948 y p-valor< 2.2e-16

# Hacemos el diagnostico grafico para detectar problemas
opar<-par(mfrow=c(2,2))
plot(fit)
par(opar)

# y un test de normalidad
shapiro.test(rstandard(fit))
# p-valor=0.6389
# Es normal, sin embargo el diagnostico gráfico muestra una cierta tendencia
# curvilínea que podemos tratar de corregir mediante trasformación de box-cox

# Buscamos el valor de lambda para la transformacion:
library(MASS)
par(mfrow=c(1,1))
boxcox(fit)

# que pinta el valor optimo para lambda y un intervalo de confianza al 95%:
bc<-boxcox(fit,plotit=F)

# el valor de lambda que maximiza SSR es
lambda<-bc$x[which.max(bc$y)]; lambda #=0.3

# Luego reajustamos el modelo con la variable Volume transformada:
library(labstatR) # librerIa que calcula la media geometrica
attach(trees)

# la variable transformada por Box-Cox es:
z<-(Volume^lambda-1)/(lambda*meang(Volume)^(lambda-1))

# y el nuevo ajuste con dicha variable
fit.bc<-lm(z~Height+Girth,data=trees)

# cuyo diagnostico grafico resultante es:
opar<-par(mfrow=c(2,2))
plot(fit.bc)
par(opar)

# Hay que tener cuidado con la la trasformación de Box-Cox pues si la
# variable respuesta tiene ceros no se puede realizar
# debido a que se emplea la media geométrica.
