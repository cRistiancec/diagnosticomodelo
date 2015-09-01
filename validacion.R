###### VALIDACION DEL MODELO

# Cargamos los datos de Bosque:
DBH <- c(10.2,13.72,15.43,14.37,15,15.02,15.12,15.24,15.24,15.28, 13.78,15.67,15.67,15.98,16.5,16.87,17.26,17.28,17.87,19.13)
D16 <-c(9.3,12.1,13.3,13.4,14.2,12.8,14,13.5,14,13.8,13.6,14, 13.7,13.9,14.9,14.9,14.3,14.3,16.9,17.3)
HT <-c(89,90.07,95.08,98.03,99,91.05,105.6,100.8,94,93.09,89, 102,99,89.02,95.09,95.02,91.02,98.06,96.01,101)
VOL <-c(25.93,45.87,56.2,58.6,63.36,46.35,68.99,62.91,58.13, 59.79,56.2,66.16,62.18,57.01,65.62,65.03,66.74,73.38,82.87,95.71)
bosque<-data.frame(VOL=VOL,DBH=DBH,D16=D16,HT=HT)

# Queremos particionar el banco de datos en dos trozos
# de tamaños similares. Seleccionamos pues, dos muestras
# al azar:
n<-length(VOL)
n1<-round(n/2)
n2<-n-n1
muestra1<-sample(1:20,n1)
muestra2<-(1:n)[-muestra1]
s1<-bosque[muestra1,]
s2<-bosque[muestra2,]
# Y ajustamos los modelos correspondientes:
formula<-VOL~D16+HT
fit1<-lm(formula,data=s1)
fit2<-lm(formula,data=s2)
# para compararlos con el ajuste global
fit<-lm(formula, data=bosque)
# y seleccionamos las sumas de cuadrados del error:
p<-length(fit1$coefficients) # número de coeficientes estim
sse1<-anova(fit1)[[2]][p]
sse2<-anova(fit2)[[2]][p]
sset<-anova(fit)[[2]][p]
# para calcular el estadístico F:
f<-((sset-sse1-sse2)/p)/((sse1+sse2)/(n-2*p))
# y el p-valor correspondiente
1-pf(f,p,n-2*p)
# Y pintamos los ajustes
d16<-seq(min(D16),max(D16),length=20)
ht<-seq(min(HT),max(HT),length=20)
newdata<-data.frame(D16=d16,HT=ht)
par(mfrow=c(1,2))
plot(d16,predict(fit,newdata),type="l",lwd=2,xlab="D16",ylab="VOL")
# superponemos el ajuste f1
lines(d16,predict(fit1,newdata),lty=2,lwd=2,col="red")
# y el ajuste f2
lines(d16,predict(fit2,newdata),lty=3,lwd=2,col="blue")
plot(ht,predict(fit,newdata),type="l",xlab="HT",ylab="VOL")
lines(ht,predict(fit1,newdata),lty=2,lwd=2,col="red")
lines(ht,predict(fit2,newdata),lty=3,lwd=2,col="blue")
legend(90,95,c("Ajuste Global","Ajuste M1","Ajuste M2"),lty=1:3,
lwd=2,col=c("black","red","blue"))

################

# Partimos del ajuste inicial para acceder a la matriz
# de diseño X:
fit<-lm(VOL~D16+HT,data=bosque,x=T)
x<-fit$x
# y calculamos las predicciones yi(i) sin la observación i:
n<-length(VOL) # número de datos
y.i<-vector(length=n)
for(i in 1:n)
y.i[i]<-x[i,]%*%coef(lm(VOL~D16+HT,data=bosque[-i,]))
# para calcular el error cuadrático de validación
ecv<-sum((VOL-y.i)^2) #=251
# y el coeficiente de robustez
b2<-sum(residuals(fit)^2)/ecv;b2 #= 0.707
