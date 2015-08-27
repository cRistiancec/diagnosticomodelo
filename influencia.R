#######################################
#      ANÁLISIS DE LA INFLUENCIA      #
#######################################

# En ocasiones existen observaciones que afectan el modelo en su ajuste
# de manera desproporcionada. Es importante localizar e identificar dichas
# observaciones y evaluar su impacto en el modelo.
# Si estos datos son 'malos' deberán excluirse del modelo (errores de medición, etc)
# Si son 'buenos' contendrán información sobre ciertas características del modelo.

## INFLUENCIA SOBRE LOS COEFICIENTES:
	# Distancia de Cook: Mide la distancia entre la estimación MC y la obtenida
	# eliminando la observación influyente.
	
	# DFBETAS: Cuántas 's' cambia Beta si se excluye la observación.

## INFLUENCIA SOBRE LAS PREDICCIONES:
	# DFFITS: Cuántas 's' cambian las predicciones si se excluye la observación.
	# PRESS: Calcula el error de predicción del modelo.

## INFLUENCIA SOBRE LA PRECISION:
	# COVRATIO: Cuantifica el efecto de la observación sobre la precisión
	# de las estimaciones.


#######################################
#         EJEMPLO DATA BOSQUE         #
#######################################

DBH <- c(10.2,13.72,15.43,14.37,15,15.02,15.12,15.24,15.24,15.28, 13.78,15.67,15.67,15.98,16.5,16.87,17.26,17.28,17.87,19.13)
D16 <-c(9.3,12.1,13.3,13.4,14.2,12.8,14,13.5,14,13.8,13.6,14, 13.7,13.9,14.9,14.9,14.3,14.3,16.9,17.3)
HT <-c(89,90.07,95.08,98.03,99,91.05,105.6,100.8,94,93.09,89, 102,99,89.02,95.09,95.02,91.02,98.06,96.01,101)
VOL <-c(25.93,45.87,56.2,58.6,63.36,46.35,68.99,62.91,58.13, 59.79,56.2,66.16,62.18,57.01,65.62,65.03,66.74,73.38,82.87,95.71)
bosque<-data.frame(VOL=VOL,DBH=DBH,D16=D16,HT=HT)

fit<-lm(VOL~D16+HT,data=bosque)
# calculamos las medidas de influencia
im<-influence.measures(fit); im
# que proporciona los DFBETAS,DFFITS, COVRATIO y Distancia de Cook,
# indicando si cada punto es influyente (columna 'inf').

# Además, identifica los puntos influyentes con un asterisco:
im$is.inf
summary(im)
# Los valores de los estadísticos los podemos representar para apreciar
# mejor los puntos influyentes:
n<-length(VOL)
opar<-par(mfrow=c(2,3))
estadistico<-dimnames(im$infmat)[[2]]
nstat<-dim(im$infmat)[2]

# Los DFBETA:
for(i in 1:3){
dotchart(im$infmat[,i],main=estadistico[i],xlim=c(-1,1))
# con una línea para el valor crítico 2/sqrt(n):
abline(v=2/sqrt(n),lty=2)
abline(v=-2/sqrt(n),lty=2)}

# El DFFIT:
i<-4
dotchart(im$infmat[,i],main=estadistico[i])
# con valores influyentes en rojo:
puntos.i<-which(im$is.inf[,i]==T)
vpuntos.i<-im$infmat[puntos.i,i]
points(vpuntos.i,puntos.i,pch=21,bg="red",col="red")
# y valores críticos:
p<-length(fit$coef) # número de coeficientes del modelo
abline(v=2*sqrt(p/n),lty=2)
abline(v=-2*sqrt(p/n),lty=2)

# El COVRATIO:
i<-5
dotchart(im$infmat[,i],main=estadistico[i])
# con valores influyentes en rojo:
puntos.i<-which(im$is.inf[,i]==T)
vpuntos.i<-im$infmat[puntos.i,i]
points(vpuntos.i,puntos.i,pch=21,bg="red",col="red")
# y valores críticos:
abline(v=1+3*p/n,lty=2)
abline(v=1-3*p/n,lty=2)
# La distancia de Cook
i<-6
dotchart(im$infmat[,i],main=estadistico[i])
# con valores críticos:
abline(v=qf(0.5,p,n-p),lty=2)
par(opar)

