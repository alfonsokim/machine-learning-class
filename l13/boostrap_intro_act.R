library(plyr)
library(ggplot2)
library(reshape2)
# ============
# = Población y estimador =
# ============

#Consideremos la siguiente población fija (que usualmente no conocemos):
set.seed(280572)
poblacion <- rexp(100000)
#Y queremos estimar (que usualmente no conocemos):
mean(poblacion)  ## este número es "desconocido"
qplot(poblacion)



# Para entender qué tan bien podemos estimar la media poblacional
# con una muestra de 200 usando la media muestral, podemos
# hacer muchas repeticiones del proceso de muestreo,
# y juzgamos la variabilidad

repeticiones <- rdply(10000, mean(sample(poblacion, 200, replace = TRUE)))
head(repeticiones)

#Esto muestra la distribución del estimador de la media con
# tamaño de muestra 100:
ggplot(repeticiones, aes(x = V1)) + geom_histogram(binwidth=0.02) +
    geom_vline(xintercept = mean(poblacion), colour="red")

## Y podemos aproximar con las 10 mil simulaciones la varianza de este estimador:
var(repeticiones[,2])
#Error estándar
sqrt(var(repeticiones[,2]))



# =============
# = Bootstrap =
# =============
#Ahora hacemos estimación con una muestra de tamaño 200
set.seed(2805721)
muestra <- sample(poblacion, 200, replace = TRUE)
#Estimación con muestra:
mean(muestra)

## Evaluar la precisión de nuestro estimador? Usaremos bootstrap

## La idea es hacer lo mismo que en el paso anterior
## pero usando **como población a la muestra**

# Asi que repetimos el proceso de muestreo, pero dentro de la muestra (remuestra):
repeticiones.2 <- rdply(10000, mean(sample(muestra, 200, replace = TRUE)))
ggplot(repeticiones.2, aes(x = V1)) + geom_histogram(binwidth=0.02)

## esta gráfica nos da una idea de la variabilidad de la media muestral
## cuando el tamaño de muestra es de 200.

## Nuestra estimación del error estándar   
bstrap.ee <- sqrt(var(repeticiones.2[,2]))
bstrap.ee
##NOTA: el estiamador usual es:
sd(muestra)/sqrt(200)


## Intervalo del 95% aproximado con el error estándar (hay otras maneras):
(c(mean(muestra) - 2*bstrap.ee, mean(muestra) + 2*bstrap.ee))



###===============================
#### Acerca de funciones de distribución
##=====================================
# En el ejemplo anterior, la función de distribución de donde
# se obtienen las muestras es:
fd.pob <- ecdf(poblacion)
curve(fd.pob)

## Cuando remuestreamos de la muestra, la función de distribución
# de donde se obtienen muestras es:
fd.muestral <- ecdf(muestra)
curve(fd.muestral, add=T, col='red')
 ## para una muestra de 200 son bastante similares.


# =======================================================
# = Otro ejemplo: estimar el coeficiente de correlación =
# =======================================================
data(tips)
propinas <- tips$tip
cuenta <- tips$total_bill
theta <- cor(propinas, cuenta) # correlación muestral
qplot(cuenta, propinas)
#Estimador de la correlación
round(theta,2)

B <- 3000

corr.rep <- function(){
    muestra.ind <- sample(1:length(propinas), length(propinas), replace=TRUE)
    cor(propinas[muestra.ind], cuenta[muestra.ind])
}
correl.boot <- rdply(B, corr.rep())

ggplot(correl.boot, aes(x=V1)) + geom_histogram()

#Error estándar:
sqrt(var(correl.boot$V1))

##Podemos hacer un intervalo del 90% usando percentiles (hay otras maneras)
round(quantile(correl.boot$V1, probs=c(0.025, 0.975)), 2)



