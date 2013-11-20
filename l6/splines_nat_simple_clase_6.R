
library(splines)
## Ejemplo simple de splines
 
 # una entrada
 x.entrada <- rnorm(200)
 # salida
 y <- x.entrada^3 - 2*x.entrada^2 + rnorm(100,0,3)
 plot(x.entrada,y)

 ## Modelo lineal
 lm.1 <- lm(y ~ x.entrada)
 lm.1
 plot(x.entrada,y)
 lines(x.entrada, fitted(lm.1), col='red')

## Ahora intentamos con un spline con 1 solo nodo.
## Serán K+3 = 1+3 = 4 funciones base (sin contar constante)

## La función bs calcula nodos según cuantiles de x,
## y evalúa la base de splines en los valores de x.
## Devuelve una matriz de 200 x 4 (las nuevas 4 entradas para el modelo extendido)
library(splines)
bs.1 <- bs(x.entrada, df=4)
bs.1
# Nótese que hay un nudo en el cuantil 50% (la mediana)
dim(bs.1)


# ¿Cómo ve ven las nuevas entradas?
plot(x.entrada, bs.1[,1], ylim=c(-1,1))
points(x.entrada, bs.1[,2], col='red')
points(x.entrada, bs.1[,3], col='purple')
points(x.entrada, bs.1[,4], col='yellow')
points(x.entrada, bs.1[,5], col='green')
points(x.entrada, bs.1[,6], col='lightblue')

## estas gráficas dan los elementos de la base evaluadas en los
## datos de entrenamiento

## Ahora construimos un modelo:

lm.spline <- lm(y ~ bs.1)
lm.spline

## Cada elemento de la base tiene un coeficiente,
## y ahora hacemos la combinación lineal para ver los
## ajustados:

plot(x.entrada, y)
points(x.entrada, fitted(lm.spline), col='red')

## Los ajustados también se pueden calcular como:
coefs.1 <- coef(lm.spline)[-1]
coefs.1

pred.manual <- bs.1%*%coefs.1 + coef(lm.spline)[1]
plot(pred.manual, fitted(lm.spline))
