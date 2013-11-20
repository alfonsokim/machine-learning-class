library(ggplot2)
library(plyr)
## Ejemplo de regresión polinomial, una sola variable:
set.seed(42)
x <- runif(100, -1, 1)
y <- 0 + rnorm(100, 0, 1)

datos.orig <- data.frame(y=y, x=x)
head(datos.orig)
?poly
# intentaremos grado 5. Hacemos la expansión de entradas:
datos.exp <- data.frame(y=y, poly(x, 6, raw=TRUE))
head(datos.exp)

mod.1 <- lm(y ~ ., data=datos.exp)
summary(mod.1)

dat.graf <- datos.exp
dat.graf$pred <- predict(mod.1)

ggplot(dat.graf, aes(x=X1, y=y)) + geom_point() +
  geom_line(aes(x=x, y=pred), col='red')

#############################
# Notamos la variabilidad en los extremos:
sims <- rdply(50, function(i){
  x <- runif(100, -1, 1)
  y <- 0 + rnorm(100, 0, 1)
  datos.exp <- data.frame(y=y, poly(x, 10, raw = TRUE))
  mod.1 <- lm(y ~ ., data=datos.exp)
  dat.graf <- datos.exp
  dat.graf$pred <- predict(mod.1)
  dat.graf
})
ggplot(sims, aes(x=X1, y=pred, group=.n)) + geom_line(colour='red', alpha=0.6)+
  ylim(c(-3,3))
###########################



