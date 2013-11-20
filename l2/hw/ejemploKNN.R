# install.packages("ISLR", dependencies=TRUE)
library(ggplot2)
library(ISLR)
datos <- Auto[, c("name", "weight", "year", "mpg")]
datos$peso_kg <- datos$weight * 0.45359237
datos$rendimiento_kpl <- datos$mpg * (1.609344/3.78541178)
?runif
datos$tipo <- runif(nrow(datos), 0, 1) # Random para la seleccion aleatorio
datos.entrena <- subset(datos, tipo > 0.25)
datos.prueba <- subset(datos, tipo <= 0.25)

summary(datos)

ggplot(datos.entrena, aes(x = peso_kg, y = rendimiento_kpl)) +
  geom_point()

# install.packages("kknn", dependencies=TRUE)
# install.packages("igraph", dependencies=TRUE)
library(kknn)
?kknn

summary(datos.entrena)
seq(700,2200, by=10)
mod.15vmc <- kknn(rendimiento_kpl ~ peso_kg, train=datos.entrena,
                 test=data.frame(peso_kg=seq(700,2200, by = 10)), k=15 )

summary(mod.15vmc)
?predict
predict(mod.15vmc)
dat.pred <- data.frame(peso_kg=seq(700,2200, by = 10),
                       rendimiento_kpl=predict(mod.15vmc))
summary(dat.pred)
summary(datos.entrena)
?aes
ggplot(datos.entrena, aes(x=peso_kg, y=rendimiento_kpl)) +
  geom_point(alpha=0.6) +
  geom_line(data=dat.pred, col='red', size=1.2)

mod.5vmc <- kknn(rendimiento_kpl ~ peso_kg, train=datos.entrena,
                 test=data.frame(peso_kg=seq(700,2200, by=10)), k=200 )
dat.pred <- data.frame(peso_kg=seq(700,2200, by = 10),
                       rendimiento_kpl=predict(mod.5vmc))
ggplot(datos.entrena, aes(x=peso_kg, y=rendimiento_kpl)) +
  geom_point(alpha=0.6) +
  geom_line(data=dat.pred, col='red', size=1.2)


fun.1 <- function(x) {
  exp(-8 * sum(x^2))
}
x.1 <- runif(1000, -1, 1)
x.2 <- runif(1000, -1, 1)
dat <- data.frame(x.1, x.2)
dat$y <- apply(dat, 1, fun.1)
ggplot(dat, aes(x = x.1, y = x.2, colour = y)) + geom_point()


dist.origen <- apply(dat[, 1:2], 1, function(x) {
  sqrt(sum(x^2))
})
mas.cercano.indice <- which.min(dist.origen)
mas.cercano <- dat[mas.cercano.indice, ]
mas.cercano


fun.1 <- function(x) {
  exp(-8 * sum(x^2))
}
sims.1 <- lapply(1:8, function(i) {
  runif(1e10, -1, 1)
})
dat <- data.frame(Reduce(cbind, sims.1))
dat$y <- apply(dat, 1, fun.1)


dist.origen <- apply(dat[, 1:8], 1, function(x) {
  sqrt(sum(x^2))
})
mas.cercano.indice <- which.min(dist.origen)
mas.cercano <- dat[mas.cercano.indice, ]
mas.cercano




mas.cercano$y


fun.2 <- function(x) {
  0.5 * (1 + x[1])^3
}

set.seed(111)
sims.1 <- lapply(1:40, function(i) {
  runif(1000, -1, 1)
})

dat <- data.frame(Reduce(cbind, sims.1))
dat$y <- apply(dat, 1, fun.2)
dist.origen <- apply(dat[, 1:40], 1, function(x) {
  sqrt(sum(x^2))
})

mas.cercano.indice <- which.min(dist.origen)
mas.cercano <- dat[mas.cercano.indice, ]
mas.cercano
mas.cercano$y

mod.1 <- lm(y ~ ., data = dat)
sum(coef(mod.1) * c(1, rep(0, 40)))
