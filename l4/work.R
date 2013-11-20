h <- function(x) {
    exp(x)/(1 + exp(x))
}
curve(h, from = -6, to = 6)

library(shiny)
setwd()
runApp('/Users/Alfonso/r-workspace/machine-learning/l3/regresion_logistica')


library(MASS)
library(ggplot2)
data(Pima.tr)
head(Pima.tr)

plot(Pima.tr, aes(x=glu, y=as.numeric(type)-1, colour=type, group=1)) +
geom_jitter(position=position_jitter(height=0.05)) +
    geom_smooth(se = FALSE, span=0.8) +
    ylab('Probabilidad de tener diabetes')
# ¿? MASS

data(Pima.te)
?Pima.tr
Pima.tr

library(kknn)
?kknn
knn.mean <- function (numK) {
    knn <- kknn(type ~ glu, train=Pima.tr, 
                test=Pima.te, k=numK, kernel="rectangular")
    mean(knn$fitted.values != Pima.te$type)
}
plot(sapply(c(1,5,10,20,50,100), knn.mean))
plot(sapply(1:100, knn.mean))

glm(type ~ glu, data = Pima.tr, family = "binomial")

Pima.tr$glu.st <- (Pima.tr$glu - mean(Pima.tr$glu))/sd(Pima.tr$glu)
mod.1 <- glm(type ~ glu.st, data = Pima.tr, family = "binomial")
coef(mod.1)

grid.glu <- seq(-2.2, 2.4, 0.1)
preds.grid <- predict(mod.1, newdata = data.frame(glu.st = grid.glu),
                      type = "response")
dat.graf <- data.frame(glu.st = grid.glu, prob = preds.grid)
ggplot(dat.graf, aes(x = glu.st, y = prob)) + geom_line()

## Datos Originales
mod.original <- glm(type ~ glu, data=Pima.tr, family = "binomial")
coef(mod.original)
grid.glu.original <- seq(-6, 6, 1)
preds.grid.original <- predict(mod.original, newdata=data.frame( glu=grid.glu.original ), type="response")
dat.graf.original <- data.frame(glu=grid.glu.original, prob=preds.grid.original)
ggplot(dat.graf.original, aes(x=glu, y=prob)) + geom_line()


