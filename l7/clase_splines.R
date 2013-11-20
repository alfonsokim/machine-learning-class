

library(knitr)
library(ggplot2)
library(plyr)
library(reshape2)
#options(replace.assign=TRUE,width=55)
#knit_hooks$set(par=function(before, options, envir){if (before) par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)})



library(ggplot2)
library(plyr)
## Ejemplo de regresión polinomial, una sola variable:
set.seed(42)
x <- runif(100, -1, 1)
y <- 0 + rnorm(100, 0, 1) # suponemos un modelo nulo.

datos.orig <- data.frame(y=y, x=x)
head(datos.orig)
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



library(splines)
bs.1 <- bs(x.entrada, df=4)
head(bs.1)
str(bs.1)
# Nótese que hay un nudo en el cuantil 50% (la mediana)
dim(bs.1)



plot(x.entrada, bs.1[,1], ylim=c(-1,1))
points(x.entrada, bs.1[,2], col='red')
points(x.entrada, bs.1[,3], col='purple')
points(x.entrada, bs.1[,4], col='yellow')




lm.spline <- lm(y ~ bs.1)
lm.spline



plot(x.entrada, y)
points(x.entrada, fitted(lm.spline), col='red')



coefs.1 <- coef(lm.spline)[-1]
coefs.1
pred.manual <- bs.1%*%coefs.1 + coef(lm.spline)[1]
plot(pred.manual, fitted(lm.spline))



lm.spline.2 <- lm(y ~ bs(x.entrada, df = 4))
lm.spline.2



library(ggplot2)
library(plyr)
library(splines)
## Ejemplo de regresión polinomial, una sola variable:
set.seed(42)
x <- runif(100, -1, 1)
y <- 0 + rnorm(100, 0, 1)

datos.orig <- data.frame(y=y, x=x)
head(datos.orig)
# intentaremos 5 elementos en la base. Hacemos la expansión de entradas:
datos.exp <- data.frame(y=y, bs(x, df=10))
head(datos.exp)

mod.1 <- lm(y ~ ., data=datos.exp)
summary(mod.1)

dat.graf <- datos.exp
dat.graf$x <- x
dat.graf$pred <- predict(mod.1)

ggplot(dat.graf, aes(x=x, y=y)) + geom_point() +
  geom_line(aes(x=x, y=pred), col='red')



#############################
# Notamos la variabilidad en los extremos:
sims <- rdply(50, function(i){
  x <- runif(200, -1, 1)
  y <- 0 + rnorm(200, 0, 1)
  datos.exp <- data.frame(y=y, bs(x, df=10))
  mod.1 <- lm(y ~ ., data=datos.exp)
  dat.graf <- datos.exp
  dat.graf$x <- x
  dat.graf$pred <- predict(mod.1)
  dat.graf
})
ggplot(sims, aes(x=x, y=pred, group=.n)) + geom_line(colour='red', alpha=0.6)+
  ylim(c(-3,3))
###########################



library(ggplot2)
library(plyr)
library(splines)
## Ejemplo de regresión polinomial, una sola variable:
set.seed(42)
x <- runif(100, -1, 1)
y <- 0 + rnorm(100, 0, 1)

datos.orig <- data.frame(y=y, x=x)
head(datos.orig)
# intentaremos grado 5. Hacemos la expansión de entradas:
datos.exp <- data.frame(y=y, ns(x, df=10, Boundary.knots=c(-0.8,0.8)))
head(datos.exp)

mod.1 <- lm(y ~ ., data=datos.exp)
summary(mod.1)

dat.graf <- datos.exp
dat.graf$x <- x
dat.graf$pred <- predict(mod.1)

ggplot(dat.graf, aes(x=x, y=y)) + geom_point() +
  geom_line(aes(x=x, y=pred), col='red')



#############################
# Notamos la variabilidad en los extremos:
sims <- rdply(50, function(i){
  x <- runif(200, -1, 1)
  y <- 0 + rnorm(200, 0, 1)
  datos.exp <- data.frame(y=y, ns(x, df=10, Boundary.knots=c(-0.8,0.8)))
  mod.1 <- lm(y ~ ., data=datos.exp)
  dat.graf <- datos.exp
  dat.graf$x <- x
  dat.graf$pred <- predict(mod.1)
  dat.graf
})
ggplot(sims, aes(x=x, y=pred, group=.n)) + geom_line(colour='red', alpha=0.6)+
  ylim(c(-3,3))
###########################



library(ElemStatLearn)
library(plyr)
library(ggplot2)
library(splines)
library(arm)
data(SAheart)

head(SAheart)
SAheart$id <- 1:nrow(SAheart)



dat.1 <- ldply(c('sbp', 'tobacco', 'ldl','typea','obesity','age'), function(nom){
  cuant.1 <- quantile(SAheart[,nom], probs=c(0.05,0.95))
  sp.1 <- ns(SAheart[,nom], df=4, Boundary.knots=cuant.1)  
  dat.temp <- data.frame(sp.1)
  names(dat.temp) <- paste0(nom,1:4)
  dat.temp$var <- nom
  dat.temp$id <- SAheart$id
  melt(dat.temp, id.var=c('id','var'))
})

dat.2 <- dcast(dat.1, id~variable)
head(dat.2)
#agregamos también historia familiar
dat.2$famhist <- SAheart$famhist=='Present'

X <- as.matrix(dat.2[,-1])



library(glmnet)
mod.1 <- cv.glmnet(x=X, y=SAheart$chd, alpha=0, family='binomial')
plot(mod.1)

mod.2 <- cv.glmnet(x=X, y=SAheart$chd, alpha=0, family='binomial',
  type.measure='class')
plot(mod.2)



## Coeficientes óptimos
coefs.1 <- coef(mod.1)
coefs.1



rownames(coefs.1)
#Tomamos los de edad
coefs.edad <- coefs.1[22:25]
## multiplicamos por la base de splines:
efecto.edad <- X[,21:24]%*%coefs.edad

qplot(SAheart$age, efecto.edad) + geom_line()+ geom_point()



coefs.fumar <- coefs.1[6:9]
efecto.fumar <- X[,5:8]%*%coefs.fumar
qplot(SAheart$tobacco, efecto.fumar) + geom_line()+ geom_point()



## ldl
coefs.ldl <- coefs.1[10:13]
efecto.ldl <- X[,9:12]%*%coefs.ldl
qplot(SAheart$ldl, efecto.ldl) + geom_line()+ geom_point()



## obesity
coefs.ob <- coefs.1[18:21]
efecto.ob <- X[,17:20]%*%coefs.ob
qplot(SAheart$obesity, efecto.ob) + geom_line()+ geom_point()


