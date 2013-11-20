
install.packages('knitr', dependencies = TRUE)

library(knitr)
options(replace.assign=TRUE,width=100)
knit_hooks$set(par=function(before, options, envir){if (before) par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)})


install.packages("ElemStatLearn", dependencies=TRUE)
install.packages("ggplot2", dependencies=TRUE)
install.packages("shiny", dependencies=TRUE)

library(ElemStatLearn)
library(ggplot2)
prostate.train <- subset(prostate, train)
ggplot(prostate.train, aes(x=lcavol,y=lpsa)) + geom_point() +
  geom_smooth(method='lm', se=FALSE)



h <- function(x){
  x^2 + (x-2)^2 - log(x^2+1)
}



h.prima <- function(x){
  2*x + 2*(x-2) - 1/(x^2 + 1)
}


## ===================================================

iteraciones.x <- rep(NA,20)
eta <- 0.1
x.ant <- 5
for(i in 1:20){
  x.nueva <- x.ant - eta * h.prima(x.ant)
  iteraciones.x[i] <- x.nueva
  x.ant <- x.nueva
}



curve(h, xlim=c(-2,6))
points(iteraciones.x, h(iteraciones.x))



h <- function(x){
  x[1]^2 + x[2]^2 - x[1]*x[2]
}

rejilla.1 <- expand.grid(x=seq(-3,3,0.1), y=seq(-3,3,0.1))
rejilla.1$z <- apply(rejilla.1, 1, h)
ggplot(rejilla.1, aes(x=x, y=y, z=z)) + geom_contour(binwidth=1.5,
  aes(colour=..level..))



h.grad <- function(x){
  c(2*x[1]-x[2], 2*x[2]-x[1])
}



grad.1 <- h.grad(c(0.5,-1))
grad.2 <- h.grad(c(1,1))
eta <- 0.2
library(grid)
ggplot(rejilla.1, aes(x=x, y=y, z=z)) + geom_contour(binwidth=1.5) +
        geom_segment(aes(x=0.5, xend=0.5-eta*grad.1[1], y=-1,
     yend=-1-eta*grad.1[2]), 
     arrow = arrow(length = unit(0.2,"cm")))+ 
  geom_segment(aes(x=1, xend=1-eta*grad.2[1], y=1,
     yend=1-eta*grad.2[2]), 
     arrow = arrow(length = unit(0.2,"cm")))+ 
  coord_fixed(ratio = 1) 



eta <- 0.05
z.anterior <- c(0, 0)
z.nueva <- c(2,0)
iteraciones <- list()
i <- 1
while(abs(h(z.anterior)-h(z.nueva)) > 0.0001){
  z.anterior <- z.nueva
  z.nueva <- z.anterior - eta * h.grad(z.anterior)
  i <- i +1
  iteraciones[[i]] <- z.nueva
}
iteraciones[1:5]
mat.iter <- Reduce(rbind, iteraciones)
rownames(mat.iter) <- 1:nrow(mat.iter)
df.iter <- data.frame(mat.iter)
names(df.iter) <- c('x','y')
ggplot(rejilla.1, aes(x=x, y=y, z=z)) +
  geom_contour(binwidth=1.5) + 
  geom_point(data=df.iter, aes(x=x, y=y, z=0, label=rownames(df.iter)), size=3) +
  geom_text(data=df.iter, aes(x=x, y=y, z=0, label=rownames(df.iter)),
    col='gray50', vjust=-0.5, size=4)
 ## ===================================================


eta <- 0.8
z.anterior <- c(0, 0)
z.nueva <- c(2.5, -2)
iteraciones <- list()
iteraciones[[1]] <- z.nueva
i <- 1
for(j in 1:10){
  z.anterior <- z.nueva
  z.nueva <- z.anterior - eta * h.grad(z.anterior)
  i <- i +1
  iteraciones[[i]] <- z.nueva
}
iteraciones[1:5]
mat.iter <- Reduce(rbind, iteraciones)
rownames(mat.iter) <- 1:nrow(mat.iter)
df.iter <- data.frame(mat.iter)
names(df.iter) <- c('x','y')
ggplot(rejilla.1, aes(x=x, y=y, z=z)) +
  geom_contour(binwidth=1.5) + 
  geom_point(data=df.iter, aes(x=x, y=y, z=0, label=rownames(df.iter)), size=3) +
  geom_text(data=df.iter, aes(x=x, y=y, z=0, label=rownames(df.iter)),
    col='gray50', vjust=-0.5, size=4)



library(plyr)
pruebas <- ldply(c(0.001,0.01,0.1,0.5,1), function(eta){
  z.anterior <- c(4.5, -2)
  iteraciones <- list()
  for(j in 1:10){
    z.nueva <- z.anterior - eta * h.grad(z.anterior)
    iteraciones[[j]] <- z.nueva
    z.anterior <- z.nueva
  }
  data.frame(eta=eta, iter.num= 1:10, 
    iteraciones=sapply(iteraciones, h))
})

ggplot(pruebas, aes(x=iter.num, y=iteraciones)) + 
  facet_wrap(~eta, scales = 'free') +
  geom_line() 



rss <- function(beta){
  res <- prostate.train$lpsa - beta[1] - prostate.train$lcavol*beta[2]
  mean(res^2)
}

rss.gradiente <- function(beta){
  gradiente <- rep(NA, 2)
  f.beta <- beta[1] + beta[2]*prostate.train$lcavol
  temp <- f.beta-prostate.train$lpsa
  gradiente[1] <- 2*mean(temp)
  gradiente[2] <- 2*mean(temp*prostate.train$lcavol)
  gradiente
}

betas <- list()
beta.ant <- c(0,0)
eta <- 0.1
for(i in 1:150){
  beta.nueva <- beta.ant - eta*rss.gradiente(beta.ant)
  betas[[i]] <- c(beta.nueva, rss(beta.nueva))
  beta.ant <- beta.nueva
}
beta.nueva



qplot(1:length(betas), sapply(betas, function(x){log(x[3])}))



beta.nueva
ggplot(prostate.train, aes(x=lcavol, y=lpsa)) + geom_point()+
  geom_abline(yintercept=beta.nueva[1], intercept=beta.nueva[2])



head(prostate.train)
summary(prostate.train)



x.2 <- rnorm(100,0,1) 
x.1 <- rnorm(100,0,10) + 5*x.2
y <- 0.1*x.1 + x.2 + rnorm(100,0,1)

dat <- data.frame(x.1,x.2,y)

rss <- function(beta){
  mean((as.matrix(dat[,1:2])%*%beta - y)^2)
}

grid.beta <- expand.grid(beta.1=seq(-10,10,0.5), beta.2=seq(-10,10,0.5))
rss.1 <- apply(grid.beta, 1, rss)
dat.x <- data.frame(grid.beta, rss.1)



ggplot( dat.x, aes(x=beta.1, y=beta.2,z=rss.1)) + geom_contour()



dat.norm <- data.frame(scale(data.frame(x.1,x.2)))
dat.norm$y <- y
rss.norm <- function(beta){
  mean((as.matrix(dat.norm[,1:2])%*%beta - y)^2)
}
rss.norm.1 <- apply(grid.beta, 1, rss.norm)
dat.y <- data.frame(grid.beta, rss.norm.1)



ggplot( dat.y, aes(x = beta.1, y = beta.2, z = rss.norm.1)) + geom_contour()















library(ISLR)
datos <- Auto[, c('name', 'weight','year', 'mpg')]
datos$peso_kg <- datos$weight*0.45359237
datos$rendimiento_kpl <- datos$mpg*(1.609344/3.78541178)
datos$tipo <- runif(nrow(datos), 0, 1)
datos.entrena <- subset(datos, tipo > 0.25)
datos.prueba <- subset(datos, tipo <= 0.25)

ggplot(datos.entrena, aes(x=peso_kg, y=rendimiento_kpl)) + geom_point()



library(kknn)
mod.5vmc <- kknn(rendimiento_kpl ~ peso_kg, train=datos.entrena, 
  test=data.frame(peso_kg=seq(700,2200, by = 10)), k=15 )
dat.pred <- data.frame(peso_kg=seq(700,2200, by = 10), 
  rendimiento_kpl=predict(mod.5vmc))
ggplot(datos.entrena, aes(x=peso_kg, y=rendimiento_kpl)) + 
  geom_point(alpha=0.6) +
  geom_line(data=dat.pred, col='red', size=1.2)



mod.5vmc <- kknn(rendimiento_kpl ~ peso_kg, train=datos.entrena, 
  test=data.frame(peso_kg=seq(700,2200, by = 10)), k=200 )
dat.pred <- data.frame(peso_kg=seq(700,2200, by = 10), 
  rendimiento_kpl=predict(mod.5vmc))
ggplot(datos.entrena, aes(x=peso_kg, y=rendimiento_kpl)) + 
  geom_point(alpha=0.6) +
  geom_line(data=dat.pred, col='red', size=1.2)



fun.1 <- function(x){
  exp(-8*sum(x^2))
}
x.1 <- runif(1000, -1, 1)
x.2 <- runif(1000, -1, 1)
dat <- data.frame(x.1, x.2)
dat$y <- apply(dat, 1, fun.1)
ggplot(dat, aes(x=x.1,y=x.2, colour=y)) + geom_point()



dist.origen <- apply(dat[,1:2], 1, function(x){sqrt(sum(x^2))})
mas.cercano.indice <- which.min(dist.origen)
mas.cercano <- dat[mas.cercano.indice, ]
mas.cercano



fun.1 <- function(x){
  exp(-8*sum(x^2))
}

sims.1 <- lapply(1:8, function(i){runif(1000, -1, 1)})
dat <- data.frame(Reduce(cbind, sims.1))
dat$y <- apply(dat, 1, fun.1)



dist.origen <- apply(dat[,1:8], 1, function(x){sqrt(sum(x^2))})
mas.cercano.indice <- which.min(dist.origen)
mas.cercano <- dat[mas.cercano.indice, ]
mas.cercano



mas.cercano$y



fun.2 <- function(x){
  0.5*(1+x[1])^3
}
set.seed(111)
sims.1 <- lapply(1:40, function(i){runif(1000, -1, 1)})
dat <- data.frame(Reduce(cbind, sims.1))
dat$y <- apply(dat, 1, fun.2)

dist.origen <- apply(dat[,1:40], 1, function(x){sqrt(sum(x^2))})
mas.cercano.indice <- which.min(dist.origen)
mas.cercano <- dat[mas.cercano.indice, ]
mas.cercano
mas.cercano$y
mod.1 <- lm(y~. , data=dat)
sum(coef(mod.1)*c(1,rep(0,40)))


