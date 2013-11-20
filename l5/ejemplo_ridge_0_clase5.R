
library(knitr)
library(ggplot2)
library(plyr)
library(reshape2)
#install.packages("glmnet")


h <- function(x){ exp(x) / (1 + exp(x))}
set.seed(2404)
beta <- rnorm(100,0,0.1)
beta

devianza  <- function(prob, y){
  -2*mean(y*log(prob)+(1-y)*log(1-prob)    )
}

?rbinom

as.matrix(rdply(10, rnorm(50))[,-1]) + rnorm(50)

sim.1 <- function(n, m, p=100){
  #n = casos de entrenamiento, m= casos de prueba, p=num variables
  mat.entrena <- as.matrix(rdply(n, rnorm(p))[,-1]) + rnorm(p)
  mat.prueba <- as.matrix(rdply(m, rnorm(p))[,-1]) + rnorm(p)
  g.entrena = rbinom(n, 1, h(mat.entrena%*%beta))
  g.prueba = rbinom(m, 1, h(mat.prueba%*%beta))
  list(mat.entrena = mat.entrena, mat.prueba = mat.prueba, g.entrena=g.entrena, 
    g.prueba=g.prueba)
}

salida <- sim.1(n=400, m=2000)
names(salida$mat.entrena)

class(salida)

mod.ridge <- glmnet(x=salida$mat.entrena, y=salida$g.entrena, 
                    alpha=0, family='binomial', 
                    intercept=F, nlambda=50)

str(mod.ridge)

dat.r <- ldply(1:50, function(i){
  dat.prueba.r <- data.frame(i=i, lambda=mod.ridge$lambda[i], 
                             prob.hat.1=h(salida$mat.prueba %*% as.numeric(coef(mod.ridge)[,i][-1])), 
                             clase = salida$g.prueba)
  dat.prueba.r
})

devianza.prueba <- ddply(dat.r, c('i','lambda'), summarise, 
  dev = devianza(prob.hat.1, clase))
qplot(log(devianza.prueba$lambda), devianza.prueba$dev)


library(glmnet)
?cv.glmnet
?glmnet
cv.mod.ridge <- cv.glmnet(x=salida$mat.entrena, y=salida$g.entrena, 
                          alpha=0, family='binomial', intercept=F, 
                          nfolds=10, nlambda=50)
plot(cv.mod.ridge)
cv.mod.ridge$lambda.min
cv.mod.ridge$lambda.1se


?melt
comp.dat <- data.frame(log.lambda=log(devianza.prueba$lambda),
  devianza.prueba=devianza.prueba$dev,
  devianza.vc=cv.mod.ridge$cvm)

comp.dat.m <- melt(comp.dat, id.vars='log.lambda')

?cv.mod.ridge
plot(cv.mod.ridge)

ggplot(comp.dat.m, aes(x=log.lambda, y=value, colour=variable)) +
  geom_point()
