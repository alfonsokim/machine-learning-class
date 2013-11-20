

library(knitr)
library(ggplot2)
library(plyr)
library(reshape2)
#options(replace.assign=TRUE,width=55)
#knit_hooks$set(par=function(before, options, envir){if (before) par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)})



h <- function(x){ exp(x) / (1 + exp(x))}
set.seed(2805)
beta <- rnorm(100,0,0.1)
beta



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
names(salida)



mod.1 <- glm.fit(x=salida$mat.entrena,
                 y=salida$g.entrena, family = binomial())



qplot(beta, mod.1$coefficients) + 
  xlab('Coeficientes') + 
  ylab('Coeficientes estimados') +
  geom_abline(xintercept=0, slope =1) +
  xlim(c(-0.4,0.4))+ ylim(c(-0.8,0.8))



salida.2 <- sim.1(n=400, m=10)
names(salida.2)
mod.2 <- glm.fit(x = salida.2$mat.entrena,
                 y = salida.2$g.entrena, family = binomial())
qplot(beta, mod.2$coefficients) + xlab('Coeficientes') + 
  ylab('Coeficientes estimados') +
  geom_abline(xintercept=0, slope =1) + xlim(c(-0.4,0.4))+ ylim(c(-0.8,0.8))



dat.sim <- ldply(1:50, function(i){
  salida.2 <- sim.1(n=400, m=10)
  mod.2 <- glm.fit(x = salida.2$mat.entrena,
                 y = salida.2$g.entrena, family = binomial())
  data.frame(rep=i, vars=names(coef(mod.2)), coefs=coef(mod.2))
})



dat.sim$vars <- reorder(as.character(dat.sim$vars), dat.sim$coefs, median)
ggplot(dat.sim, aes(x=vars, y=coefs)) + geom_boxplot() +
  geom_line(data=data.frame(coefs=beta, vars=names(coef(mod.2))), 
    aes(y=beta, group=1), col='red',size=1.1) + coord_flip()



dat.entrena <- data.frame(prob.hat.1=h(mod.1$fitted.values), prob.1=h(salida$mat.entrena%*%beta),
  clase = salida$g.entrena)
dat.prueba <- data.frame(prob.hat.1=h(salida$mat.prueba%*%(mod.1$coefficients)), 
  prob.1=h(salida$mat.prueba%*%beta),
  clase = salida$g.prueba)



ggplot(dat.entrena, aes(x=prob.1, y=prob.hat.1, colour=factor(clase))) + geom_point()



ggplot(dat.prueba, aes(x=prob.1, y=prob.hat.1, colour=factor(clase))) + geom_point()



table(dat.prueba$prob.hat.1>0.5,dat.prueba$clase)
prop.table(table(dat.prueba$prob.hat.1>0.5,dat.prueba$clase), margin=2)



library(glmnet)
mod.restringido <- glmnet(x=salida$mat.entrena, y=salida$g.entrena, 
  alpha = 0,
  family='binomial', intercept = F, 
  lambda = 0.15)
beta.restr <- coef(mod.restringido)[-1]



qplot(beta, beta.restr) + 
  xlab('Coeficientes') + 
  ylab('Coeficientes estimados') +
  geom_abline(xintercept=0, slope =1) +
  xlim(c(-0.4,0.4))+ ylim(c(-0.7,0.7))



dat.entrena.r <- data.frame(prob.hat.1= h(salida$mat.entrena%*%as.numeric(beta.restr)), 
  prob.1=h(salida$mat.entrena%*%beta),
  clase = salida$g.entrena)
dat.prueba.r <- data.frame(prob.hat.1=h(salida$mat.prueba%*%as.numeric(beta.restr)), 
  prob.1=h(salida$mat.prueba%*%beta),
  clase = salida$g.prueba)



ggplot(dat.entrena.r, aes(x=prob.1, y=prob.hat.1, colour=factor(clase))) + geom_point()



ggplot(dat.prueba.r, aes(x=prob.1, y=prob.hat.1, colour=factor(clase))) + geom_point()



table(dat.prueba.r$prob.hat.1>0.5, dat.prueba.r$clase)
prop.table(table(dat.prueba.r$prob.hat.1>0.5, dat.prueba.r$clase), margin=2)



library(ROCR)
pred <- prediction(predictions= dat.prueba$prob.hat.1, labels = dat.prueba$clase)
perf <- performance(pred, measure = "sens", x.measure = "fpr") 
plot(perf)
pred.r <- prediction(predictions= dat.prueba.r$prob.hat.1, labels = dat.prueba.r$clase)
perf.r <- performance(pred.r, measure = "sens", x.measure = "fpr") 
plot(perf.r, add =T, col ='red')
abline(a=0, b=1, col ='gray')



library(ROCR)
pred <- prediction(predictions= dat.entrena$prob.hat.1, labels = dat.entrena$clase)
perf <- performance(pred, measure = "sens", x.measure = "fpr") 
plot(perf)
pred.r <- prediction(predictions= dat.entrena.r$prob.hat.1, labels = dat.entrena.r$clase)
perf.r <- performance(pred.r, measure = "sens", x.measure = "fpr") 
plot(perf.r, add =T, col ='red')
abline(a=0, b=1, col ='gray')



library(ElemStatLearn)
library(arm)
dim(zip.train)
dim(zip.test)
mat.coeficientes <- matrix(NA, nrow=256+1, ncol=10)
for(i in 0:9){
  clase.bin <- as.numeric(zip.train[,1] == i)
  dat <- data.frame(clase=clase.bin, zip.train[,-1])
  modelo <- bayesglm(clase ~ ., data=dat, prior.scale=0.01, prior.df=Inf)
  mat.coeficientes[,i + 1] <- coef(modelo)
}
library(reshape2)
library(Hmisc)
mat.2 <- melt(mat.coeficientes[-1,])
mat.2$y <- -(mat.2$Var1 %/% 16 )
mat.2$x <- mat.2$Var1 %% 16 
mat.2$Var2 <- mat.2$Var2 - 1
ggplot(mat.2, aes(x=x, y=y)) + 
  geom_tile(aes(fill=cut2(value,g=3))) + facet_wrap(~Var2)
h <- function(x){exp(x)/(1+exp(x))}
pred.digito <- function(mat, beta){
   prob.pred <- h(cbind(1, mat)%*%beta)
   apply(prob.pred, 1, which.max) - 1
}


clasif.prueba <- pred.digito(zip.test[,-1], mat.coeficientes)
mean(clasif.prueba != zip.test[,1])
table(clasif.prueba, zip.test[,1])
round(prop.table(table(clasif.prueba, zip.test[,1]), margin = 2),2)




library(glmnet)
mod.ridge <- glmnet(x=salida$mat.entrena, y=salida$g.entrena, 
  alpha = 0,
  family='binomial', intercept = F, nlambda=50)



dim(coef(mod.ridge))
plot(mod.ridge, xvar='lambda')



devianza  <- function(prob, y){
  -2*mean(y*log(prob)+(1-y)*log(1-prob)    )
}

dat.r <- ldply(1:50, function(i){
  dat.prueba.r <- data.frame(i=i, lambda=mod.ridge$lambda[i],
    prob.hat.1=h(salida$mat.prueba%*%as.numeric(coef(mod.ridge)[,i][-1])), 
  clase = salida$g.prueba)
  dat.prueba.r
})

devianza.prueba <- ddply(dat.r, c('i','lambda'), summarise, 
  dev = devianza(prob.hat.1, clase))
qplot(log(devianza.prueba$lambda), devianza.prueba$dev)



mod.ridge$lambda
pred.prueba.final <- salida$mat.prueba%*%(coef(mod.ridge)[ , 40][-1])
table(pred.prueba.final > 0.5, salida$g.prueba)
prop.table(table(pred.prueba.final > 0.5, salida$g.prueba), margin=2)
prop.table(table(pred.prueba.final > 0.1, salida$g.prueba), margin=2)

#beta.r <- coef(mod.ridge)[]



library(glmnet)
cv.mod.ridge <- cv.glmnet(x=salida$mat.entrena, y=salida$g.entrena, 
  alpha = 0,
  family='binomial', intercept = F, nfolds = 10, nlambda=50)
plot(cv.mod.ridge)
cv.mod.ridge$lambda.min
cv.mod.ridge$lambda.1se



comp.dat <- data.frame(log.lambda=log(devianza.prueba$lambda),
  devianza.prueba = devianza.prueba$dev,
  devianza.vc = cv.mod.ridge$cvm)
comp.dat.m <- melt(comp.dat, id.vars='log.lambda')
ggplot(comp.dat.m, aes(x=log.lambda, y=value, colour=variable)) +
  geom_point()



library(glmnet)
library(ggplot2)
# leer acerca de los datos en el archivo info que acompaña.
bodyfat <- read.table("./datos/bodyfat.txt",header=TRUE)
names(bodyfat)
nrow(bodyfat)
# Separamos en muestra de entrenamiento y de prueba: el modelo
# lo ajustamos con la de entrenamiento, y medimos el error de
# predicción con la de prueba
#Tamaño de muestra de entrenamiento 
N <- 220
set.seed(28)
bodyfat.2 <- bodyfat[ sample(1:nrow(bodyfat), nrow(bodyfat)), ]
bodyfat.entrena <- bodyfat[1:N,   ]
bodyfat.prueba <- bodyfat[(N+1):nrow(bodyfat), ]

#revisamos los datos de entrenamiento:
summary(bodyfat.entrena)
#splom(bodyfat.entrena)
#notamos atípicos - estatura demasiado baja:
head(bodyfat.entrena[order(bodyfat.entrena$estatura),    ])

#eliminamos un caso que tiene una medición errónea de estatura
bodyfat.entrena.2 <- bodyfat.entrena




#Ahora hacemos lasso: alpha =1
modelos.lasso <- glmnet(x = as.matrix(bodyfat.entrena.2[, 3:15]),
    y = bodyfat.entrena.2$grasacorp, alpha=1, family = "gaussian")

#Aquí vemos cómo se encogen y salen variables conforme
#aumentamos la penalización (escrita en norma -1)
#plot(modelos.lasso)
plot(modelos.lasso, xvar = "lambda")
#podemos examinar el objeto lass:
names(modelos.lasso)



## Validación cruzada - veremos más adelante. Por el momento,
## son estimaciones del error de prueba.
modelos.lasso.cv <- cv.glmnet(x = as.matrix(bodyfat.entrena.2[, 3:15]),
    y = bodyfat.entrena.2$grasacorp, alpha=1, family = "gaussian",
    nfolds =10)
plot(modelos.lasso.cv)




##Ahora vemos el modelo que ajustamos:
#usamos predict para ver coeficientes.
#Nótese que este modelo solo usa 5 variables
coefs.lasso <- predict(modelos.lasso, s=exp(-1.8), type="coef")
round(coefs.lasso,2)



##otro más encogido
coefs.lasso <- predict(modelos.lasso, s=exp(-0), type="coef")
round(coefs.lasso,2)



#y también para predecir con la muestra de prueba:
pred.lasso <- predict(modelos.lasso, s=exp(-1), newx=as.matrix(bodyfat.prueba[,3:15]))
qplot(bodyfat.prueba$grasacorp, as.numeric(pred.lasso)) +
    geom_abline(xintercept=0, slope=1)
## El error es:
mean((bodyfat.prueba$grasacorp- as.numeric(pred.lasso))^2)
#con error estándar
sd((bodyfat.prueba$grasacorp- as.numeric(pred.lasso))^2)/sqrt(length(bodyfat.prueba$grasacorp))



