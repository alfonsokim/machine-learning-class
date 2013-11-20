

library(knitr)
library(ggplot2)
library(plyr)
library(reshape2)
#options(replace.assign=TRUE,width=55)
#knit_hooks$set(par=function(before, options, envir){if (before) par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)})



library(igraph)
 gr <- graph(
   c(c(1,4,1,5,1,6,2,4,2,5,2,6,3,4,3,5,3,6), c(4,7,5,7,6,7)))
 plot(gr, layout = matrix(c(-4,1,-4,0,-4,-1,0,1,0,-1,0,0,4,0), byrow=T, ncol=2),
      vertex.label=c('X1','X2','X3','a1','a2','a3','p1'), 
      vertex.size=50, vertex.color='salmon', vertex.label.cex=3,
      vertex.label.color='white',vertex.frame.color=NA)



h <- function(x){
    exp(x)/(1+exp(x))
}
x <- seq(-2,2,0.1)
p <- h(2-3*x^2) #probabilidad condicional de clase 1 (vs. 0)
set.seed(2805721)
x.1 <- runif(30, -2, 2)
g.1 <- rbinom(30, 1, h(2-3*x.1^2))
datos <- data.frame(x.1,g.1)
dat.p <- data.frame(x,p)
g <- qplot(x,p, geom='line', colour='red')
g + geom_point(data = datos, aes(x=x.1,y=g.1))



library(igraph)
 gr <- graph(c(1,2,1,3,2,4,3,4))
 plot(gr, layout = matrix(c(-2,0,0,1,0,-1,2,0), byrow=T, ncol=2),
      vertex.label=c('X1','a_1','a_2','p1'), 
      vertex.size=50, vertex.color='salmon', vertex.label.cex=1.5,
   vertex.label.color='white',vertex.frame.color=NA
   )



a.1 <- h( 1 + 2*x)  # 2(x+1/2)
a.2 <- h(-1 + 2*x)  # 2(x-1/2) # una es una versión desplazada de otra.



library(reshape)
dat.a <- data.frame(x=x, a.1=a.1, a.2=a.2)
dat.a2 <- melt(dat.a, id.vars='x')
ggplot(dat.a2, aes(x=x, y=value, colour=variable, group=variable)) + geom_line()



dat.a <- data.frame(x=x, a.1=-4+12*a.1, a.2=-12*a.2, suma=-4+12*a.1-12*a.2)
dat.a2 <- melt(dat.a, id.vars='x')
ggplot(dat.a2, aes(x=x, y=value, colour=variable, group=variable)) + geom_line()



dat.2 <- data.frame(x, p2=h(-4 + 12*a.1 - 12*a.2))
ggplot(dat.2, aes(x=x, y=p2)) + geom_line()+
geom_line(data=dat.p, aes(x=x,y=p), col='red') +ylim(c(0,1))+
   geom_point(data = datos, aes(x=x.1,y=g.1))



## esta función calcula los valores de cada nodo en toda la red,
## para cada entrada
feed.fow <- function(beta, x){
  a.1 <- h(beta[1] + beta[2]*x) # calcula variable 1 de capa oculta
  a.2 <- h(beta[3] + beta[4]*x) # calcula variable 2 de capa oculta
  p <- h(beta[5]+beta[6]*a.1 + beta[7]*a.2) # calcula capa de salida
  p
}




devianza.func <- function(x, g){
    # esta función es una fábrica de funciones
   devianza <- function(beta){
         p <- feed.fow(beta, x)
      - 2 * mean(g*log(p) + (1-g)*log(1-p))
   }
  devianza
}



dev <- devianza.func(x.1, g.1) # crea función dev
## ahora dev toma solamente los 7 parámetros beta:
dev(c(0,0,0,0,0,0,0))
dev(rnorm(7))



set.seed(5)
salida <- optim(rnorm(7), dev, method='BFGS') # inicializar al azar punto inicial
salida
beta <- salida$par




## hacer feed forward con beta encontroados
p.2 <- feed.fow(beta, x)
dat.2 <- data.frame(x, p.2=p.2)
ggplot(dat.2, aes(x=x, y=p.2)) + geom_line()+
geom_line(data=dat.p, aes(x=x,y=p), col='red') +ylim(c(0,1))+
   geom_point(data = datos, aes(x=x.1,y=g.1))



beta



devianza.reg <- function(x, g, lambda){
    # esta función es una fábrica de funciones
   devianza <- function(beta){
         p <- feed.fow(beta, x)
      - 2 * mean(g*log(p) + (1-g)*log(1-p)) + lambda*sum(beta^2)
   }
  devianza
}



dev.r <- devianza.reg(x.1, g.1, 0.001) # crea función dev
set.seed(5)
salida <- optim(rnorm(7), dev.r, method='BFGS') # inicializar al azar punto inicial
salida
beta <- salida$par
dev(beta)
p.2 <- feed.fow(beta, x)
dat.2 <- data.frame(x, p.2=p.2)
ggplot(dat.2, aes(x=x, y=p.2)) + geom_line()+
geom_line(data=dat.p, aes(x=x,y=p), col='red') +ylim(c(0,1))+
   geom_point(data = datos, aes(x=x.1,y=g.1))



library(nnet)
set.seed(12)
nn <- nnet(g.1~x.1, data=datos, size = 2, decay=0.00, entropy = T)
nn$wts
nn$value



2*nn$value/30
dev(nn$wts)
qplot(x, predict(nn, newdata=data.frame(x.1=x)), geom='line')



h <- function(x){
    exp(x)/(1+exp(x))
}
x <- seq(-2,2,0.05)
p <- h(3 + x- 3*x^2 + 3*cos(4*x))
set.seed(280572)
x.2 <- runif(300, -2, 2)
g.2 <- rbinom(300, 1, h(3 + x.2- 3*x.2^2 + 3*cos(4*x.2)))
datos <- data.frame(x.2,g.2)
dat.p <- data.frame(x,p)
g <- qplot(x,p, geom='line', col='red')
g + geom_jitter(data = datos, aes(x=x.2,y=g.2), col ='black',
  position =position_jitter(height=0.05), alpha=0.4)



 gr <- graph(
   c(1,4,1,5,1,6,2,4,2,5,2,6,2,4,2,5,2,6,3,4,3,5,3,6,4,7,4,8,5,7,5,8,6,7,6,8,7,8,7,9,8,9))
plot(gr, layout=matrix(c(-1,1,-1,0,-1,-1,0,1,0,0,0,-1,1,0.5,1,-0.5,2,0), byrow=T,ncol=2),
      vertex.size=50, vertex.color=c('salmon'),
     vertex.frame.color=NA, edge.curved=FALSE)



 gr <- graph(
   c(c(1,4,1,5,2,4,2,5,3,4,3,5)))
 plot(gr, layout = matrix(c(-4,1,-4,0,-4,-1,0,1,0,-1), byrow=T, ncol=2),
      vertex.label=c(expression(a[1]^2),expression(a[2]^2),expression(a[3]^2),
        expression(a[1]^3), expression(a[2]^3)), 
      vertex.size=50, vertex.color=c('salmon','salmon','salmon','red','red'), vertex.label.cex=1.5,
      vertex.label.color='white',vertex.frame.color=NA,
   edge.label=c(expression(theta[11]^3),expression(theta[21]^3),
     expression(theta[12]^3),  expression(theta[22]^3),
      expression(theta[13]^3), expression(theta[23]^3)))



 gr <- graph(
   c(c(1,5,1,6,2,5,2,6,3,5,3,6,4,5,4,6)))
 plot(gr, layout = matrix(c(-4,3,-4,1,-4,0,-4,-1,0,1,0,-1), byrow=T, ncol=2),
      vertex.label=c(expression(a[0]^2), expression(a[1]^2),
        expression(a[2]^2),expression(a[3]^2),
        expression(a[1]^3), expression(a[2]^3)), 
      vertex.size=50, 
   vertex.color=c('gray','salmon','salmon','salmon','red','red'), vertex.label.cex=1.5,
      vertex.label.color='white',vertex.frame.color=NA,
   edge.label=c(expression(theta[10]^3),expression(theta[20]^3),
     expression(theta[11]^3),expression(theta[21]^3), expression(theta[12]^3),  expression(theta[22]^3), expression(theta[13]^3), expression(theta[23]^3)))



feed.forward <- function(a, Theta){
  # a_{l-1} da los valores de la primera capa, la función debe regresar
  # los valores de la siguiente capa a_l. Theta da los pesos
  h(Theta %*% a)
}



 gr <- graph(
   c(c(1,4,1,5,2,4,2,5,3,4,3,5)))
 plot(gr, layout = matrix(c(-4,1,-4,0,-4,-1,0,1,0,-1), byrow=T, ncol=2),
      vertex.label=c('-2','5','1','a_1 ?','a_2 ?'), vertex.label.cex=1.5,
      vertex.size=50, vertex.color=c('salmon','salmon','gray','red','red'), vertex.label.cex=3,
      vertex.label.color='white',vertex.frame.color=NA,
   edge.label=c(1,2,-1,0.5,3,1))



Theta = t(matrix(c(3,1,-1,1,0.5,2), byrow=F, ncol=2))
Theta



feed.forward(c(1,-2,5), Theta = Theta)



 gr <- graph(
   c(c(1,4,1,5,2,4,2,5,3,4,3,5)))
 plot(gr, layout = matrix(c(-4,1,-4,0,-4,-1,0,1,0,-1), byrow=T, ncol=2),
      vertex.label=c('-2','5','1','a_1=0.017','a_2=0.999'), 
      vertex.size=50, vertex.color=c('salmon','salmon','gray','red','red'), vertex.label.cex=1.5, edge.label.cex=1.5,
      vertex.label.color='white',vertex.frame.color=NA,
   edge.label=c(1,2,-1,0.5,3,1))



library(MASS)
library(nnet)
source('graf_nnet.R')
set.seed(280573)
diabetes.red.1 <- nnet(type~., data=Pima.te, size = 8,  decay = 0.5, 
  maxit = 500, MaxNWts=10000)
diabetes.red.2 <- nnet(type~., data=Pima.te, size = 8,  decay = 0.5, 
  maxit = 500, MaxNWts=1000)
  diabetes.red.3 <- nnet(type~., data=Pima.te, size = 8,  decay = 0.5, 
  maxit = 500, MaxNWts=1000)
plot(diabetes.red.1)



qplot(predict(diabetes.red.1, Pima.tr))
tab.1 <- table(predict(diabetes.red.1, Pima.tr)>0.5, Pima.tr$type)
sum(diag(tab.1))/sum(tab.1)



library(glmnet)
mod.net <- cv.glmnet(y=as.numeric(Pima.te$type=='Yes'), 
  x=as.matrix(Pima.te[,1:7]), family='binomial',
  alpha=1)
preds.reg <- predict(mod.net, newx = as.matrix(Pima.tr[,1:7]), type='response')



library(ROCR)
pred.1 <- ROCR:::prediction(preds.reg, Pima.tr$type)
perf.1 <- performance(pred.1, 'sens', 'fpr')
pred.2 <-  ROCR:::prediction(predict(diabetes.red.3, Pima.tr), Pima.tr$type)
perf.2 <- performance(pred.2, 'sens', 'fpr')
plot(perf.1)
plot(perf.2, add=T, col = 'red')



library(plyr)
set.seed(1249)
params <- expand.grid(.decay=c(0.001,0.01,0.1,1,10,100),.size=c(1,2,3,4,5,8,10),
  reps=1:20)
modelos <- dlply(params, c('.decay','.size','reps'), function(df){
  diabetes.red <- nnet(type~., data=Pima.te, size = df$.size,  decay = df$.decay, 
  maxit = 500, MaxNWts=10000, trace=FALSE)
  diabetes.red
})
devianzas.prueba <- ldply(modelos, function(mod){
  probs <- predict(mod, Pima.tr)
  y <- Pima.tr$type=='Yes'
  -2*mean(y*log(probs+0.0001) +(1-y)*log(1-probs+0.0001))
} )
dev.media <- ddply(devianzas.prueba,c('.size','.decay'), summarise, mean.dev=mean(V1) )
ggplot(dev.media, aes(x=(.decay), y=mean.dev, colour=factor(.size), group=.size)) +
  geom_line() + geom_point() + scale_x_log10(breaks=c(0.001,0.01,0.1,1,10,100))



set.seed(2805799)
diabetes.red.1 <- nnet(type~., data=Pima.te, size = 3,  decay = 0.1, 
  maxit = 500, MaxNWts=10000)
diabetes.red.2 <- nnet(type~., data=Pima.te, size = 3,  decay = 0.1, 
  maxit = 500, MaxNWts=1000)
  diabetes.red.3 <- nnet(type~., data=Pima.te, size = 3,  decay = 0.1, 
  maxit = 500, MaxNWts=1000)
plot(diabetes.red.3)



library(ROCR)
pred.1 <- ROCR:::prediction(preds.reg, Pima.tr$type)
perf.1 <- performance(pred.1, 'sens', 'fpr')
pred.2 <-  ROCR:::prediction(predict(diabetes.red.3, Pima.tr), Pima.tr$type)
perf.2 <- performance(pred.2, 'sens', 'fpr')
plot(perf.1)
plot(perf.2, add=T, col = 'red')



set.seed(280)
redes.20 <- ldply(1:10, function(i){
  diabetes.red.1 <- nnet(type~., data=Pima.te, size = 3,  decay = 0.1,
    trace=FALSE,
  maxit = 500, MaxNWts=10000)
  data.frame(id=1:nrow(Pima.tr), rep=i, pred=predict(diabetes.red.1, newdata=Pima.tr))
})
redes.20.prom <- ddply(redes.20, 'id', summarise, mean.pred=mean(pred))



library(ROCR)
pred.1 <- ROCR:::prediction(preds.reg, Pima.tr$type)
perf.1 <- performance(pred.1, 'sens', 'fpr')
pred.2 <-  ROCR:::prediction(redes.20.prom$mean.pred, Pima.tr$type)
perf.2 <- performance(pred.2, 'sens', 'fpr')
plot(perf.1)
plot(perf.2, add=T, col = 'red')



library(caret)
set.seed(1234)
control <- trainControl(method='repeatedcv', number=10, repeats=10,
                  summaryFunction=twoClassSummary, classProbs=TRUE)
model <- train( type~ ., Pima.te, method='nnet', linout=FALSE, trace = FALSE,
                metric='ROC',
                trControl=control,
                #Grid of tuning parameters to try:
                tuneGrid=expand.grid(.size=c(2,3,4,5),
                  .decay=c(0.001,0.01,0.1,1,10))) 
model
plot(model)



library(ElemStatLearn)
zip.d <- data.frame(zip.train)
zip.d$digit <- factor(zip.d[,1])
zip.d <- zip.d[,-1]
set.seed(125)
mod.zip <-  nnet(digit~., data=zip.d, size = 10,  decay = 5, 
  maxit = 500, MaxNWts=10000)
preds.dig <- predict(mod.zip, data.frame(zip.test))
pred.1 <- apply(preds.dig, 1,which.max)
tab.1 <- table(pred.1, zip.test[,1])
tab.1
sum(diag(as.matrix(tab.1)))/sum(tab.1)


