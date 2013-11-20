

library(knitr)
options(replace.assign=TRUE,width=55)
knit_hooks$set(par=function(before, options, envir){if (before) par(mar=c(4,4,.1,.1),cex.lab=.95,cex.axis=.9,mgp=c(2,.7,0),tcl=-.3)})



library(MASS)
library(ggplot2)
data(Pima.tr)
head(Pima.tr)
ggplot(Pima.tr, aes(x=glu, y=as.numeric(type)-1, colour=type, group=1)) + 
  geom_jitter(position=position_jitter(height=0.05)) + 
  geom_smooth(se = FALSE, span=0.8) + 
  ylab('Probabilidad de tener diabetes')



data(Pima.te)
pima.ej <- Pima.te[1:5, c('glu','type') ]
pima.ej$prob.Yes <- c(0.62, 0.23, 0.32, 0.20, 0.75)
pima.ej$prob.No <- 1 - pima.ej$prob.Yes



pima.ej



h <- function(x){exp(x)/(1+exp(x)) }
curve(h, from=-6, to =6)



glm(type ~ glu, data=Pima.tr, family = 'binomial')



Pima.tr$glu.st <- (Pima.tr$glu - mean(Pima.tr$glu))/sd(Pima.tr$glu)
mod.1 <- glm(type ~ glu.st, data=Pima.tr, family = 'binomial')
coef(mod.1)



quantile(Pima.tr$glu.st)
grid.glu <- seq(-2.2, 2.4, 0.1)
preds.grid <- predict(mod.1, newdata = data.frame(glu.st=grid.glu), 
  type='response')
dat.graf <- data.frame(glu.st=grid.glu, prob=preds.grid)
ggplot(dat.graf, aes(x=glu.st, y = prob)) + geom_line()



tabla.1 <- data.frame(A=c(50,20,20), B=c(2,105,10), C=c(0,30,30))
rownames(tabla.1) <- c('A.pred', 'B.pred', 'C.pred')
tabla.1 <- as.table(as.matrix(tabla.1))
tabla.1



round(prop.table(tabla.1, 2),2)



round(prop.table(tabla.1, 1),2)



modelo.1 <- glm(type ~ glu, data = Pima.tr, family = 'binomial')
preds.1 <- fitted(modelo.1) > 0.5
head(preds.1)



tab.confusion.entrena <- table(preds.1, Pima.tr$type)
tab.confusion.entrena
prop.table(tab.confusion.entrena, 2)



probs.prueba <- predict(modelo.1, newdata=Pima.te, type='response')
preds.prueba <- probs.prueba > 0.5
tab.confusion.prueba <- table(preds.prueba, Pima.te$type)
tab.confusion.prueba
prop.table(tab.confusion.prueba, 2)



prop.table(tab.confusion.prueba, 1)



probs.prueba <- predict(modelo.1, newdata=Pima.te, type='response')
preds.prueba <- probs.prueba > 0.7
tab.confusion.prueba <- table(preds.prueba, Pima.te$type)
tab.confusion.prueba
prop.table(tab.confusion.prueba, 2)



probs.prueba <- predict(modelo.1, newdata=Pima.te, type='response')
preds.prueba <- probs.prueba > 0.3
tab.confusion.prueba <- table(preds.prueba, Pima.te$type)
tab.confusion.prueba
prop.table(tab.confusion.prueba, 2)



clasif.1 <- data.frame(
  corte = c('0.3','0.5','0.7','perfecto','azar'),
  tasa_falsos_pos=c(0.24,0.08,0.02,0,0.7),
  sensibilidad =c(0.66, 0.46,0.19,1,0.7))
ggplot(clasif.1, aes(x=tasa_falsos_pos, y=sensibilidad,
  label=corte)) + geom_point() + 
  geom_abline(intercept=0, slope=1) +
  xlim(c(0,1)) +ylim(c(0,1)) + geom_text(hjust=-0.3, col='red')+
  xlab('1-especificidad (tasa falsos pos)')




# install.packages("tabplot")
library(plyr)
library(tabplot)
Pima.te.2 <- Pima.te
Pima.te.2$probs.prueba <- predict(modelo.1, newdata=Pima.te, type='response')
head(arrange(Pima.te.2, desc(probs.prueba)))
tableplot(Pima.te.2, sortCol=probs.prueba)



modelo.2 <- glm(type ~ ., data = Pima.tr[,2:8], family = 'binomial')
Pima.te.2$probs.prueba.2 <- predict(modelo.2, newdata=Pima.te, type='response')
tableplot(Pima.te.2, sortCol=probs.prueba.2)



library(ROCR)
pred.rocr.1 <- prediction(Pima.te.2$probs.prueba, Pima.te.2$type)
perf.1 <- performance(pred.rocr.1, measure='sens', x.measure='fpr')
plot(perf.1)
pred.rocr.2 <- prediction(Pima.te.2$probs.prueba.2, Pima.te.2$type)
perf.2 <- performance(pred.rocr.2, measure='sens', x.measure='fpr')
plot(perf.2, add=TRUE, col='red')



library(ElemStatLearn)
dim(zip.train)
dim(zip.test)
mat.coeficientes <- matrix(NA, nrow=256+1, ncol=10)
for(i in 0:9){
  clase.bin <- as.numeric(zip.train[,1] == i)
  dat <- data.frame(clase=clase.bin, zip.train[,-1])
  modelo <- glm(clase ~ ., data=dat)
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



library(ElemStatLearn)
clasif.entrena <- pred.digito(zip.train[,-1], mat.coeficientes)
head(clasif.entrena)



mean(clasif.entrena != zip.train[,1])
table(clasif.entrena, zip.train[,1])



clasif.prueba <- pred.digito(zip.test[,-1], mat.coeficientes)
mean(clasif.prueba != zip.test[,1])
table(clasif.prueba, zip.test[,1])
round(prop.table(table(clasif.prueba, zip.test[,1]), margin = 2),2)


