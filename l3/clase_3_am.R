

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
glm(type ~ glu.st, data=Pima.tr, family = 'binomial')



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


