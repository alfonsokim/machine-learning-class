library(ggplot2)
library(MASS)
summary(Pima.tr)

set.seed(280572)

type <- as.numeric(Pima.tr$type) - 1  
nno <- length(type[which(type == 'No')])
?rep
type[which(type == 'No')] <- rep(0, nno)

## 1: normalizacion
X.df <- scale(Pima.tr[, 2:7])
X.df
X <- as.matrix(X.df)

## 2.1
h <- function(x){
    ( exp(x) / (1+exp(x)) )
}

## 2. Predicciones
p.beta <- function(X, beta){
    mat <- cbind(1, X)
    preds <- h(mat %*% beta)
    preds
}

start.beta=rep(1,ncol(X)+1)
p.beta(X=X, beta=start.beta)


## 4. Devianza
devianza  <- function(X, y, beta){
    ## completar
    #formula de la devianza con p.beta
    mean( (y * log10(p.beta(X, beta)) ) + ( (1 - y) * log(1 - p.beta(X, beta)) ) ) * -2
}

data(Pima.tr)
p.beta(X, start.beta)[1:3]

log10(p.beta(X, start.beta))



(as.numeric(Pima.tr$type) - 1) * log10(p.beta(X, start.beta))

devianza(X=X, y=(as.numeric(Pima.tr$type) - 1), beta=start.beta)


## 4. gradiente de devianza

dev.gradiente <- function(X, y, beta){
  p.1 <- length(beta) 
  gradiente <- rep(NA, p.1)
  X.1 <- cbind(1, X)
  gradiente[1] <- 2 * mean( p.beta(X, beta) - y )
  for(j in 2:p.1){
    #completar
      #gradiente[j] <- 2 * mean( (p.beta(X, beta)) )
      gradiente[j] <- 2 * mean( ( p.beta(X, beta) - y ) * X[ ,j-1] )
  }
  gradiente
}

nrow(X)
nrow(Pima.tr)
dev.gradiente(X, y=as.numeric(Pima.tr$type=='Yes'), rep(2,7))

## 5. descenso en gradiente
error <- 1
betas.dev <- list()
beta.ant <- rep(2,7)
beta.nueva <- rep(0,7)
y <- as.numeric(Pima.tr$type=='Yes')
eta <- 0.01
while( error >= 0.0001 ){
  beta.ant <- beta.nueva
  beta.nueva <- beta.ant - eta * dev.gradiente(X, y=y, beta.ant)
  #print(devianza(X, y, beta.nueva))
  error <- sum(abs(beta.ant - beta.nueva))
  betas.dev <- c(betas.dev, error)
 # print(beta.nueva)
}

beta.nueva

qplot(1:length(betas.dev), sapply(betas.dev, function(x) { log(x) }))

mod.x <- glm(type ~ ., data=data.frame( X.df, type=y ), family='binomial')
mod.glu <- glm(type ~ glu, data=data.frame( X.df, type=y ), family='binomial')
probs.prueba <- predict(mod.x, newdata=as.data.frame(X.prueba), type="response")
probs.prueba.glu <- predict(mod.glu, newdata=as.data.frame(X.prueba), type="response")

mean.todos <- mean((probs.prueba >= 0.5) != ( as.numeric( Pima.te$type ) -1 >= 0.5 ))
mean.glu <- mean((probs.prueba.glu >= 0.5) != ( as.numeric( Pima.te$type ) -1 >= 0.5 ))

coef(mod.x)
coef(mod.glu)

beta.nueva

###### Completar evaluar en muestra de preuba
X.prueba <- scale(Pima.te[2:7], 
  center=attr(X.df, 'scaled:center'),
  scale=attr(X.df, 'scaled:scale'))


#### Calcular predicciones de prueba y tasa de error

preds.prueba <- h(cbind(1, X.prueba) %*% beta.nueva)
preds.prueba.table <- table(preds.prueba >= 0.5, Pima.te$type)

?predict
probs.prueba <- predict(mod.x, newdata=as.data.frame(X.prueba), type="response")
probs.prueba.03 <- probs.prueba >= 0.3
probs.prueba.05 <- probs.prueba >= 0.5
probs.prueba.07 <- probs.prueba >= 0.7

str(probs.prueba)
plot(probs.prueba)

tabla.03 <- prop.table(table(probs.prueba.03, Pima.te$type), 2)
tabla.05 <- prop.table(table(probs.prueba.05, Pima.te$type), 2)
tabla.07 <- prop.table(table(probs.prueba.07, Pima.te$type), 2)

clasif.1 <- data.frame(
    corte = c('0.3','0.5','0.7'),
    tasa_falsos_pos=c(0.21, 0.11, 0.04),
    sensibilidad =c(0.82, 0.59, 0.40))
ggplot(clasif.1, aes(x=tasa_falsos_pos, y=sensibilidad,
                     label=corte)) + geom_point() + 
    geom_abline(intercept=0, slope=1) +
    xlim(c(0,1)) +ylim(c(0,1)) + geom_text(hjust=-0.3, col='red')+
    xlab('1-especificidad (tasa falsos pos)')


Pima.te.2$probs.prueba.2 <- predict(modelo.2, newdata=Pima.te, type="response")
tableplot(Pima.te.2, sortCol=probs.prueba.2)

library(ROCR)
Pima.te.2 <- Pima.te
Pima.te.2$probs.prueba <- predict(mod.x, newdata=Pima.te, type="response")
pred.rocr.1 <- prediction(Pima.te.2$probs.prueba, as.numeric(Pima.te.2$type)-1)
perf.1 <- performance(pred.rocr.1, measure = "sens", x.measure = "fpr")
plot(perf.1)

mean.05 <- mean((preds.prueba >= 0.5) != ( as.numeric( Pima.te$type ) -1 >= 0.5 ))
mean.05 <- mean((preds.prueba >= 0.5) != ( as.numeric( Pima.tr$type ) -1 >= 0.5 ))

#pred.rocr.2 <- prediction(Pima.te.2$probs.prueba.2, Pima.te.2$type)
#perf.2 <- performance(pred.rocr.2, measure = "sens", x.measure = "fpr")
#plot(perf.2, add = TRUE, col = "red")

#mean.05 <- mean((preds.prueba >= 0.5) != ( as.numeric( Pima.te$type ) -1 >= 0.5 ))
#p.05 <- prop.table(preds.prueba.table, 2)
#mean.03 <- mean( (preds.prueba >= 0.3) != ( as.numeric(Pima.te$type) -1 >= 0.3 ) )
#p.03 <- prop.table(table(preds.prueba >= 0.3, Pima.te$type))


