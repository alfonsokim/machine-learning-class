
library(ElemStatLearn)
data(prostate)
head(prostate)
?prostate

prostate.train <- subset(prostate, train)
nrow(prostate.train)
## 1: normalizacion

?mean
?sd

medias.train <- apply(prostate.train[,1:8], 2, mean)
sd.train <-  apply(prostate.train[,1:8], 2, sd)

head(prostate.train[,1:8], 5)
head(medias.train, 5)

X.df <- scale(prostate.train[,1:8], center=medias.train, scale = sd.train)
X.df
head(X.df)
X <- as.matrix(X.df)
head(X)

## 2. Predicciones
f.beta <- function(X, beta){
  # para hacerlo mas facilmente, agregamos columna de unos
  mat <- cbind(1, X) # para ajustar la dimension de X con la de beta
  preds <- mat %*% beta
  preds
}

f.beta(X=X, beta=rep(1,9))
f.beta(X=X, beta=c(2, rep(0, 8)))
f.beta(X, rep(1,9))[1:3]

## 3. rss
rss <- function(X, y, beta){
  mean((y-f.beta(X, beta))^2)
}

rss(X=X, y=prostate.train$lpsa, beta=rep(2,9))

## 4. gradiente de rss
rss.gradiente <- function(X, y, beta){
  # preparacion
  p.1 <- length(beta) 
  gradiente <- rep(NA, p.1)
  # como ejemplo, calculamos primera componente de gradiente
  sum.1 <- mean(f.beta(X, beta) - y)
  gradiente[1] <- 2*sum.1
  
  for(j in 2:p.1){
    ### rellenar aqui codigo
    gradiente[j] <- 2*mean( ( f.beta(X, beta) - y ) * X[,j-1] )
  }
  gradiente
}

rss.gradiente(X, y=prostate.train$lpsa, rep(2,9))

## 5. descenso en gradiente
beta.ant <- rep(2, 9)
beta.nueva <- rep(1, 9)
betas.rss <- list()
eta <- 0.01
step <- 1
train.data = prostate.train$lpsa

rss.beta.ant <- rss.gradiente(X, y=prostate.train$lpsa, beta.ant)
rss.beta.ant

while( step > 0.0001 ) {
  beta.ant <- beta.nueva
  beta.nueva <- beta.ant - eta * rss.gradiente(X, y=train.data, beta.ant)
  step <- abs(rss(X, y=train.data, beta.ant) - rss(X, y=train.data, beta.nueva))
  betas.rss <- c(betas.rss, step)
}

# install.packages("ggplot2", dependencies=TRUE)
# library(ggplot2)

qplot(1:length(betas.rss), sapply(betas.rss, function(x) { log(x) }))

?qplot
?sapply

summary(beta.nueva)
head(beta.nueva)

mod.x <- lm(lpsa ~ ., data=data.frame(X.df, lpsa=prostate.train$lpsa))
coef(mod.x)
summary(mod.x)
##6. predicciones y evaluacion de error

##Normalizamos con medias y desvest de entrenamiento
medias.test <- apply(prostate.train[,1:8], 2, mean)
sd.test <-  apply(prostate.train[,1:8], 2, sd)
prostate.test <- subset(prostate, !train)
X.df.prueba <- scale(prostate.test[,1:8], center=medias.test, scale=sd.test)
  ## rellenar lo que falta en esta linea

preds.prueba <- f.beta(X.df.prueba, beta.nueva)
summary(preds.prueba)
head(preds.prueba)

# como se ven las predicciones?
qplot(preds.prueba, prostate.test$lpsa) + geom_abline(intercept=0, slope=1)
mod.x.test <- lm(lpsa ~ ., data=data.frame(X.df.prueba, lpsa=prostate.test$lpsa))

# Evaluar error de prediccion
coef(mod.x.test)
preds.prueba
