
library(ElemStatLearn)
data(prostate)
head(prostate)
?prostate

prostate.train <- subset(prostate, train)

## 1: normalizaci??n
?scale
X.df <- scale(prostate.train[,1:8])
X.df
X <- as.matrix(X.df)

head(X)
head(prostate)

## 2. Predicciones
f.beta <- function(X, beta){
  mat <- cbind(1, X)
  preds <- mat %*% beta
  preds
}

ncol(X)

f.beta(X, rep(1,9))
joe <- cbind(1, X)
joe %*% rep(1,9)
f.beta(X, rep(1,9))[1:3]
f.beta(X, rep(1,9))

## 3. rss
rss <- function(X, y, beta){
  mean((f.beta(X, beta) - y)^2)
}

ncol(X); nrow(X)
ncol(as.matrix(prostate.train$lpsa)); nrow(as.matrix(prostate.train$lpsa))
ncol(as.matrix(rep(2,9))); nrow(as.matrix(rep(2,9)))
ncol(X); nrow(as.matrix(rep(2,9)))
rss(X = X, y = prostate.train$lpsa, beta=rep(2,9))

## 4. gradiente de rss

rss.gradiente <- function(X, y, beta){
  p.1 <- length(beta) 
  gradiente <- rep(NA, p.1)
  sum.1 <- mean(f.beta(X, beta) - y)
  gradiente[1] <- 2*sum.1
  for(j in 2:p.1){
    gradiente[j] <- 2*mean((f.beta(X, beta) - y)*X[, j-1])
  }
  gradiente
}

rss.gradiente(X, y=prostate.train$lpsa, rep(2,9))

## 5. descenso en gradiente
beta.ant <- rep(2,9)
beta.nueva <- rep(1,9)
eta <- 0.01
while(sum(abs(beta.ant-beta.nueva)) > 0.00001){
  beta.ant <- beta.nueva
  beta.nueva <- beta.ant - eta*rss.gradiente(X, y=prostate.train$lpsa, beta.ant)
  # print(beta.nueva)
}

beta.nueva

mod.x <- lm(lpsa ~ ., data=data.frame(X.df, lpsa=prostate.train$lpsa))
coef(mod.x)

##6. predicciones y evaluaci??n de error

##Normalizamos con medias y desvest de entrenamiento
prostate.test <- subset(prostate, !train)
X.df.prueba <- scale(prostate.test[,1:8],
                     center = attr(X.df,"scaled:center"),
                     scale =  attr(X.df,"scaled:scale"))

preds.prueba <- f.beta(X.df.prueba, beta.nueva)

# ??C??mo se ven las predicciones?
qplot(preds.prueba, prostate.test$lpsa) + geom_abline(intercept=0, slope=1)

# Evaluar error de predicci??n

(mean((preds.prueba-prostate.test$lpsa)^2))

## Error est??ndar de la estimaci??n:
sd( ( preds.prueba - prostate.test$lpsa ) ^ 2 ) / 
      sqrt( length( prostate.test$lpsa ) )

mean(abs(preds.prueba-prostate.test$lpsa))
