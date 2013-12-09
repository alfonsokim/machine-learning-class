adaboost.s <- function(){
library(plyr)
library(rpart)
set.seed(260572)
N <- 250
entrena <- rdply(N, rnorm(10))
entrena$y <- 2*(apply(entrena[,-1]^2, 1, sum) > 9)-1

prueba <- rdply(1000, rnorm(10))
prueba$y <- 2*(apply(prueba[,-1]^2, 1, sum) > 9)-1


stump.1 <- rpart(y~., data = entrena, maxdepth=1, method="class")

## Error de árbol con un corte:
mean(predict(stump.1, newdata = prueba, type="class")!=prueba$y)


arbol.1 <- rpart(y~., data = entrena, method="class", cp=0)
printcp(arbol.1)

arbol.pod <- prune(arbol.1, cp=0.002)
mean(predict(arbol.pod, newdata = prueba, type="class")!=prueba$y)

## Ada Boost con árboles de 1 corte

arbol.lista <- list()
M <- 2000
alpha <- numeric(M)
error <- numeric(M)
error.prueba <- numeric(M)
error.entrena <- numeric(M)
w <- rep(1,nrow(entrena))
preds.boost <- numeric(N)
preds.boost.entrena <- numeric(N)

for(i in 1:M){
    #print(i)
    arbol.lista[[i]] <- rpart(y~., data=entrena, 
        weights = w, maxdepth=1, method="class")
    preds <- predict(arbol.lista[[i]], newdata = entrena, type = "class")
    incorrectos <-  preds != entrena$y
    error[i] <- sum(incorrectos*w)/sum(w)
    alpha[i] <- log((1-error[i])/error[i])
    w <- w*exp(alpha[i]*(preds != entrena$y))
    # Función de predicción y evaluación
    preds.boost <- preds.boost + 0.5*alpha[i]*
        as.numeric(as.character(predict(arbol.lista[[i]], 
            newdata = prueba, type="class")))
    preds.boost.entrena <- preds.boost.entrena + 0.5*alpha[i]*
                as.numeric(as.character(predict(arbol.lista[[i]], 
                    newdata = entrena, type="class")))
    error.entrena[i] <- mean(sign(preds.boost.entrena) != entrena$y)
    error.prueba[i] <- mean(sign(preds.boost) != prueba$y)
    
}

mean(sign(preds.boost) != prueba$y)


plot(error.entrena, type="l", ylim=c(0,0.6))
lines(error.prueba, col="red")
abline(h=mean(predict(arbol.pod, newdata = prueba, type="class")!=prueba$y), col="blue")
abline(h=mean(predict(stump.1, newdata = prueba, type="class")!=prueba$y), col="green")
abline(h=0)
}