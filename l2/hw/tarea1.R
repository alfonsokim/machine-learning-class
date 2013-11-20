
install.packages("ROCR")
setwd("/Users/Alfonso/r-workspace/machine-learning/l2/hw") 
data <- as.matrix( read.table("housing.data", sep="\t") )

colnames(data) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", 
                    "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

nrow(data)
ncol(data)
head(data)
summary(data)

data <- as.data.frame(data)

# 1: Separar en entrenamiento y pruebas
train.size <- 400
set.seed(123)
train.idx <- sample(seq_len(nrow(data)), size=train.size)
train.set <- data[train.idx, ][,1:13]
test.set <- data[-train.idx, ][,1:13]
train.target <- as.data.frame(data[train.idx, ])$MEDV
test.target <- as.data.frame(data[-train.idx, ])$MEDV

nrow(train.set)
nrow(test.set)

### Normalizar ###
normal.train.set <- scale(train.set)
head(normal.train.set)
head(train.set)

# 2: Describir los sets
summary(train.set)
summary(test.set)

summary(data)

?geom_point
?aes

# 3: Modelo lineal
f.beta <- function(X, beta){
  mat <- cbind(1, X)
  preds <- mat %*% beta
  preds
}

start.beta=rep(1,ncol(normal.train.set)+1)

f.beta(X=normal.train.set, beta=start.beta)[1:3]

rss <- function(X, y, beta){
  mean( (y-f.beta( X, beta ) )^2 )
}

rss(X=normal.train.set, y=train.target, beta=start.beta)

rss.gradiente <- function(X, y, beta){
  p.1 <- length(beta) 
  gradiente <- rep(NA, p.1)
  sum.1 <- mean(f.beta(X, beta) - y)
  gradiente[1] <- 2*sum.1
  
  for(j in 2:p.1){
    gradiente[j] <- 2*mean( ( f.beta(X, beta) - y ) * X[ ,j-1] )
  }
  gradiente
}

# Normalizar la variable
rss(X=normal.train.set, y=train.target, beta=start.beta)

eta <- 0.01
error <- 1
betas.rss <- list()
beta.current <- rep(1,ncol(normal.train.set)+1)
beta.prev <- rep(2,ncol(normal.train.set)+1)

beta.current
beta.prev

# Descenso
while( error > 0.00001 ) {
  beta.prev <- beta.current
  beta.current <- beta.prev - eta * rss.gradiente(X=normal.train.set, y=train.target, beta=beta.prev)
  error <- sum(abs(beta.prev - beta.current))
  betas.rss <- c(betas.rss, error)
}

betas.rss
qplot(1:length(betas.rss), sapply(betas.rss, function(x) { log(x) }))

# Verificar resultados
?lm
?data.frame
mod.x <- lm(medv ~ ., data=data.frame(normal.train.set, medv=train.target))
mod.x$coefficients

my.coefficients <- t(beta.current)
dim(my.coefficients)
colnames(my.coefficients) <- c("Y", "CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", 
                            "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT")
my.coefficients
mod.x$coefficients
abs( my.coefficients - mod.x$coefficients )

# Evaluar contra el set de pruebas
train.mean <- apply(train.set, 2, mean)
train.sd <-  apply(train.set, 2, sd)
normal.test.set <- scale(test.set, center=train.mean, scale=train.sd)
head(normal.test.set)

test.preds <- f.beta(X=normal.test.set, beta.current)
test.preds
qplot(test.preds, test.target) + geom_abline(intercept=0, slope=1)

test.error <- rss( X=normal.test.set, y=test.target, beta=beta.current )
lm.test.error <- rss( X=normal.test.set, y=test.target, beta=mod.x$coefficients )

mean( ( test.preds - test.target ) ^2 ) 

## Error est??ndar de la estimacion:
sd( ( test.preds - test.target ) ^ 2 ) / 
  sqrt( length( test.target ) )

mean(abs(test.preds-test.target))

sqrt(lm.test.error)
sqrt(test.error)

## KNN ##
library(kknn)
?kknn
knn.train.set <- as.matrix(data[train.idx, ])
knn.test.set <- as.matrix(data[-train.idx, ])
typeof(data.frame(data))
is.matrix(knn.train.set)
m <- matrix(knn.train.set)
m
knn.5 <- kknn(MEDV ~ ., train=knn.train.set, 
              test=data.frame(knn.test.set, MEDV=test.target), k=5, kernel='rectangular')

mean(abs(knn.5$fitted.values - test.target))


knn.train.set <- as.matrix(data[train.idx, ])
knn.test.set <- as.matrix(data[-train.idx, ])

setwd("/Users/Alfonso/r-workspace/machine-learning/l2/hw") 
data <- as.matrix( read.table("housing.data", sep="\t") )

colnames(data) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", 
                    "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

train.size <- 400
set.seed(123123123)
train.idx <- sample(seq_len(nrow(data)), size=train.size)
train.set <- data[train.idx, ][,1:13]
test.set <- data[-train.idx, ][,1:13]
train.target <- as.data.frame(data[train.idx, ])$MEDV
test.target <- as.data.frame(data[-train.idx, ])$MEDV

### ===================================================

library(knitr)
?knitr
train.size <- 400
set.seed(240484)
data <- as.matrix( read.table("housing.data", sep="\t") )

colnames(data) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", 
                    "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

train.idx <- sample(seq_len(nrow(data)), size=train.size)
train.set <- as.data.frame(scale(data[train.idx, ]))
test.set <- as.data.frame(scale(data[-train.idx, ]))

knn.medv <- function (numK) {
    knn <- kknn(MEDV ~ ., train=train.set, 
                test=test.set, k=numK)
    mean( (knn$fitted.values - test.set$MEDV) ^ 2)
}
plot(sapply(c(1,5,10,20,50,100), knn.medv))

knn.1 = kknn(MEDV ~ ., train=train.set, test=test.set, k=1)
str(knn.1)
knn.1$fitted.values

knn.5 = kknn(MEDV ~ ., train=train.set, test=test.set, k=1)
knn.5$fitted.values

### ===================================================

knn.medv <- function (numK) {
    knn <- kknn(MEDV ~ ., train=data.frame(knn.train.set, MEDV=train.target), 
                  test=data.frame(knn.test.set, MEDV=test.target), 
                k=numK)
    mean((knn$fitted.values - test.target) ^ 2)
}

plot(sapply(c(1,5,10,20,50,100), knn.medv))

knn.1 = kknn(MEDV ~ ., train=data.frame(knn.train.set), 
             test=data.frame(knn.test.set, MEDV=test.target), 
             k=1)
knn.1$fitted.values
mean((knn.1$fitted.values - test.target) ^ 2)
mean(abs(knn.1$fitted.values - test.target))

summary(knn.1)

summary(knn.1$fitted.values)
summary(test.target)

#library(ggplot2)
qplot(knn.1$fitted.values, test.target) + geom_abline(intercept=0, slope=1)
 
### ============================================================

knn.5 = kknn(MEDV ~ ., train=data.frame(knn.train.set), 
             test=data.frame(knn.test.set, MEDV=test.target), 
             k=5, kernel='rectangular')
knn.5$fitted.values
mean((knn.5$fitted.values - test.target) ^ 2)
mean(abs(knn.5$fitted.values - test.target))

summary(knn.5$fitted.values)
summary(test.target)

qplot(knn.5$fitted.values, test.target) + geom_abline(intercept=0, slope=1)


### ============================================================

knn.20 = kknn(MEDV ~ ., train=data.frame(knn.train.set), 
             test=data.frame(knn.test.set, MEDV=test.target), 
             k=20, kernel='rectangular')
knn.20$fitted.values
mean((knn.20$fitted.values - test.target) ^ 2)
mean(abs(knn.20$fitted.values - test.target))

summary(knn.20$fitted.values)
summary(test.target)

qplot(knn.20$fitted.values, test.target) + geom_abline(intercept=0, slope=1)


### ============================================================

knn.50 = kknn(MEDV ~ ., train=data.frame(knn.train.set), 
              test=data.frame(knn.test.set, MEDV=test.target), 
              k=50, kernel='rectangular')
knn.50$fitted.values
mean((knn.50$fitted.values - test.target) ^ 2)
mean(abs(knn.50$fitted.values - test.target))

summary(knn.50$fitted.values)
summary(test.target)

qplot(knn.50$fitted.values, test.target) + geom_abline(intercept=0, slope=1)
