
data <- read.table("/Users/Alfonso/r-workspace/machine-learning/l5/hw2/housing.data", 
                              sep="\t")

colnames(data) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", 
                    "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

# 1: Separar en entrenamiento y pruebas
train.size <- 400
set.seed(240484)
train.idx <- sample(seq_len(nrow(data)), size=train.size)
train.set <- scale(data[train.idx, ][,1:13])
test.set <- scale(data[-train.idx, ][,1:13],
                  center=attr(train.set, 'scaled:center'),
                  scale=attr(train.set, 'scaled:scale'))
train.target <- scale(as.data.frame(data[train.idx, ])$MEDV)
#test.target <- as.data.frame(data[-train.idx, ])$MEDV
test.target <- scale(as.data.frame(data[-train.idx, ])$MEDV,
                     center=attr(train.target, 'scaled:center'),
                     scale=attr(train.target, 'scaled:scale'))
summary(train.set)
summary(test.set)

mod.x <- lm(medv ~ ., data=data.frame(train.set, medv=train.target))
str(mod.x)
mod.x$coefficients

x.preds <- cbind(1, test.set) %*% mod.x$coefficients

mean((x.preds - test.target)^2)

qplot(test.target, x.preds) + 
    xlab('Valores reales') + 
    ylab('Valores estimados') +
    geom_abline(xintercept=0, slope =1)

?glmnet
nrow(data)
length(data[train.idx, ])
    
library(glmnet)
?glmnet
dim(train.set)
length(train.target)

#train.target <- scale(as.data.frame(data[train.idx, ])$MEDV)
#test.target <- scale(as.data.frame(data[-train.idx, ])$MEDV,
#                     center=attr(train.target, 'scaled:center'),
#                     scale=attr(train.target, 'scaled:scale'))

mod.ridge <- glmnet(x=train.set, y=train.target, 
                    alpha=0, family='gaussian', 
                    intercept=T, nlambda=50)

plot(mod.ridge)
plot(mod.ridge, xvar="lambda", label=TRUE)

str(mod.ridge)

ridge.preds <- test.set %*% coef(mod.ridge)[,10][-1]
#modelo lineal: 0.2897059
#ridge: 0.9970792
#lasso: 0.3964291
mean((test.target - ridge.preds) ^ 2)

dat.r <- ldply(1:50, function(i){
    dat.prueba.r <- data.frame(i=i, lambda=mod.ridge$lambda[i], 
                               prob.hat.1=(test.set %*% coef(mod.ridge)[,i])
    )
    dat.prueba.r
})


str(dat.r)
head(dat.r)
str(test.target)

?ddply

rss <- function(y, y.hat){
    mean( (y - y.hat ) ^ 2 )
}

rss.prueba <- ddply(dat.r, c('i','lambda'), summarise, 
                         err=rss(test.target, prob.hat.1))

qplot(log(rss.prueba$lambda), rss.prueba$err)


cv.mod.ridge <- cv.glmnet(x=train.set, y=train.target, 
                          alpha=0, family='gaussian', intercept=F, 
                          nfolds=10, nlambda=50)

?cv.glmnet
str(cv.mod.ridge$glmnet.fit$beta)
cv.mod.ridge$lambda.min
cv.mod.ridge$lambda.lse

dim(coef(cv.mod.ridge))
dim(test.set)

x.cv.coefs <- coef(cv.mod.ridge)[-1] * attr(test.set, "scaled:scale") + 
    attr(test.set, "scaled:center")
#x.cv.preds <- test.set %*% x.cv.coefs
x.cv.preds <- test.set %*% coef(cv.mod.ridge)[-1]

qplot(test.target, x.cv.preds) + 
    xlab('Valores reales') + 
    ylab('Valores estimados') +
    geom_abline(xintercept=0, slope =1)

mean((test.target - x.cv.preds) ^ 2)

plot(cv.mod.ridge)

comp.dat <- data.frame(log.lambda=log(rss.prueba$lambda),
                       rss.prueba=rss.prueba$err,
                       rss.vc=cv.mod.ridge$cvm)

dim(comp.dat)
comp.dat.m <- melt(comp.dat, id.vars='log.lambda')
dim(comp.dat.m)

ggplot(comp.dat.m, aes(x=log.lambda, y=value, colour=variable)) +
    geom_point()




mod.lasso <- glmnet(x=train.set, y=train.target, 
                    alpha=1, family='gaussian', 
                    intercept=F, nlambda=50)

plot(mod.lasso)
plot(mod.lasso, xvar="lambda", label=TRUE)

lasso.preds <- test.set %*% coef(mod.lasso)[,10][-1]
mean((test.target - lasso.preds) ^ 2)

cv.mod.lasso <- cv.glmnet(x=train.set, y=train.target, 
                          alpha=1, family='gaussian', intercept=F, 
                          nfolds=10, nlambda=50)

plot(pcv.mod.lasso)

l.cv.coefs <- coef(cv.mod.lasso)[-1] * attr(test.set, "scaled:scale") + 
    attr(test.set, "scaled:center")
#x.cv.preds <- test.set %*% x.cv.coefs
l.cv.preds <- test.set %*% coef(cv.mod.lasso)[-1]

qplot(test.target, l.cv.preds) + 
    xlab('Valores reales') + 
    ylab('Valores estimados') +
    geom_abline(xintercept=0, slope =1)
