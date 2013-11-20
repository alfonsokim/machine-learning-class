library(ggplot2)

data <- read.table("/Users/Alfonso/r-workspace/machine-learning/l2/hw/housing.data", sep="\t")

colnames(data) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", 
                    "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")

summary(data)

train.size <- 400
set.seed(840424)
train.idx <- sample(seq_len(nrow(data)), size=train.size)
train.set <- data[train.idx, ]
test.set <- data[-train.idx, ]

summary(train.set)
summary(test.set)

for(i in 1:10){
    col.name = paste("DIS", i, sep=".")
    train.set[[ col.name ]] <- train.set$DIS^i
    test.set[[ col.name ]] <- test.set$DIS^i
}

setwd("/Users/Alfonso/r-workspace/machine-learning/l6/hw")
for(j in 1:10){
    vars <- c()
    errors <- c()
    for(k in 1:j){
        vars <- c(vars, paste("DIS", k, sep="."))
    }
    formula <- paste("NOX", paste(vars, collapse=" + "), sep=" ~ ")
    mod.j <- lm(as.formula(formula), data=train.set)
    nox.j <- paste("NOX", j, sep=".")
    test.set[[ nox.j ]] <- predict( mod.j, newdata=test.set )
    preds.j <- data.frame( DIS=test.set$DIS )
    preds.j$NOX <- predict( mod.j, newdata=test.set )
    #ggplot(test.set, aes(x=DIS, y=NOX)) +
    #    xlab("DIS") + 
    #    geom_jitter() + 
    #    ylab("NOX") +
    #    labs(title=formula) +
    #    geom_line(data=preds.j, col="blue", size=2)
    
    #ggsave(paste("plot", j, ".png", sep=""))
    
    mean.j <- mean(preds.j$NOX - test.set$NOX)
    errors <- c(errors, mean.j)
    cat("polinomio ", j, " - Error=", mean.j, "\n")
}

library(splines)
train.set$DIS
bs.1 <- bs(train.set$DIS, df=4)

lm.spline <- lm(train.set$NOX ~ bs.1)
lm.spline

plot(train.set$DIS, train.set$NOX)
points(train.set$DIS, fitted(lm.spline), col='red')


##---------------------------
bs.10 <- bs(train.set$DIS, df=10)

lm.spline.10 <- lm(train.set$NOX ~ bs.10)
lm.spline.10

plot(train.set$DIS, train.set$NOX)
points(train.set$DIS, fitted(lm.spline.10), col='red')1

?plyr
for(degree in 4:10){
    
}


#############################
# Notamos la variabilidad en los extremos:
sims <- rdply(50, function(i){
    x <- runif(200, -1, 1)
    y <- 0 + rnorm(200, 0, 1)
    datos.exp <- data.frame(y=y, ns(x, df=10, Boundary.knots=c(-0.8,0.8)))
    mod.1 <- lm(y ~ ., data=datos.exp)
    dat.graf <- datos.exp
    dat.graf$x <- x
    dat.graf$pred <- predict(mod.1)
    dat.graf
})
ggplot(sims, aes(x=x, y=pred, group=.n)) + geom_line(colour='red', alpha=0.6)+
    ylim(c(-3,3))
###########################


library(glmnet)
?cv.glmnet
length(train.set$NOX)

cv.train.set <- as.matrix( train.set[, names(train.set) != "NOX"] )
cv.train.target = as.matrix( train.set$NOX )
mod.1 <- cv.glmnet(x=cv.train.set, y=cv.train.target, intercept=F,
                   alpha=0, family='gaussian', nfolds=10, nlambda=50)
plot(mod.1)

coefs.1 <- coef(mod.1)
coefs.1

#Tomamos los de edad
## multiplicamos por la base de splines:
nox <- cv.train.set %*% coefs.1

qplot(SAheart$age, efecto.edad) + geom_line()+ geom_point()


mod.2 <- cv.glmnet(x=cv.train.set, y=cv.train.target,
                   nfolds=10, nlambda=50,
                   alpha=0, family='gaussian', type.measure='class')
plot(mod.2)
coef(mod.2)




h <- function(x) {
    exp(x)/(1 + exp(x))
}

xor <- function(x1, x2) {
    a.1 <- h(100 + x1)
    print(a.1)
    a.2 <- h(100 + x2 * -1)
    print(a.2)
    p <- h(a.1 * a.2)
    p
}

xor(0, 1)
xor(1, 1)


