library(randomForest)
library(ISLR)
library(plyr)
library(reshape)
library(ggplot2)

data(OJ)

## Sampling
set.seed(2404)
train <- sample(1:nrow(OJ), ceiling(nrow(OJ) * 0.7))
OJ$train <- FALSE
OJ$train[train] <- TRUE
OJ.train <- OJ[OJ$train, ]
OJ.test <- OJ[!OJ$train, ]
OJ$train <- NULL

?OJ

ncol(OJ)
nrow(OJ.train); nrow(OJ.test)
names(OJ)
#CH = Citrus Hill, MM = Minute Maid
?randomForest
oj.rf <- randomForest(Purchase ~ ., data=OJ.train, ntree=500,
                      importance=T, do.trace=T)

str(oj.rf)

plot(predict(oj.rf), OJ.train$Purchase)
plot(predict(oj.rf, newdata=OJ.test), OJ.test$Purchase)

sum(as.integer(predict(oj.rf, newdata=OJ.test) != OJ.test$Purchase))

?ldply
?expand.grid
test.grid <- expand.grid(ntree=c(200,400,600), mtry=seq(2, 10, 2))

results <- ddply(test.grid, c("ntree", "mtry"), function(d) {
    rf.x <- randomForest(Purchase ~ ., data=OJ.train, ntree=d$ntree,
                         mtry=d$mtry, importance=T)
    train.error <- sum(as.integer(predict(rf.x) != OJ.train$Purchase)) / nrow(OJ.train)
    test.error <- sum(as.integer(predict(rf.x, newdata=OJ.test) != OJ.test$Purchase)) / nrow(OJ.test)
    data.frame(
        train.error=train.error,
        test.error=test.error
    )
})

?melt
rf.melt <- melt(results, id.vars=c("ntree", "mtry"), 
                measure.vars=c("train.error", "test.error"))

ggplot(rf.melt, aes(x=ntree, y=value, colour=variable)) +
    geom_point() + geom_line() + ylab("Error")

## Mejor Arbol = 200 arboles a mtry = 6
oj.rf <- randomForest(Purchase ~ ., data=OJ.train, ntree=200,
                      mtry=6, importance=T)

oj.rf$importance
# +++ LoyalCH: 0.1335637320, 0.245243753, 0.175936278, 166.822569
# --- DiscCH: 0.0003053132, 0.002447186, 0.001127093, 2.654760
# LoyalCH: Customer brand loyalty for CH
# DiscCH: Discount offered for CH


