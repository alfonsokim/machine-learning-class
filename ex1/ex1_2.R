
install.packages('knitrBootstrap')

data.path <- "/Users/Alfonso/r-workspace/machine-learning/ex1/data/optdigits"

set.seed(145849)
?read.csv
train.data <- read.csv(paste(data.path, "optdigits.tra", sep="/"), 
                       na.strings=0, header = F)
test.data <- read.csv(paste(data.path, "optdigits.tes", sep="/"), 
                      na.strings=0, header = F)

train.data[ is.na( train.data ) ] <- 0
test.data[ is.na( test.data ) ] <- 0

digits.cols <- paste("c", 1:64, sep="")

names(train.data) <- c(digits.cols, "i")
names(test.data) <- c(digits.cols, "i")

train.data$i <- as.factor(train.data$i)
test.data$i <- as.factor(test.data$i)

'
tdf <- train.data
nrow(tdf)
unique(tdf$i)
tdf$class[tdf$i == 0] <- rep(1, nrow(subset(tdf, i == 0)))
tdf$class[tdf$i != 0] <- rep(0, nrow(subset(tdf, i != 0)))
tdf$class <- as.factor(tdf$class)
tdf <- tdf[, c(digits.cols, "class")]

summary(tdf$class)
?glm.fit
model.test <- glm(class ~ ., data=tdf, family="binomial",
                  control=glm.control(maxit=100))

preds.1 <- fitted(model.test) > 0.5
head(preds.1)
test.coefs <- coef(model.test)
test.coefs[is.na(test.coefs)] <- 0

tab.confusion.entrena <- table(preds.1, tdf$class) 
tab.confusion.entrena

?predict.glm
preds <- predict(model.test, newdata=test.data, type="response")
ttt <- test.data
ttt$preds <- preds

subset(ttt, (preds > 0.5 & i != 0))[c("i", "preds")]
'

# Crear data frames de entrenamiento
library(glmnet)
models <- data.frame(i=1)
predictions <- data.frame(i=test.data$i)
?cv.glmnet

train.df <- train.data
train.df$class <- as.factor(ifelse(train.df$i == 7, 1, 0))
head(train.df$i, 20)
head(train.df$class, 20)

class <- as.factor(ifelse(train.df$i == 7, 1, 0))

library(glmnet)
glm.models <- lapply(0:9, function(i){
    train.df <- train.data
    class <- as.factor(ifelse(train.df$i == i, 1, 0))
    train.df <- train.df[, c(digits.cols)]
    model <- cv.glmnet(x=as.matrix(train.df), y=class,
                       alpha = 1, intercept = T, family="binomial",
                       nfolds = 10, nlambda = 100)
    model
})

class(glm.models)
class(glm.models[[1]])
?save
save("glm.models", file="~/r-workspace/machine-learning/ex1/glm_models.Rdata")

model.0 <- glm.models[[1]]
str(model.0)
?predict.glmnet

test.df.0 <- test.data
class.0 <- as.factor(ifelse(test.df.0$i == 0, 1, 0))
test.df.0 <- test.df.0[, c(digits.cols)]
m0.preds <- predict(model.0, newx=as.matrix(test.df.0), type="response", s=model.0$lambda.1se)

m0.results <- data.frame(preds=m0.preds, class=class.0)

head(m0.results)
tp <- nrow(subset(m0.results, X1 >= 0.5 & class == 1))
fp <- nrow(subset(m0.results, X1 >= 0.5 & class == 0))

?prediction
m0.performance <- performance(prediction(m0.preds, class.0), "sens", "fpr") 
digits.glm.preds = prediction(predictions$p8, ifelse(predictions$i == 8, 1, 0))

str(m0.performance)

error.rates <- c()
for(i in 1:9){
    model.i <- glm.models[[i]]
    test.df <- test.data
    class.i <- as.factor(ifelse(test.df$i == i, 1, 0))
    test.df <- test.df[, c(digits.cols)]
    preds.i <- predict(model.i, newx=as.matrix(test.df), 
                       type="response", s=model.i$lambda.1se)
    results.i <- data.frame(preds=preds.i, class=class.i)
    fp.i <- nrow(subset(results.i, X1 >= 0.5 & class == 0)) / nrow(test.df)
    error.rates <- c(error.rates, fp.i)
}

'
for( c in 0:9 ){
    train.df <- train.data
    train.df$class[train.df$i == c] <- rep(1, nrow(subset(train.df, i == c)))
    train.df$class[train.df$i != c] <- rep(0, nrow(subset(train.df, i != c)))
    train.df$class <- as.factor(train.df$class)
    #y <- as.factor(train.df$class)
    #train.df <- train.df[, c(digits.cols)]
    train.df <- train.df[, c(digits.cols, "class")]
    model <- glm(class ~ ., data=train.df, family="binomial",
                 control=glm.control(maxit=100))
    #model <- cv.glmnet(x=as.matrix(train.df), y=y, nfolds=10, family"binomial", alpha=1)
    models[paste("c", c, sep="")] <- model
    
    # Pruebas
    test.df <- test.data
    test.df$class[test.df$i == c] <- rep(1, nrow(subset(test.df, i == c)))
    test.df$class[test.df$i != c] <- rep(0, nrow(subset(test.df, i != c)))
    test.df$class <- as.factor(test.df$class)
    test.df <- test.df[, c(digits.cols, "class")]
    pred <- predict(model, newdata=test.df, type="response")
    
    predictions[paste("p", c, sep="")] <- pred
    
}
'

test.df$class <- as.factor(ifelse(test.df$i == 0, 1, 0))
test.df <- test.df[, c(digits.cols, "class")]
pred <- predict(model, newdata=test.df, type="response")



test.df <- test.data
test.df$class <- as.factor(ifelse( test.df$i == 0, 1, 0) )

predict(models[,1], newdata=test.df)



str(models)
summary(models[1])
models[1]$model

subset(predictions, i == 8 & p8 >= 0.5)[, c("p8", "i")]
subset(predictions, i != 8 & p8 >= 0.5)[, c("p8", "i")]

## Redes Neruonales
library(nnet)
source("~/r-workspace/machine-learning/ex1/nnet_plot.R")
?nnet
names(train.data)
train.data$i <- as.factor(train.data$i)
digits.net.1 <- nnet(i ~ ., data = train.data, size = 64,
                     decay = 0.5, maxit = 5000, MaxNWts = 10000)

?predict.nnet

plot(digits.net.1)
str(digits.net.1)

net.1.preds <- predict(digits.net.1, newdata=test.data)
dim(net.1.preds)

which.max(net.1.preds[200,])

net.preds.1 <- apply(net.1.preds, 1, which.max) - 1
net.preds.1[51]
test.data$i[51]

test.data$pred <- net.preds.1
subset(test.data, pred != i)[, c("pred", "i")]

nrow(subset(test.data, pred != i)) / nrow(test.data)

head(net.preds.1, 100)
tab.1 <- table(pred.1, zip.test[, 1])

tune.params <- expand.grid(decay = c(0.1, 0.5, 1, 10), 
                           size = c(64, 32, 16))

#tune.params <- expand.grid(decay = c(0.5), 
#                           size = c(64))

library(plyr)
?dlply
digits.models <- dlply(tune.params, c("decay", "size"), function(df) {
    digits.red <- nnet(i ~ ., data = train.data, 
                         size = df$size, decay = df$decay, 
                         maxit = 5000, MaxNWts = 10000)
    digits.red
})


test.error <- ldply(digits.models, function(mod) {
    net.probs <- predict(mod, newdata=test.data)
    net.preds <- apply(net.probs, 1, which.max) - 1
    
    results <- data.frame(i=test.data$i, pred=net.preds)
    
    nrow(subset(results, pred != i)) / nrow(results)
})

library(ggplot2)
ggplot(test.error, aes(x=decay, y=V1, colour=factor(size), group=size)) + 
    geom_line() + 
    geom_point() + 
    scale_x_log10(breaks = c(0.1, 0.5, 1, 10))


# La mejor red es la de 32 neuronas en la capa opculta, con decay=1
best.net <- digits.models[[8]]

load('~/Documents/MCC/Aprendizaje/Examen/ambiente_redes_con_roc_glm.RData')
# Graficar las pesos de la capa oculta
str(best.net)
length(best.net$wts)
coef(best.net)

hidden.weights <- ldply(0:9, function(i) {
    best.net$wts[(65+(31*i)):(65+31+(65*i))]
})



library(ROCR)
?prediction

roc.8.df <- test.data
roc.8.df$class[roc.8.df$i == 8] <- rep(1, nrow(subset(roc.8.df, i == 8)))
roc.8.df$class[roc.8.df$i != 8] <- rep(0, nrow(subset(roc.8.df, i != 8)))
roc.8.df$class <- as.factor(roc.8.df$class)
#roc.8.df <- roc.8.df[, c(digits.cols, "class")]

?prediction
head(predict(best.net, roc.8.df))
(as.data.frame(roc.8.df$class))

predict(best.net, roc.8.df)[,8]

digits.net.preds <- prediction(apply(predict(best.net, roc.8.df), 1, which.max), 
                               as.data.frame(roc.8.df$class))

net.roc <- performance(digits.net.preds, "sens", "fpr") 

head(predictions$p8)
head(ifelse(predictions$i == 8, 1, 0), 30)
head(predictions$i, 30)
digits.glm.preds = prediction(predictions$p8, ifelse(predictions$i == 8, 1, 0))
glm.roc <- performance(digits.glm.preds, "sens", "fpr")

plot(net.roc)
plot(glm.roc, add=T, col="red")

