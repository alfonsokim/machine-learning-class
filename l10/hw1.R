
library(ISLR)
library(glmnet)
library(ggplot2)
data(Hitters)

?Hitters

dim(Hitters)

clean <- Hitters[!is.na(Hitters$Salary), ]

dim(clean)

?subset
train.1 <- head(clean, 100)
#salary.1 <- train.1$salary
#train.1$salary <- NULL

train.2 <- head(clean, 200)
#salary.2 <- train.2$salary
#train.2$salary <- NULL

test <- tail(clean, 63)


dim(train.1); dim(train.2); dim(test)


mod.x <- glm(Salary ~ ., data=train.1, family="gaussian")
mod.x$fitted.values
preds.x <- predict(mod.x)
err.x <- mean((preds.x - train.1$Salary)^2)
str(mod.x)

preds.test <- predict(mod.x, newdata=test)
err.test <- mean((preds.test - test$Salary)^2)

curva.ap.1 <- ldply(seq(50, 100, 5), function(i) {
    mod.1 <- glm(Salary ~ ., data=train.1[1:i, ], family="gaussian")
    error.entrena <- mean((predict(mod.1) - train.1[1:i, ]$Salary) ^ 2)
    error.valida <- mean((predict(mod.1, newdata=test) - test$Salary) ^ 2)
    data.frame(i = i, 
               error.entrena = error.entrena,
               error.valida = error.valida)
})

curva.ap.1.m <- melt(curva.ap.1, id.var = "i")
ggplot(curva.ap.1.m, aes(x = i, y = value, colour = variable)) +
    geom_point() + geom_line() + ylab("ECM")

# Rx: Aun hay mucho que hacer ya que las 2 curvas estan muy distantes una de la otra,
# incrementar los datos de entrenamiento va a mejorar la prediccion.
# Conviene probar modelos mas estructurados o mas varialbes de entrada, 
# ya que puede ser que la varianza sea alta.

# Ridge
?glmnet
salary.1 <- train.1$Salary
# Quitar variables no numericas
glm.train <- subset(train.1, select=-c(League, Division, NewLeague))
glm.train$Salary <- NULL
#train.1$salary <- NULL
mod.ridge <- glmnet(x=as.matrix(glm.train), y=as.matrix(salary.1), 
                       family="gaussian", alpha=0, intercept=T)
?glmnet
?predict.glmnet
plot(mod.ridge, xvar="lambda")
str(mod.ridge)

mod.ridge[[1]]

predict(mod.ridge, newx=as.matrix(glm.train))

set.seed(840424)
sample(runif(1000), 1)

## Modelos con 200 datos de entrenamiento

curva.ap.2 <- ldply(seq(50, 200, 10), function(i) {
    mod.2 <- glm(Salary ~ ., data=train.2[1:i, ], family="gaussian")
    error.entrena <- mean((predict(mod.2) - train.2[1:i, ]$Salary) ^ 2)
    error.valida <- mean((predict(mod.2, newdata=test) - test$Salary) ^ 2)
    data.frame(i = i, 
               error.entrena = error.entrena,
               error.valida = error.valida)
})

curva.ap.2.m <- melt(curva.ap.2, id.var = "i")
ggplot(curva.ap.2.m, aes(x = i, y = value, colour = variable)) +
    geom_point() + geom_line() + ylab("ECM")

